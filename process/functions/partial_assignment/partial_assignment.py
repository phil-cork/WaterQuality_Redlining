#!/usr/bin/env python3
# -*- coding: utf-8 -*-


# Import packages
import numpy as np
import pandas as pd
import geopandas as gpd
from shapely.strtree import STRtree
from tqdm.auto import tqdm


def get_shape_matches(target_sf: pd.DataFrame, source_sf: pd.DataFrame, target_sf_uid: str, source_sf_uid: str):
    '''
    Helper function transforms a geopands dataframe into a tree structure for faster querying.
    Compares each shape in the tree structure against a set of shapes to be queried against.
    Returns a dataframe of the unique ids from each dataframe for which an intersection exists.
    This can then be fed into functions for calculating intersections and overlaps much faster than using the regular dataframe structure.
    
    Parameters
    ----------
    target_sf : Geopanda dataframe
        The shapefile of primary interest to see how much of each observation in source_sf overlaps with target_sf.
        This dataframe is transformed into a tree data structure.
    source_sf : Geopanda dataframe
        The dataframe of shapes for which each observation in target_sf is looped through to see how much they overlap.
        This can be thought of as where the variables of interest come from.
        IE if we want to merge custom shape files with census data to understand the population,
        the census shapes would be the source and the custom shape files would be the target.
        This dataframe becomes the query for each geometry compared.
    target_sf_uid, source_sf_uid: string
        For each dataframe respectively, the unique id that denotes each observation.
        Returned in the final dataframe to match overlapping shapefiles.
    '''

    # store the target geometry shapes in a tree structure
    tree = STRtree(target_sf['geometry'])

    # while loop through the source shape file, which serve as the query shapes
    all_matches = []
    i = 0

    pbar = tqdm(desc='Get Shape Matches', total=len(source_sf))
    while i < len(source_sf):
        
        # store each source shape as an individual shape to query the entire tree
        query_geom = source_sf['geometry'][i]
        
        # define a dictionary of indices for the shapes in the tree for referencing the original dataframe
        index_by_id = dict((id(shape), i) for i, shape in enumerate(target_sf['geometry']))

        # for each shape in the tree, query the shape to see if it intersects with the query shape
        for shape in tree.query(query_geom):
            if shape.intersects(query_geom):
                # store the unique ids for each query shape and tree shape that match from both dataframe parameters
                match = (source_sf[source_sf_uid][i], target_sf.iloc[index_by_id[id(shape)], target_sf.columns.get_loc(target_sf_uid)])
                all_matches.append(match)
        i += 1
        pbar.update(1)
    pbar.close()
    # convert the list of tuples to a dataframe and return it        
    df_matches = pd.DataFrame(all_matches, columns =[source_sf_uid, target_sf_uid])

    return df_matches


def get_size_of_intersection(row):
    '''
    Helper function calculates the area of intersection between two geometries.
    Built to be used in an apply() function to calculate area of intersection for every row of a dataframe.
    
    Parameters
    ----------
    row : int
        Row # of the dataframe to perform calculations on
    '''

    polygon = gpd.GeoSeries(row['geometry_x'], crs="EPSG:4269").intersection(gpd.GeoSeries(row['geometry_y'], crs="EPSG:4269"))
    return polygon.to_crs(3857).area


def get_percent_overlap(row):
    '''
    Helper function calculates the percent overlap between two polygons, given size of intersection
    Built to be used in an apply() function to calculate % overlap for every row of a dataframe.
    Assumes that the user wants the denomenator for calculating the percent overlap to be the geometry from source_sf.
    
    Parameters
    ----------
    row : int
        Row # of the dataframe to perform calculations on
    '''

    source_sf_polygon = gpd.GeoSeries(row['geometry_y'], crs="EPSG:4269")
    return round((row['intersection_size']/source_sf_polygon.to_crs(3857).area)*100, 2)


def calculate_area_percent(target_sf: pd.DataFrame, source_sf: pd.DataFrame, target_sf_uid: str, source_sf_uid: str) -> pd.DataFrame:
    '''
    Takes a pair of geopandas dataframes of intersecting geometries and calculates the percent overlap
    between each polygon pair between the two dataframes appended to a merged dataset that retain the target_sf's geometry.

    IE if we want to merge custom shape files with census data to understand the population within the custom shape,
    the census shapes would be the source and the custom shape files would be the target.

    Parameters
    ----------
    target_sf : Geopanda dataframe
        The shapefile of primary interest to see how much of each observation in source_sf overlaps with target_sf.

    source_sf : Geopanda dataframe
        The dataframe of shapes for which each observation in target_sf is looped through to see how much they overlap.
        This can be thought of as where the variables of interest come from.
        
    target_sf_uid ,source_sf_uid: string
        The column name in the dataset with the presumed larger shapefiles that is used to join source_sf to the other two files.
    '''

    # create a dataframe of matched unique ids for overlapping shapes
    tree_matched = get_shape_matches(target_sf, source_sf, target_sf_uid, source_sf_uid)

    # merge the shapefiles together using the matches as keys for both datamframes
    combined = target_sf.merge(tree_matched, how = "inner", on = target_sf_uid)
    combined = combined.merge(source_sf, how = "inner", on = source_sf_uid)
    #combined = combined.dropna()

    tqdm.pandas()
    # calculate the area of the intersecting polygon for each set of shapes
    tqdm.pandas(desc="Get Size of Intersection")
    combined['intersection_size'] = combined.progress_apply(lambda row : 
                                       get_size_of_intersection(row), axis=1)
    # calculate what percent of the larger shape the intersection polygon overlaps
    tqdm.pandas(desc="Get Percent Overlap")
    combined['percent_overlap'] = combined.progress_apply(lambda row:
                                                 get_percent_overlap(row), axis = 1)
    # drop source_sf's geometry after calculating overlaps
    combined= combined.drop(columns = ["geometry_y"])
    # rename target_sf's geometry column for downstream analysis
    combined = combined.rename(columns = {"geometry_x":"geometry"})
    
    return combined


def aggregate_variables(merged_df:pd.DataFrame, target_sf_uid:str, var_col:str, function:str, partial:bool, percent_col="") -> pd.DataFrame:
    '''
    Takes in a dataframe that has already run through the calculate_area_percent() function.
    Returns a dataframe with the unique id as one column and a new column with the variable of interest calculated as a weighted sum or average.

    Parameters
    ----------
    merged_df: pandas Dataframe
        The dataframe resulting from calculate_area_percent() that includes the merged source and target columns and the percent to which they overlap.

    target_sf_uid: string
        Name of the column in merged_df that is the unique identifier preserved from the target shapefile

    var_col: string
        Name of the column in merged_df that contains the variable of interest to be summed up

    percent_col: string (optional)
        Name of the column in merged_df that contains the percent overlap of the merged shapes.

    function: string
        Which aggregation technique should be used. Accepts "sum" or "mean".
    '''
    
    var_est_list = []
    
    # for each target_uid that is unique in the list of all unique ids from the target sahpefile
    target_id_list = merged_df[target_sf_uid].unique().tolist()
    
    pbar = tqdm(desc=str('Aggregate Weighted Variable: ' + var_col), total=len(target_id_list))
    for target_uid in target_id_list:
        # subset to only each target_id's matched rows
        temp_df = merged_df[merged_df[target_sf_uid]==target_uid]

        col_name = function + '_' + var_col

        if partial:
            col_name = 'wgtd_' + col_name
            if function == "sum":
                var_est = int(round(sum(temp_df[var_col]*(temp_df[percent_col]/100)),0))
            elif function == "mean":
                var_est = int((temp_df[var_col]*(temp_df[percent_col])).mean())
        
        else:
            if function == "sum":
                var_est = int(sum(temp_df[var_col]))
            elif function == "mean":
                var_est = int(temp_df[var_col].mean())

        # store the target uid and variable estimate
        row = (target_uid, var_est)
        var_est_list.append(row)
    
        pbar.update(1)
    pbar.close()
    # transform the list of tuples to a dataframe and return
    var_est_df = pd.DataFrame(var_est_list, columns=[target_sf_uid, col_name])

    return var_est_df