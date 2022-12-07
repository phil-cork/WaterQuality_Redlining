
# imports
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(scales)

# Define UI for application
ui <- fluidPage(

    # title and credit
    titlePanel("Investigating Redlining's Impact on Safe Drinking Water Access"),
    p("Phil Cork, Fall 2022"),
    br(),
    
    # introduction content
    p("Through the Justice40 Initiative, the Biden Administration has made historic investments to address environmental injustices and the climate crisis while prioritizing systematically and historically marginalized communities. With $50 billion set aside for improving access to safe drinking water, it is crucial these funds go to the communities that need them most."),
    
    p("While safe drinking water was recently added as a consideration for the Justice40 Initiative in its 1.0 release as of November 2022, there remains a lack of comprehensive data about where communities get their water and to what extent these water systems have been historically funded to relieve environmental injustices nationwide."),
    
    p("Until this data becomes available, can we find alternative data sources to help fill in the gaps?"),
    
    p("To this end, this analysis considers the potential usefulness of deploying historic redlining data as a proxy for the flow of historic funding to identify ways to prioritize improving safe drinking water access in the future."),
    
    # first body section
    h2("Evaluating Safe Drinking Water Access by State"),
    
    p("One hurdle in this process is that funding and reporting for utility infrastructure occurs at the community water system (CWS) level, often not reporting exactly who will benefit from the investments or those who are harmed by insufficient drinking water quality. What data is available is made publicly available through the Safe Drinking Water Information System (SDWIS) and includes, among other details, the reported violations for each CWS as well as how many are served within the system, even if these numbers are widely considered imprecise at best."),
    
    p("There remains a significant lack of high-quality data regarding the service area boundaries of these community water systems, making it difficult to know exactly where each CWS operates and the communities represented. As such, all analysis is currently limited to a subset of states where sufficiently accurate geospatial data exists. Throughout this exploratory stage, we focus predominantly on water systems in Texas, while also providing other state’s results for comparison and further exploration."),
    
    p("For instance, we begin by examining how many health-related violations have affected communities in a given state over the last five years. Total Violations show how many individual violations were reported during that time across each CWS in the state. Violations Days are counted as the sum of days for each violation (so if a CWS had three simultaneous violations, if each day they were all present would count as three 'violation days'). One can also explore what percent of the systems across the state experienced any violations during that time or how many of the violations were ‘persistent,’ meaning they lasted more than 30 consecutive days."),
    br(),
    br(),
    
    ## 1. Data Viz 1 UI
    # Sidebar for choosing variable to plot
    sidebarLayout(
        sidebarPanel(
          helpText("Compare states by health violations present in community water systems over the last five years"),
          # allow users to select which variable to appears in the accompanying bar plot
          selectInput("var", 
                      label = "Choose a variable to display",
                      choices = list("Total Violations",
                                     "Total Persistent Violations",
                                     "Total Days of Violation",
                                     "Percent of Systems with Violations"),
                      selected = "Total Violations")
          ), # close sidebar

        # Show a plot of the generated bar graph
        mainPanel(
           plotOutput("violation_barPlot")
        ) #close mainPanel
        
    ), #close sidebarLayout
    
    br(),
    br(),
    p("In the case of Texas, it is clearly a situation worth examining further, as it leads in Total Violations and many of its violations are persistent. It is also second in the number of Violation Days. Interestingly, it is actually near the bottom of the list in terms of how many systems experiencing violations. This contrast highlights that the violations are likely concentrated in a select number of CWS that should be further prioritized for investment."),
    
    p("Can historic redlining data help us better understand who is negatively impacted by these drinking water quality issues?"),
    
    # Introducing redlining data
    h2("Mapping Redlining Data"),
    
    p("The historic practice of redlining continues to have disproportionate negative affects on communities of color and other minority populations. The roots of this form of intentional, systemic disenfranchisement date back to the 1930s, when the Home Owners' Loan Corporation created maps of cities and towns across the country, grading portions of the city based on their ‘mortgage security.’ The neighborhoods deemed to be of the lowest risk when lenders decided who should receive loans were denoted with an ‘A’ grade and color-coded in green. Meanwhile, the neighborhoods receiving 'D' grades were labeled as ‘hazardous’ and color-coded in red."),

    # generate HTML color-coordinated list of HOLC grades
    HTML("<strong>HOLC Grades</strong>
          <ul>
         <li><span style='background-color:#00EE76;color:white;padding-left:5px;padding-right:5px;'>A: 'Best'</span></li>
         <li><span style='background-color:#4F94CD;color:white;padding-left:5px;padding-right:5px;'>B: 'Still Desirable'</span></li>
         <li><span style='background-color:#FFD700;padding-left:5px;padding-right:5px;'>C: 'Definitely Declining'</span></li>
         <li><span style='background-color:#EE2C2C;padding-left:5px;padding-right:5px;'>D: 'Hazardous'</span></li>
         </ul>"),
    
    p("Lenders were considered prudent to refuse loans in these areas or to only provide investment on a conservative basis. These grades took into account not only the area’s mix of residential and industrial development, but the racial and ethnic makeup of its communities as well. These discriminatory loan practices, grounded in rampant, systemic racism, directly prevented the accumulation of intergenerational wealth and stymied the flow of both private and public capital. Further, the legacy of redlining can be observed across a number of environmental vulnerabilities including flood risk, extreme heat, and others, suggesting it may also play a role in understanding safe drinking water access. "),

    # display interactive map
    h3("Community Water Systems & HOLC Grades"),
    p("In this interactive map, we can directly observe how the HOLC grade maps overlay with community water systems in urban areas across the state of Texas. In San Antonio, for instance, we can see many of the areas in the center of the city were denoted with C or D grades, with the suburbs receiving more favorable marks. The same is true in Austin to the northeast. The map also includes other Texas cities that can be explored. "),
    br(),
    br(),
    
    ## 2. Data Viz 2 UI
    leafletOutput("red_sab_map"),
    
    br(),
    br(),
    p("It is also worth noting all of the areas that these water systems serve that do not overlap with the redlining map. This divergence is to be expected, given the expansion of these urban regions since the grades were initially assigned some 90 years ago. Even so, given that historic funding followed these grades over time, it is still possible that evaluating the overlap between these water systems and historic redlining practices can help fill the data gaps we currently experience."),

    # connecting redlining and violations data section
    h2("Combining Redlining & Drinking Water Quality"),
    
    p("Having explored both the prevalence of drinking water quality issues over the last five years and the overlap between urban community water systems and historic redlining practices, we can dive into the question of how related the two may be."),

    p("To make these comparisons, we first consider how much of each system overlaps with redlining data, as seen in the map above, and then divide that area by the grade each neighborhood received, giving us a percentage of how much of the water system, as it existed at the time, served neighborhoods with each HOLC grade."),

    p("Then, we can compare these percentages across each system in a given state, as depicted below. The systems for which the CWS name is in red denotes a system in which a persistent (lasting more than thirty consecutive days) has been reported in the last five years. We once again focus on the state of Texas, with two other states available for comparison and context."),
    br(),
    br(),
    
    ## 3. Data Viz 3 UI
    # Sidebar for choosing which state to plot for the redlining & violations viz
    sidebarLayout(
      sidebarPanel(
        helpText("Compare community water systems within a state by overlapping area with HOLC graded neighborhoods."),
        
        selectInput("state", 
                    label = "Choose a state to display",
                    choices = list("PA", "TX", "UT"),
                    selected = "TX")
      ), # close sidebar
      
      # Show a plot of the generated bar graph
      mainPanel(
        plotOutput("holc_barPlot")
      ) #close mainPanel
      
    ), #close sidebarLayout
    
    br(),
    br(),
    p("In Texas, we note three systems that have experienced persistent health violations, two of which overlap with only redlined communities. Similarly, many of the systems in Pennslyvania reporting health violations overlap with predominantly redlined neighborhoods. In Utah, for contrast, however, Salt Lake City is the only city that reports persistent health violations and two CWS that serve completely redlined communities do not."),

    # recommendations and conclusion sections
    h2("Recommendations"),
    p("With these findings in mind, there are two recommendations."),
    h5("1. Enhance Data Collection"),
    p("First, local, state, and federal governments should commit to enhancing data gathering capabilities and increased transparency. This analysis is decisively hindered by many states having insufficient data quality necessary for such work. It is possible a more definitive relationship would be present were the data more comprehensive or that more targeted investment recommendations could be made if the data were more granular and precise."),
    h5("2. Prioritize Redlined Communities"),
    p("Second, with the data that is available, decision makers should deploy it to prioritize investing in safe drinking water where communities have been typically marginalized. In this case, there are clearly a handful of cities in Texas where historically under-invested communities lack consistent access to safe drinking water. The state can and should provide resources to those communities to ensure they apply for grants and are able to receive part of the historic investments being made in drinking water infrastructure. Thankfully, this process is underway, now that both water pollution and redlining are being folded into the Justice40 Initiative definition of whether a community will receive priority funding."),

    h2("Conclusion"),
    p("The Biden administration has made admirable steps in pursuing environmental justice and tackling the existential crisis of climate change. The scope of these challenges require broad-reaching initiatives and holistic solutions powered by decisions informed by the best available data. While the tools and methods for these efforts are being developed, it is crucial to continue moving forward and implementing changes with the currently available resources. This report shows that while an imperfect measure, the historic redline practices can serve as a starting point worth considering for ensuring federal funds are improving lives of those most in need.")
    
) #close ui fluidpage

### SERVER
server <- function(input, output) {
  
    ## 1. VIOLATIONS BY STATE VIZ
    
    # import and pre-process
    states_df <- read.csv("data/states_df.csv")
    states_df$texas <- as.factor(states_df$texas)

    # create bar graph that responds to user's chosen variable
    output$violation_barPlot <- renderPlot({

        # translate user selected option into variable name in the dataframe
        x_var <- case_when(
          input$var == "Percent of Systems with Violations" ~ "pct_violations",
          input$var == "Total Persistent Violations" ~ "total_violation30",
          input$var == "Total Violations" ~ "total_violations",
          input$var == "Total Days of Violation" ~ "violation_days"
        )
        
        # store column name to maintain modular format
        y_var <- "state_code"
        
        # plot a bar graph of the states with the x-axis determined by user input. sort states by chosen variable.
        # set fill to highlight texas
        ggplot(states_df, aes(x=.data[[x_var]], y=reorder(.data[[y_var]], .data[[x_var]]), fill=texas)) + 
          geom_bar(stat="identity") + 
          # add text to show bar's true value
          geom_text(aes(label=comma(.data[[x_var]]), color=texas), hjust="inward") + 
          scale_color_manual(values=c("black", "lightgrey")) + 
          scale_fill_manual(values=c("lightgrey", "black")) + 
          labs(y="", x=input$var,
               title=paste(input$var, "by State, 2017-2022")) + 
          theme_minimal() + 
          theme(panel.grid.major.y = element_blank(), panel.grid.minor=element_blank(), legend.position="None")
    })
    
    ## 2. REDLINING & SAB MAP
    
    # import spatial datasets
    red <- st_read("data/shapefiles/redline_tx.shp")
    sab <- st_read("data/shapefiles/overlap_sab.shp")

    
    # make PWS names more readable
    sab$pws_name <- str_to_title(sab$pws_name, locale = "en")
    sab$pws_name <- str_replace(sab$pws_name, " Of ", " of ")
    
    # set up HOLC-grade based color scheme for redlining data polygons
    factpal <- colorFactor(c("springgreen2", "steelblue3", "gold", "firebrick2"), red$holc_grade)
    
    # create leaflet interactive map to display
    output$red_sab_map <- renderLeaflet({
      leaflet() %>% 
        # import background layer
        addProviderTiles(providers$CartoDB.Positron) %>%
        # add water system shapes
        addPolygons(data=sab,
                    # add the default service area bound polygons
                    weight = 1, fillOpacity = 0.5, fillColor='lightblue', color='blue',
                    # create popup that shows SAB name with population and connection details
                    popup = ~paste(pws_name, "</br>", "Population Served: ", comma(population), "</br>", "Connections: ", comma(service_co))) %>% 
        # add redlining neighborhood shapes
        addPolygons(data=red,
                    weight=1, fillOpacity = 0.75, color='darkgrey',
                    # set color to match HOLC grades
                    fillColor=~factpal(holc_grade),
                    popup = ~paste(city, "</br>", "HOLC Grade: ", holc_grade)) %>%
        
        addLegend("bottomright", pal = factpal, values=red$holc_grade,
                  title = "HOLC Grades",
                  opacity = 1
        ) %>%
        
        # set the default view
        # 29.424349, -98.491142
        setView(lng = -98.491142, lat = 29.42349, zoom = 9)
    })
    
    ## 3. REDLINING VIZ

    # import overlapping redline and water service area bounds dataset
    red_sab_ratios <- read.csv("data/red_sab_ratios.csv")
    
    # create stacked percentage bar chart
    output$holc_barPlot <- renderPlot({
      
      # create a properly ordered list of water system names based on chosen state
      # remove duplicates and arrange as they will appear in the data viz
      red_text <- subset(red_sab_ratios, state_code==input$state) %>%
        select(pws_name, violation_30, red_percent) %>%
        distinct() %>%
        arrange(red_percent)
      # store boolean outputs for whether violations are present to correctly bold text
      face_options <- ifelse(red_text$violation_30 == 1, 'bold', 'plain')
      
      # store state name for dynamic title
      state_name <- case_when(
        input$state == "TX" ~ "Texas",
        input$state == "UT" ~ "Utah",
        input$state == "PA" ~ "Pennsylvania"
      )
      
      # remove warning about text color not being fully supported
      suppressWarnings(
      # subset dataset to only chosen state, reorder CWS by how much of each overlaps with D grade neighborhoods
      ggplot(subset(red_sab_ratios, state_code==input$state), aes(y=reorder(pws_name, red_percent), x=intersection_percent, fill=holc_grade)) + 
        geom_bar(position='fill', stat='identity') +
        # create dynamic title
        labs(title=paste("Community Water Systems in",state_name,"by Neighborhood Grade"),
             subtitle="*Systems in bold report persistent health violations",
             caption="SDWIS Health Violations, 2017-2022",
             x="Percent Overlap with HOLC Graded Neighborhoods", y="") + 
        # assign colors based on HOLC grades defined above
        scale_fill_manual(values=c("springgreen2", "steelblue3", "gold", "firebrick2"), name="HOLC Grade") + 
        # add manual white lines above bars for easier reading
        scale_x_continuous(breaks = seq(0, 1, by = .25), label=label_number(scale=100, suffix="%")) + 
        geom_vline(xintercept = .25, color='white') + 
        geom_vline(xintercept = .5, color='white') + 
        geom_vline(xintercept = .75, color='white') + 
        theme_minimal() + 
        # remove noisy gridlines and assign bold face to water systems based on violations as defined above
        theme(panel.grid.major.y = element_blank(), panel.grid.minor=element_blank(),
              axis.text.y = element_text(face=face_options), plot.subtitle = element_text(face = "bold"))
      ) # close suppress warnings
    })
      
}

# Run the application 
shinyApp(ui = ui, server = server)
