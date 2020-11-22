## Load Packages 
    library(shiny)
    library(shiny.semantic)
    library(semantic.dashboard)
    library(ggplot2)
    library(plotly)
    library(leaflet)
    
## Load Source Code
    source("data_man_source.R")
    
## User-Interface 
ui <- dashboardPage(
    ## Dashboard Header
    dashboardHeader(title = "US Traffic Accidents", color = "purple", inverted = TRUE,
                    menu_item(text = "Source Code", icon = icon("github"), href = "https://github.com/thomasjwise")),
    ## Dashboard SideBar 
    dashboardSidebar(
        size = "", color = "violet",
        sidebarMenu(
        menuItem(tabName = "main", "Main", icon = icon("home")),
        menuItem(tabName = "map", "Map Locations", icon = icon("map signs")),
        menuItem(tabName = "traffic_stats", text = "Traffic Statistics", icon = icon("chart bar")),
        menuItem(tabName = "spectraff_stats", text = "Location Statistics", icon = icon("chart bar")),
        menuSubItem(action_button(input_id = "update", 
                                  label = "Load Map")),
        menuSubItem(newtab = FALSE,
                    multiple_checkbox(
                        input_id = "us_state",
                        label = h5("State of Accident"),
                        choices = c("Alabama", "Arkansas", "Arizona", "California",
                                    "Colorado", "Connecticut", "District of Columbia", "Delaware",
                                    "Florida", "Georgia", "Iowa", "Idaho",
                                    "Illinois", "Indiana", "Kansas", "Kentucky",
                                    "Louisiana", "Massachusetts", "Maryland", "Maine",
                                    "Michigan", "Minnesota", "Missouri", "Mississippi",
                                    "Montana", "North Carolina", "North Dakota", "Nebraska",
                                    "New Hampshire", "New Jersey", "New Mexico", "Nevada",
                                    "New York", "Ohio", "Oklahoma", "Pennsylvania",
                                    "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
                                    "Texas", "Utah", "Virginia", "Vermont",
                                    "Washington", "Wisconsin", "West Virgina", "Wyoming"),
                        choices_value = c("AL", "AR", "AZ", "CA",
                                          "CO", "CT", "DC", "DE",
                                          "FL", "GA", "IA", "ID",
                                          "IL", "IN", "KS", "KY",
                                          "LA", "MA", "MD", "ME",
                                          "MI", "MN", "MO", "MS", 
                                          "MT", "NC", "ND", "NE",
                                          "NH", "NJ", "NM", "NV",
                                          "NY", "OH", "OK", "PA",
                                          "RI", "SC", "SD", "TN",
                                          "TX", "UT", "VA", "VT",
                                          "WA", "WI", "WV", "WY"),
                        selected = c("AL", "AR", "AZ", "CA",
                                     "CO", "CT", "DC", "DE",
                                     "FL", "GA", "IA", "ID",
                                     "IL", "IN", "KS", "KY",
                                     "LA", "MA", "MD", "ME",
                                     "MI", "MN", "MO", "MS", 
                                     "MT", "NC", "ND", "NE",
                                     "NH", "NJ", "NM", "NV",
                                     "NY", "OH", "OK", "PA",
                                     "RI", "SC", "SD", "TN",
                                     "TX", "UT", "VA", "VT",
                                     "WA", "WI", "WV", "WY"))))),
    
    dashboardBody(
        tabItems(
            selected = 1,
            tabItem(
                tabName = "main",
                fluidRow(
                    box(width = 8,
                        title = "Information",
                        color = "blue", ribbon = TRUE, title_side = "top left",
                        column(width = 8, 
                               p("Understanding Traffic Accidents, especially within the US, can be important in influencing Public Policy, 
                                 and the way in which organisations communicate to the public about traffic stafey, such as the influence of weather and 
                                 time of day. In this interactive dashboard application, you can see the locations of traffic accidents between 
                                 Feburary 2016 and June 2020, whilst specifying factors such as reported accident severity, catergories weather condition, 
                                 year, time of day and accident direct."),
                               br(),
                               p("Please note, this is still a work in progress, and forms part of an open portifilio who's source code 
                                 and be accessed following the link in the header. This dashboard using a sample of 10,000 from the orginial data (referenced right),
                                 this use of a sample is to demonstrate proof of concept for this application."))
                    ),
                    box(width = 8,
                        title = "Links & References",
                        color = "purple", ribbon = TRUE, title_side = "top left",
                        column(width = 8, 
                               h1("References"),
                               p("Data used within this dashboard is from:", 
                                 a("USA Accident Database, 2016-2020,", 
                                    href = "https://www.kaggle.com/sobhanmoosavi/us-accidents"),
                                 "this database contains approximately 3.5 million 
                                 records between Feburary 2016 and June 2020. Although 
                                 this database is liscienced under the", a("CC BY-NC-SA 4.0", 
                                href = "https://creativecommons.org/licenses/by-nc-sa/4.0/"), 
                                "this data was collected as part of the following studies:"),
                               br(),
                               p("- Moosavi, Sobhan, Mohammad Hossein Samavatian, Srinivasan Parthasarathy, and Rajiv Ramnath.", 
                                 a("A Countrywide Traffic Accident Dataset", href = "https://arxiv.org/abs/1906.05409"), 
                                 "2019."),
                               p("- Moosavi, Sobhan, Mohammad Hossein Samavatian, Srinivasan Parthasarathy, Radu Teodorescu, and Rajiv Ramnath.", 
                                 a("Accident Risk Prediction based on Heterogeneous Sparse Data: New Dataset and Insights.",
                                   href = "https://arxiv.org/abs/1909.09638"), 
                                   "In proceedings of the 27th ACM SIGSPATIAL International Conference on Advances in Geographic Information Systems, ACM, 2019."),
                               br(),
                               h1("Packages"),
                               p("This application uses the following applications:"),
                               a("Shiny", href = "https://cran.r-project.org/web/packages/shiny/index.html"),
                               br(),
                               a("Semantic Shiny", href = "https://cran.r-project.org/web/packages/shiny.semantic/index.html"),
                               br(),
                               a("Semantic Dashboard", href = "https://cran.r-project.org/web/packages/semantic.dashboard/index.html"),
                               br(),
                               a("Tidyverse", href = "https://cran.r-project.org/web/packages/tidyverse/index.html"),
                               br(),
                               a("plotly", href = "https://cran.r-project.org/web/packages/plotly/index.html"),
                               br(),
                               a("leaflet", href = "https://cran.r-project.org/web/packages/leaflet/index.html"),
                               br()

                        )
                    )
                )
            ),
            tabItem(
                tabName = "map",
                fluidRow(
                        ## Map Output 
                        box(width = 16,
                            title = "Mapped Locations",
                            color = "blue", ribbon = TRUE, title_side = "top left",
                            collapsible = FALSE,
                            column(width = 16, 
                                   leafletOutput("mymap"),
                                   br())),
                        
                        br(),
                        br(),
                        
                        ## Parameter Selection
                        
                        ## Box 1: Accident Severity
                        box(width = 4,
                            title = "Accident Severity",
                            color = "red", ribbon = TRUE, title_side = "top left",
                            collapsible = FALSE,
                            multiple_checkbox(
                                input_id = "acc_sev",
                                label = h3("Incident Severity"),
                                choices = c("Low", "Low-Moderate", 
                                            "High-Moderate", "High"),
                                choices_value = c(1, 2, 3, 4),
                                selected = 1)),
                        
                        ## Box 2: Weather Conditions
                        box(width = 4,
                            title = "Weather Conditions",
                            color = "green", ribbon = TRUE, title_side = "top left",
                            collapsible = FALSE,
                            multiple_checkbox(
                                input_id = "w_cat",
                                label = h3("Weather Condition"),
                                choices = c("Ash", "Clear", "Cloudy", "Dust",
                                            "Fog", "Freezing Rain", "Hail",
                                            "Heavy Rain", "Heavy Snow", "Heavy Thunderstorms",
                                            "Light Hail", "Light Rain", "Light Snow",
                                            "Lightly Cloudy", "Mist", "Other", "Rain",
                                            "Smoke", "Snow", "Thunderstorm", "Unknown", 
                                            "Very Cloudy", "Windy"),
                                selected = "Clear")
                            ),
                        
                        ## Box 3: Accident Type
                        box(width = 4,
                            title = "Dates of Interest",
                            color = "yellow", ribbon = TRUE, title_side = "top left",
                            collapsible = FALSE,
                            multiple_checkbox(
                                input_id = "date",
                                label = h3("Dates of Interest"),
                                choices = c("2016", "2017", "2018",
                                            "2019", "2020"),
                                choices_value = c("2016", "2017", "2018",
                                                  "2019", "2020"),
                                selected = c("2016", "2017", "2018",
                                             "2019", "2020"))),
                        
                        ## Box 4: Additional Notes / Options 
                        box(width = 4,
                            title = "Additional Factors",
                            color = "orange", ribbon = TRUE, title_side = "top left",
                            collapsible = FALSE,
                            multiple_checkbox(
                                input_id = "civ_twi",
                                label = h3("Time of Day"),
                                choices = c("Unknown", "Day", "Night"),
                                choices_value = c("", "Day", "Night"),
                                selected = "Day"),
                            br(),
                            multiple_checkbox(
                                input_id = "road_side",
                                label = h3("Accident Direction"),
                                choices = c("Left", "Right"),
                                choices_value = c("L", "R"),
                                selected = c("L", "R"))
                            )
                            
                            
                        )
                ),
            
            tabItem(
                tabName = "traffic_stats",
                fluidRow(
                    p(h2("General Traffic Statistics")),
                    box(width = 16,
                        title = "Influence of Weather",
                        color = "blue", ribbon = TRUE, title_side = "top left",
                        collaspible = TRUE,
                        plotOutput("weathergraph")),
                    box(width = 8, 
                        title = "Influence of Time of Day",
                        color = "green", ribbon = TRUE, title_side = "top left",
                        collaspible = TRUE,
                        plotOutput("todgraph")),
                    box(width = 8, 
                        title = "Influence of Year",
                        color = "green", ribbon = TRUE, title_side = "top left",
                        collaspible = TRUE,
                        plotOutput("yeargraph"))
                        
                )),
            
            tabItem(
                tabName = "spectraff_stats",
                fluidRow(
                    p(h2("Specific Traffic Statistics"))
                ))
                    
        )
    ), theme = "cerulean"
)

server <- shinyServer(function(input, output, session) {
    
    ## Data Manipulation and Loading
    location.dat.2 <- reactiveVal(location.dat)

    location.session.dat <- eventReactive(input$update, {
        res <- location.dat.2() %>% 
            filter(State %in% input$us_state) %>%
            filter(Severity %in% input$acc_sev) %>%
            filter(weather_cat %in% input$w_cat) %>%
            filter(Civil_Twilight %in% input$civ_twi) %>% 
            filter(year %in% input$date) %>% 
            filter(Side %in% input$road_side) 
        
        res

    })
    
    ## Weather Output Graph
    output$weathergraph <- renderPlot({
        ggplot(data = location.dat, 
               mapping = aes(x = weather_cat, fill = Severity)) + 
            geom_bar(position = "dodge") + 
            labs(x = "Weather Category", y = "Accident Count", 
                 title = "Influence of Weather on Amount of Accidents") + 
            theme_minimal()
        })
    
    ## Time of Day Output Graph
    output$todgraph <- renderPlot({
        ggplot(data = location.dat, 
               mapping = aes(x = Civil_Twilight, fill = Severity)) + 
            geom_bar(position = "dodge") + 
            labs(x = "Time of Day", y = "Accident Count", 
                 title = "Influence of Time of Day on Amount of Accidents") + 
            theme_minimal()
    })
    
    ## Year Output Graph
    output$yeargraph <- renderPlot({
        ggplot(data = location.dat, 
               mapping = aes(x = year, fill = Severity)) + 
            geom_bar(position = "dodge") + 
            labs(x = "Year", y = "Accident Count", 
                 title = "Influence of Year on Amount of Accidents") + 
            theme_minimal()
    })
    
    ## Map Output
    output$mymap <- renderLeaflet({
        leaflet() %>%
        addTiles() %>%
        addMarkers(data = location.session.dat(), 
                   lat = ~latitude, lng = ~longitude,
                   popup = ~popuplabel)
        })
})

shinyApp(ui, server)