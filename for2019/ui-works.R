source('global.R')
source('multipleDependentSelectizeArguments.R')

shinyUI(fluidPage(theme = "yeti.css", 
                  shinyjs::useShinyjs(),
                  div(
                    id = "loading_page",
                    h1("Loading...")
                  ),
                  hidden(
                    div(
                      id = "main_content",
                      navbarPage("Monitoring Network Planning App: 2019"),
                      
                      sidebarPanel(
                        #h3("Query By:"),
                        radioButtons("radio",h3("Query By:"),choices=c("Watershed Information","StationID"),selected="Watershed Information"),
                        hr(),
                        conditionalPanel(condition="input.radio == 'Watershed Information'",
                                         dynamicSelectInput("level1", "Level 1 Code", multiple = TRUE),
                                         dynamicSelectInput("level3", "Level 3 Code", multiple = TRUE),
                                         dynamicSelectInput("basin", "Major Basin", multiple = TRUE),
                                         dynamicSelectInput("watershed", "Watershed", multiple = TRUE)),
                        conditionalPanel(condition="input.radio == 'StationID'",
                                         dynamicSelectInput("stationID", "StationID", multiple = TRUE))
                        
                        
                      ),
                      
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Watershed Results",
                                   stationMapUI("watershedMap"),
                                   DT::dataTableOutput("WSHDselectedElementsTable")),
                          tabPanel("StationID Results", 
                                   stationMapUI("stationMap"),
                                   DT::dataTableOutput("STATIONIDselectedElementsTable")),
                          tabPanel("All Stations Table",
                                   h4('Search by Station Sampling Statistics'),
                                   DT::dataTableOutput("allDataTable"),
                                   hr(),
                                   h4('Search Raw Data by StationID'),
                                   textInput('stationID','Enter StationID',placeholder = 'e.g. 2-JKS000.00'),
                                   DT::dataTableOutput("rawDataTable"))
                        ))
                    ))
                  
))

