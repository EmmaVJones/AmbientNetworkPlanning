source('global.R')
source('multipleDependentSelectizeArguments.R')

shinyUI(fluidPage(theme = "yeti.css", 
                  shinyjs::useShinyjs(),
                  div(
                    id = "loading_page",
                    h1("Loading...")),
                  hidden(
                    div(
                      id = "main_content",
                      navbarPage("BRRO Monitoring Network Planning App: 2020"),
                      tabsetPanel(
                        tabPanel('Interactive Watershed Map',
                                 watershedMapUI("watershedMapQA")),
                        tabPanel("Watershed Query",
                                 br(),
                                 column(3,
                                        wellPanel(
                                          dynamicSelectInput("level1", "Level 1 Code", multiple = TRUE),
                                           #dynamicSelectInput("level3", "Level 3 Code", multiple = TRUE),
                                           dynamicSelectInput("basin", "Major Basin", multiple = TRUE),
                                           dynamicSelectInput("watershed", "Watershed", multiple = TRUE))),
                                 column(9,stationMapUI("watershedMap")),
                                 br(),
                                 column(12,
                                        DT::dataTableOutput("WSHDselectedElementsTable",height = '400px'))),
                        tabPanel("StationID Results", 
                                 br(),
                                 column(3,
                                        wellPanel(dynamicSelectInput("stationID", "StationID", multiple = TRUE))),
                                 column(9, stationMapUI("stationMap")),
                                 br(),
                                 column(12,DT::dataTableOutput("STATIONIDselectedElementsTable"))),
                        tabPanel("Historical Monitoring (Sampling Statistics Table)",
                                 h4('Search by Station Sampling Statistics'),
                                 DT::dataTableOutput("allDataTable")),
                        tabPanel("Monitoring Data (Raw Data Table)",
                                 h4('Search Raw Data by StationID'),
                                 textInput('stationID','Enter StationID',placeholder = 'e.g. 2-JKS000.00'),
                                 DT::dataTableOutput("rawDataTable"))
                        
                                 
                      )
                    ))
                  ))
                                   
                      
                     