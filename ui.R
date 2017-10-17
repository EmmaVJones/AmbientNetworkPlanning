shinyUI(fluidPage(theme = "yeti.css", 
                  shinyjs::useShinyjs(),
                  div(
                    id = "loading_page",
                    h1("Loading...")
                  ),
                  hidden(
                    div(
                      id = "main_content",
                      navbarPage("Monitoring Network Planning App"),
                      
                      sidebarPanel(
                        h3("Query By:"),
                        dynamicSelectInput("level1", "Level 1 Code", multiple = TRUE),
                        dynamicSelectInput("level3", "Level 3 Code", multiple = TRUE),
                        dynamicSelectInput("basin", "Major Basin", multiple = TRUE),
                        dynamicSelectInput("watershed", "Watershed", multiple = TRUE),
                        br(),hr(),br(),
                        dynamicSelectInput("stationID", "StationID", multiple = TRUE)
                      ),
                      
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Table",
                                   DT::dataTableOutput("selectedElementsTable")),
                          tabPanel("Map", 
                                   stationMapUI("stationMap")
                                   #bootstrapPage(div(class="outer",
                                    #                 tags$style(type ="text/css",".outer {position: fixed; top: 75px; left: 0; right: 0; bottom: 0; overflow-y: scroll; padding: 0}"),
                                     #                br(),br(),))
                                                     #leafletOutput("stationMap")
                          )
                        ))
                    ))
                  
))
                               
            