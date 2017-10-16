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
                        h5("Query By:"),
                        selectizeInput('year',h5(strong('Year')),choices=BRRO$FDT_YEAR,multiple=T),
                        selectizeInput('basin',h5(strong('Major Basin')),choices=BRRO$Basin,multiple=T),
                        selectizeInput('wshd',h5(strong('Watershed')),choices=BRRO$Huc6_Vahu6,multiple=T),
                        selectizeInput('level1',h5(strong('Level 1 Code')),choices=BRRO$STA_LV1_CODE,multiple=T),
                        selectizeInput('level3',h5(strong('Level 3 Code')),choices=BRRO$STA_LV3_CODE,multiple=T),
                        selectizeInput('surveyProgram',h5(strong('Survey Program')),choices=BRRO$FDT_SPG_CODE,multiple=T),
                        selectizeInput('regionalOffice',h5(strong('Regional Office')),choices=BRRO$STA_REC_CODE,multiple=T),
                        selectizeInput('stationID',h5(strong('Station ID')),choices=BRRO$FDT_STA_ID,multiple=T)
                        
                      ),
                      
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Map", 
                                   bootstrapPage(div(class="outer",
                                                     tags$style(type ="text/css",".outer {position: fixed; top: 75px; left: 0; right: 0; bottom: 0; overflow-y: scroll; padding: 0}"),
                                                     br(),br(),
                                                     leafletOutput("stationMap")))
                          ),
                          tabPanel("Table",
                                   DT::dataTableOutput("selectedElementsTable"))
                        ))
                    ))
                  
))
                               
            