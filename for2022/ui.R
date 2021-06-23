shinyUI(fluidPage(tags$head(
  tags$style(
    HTML(".shiny-notification {position:fixed; top: calc(60%); left: calc(10%); }"))),
  theme= "yeti.css",
  navbarPage("BRRO Water Quality Monitoring Sampling Network Planning Tool", id = 'someID',  # key for passing URL to specific Tab
             # tabPanel('How To',
             #          p('This application assists the BRRO region with the development of an annual water quality monitoring sample network.'),
             #          p('Data is rebuilt annually from the lastest IR conventionals pull and any additional sample years not included in that
             #            data window.'),
             #          h4('Users may upload an .xlsx spreadsheet with upcoming sample year station information. The application will plot any stations
             #             listed in a field named StationID on a sheet named Sample Plan. If new stations are included on this list (not yet in CEDS), 
             #             the application can only plot these stations using the corresponding Latitude and Longitude fields in the Sample Plan sheet.'),
             #          h4('Please contact Emma Jones regarding any application questions.')),
             tabPanel('Explore Network',
                      sidebarPanel( width = 2,
                        helpText('The map to the right autogenerates based on sampling information available from the previous IR window and 
                                 any available sample information available in ODS. You may upload a spreadsheet (.xlsx) of proposed sample
                                 locations for the upcoming sampling year to visualize the proposed monitoring network.'),
                        fileInput('userUploadNetwork','Upload Proposed Monitoring Plan', accept = c(".xlsx"))),
                      mainPanel(width = 10,
                        tabsetPanel(
                          tabPanel('Map',
                                   verbatimTextOutput('test'),
                                   
                                   leafletOutput('regionalMap',width = "100%", height = 600),
                                   h4('Selected Station Information'),
                                   DT::dataTableOutput('stationDetails')),
                          tabPanel('Uploaded Data')
                        )
                      )),
             tabPanel('Data')
  )))