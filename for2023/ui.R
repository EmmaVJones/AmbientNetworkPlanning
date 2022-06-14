shinyUI(fluidPage(tags$head(
  tags$style(
    HTML(".shiny-notification {position:fixed; top: calc(60%); left: calc(10%); }"))),
  theme= "yeti.css",
  navbarPage("Water Quality Monitoring Sampling Network Planning Tool", id = 'someID',  # key for passing URL to specific Tab
             # tabPanel('How To',
             #          h2('This application assists the BRRO region with the development of an annual water quality monitoring sample network.'),
             #          p('Data is rebuilt annually from the lastest IR conventionals pull and any additional sample years not included in that
             #            data window.'),
             #          p('Users may upload an .xlsx spreadsheet with upcoming sample year station information. Any number of sheets may be stored in the
             #             .xlsx, but the application will only work with the ', span(strong('sheet named `Sample Plan`')),
             #            '. The `Sample Plan` sheet must contain the following
             #             field: ', span(strong('`Monitoring Year`, `StationID`, `Latitude`, `Longitude`, `Station Type`, and `Annual Frequency`.'))), 
             #          p("`Monitoring Year` is a numeric field containing the upcoming monitoring year (e.g. 2022, only one year is able to be processed
             #             at this point). The application will plot any stations listed in a field named `StationID` on the sheet named Sample Plan. 
             #             If new stations are included on this list (not yet in CEDS), the application can only plot these stations using 
             #             the corresponding `Latitude` and `Longitude` fields in the Sample Plan sheet. `Station Type` contains information 
             #            on the sample group code (e.g. TR, AW, etc.). `Annual Frequency` is a numeric field represented the project number of
             #            samples during the upcoming monitoring year."),
             #          h4('Please contact Emma Jones regarding any application questions.')),
             tabPanel('Explore Network',
                      sidebarPanel( width = 2,
                                    h5(strong('Instructions')),
                                    helpText("Begin by selecting the DEQ Region you want to review and then press the `Begin Analysis` button."),
                                    selectInput('deqRegion', "DEQ Regional Office", 
                                                choices = c('BRRO', 'NRO', 'PRO', 'SWRO', 'TRO', 'VRO')),
                                    actionButton('begin', 'Begin Analysis'),
                                    br(),
                                    uiOutput('helpText1'),
                                    conditionalPanel(condition = "input.begin != 0",
                                      fileInput('userUploadNetwork',strong('Upload Proposed Monitoring Plan'), accept = c(".xlsx")),
                                      helpText('Use the layers control drop down on the left side of the map to reveal additional datasets. Click on stations
                                 to show more sample information below the map.'),
                                      helpText("The VAHU6 layers correspond to individual IR windows that highlight data density within each VAHU6 by IR window.
                                 Green VAHU6's have at least 10 sample events in a given IR window. Yellow VAHU6's have 1-9 sample events in a given
                                 IR window. Red VAHU6's have no sample events or stations in a given IR window."),
                                      helpText(strong("VAHU6 layers by IR window are recalculated based on user uploaded data to visualize how monitoring networks
                                        change VAHU6 projected data density."))) ),
                      mainPanel(width = 10,
                                tabsetPanel(
                                  tabPanel('Map',
                                           verbatimTextOutput('test'),
                                           uiOutput('helpText2'),
                                           leafletOutput('regionalMap',width = "100%", height = 600),
                                           h4('Selected Station Information'),
                                           DT::dataTableOutput('stationDetails'),
                                           br(), br(), br()),
                                  tabPanel('Uploaded Data',
                                           DT::dataTableOutput('uploadedDataset'))
                                )
                      ))
  )))