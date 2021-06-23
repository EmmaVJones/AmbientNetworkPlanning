shinyUI(fluidPage(tags$head(
  tags$style(
    HTML(".shiny-notification {position:fixed; top: calc(60%); left: calc(10%); }"))),
  theme= "yeti.css",
  navbarPage("BRRO Water Quality Monitoring Sampling Network Planning Tool", id = 'someID',  # key for passing URL to specific Tab
             tabPanel('How To',
                      p('This application assists the BRRO region with the development of an annual water quality monitoring sample network.'),
                      p('Data is rebuilt annually from the lastest IR conventionals pull and any additional sample years not included in that
                        data window.'),
                      h4('Users may upload an .xlsx spreadsheet with upcoming sample year station information. The application will plot any stations
                         listed in a field named StationID on a sheet named Sample Plan. If new stations are included on this list (not yet in CEDS), 
                         the application can only plot these stations using the corresponding Latitude and Longitude fields in the Sample Plan sheet.'),
                      h4('Please contact Emma Jones regarding any application questions.')),
             tabPanel('Map'),
             tabPanel('Data')
  )))