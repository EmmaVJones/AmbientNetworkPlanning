# R 3.5.1

source('global.R')
source('multipleDependentSelectizeArguments.R')

HUCS <- readOGR('data','BRRO')
HUCS_EVJ <- readOGR('data','HUCS_EVJ')

conventionalsBRRO <- read_csv('data/conventionalsBRRO2011_Nov2018.csv')
conventionalsBRRO$FDT_DATE_TIME2 <- as.POSIXct(conventionalsBRRO$FDT_DATE_TIME, format="%m/%d/%y %H:%M")





shinyServer(function(input, output, session) {
  
  # display the loading feature until data
  load_data()
  
  # Query By Selectize arguments
  the_data <- reactive({BRRO_sites2})
  
  level1_filter <- shiny::callModule(dynamicSelect, "level1", the_data, "STA_LV1_CODE" )
  #level3_filter <- shiny::callModule(dynamicSelect, "level3", level1_filter, "STA_LV3_CODE" )#level1_filter()$STA_LV3_CODE)
  basin_filter <- shiny::callModule(dynamicSelect, "basin", level1_filter, "Basin")#level3_filter, "Basin")#level3_filter()$Basin)
  watershed_filter <- shiny::callModule(dynamicSelect, "watershed", basin_filter, "Huc6_Vahu6")#year_filter()$Basin)
  
  callModule(stationMap, "watershedMap",watershed_filter,HUCS)
  
  output$WSHDselectedElementsTable <- DT::renderDataTable({
      DT::datatable(watershed_filter(),
                    options = list(scrollX = TRUE,scrollY = "400px",
                                   pageLength = nrow(watershed_filter())))})
  
  
  stationID_filter <- shiny::callModule(dynamicSelect, "stationID", the_data, "FDT_STA_ID" )
  
  callModule(stationMap, "stationMap",stationID_filter,HUCS)
  
  output$STATIONIDselectedElementsTable <- DT::renderDataTable({
      DT::datatable(stationID_filter(),
                    options = list(scrollX = TRUE, scrollY = "400px",
                                   pageLength = nrow(stationID_filter())))})

  
  output$allDataTable <- DT::renderDataTable({
    DT::datatable(BRRO_sites2,
                  options = list(scrollX = TRUE, scrollY = "400px",
                                 pageLength = nrow(BRRO_sites2)))})
  
  output$rawDataTable <- DT::renderDataTable({
    req(input$stationID)
    
    dat <- filter(conventionalsBRRO, FDT_STA_ID %in% input$stationID)
    
    DT::datatable(dat,
                  options = list(scrollX = TRUE, scrollY = "400px",
                                 pageLength = nrow(dat)))})
  
  
  callModule(watershedMap, "watershedMapQA",HUCS_EVJ,HUCS)
  
  
})