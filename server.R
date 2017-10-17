source('global.R')
source('multipleDependentSelectizeArguments.R')

HUCS <- readOGR('data','BRRO')





shinyServer(function(input, output, session) {
  
  # display the loading feature until data
  load_data()
  
  # Query By Selectize arguments
  the_data <- reactive({BRRO_sites})
  
  level1_filter <- shiny::callModule(dynamicSelect, "level1", the_data, "STA_LV1_CODE" )
  level3_filter <- shiny::callModule(dynamicSelect, "level3", level1_filter, "STA_LV3_CODE" )#level1_filter()$STA_LV3_CODE)
  basin_filter <- shiny::callModule(dynamicSelect, "basin", level3_filter, "Basin")#level3_filter()$Basin)
  watershed_filter <- shiny::callModule(dynamicSelect, "watershed", basin_filter, "Huc6_Vahu6")#year_filter()$Basin)
  
  callModule(stationMap, "watershedMap",watershed_filter,HUCS)
  
  output$WSHDselectedElementsTable <- DT::renderDataTable({
    if(input$radio == 'Watershed Information'){
      DT::datatable(watershed_filter(),
                options = list(scrollX = TRUE))}})
  
  
  stationID_filter <- shiny::callModule(dynamicSelect, "stationID", the_data, "FDT_STA_ID" )
  
  callModule(stationMap, "stationMap",stationID_filter,HUCS)
  
  output$STATIONIDselectedElementsTable <- DT::renderDataTable({
    if(input$radio == 'StationID'){
      DT::datatable(stationID_filter(),
                options = list(scrollX = TRUE))}})
  
  output$allDataTable <- DT::renderDataTable({
    DT::datatable(BRRO_sites,
                  options = list(scrollX = TRUE,
                                 pageLength = 5,
                                 lengthMenu = list(c(5,25, 50, -1), list('5', '25', '50', 'All'))))})
    
  
  
  
})