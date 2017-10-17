source('global.R')
source('multipleDependentSelectizeArguments.R')


load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}



shinyServer(function(input, output, session) {
  
  # display the loading feature until data
  load_data()
  
  # Query By Selectize arguments
  the_data <- reactive({BRRO_sites})
  
  level1_filter <- shiny::callModule(dynamicSelect, "level1", the_data, "STA_LV1_CODE" )
  level3_filter <- shiny::callModule(dynamicSelect, "level3", level1_filter, "STA_LV3_CODE" )#level1_filter()$STA_LV3_CODE)
  basin_filter <- shiny::callModule(dynamicSelect, "basin", level3_filter, "Basin")#level3_filter()$Basin)
  watershed_filter <- shiny::callModule(dynamicSelect, "watershed", basin_filter, "Huc6_Vahu6")#year_filter()$Basin)
  
  stationID_filter <- shiny::callModule(dynamicSelect, "stationID", the_data, "FDT_STA_ID" )
  
  
  output$selectedElementsTable <- DT::renderDataTable({
    
      datatable(watershed_filter())
    })
  
  callModule(stationMap, "stationMap")
  
})