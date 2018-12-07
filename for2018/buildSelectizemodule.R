
shinyApp(
  ui = fluidPage(
    #dynamicSelectInput("year", "Year", multiple = TRUE),
    dynamicSelectInput("basin", "Major Basin", multiple = TRUE),
    #dynamicSelectInput("subbasin", "Subasin", multiple = TRUE),
    dynamicSelectInput("watershed", "Watershed", multiple = TRUE),
    tableOutput("table")
  ),
  server = function(input, output, session){
    
    the_data <- reactive({
      BRRO_sites
    })
    
    #year_filter <- shiny::callModule(dynamicSelect, "year", the_data, "FDT_YEAR", default_select = NA)
    ## year_filter is then filtered by Basin
    basin_filter <- shiny::callModule(dynamicSelect, "basin", the_data, "Basin")#year_filter()$Basin)
    #subbasin_filter <- shiny::callModule(dynamicSelect, "subbasin", basin_filter, "Subbasin")#year_filter()$Basin)
    watershed_filter <- shiny::callModule(dynamicSelect, "watershed", basin_filter, "Huc6_Vahu6")#year_filter()$Basin)
    
    
    
    output$table <- renderTable({
      table <- watershed_filter()
      save(table, file = "testTable.RData")
      table
    })
    
  }
)
