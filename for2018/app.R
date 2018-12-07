library(shiny)
library(shinyjs)
library(mapview)
library(leaflet) # using development version of leaflet to get legend group options devtools::install_github('rstudio/leaflet')
library(tidyverse)
library(rgdal)

#setwd
BRRO_sites <- readRDS('data/BRROdata2011to2016_sites.RDS')
HUCS <- readOGR('data','BRRO')


# GLOBAL
# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}


# Map 
stationMapUI <- function(id){
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), height = 600))
}

stationMap <- function(input, output, session, userDataset, watershedLayer){
  ns <- session$ns
  
  pal <- colorFactor(c("blue","firebrick","ghostwhite","darkseagreen","limegreen","yellow","aquamarine","red",
                       "grey50","midnightblue","magenta",'mediumpurple4',"moccasin","sienna"),levels=unique(BRRO_sites$FDT_SPG_CODE), ordered=T)
  
  
  output$map <- renderLeaflet({
    
    leaflet(userDataset()) %>%
      addProviderTiles(providers$Thunderforest.Landscape,group='Thunderforest Landscape')%>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addProviderTiles(providers$Stamen.TerrainBackground,group='Stamen Terrain Background')%>%
      addCircleMarkers(data=userDataset(),~Longitude,~Latitude,radius=5,color=~pal(FDT_SPG_CODE),#fillColor="blue",
                       fillOpacity = 1,stroke=0,
                       group="Selected Stations",layerId=~FDT_STA_ID,
                       popup=popupTable(userDataset()))%>%
      addCircleMarkers(data=BRRO_sites,~Longitude,~Latitude,radius=5,color=~pal(FDT_SPG_CODE),fillOpacity=1,stroke=0,
                       group="All Stations",layerId=~Latitude,popup=popupTable(BRRO_sites))%>%hideGroup('All Stations')%>%
      addPolygons(data=watershedLayer,color='gray',fill=0.1,stroke=0.2,group="Watersheds",
                  popup=paste(sep='<br/>',
                              paste("HUC6: ",watershedLayer@data$VAHU6,sep=""),watershedLayer@data$VaName))%>%hideGroup('Watersheds')%>%
      addLegend(pal = pal, values = ~BRRO_sites$FDT_SPG_CODE, opacity = 1,title='Legend',group='All Stations')%>%
      addLayersControl(baseGroups=c('Thunderforest Landscape','Esri World Imagery',
                                    'Open Street Map','Stamen Terrain Background'),
                       overlayGroups=c('Selected Stations','All Stations','Watersheds'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      addMiniMap(tiles = providers$OpenStreetMap,toggleDisplay = TRUE)%>%
      mapview::addMouseCoordinates(style='basic')
  })
}

#' Safe subset
#'
#' @param df Dataframe
#' @param column One name of column to subset within
#' @param subset Vector of entries in column to subset to
#'
#' If column not in df, returns back the df
safeSubset <- function(df, column, subset){
  
  testthat::expect_is(df, "data.frame")
  testthat::expect_is(column, "character")
  testthat::expect_equal(length(column), 1)
  
  if(!is.null(subset)){
    testthat::expect_is(subset, "character")
  } else {
    message("Subset is NULL, returning original")
    out <- df
  }
  
  message(" # subsetting # original rows: ",nrow(df) ," column:", column, " by ", paste(subset, collapse = ", "))
  
  col <- df[[column]]
  
  if(!is.null(col)){
    out <- df[col %in% subset,]
    message("Subset rows: ", nrow(out))
  } else {
    message("Column not found:", column)
    out <- df
  }
  
  out
  
}


#' Dynamical Update of a selectInput
#'
#' Shiny Module: useage details at \link{dynamicSelect}
#'
#' @param id shiny id
#'
#' @return dynamicSelectInput
#' @export
dynamicSelectInput <- function(id, label, multiple = FALSE){
  
  ns <- shiny::NS(id)
  
  shiny::selectInput(ns("dynamic_select"), label,
                     choices = NULL, multiple = multiple, width = "100%")
  
}

#' Dynamical Update of a selectInput
#'
#' Shiny Module
#'
#' Use via \code{callModule(dynamicSelect, "name_select", the_data, "cyl")}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param the_data data.frame containing column of choices
#' @param column The column to select from
#' @param default_select The choices to select on load
#'
#' @seealso \link{dynamicSelectInput}
#'
#' @return the_data filtered to the choice
#' @export
dynamicSelect <- function(input, output, session, the_data, column, default_select = NULL){
  
  ns <- session$ns
  
  ## update input$dynamic_select
  observe({
    shiny::validate(
      shiny::need(the_data(),"Fetching data")
    )
    dt <- the_data()
    
    testthat::expect_is(dt, "data.frame")
    testthat::expect_is(column, "character")
    
    choice <- unique(dt[[column]])
    
    updateSelectInput(session, "dynamic_select",
                      choices = choice,
                      selected = default_select)
    
  })
  
  new_data <- reactive({
    shiny::validate(
      shiny::need(input$dynamic_select,"Select data"),
      shiny::need(the_data(), "Waiting for data")
    )
    
    sd <- the_data()
    selected <- input$dynamic_select
    
    ## will return sd even if column is NULL
    safeSubset(sd, column, selected)
    
  })
  
  return(new_data)
  
}



#### SERVER
server <- shinyServer(function(input, output, session) {
  
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


#### UI
ui <- shinyUI(fluidPage(theme = "yeti.css", 
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
                              #h3("Query By:"),
                              radioButtons("radio",h3("Query By:"),choices=c("Watershed Information","StationID"),selected="Watershed Information"),
                              hr(),
                              conditionalPanel(condition="input.radio == 'Watershed Information'",
                                               dynamicSelectInput("level1", "Level 1 Code", multiple = TRUE),
                                               dynamicSelectInput("level3", "Level 3 Code", multiple = TRUE),
                                               dynamicSelectInput("basin", "Major Basin", multiple = TRUE),
                                               dynamicSelectInput("watershed", "Watershed", multiple = TRUE)),
                              conditionalPanel(condition="input.radio == 'StationID'",
                                               dynamicSelectInput("stationID", "StationID", multiple = TRUE))
                              
                              
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Watershed Results",
                                         stationMapUI("watershedMap"),
                                         DT::dataTableOutput("WSHDselectedElementsTable")),
                                tabPanel("StationID Results", 
                                         stationMapUI("stationMap"),
                                         DT::dataTableOutput("STATIONIDselectedElementsTable")),
                                tabPanel("All Stations Table",
                                         DT::dataTableOutput("allDataTable"))
                              ))
                          ))
                        
))

shinyApp(ui=ui,server=server)
