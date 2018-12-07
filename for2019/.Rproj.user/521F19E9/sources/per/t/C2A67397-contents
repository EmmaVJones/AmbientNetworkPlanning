# R 3.5.1

library(shiny)
library(shinyjs)
library(mapview)
library(leaflet) # using development version of leaflet to get legend group options devtools::install_github('rstudio/leaflet')
library(tidyverse)
library(rgdal)
library(DT)
library(testthat)


##BRRO_sites <- readRDS('data/BRROdata2011toNov2018_sites.RDS')
BRRO_sites2 <- suppressMessages(read_csv('data/BRROdata_sites2011_Nov2018.csv')) # made in AmbientNetworkRebuild2019.Rmd

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
    leafletOutput(ns("map"), height = 450))
}

stationMap <- function(input, output, session, userDataset, watershedLayer){
  ns <- session$ns
  
  val <- c('RB', 'TR', 'CB', 'AW', 'TM', 'FP', 'RL', 'PA', 'QA', 'SS', 'BN', 'IM', 'DR')
  pal <- colorFactor(c("blue","firebrick","ghostwhite","darkseagreen","limegreen","yellow","aquamarine","red",
                       "grey50","midnightblue","magenta",'mediumpurple4',"moccasin","sienna"),levels=val, ordered = T)#unique(BRRO_sites$FDT_SPG_CODE), ordered=T)

  output$map <- renderLeaflet({
    
    leaflet(BRRO_sites2) %>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$Stamen.TerrainBackground,group='Stamen Terrain Background')%>%
      addCircleMarkers(data=BRRO_sites2,~Longitude,~Latitude,radius=5,
                       color='black',fillOpacity=1,stroke=0,
                       #color=~pal(FDT_SPG_CODE),fillOpacity=1,stroke=0,
                       group="All Stations",layerId=~Latitude,
                       popup=popupTable(BRRO_sites2,
                                        zcol=c("FDT_STA_ID","Huc6_Vahu6","FDT_SPG_CODE","nObservations","yearsSampled")))%>%
      hideGroup('All Stations')%>%
      addPolygons(data=watershedLayer,color='gray',fill=0.1,stroke=0.2,group="Watersheds",
                  popup=paste(sep='<br/>',
                              paste("HUC6: ",watershedLayer@data$VAHU6,sep=""),watershedLayer@data$VaName))%>%hideGroup('Watersheds')%>%
      addLegend(pal = pal, values = ~val, #levels(BRRO_sites$FDT_SPG_CODE), #~BRRO_sites$FDT_SPG_CODE, 
                opacity = 1,title='Legend',group='Legend')%>%
      addLayersControl(baseGroups=c('Open Street Map','Esri World Imagery','Stamen Terrain Background'),
                       overlayGroups=c('All Stations','Watersheds'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      addMiniMap(tiles = providers$OpenStreetMap,toggleDisplay = TRUE)%>%
      mapview::addMouseCoordinates(style='basic')
  })
  
  observe({
    leafletProxy( ns("map"), data = userDataset()) %>%
      clearGroup('Selected Stations') %>%
      addCircleMarkers(data=userDataset(),~Longitude,~Latitude,radius=5,color=~pal(FDT_SPG_CODE),#fillColor="blue",
                       fillOpacity = 1,stroke=0, group="Selected Stations",layerId=~FDT_STA_ID, 
                       popup=popupTable(userDataset(),zcol=c("FDT_STA_ID","Huc6_Vahu6","FDT_SPG_CODE","nObservations","yearsSampled"))) %>%
      addLayersControl(baseGroups=c('Open Street Map','Esri World Imagery','Stamen Terrain Background'),
                       overlayGroups=c('Selected Stations','All Stations','Watersheds'),
                       options=layersControlOptions(collapsed=T), position='topleft')
  })
}




watershedMapUI <- function(id){
  ns <- NS(id)
  tagList(
    h4('Watersheds Sampling Needs'),
    column(3, 
           wellPanel(
             selectInput(ns('polygonChoices'), 'Watershed Choices', 
                         choices = c('All BRRO Watersheds','2020IR Needs','2022IR Needs',
                                     "Not On Mary's List"),selected = NULL))),
    column(9,leafletOutput(ns('QAmap'),height = 450)),
    
    
    h5('IR Windows:'),
    p('- IR2020: 2013 - 2018'),
    p('- IR2022: 2015 - 2020'),
    helpText("Use the above map to visually explore:
                                          - Watersheds that need additional samples for 2020 IR Window (2020IR Needs)
                                          - Watersheds that need additional samples for 2022 IR Window (2022IR Needs)
                                          - Watersheds we may be able to stop sampling (Not On Mary's List). These are 
                                            the watersheds that Mary doesn't have as BRRO needing to sample but are 
                                            attributed to BRRO in Assessment layer. Need attention.")
  )
}

watershedMap <- function(input, output, session, userWatershedDataset, 
                         watershedLayer){
  ns <- session$ns
  
  
  val2 <- c('Review Watershed for Sample Needs','probably cool')
  pal2 <- colorFactor(c("yellow", "gray"),levels=val2, ordered = T)
  
  colName <- reactive({
    switch(input$polygonChoices,
           'All BRRO Watersheds' = 'AllWtrs',
           '2020IR Needs' = 'IR2020r',
           '2022IR Needs'= 'IR2022r',
           "Not On Mary's List" = 'Mary')})#,


  output$QAmap <- renderLeaflet({
    leaflet(BRRO_sites2) %>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$Stamen.TerrainBackground,group='Stamen Terrain Background')%>%
      
      #addPolygons(data=userWatershedDataset,color=~pal2(get(colName())),
      #            layerId=~userWatershedDataset@data$VAHUC6,
      #            fill=0.1,stroke=0.2,group="Watershed Query",
      #            popup=popupTable(userWatershedDataset@data,
      #                             zcol=c('VAHU6', 'VaName', 'VAHU5'))) %>%
      
      
      addCircleMarkers(data=BRRO_sites2,~Longitude,~Latitude,radius=5,
                       color='black',fillOpacity=1,stroke=0,
                       group="All Stations",layerId=~Latitude,
                       popup=popupTable(BRRO_sites2,
                                        zcol=c("FDT_STA_ID","Huc6_Vahu6","FDT_SPG_CODE",
                                               "nObservations","yearsSampled")))%>% hideGroup('All Stations')%>%
      #addPolygons(data=watershedLayer,color='gray',fill=0.1,stroke=0.2,group="All Watersheds",
      #            popup=paste(sep='<br/>',
      #                        paste("HUC6: ",watershedLayer@data$VAHU6,sep=""),watershedLayer@data$VaName))%>%hideGroup('All Watersheds')%>%
      addLayersControl(baseGroups=c('Open Street Map','Esri World Imagery','Stamen Terrain Background'),
                       overlayGroups=c('All Stations'),#,'All Watersheds'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      addMiniMap(tiles = providers$OpenStreetMap,toggleDisplay = TRUE)%>%
      mapview::addMouseCoordinates(style='basic')
  })
  
  observe({
    leafletProxy( ns("QAmap"), data = userWatershedDataset) %>%
      clearGroup('Watershed Query') %>%
      addPolygons(data=userWatershedDataset,color=~pal2(get(colName())),
                  layerId=~userWatershedDataset@data$VAHUC6,
                  fill=0.1,stroke=0.2,group="Watershed Query",
                  popup=popupTable(userWatershedDataset@data,
                                   zcol=c('VAHU6', 'VaName', 'VAHU5'))) %>%
      addLegend(pal = pal2, values = ~val2, 
                opacity = 1,title='Legend',group='Legend')%>%
      addLayersControl(baseGroups=c('Open Street Map','Esri World Imagery','Stamen Terrain Background'),
                     overlayGroups=c('Watershed Query','All Stations','All Watersheds'),
                       options=layersControlOptions(collapsed=T), position='topleft')
  })
}





