
# new stuff here

library(shiny)
library(shinyjs)
library(mapview)
library(leaflet) # using development version of leaflet to get legend group options devtools::install_github('rstudio/leaflet')
library(tidyverse)
library(rgdal)

BRRO_sites <- readRDS('data/BRROdata2011to2016_sites.RDS')

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