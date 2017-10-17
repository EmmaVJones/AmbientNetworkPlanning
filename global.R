library(shiny)
library(shinyjs)
library(mapview)
library(leaflet) # using development version of leaflet to get legend group options devtools::install_github('rstudio/leaflet')
library(tidyverse)
library(rgdal)
library(DT)

BRRO_sites <- readRDS('data/BRROdata2011to2016_sites.RDS')
BRRO <- readRDS('data/BRROdata2011to2016.RDS')
HUCS <- readOGR('data','BRRO')



# Map 
stationMapUI <- function(id){
  ns <- NS(id)
  tagList(
      leafletOutput(ns("map"), height = 800))
}

stationMap <- function(input, output, session){
  ns <- session$ns
  
  pal <- colorFactor(c("blue","firebrick","ghostwhite","darkseagreen","limegreen","yellow","aquamarine","red",
                       "grey50","midnightblue","magenta",'mediumpurple4',"moccasin","sienna"),levels=unique(BRRO_sites$FDT_SPG_CODE), ordered=T)
  
  output$map <- renderLeaflet({
    #require(gageLocation())
    
    leaflet(BRRO_sites) %>%
      addProviderTiles(providers$Thunderforest.Landscape,group='Thunderforest Landscape')%>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addProviderTiles(providers$Stamen.TerrainBackground,group='Stamen Terrain Background')%>%
      addCircleMarkers(data=BRRO_sites,~Longitude,~Latitude,radius=5,color=~pal(FDT_SPG_CODE),#fillColor="blue",
                       fillOpacity = 1,stroke=0,
                       group="Stations",layerId=~FDT_STA_ID,
                       popup=popupTable(BRRO_sites))%>%
      addPolygons(data=HUCS,color='gray',fill=0.1,stroke=0.2,group="Watersheds",
                  popup=paste(sep='<br/>',
                              paste("HUC6: ",HUCS@data$VAHU6,sep=""),HUCS@data$VaName))%>%hideGroup('Watersheds')%>%
      addLegend(pal = pal, values = ~FDT_SPG_CODE, opacity = 1,group='Stations')%>%
      addLayersControl(baseGroups=c('Thunderforest Landscape','Esri World Imagery',
                                    'Open Street Map','Stamen Terrain Background'),
                       overlayGroups=c('Stations','Watersheds'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      addMiniMap(tiles = providers$OpenStreetMap,toggleDisplay = TRUE)%>%
      mapview::addMouseCoordinates(style='basic')
  })
}