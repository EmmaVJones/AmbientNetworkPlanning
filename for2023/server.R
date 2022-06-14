# source('global.R')
# 
# assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
# ## use same layers as weekly pin (where WQM_Stations_Spatial comes from)
# vahu6 <- st_read('data/GIS/VA_SUBWATERSHED_6TH_ORDER_STG.shp') # this version of vahu6 layer goes outside state boundary
# subbasinConversion <- read_csv('data/subbasinToVAHU6conversion.csv')
# unrestrictedAssessmentRegionVAHU6Subbasin <- left_join(vahu6, subbasinConversion, by = c('VAHU6', 'VAHU5'))
# 



# To update application, search for "# Update every two years" and adjust accordingly
# vahu6Layout() also needs to be updated


shinyServer(function(input, output, session) {

  # user select Region to begin
  regionData <- reactive({req(input$begin)
    readRDS(paste0('data/', input$deqRegion, 'regionInfo.RDS'))})
  
  #output$test <- renderPrint({regionData()})
  
  output$helpText1 <- renderUI({req(regionData())
    helpText(paste0('The map to the right autogenerates based on sampling information available from the previous IR window and 
  any available sample information available in ODS as of the last application update (',
                    max(regionData()$sampleWindow),
                    '). You may upload a spreadsheet (.xlsx) of proposed sample locations for the upcoming sampling year to visualize 
                  the proposed monitoring network.'))})
  
  output$helpText2 <- renderUI({req(regionData())
    helpText(span('Map requires a moment to load.',
                  strong(paste0('To expedite map rendering, the available dataset is refelects sample information
                                                        in ODS as of ', max(regionData()$sampleWindow), '.')), 
                  "Uploading a proposed monitoring network requeries this information from ODS to reflect
                                                 the most recent available data."))})
  
  
  
  ## map
  output$regionalMap <- renderLeaflet({req(assessmentRegions, regionData)
    pal <- colorFactor(
      palette = rainbow(7),
      domain = assessmentRegions$ASSESS_REG)
    
    pal2 <- colorFactor(
      palette = c('yellow', 'green','red'),
      domain = levels(regionData()$ir2022$`IR Sampling Status`))
    
    stationPal <- colorFactor(palette = rainbow(11),
                              domain = unique(regionData()$windowSampleInfoFinalSummary %>% 
                                                dplyr::select(contains('SPG codes First')) %>% 
                                                pivot_longer(cols = contains('SPG codes First'), names_to = 'year', values_to = 'SPG') %>% 
                                                filter(!is.na(SPG)) %>% 
                                                distinct(SPG) %>% 
                                                pull(SPG)))
    
    
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE,
                 options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20,
                                         preferCanvas = TRUE)) %>%
      setView(-79.1, 37.7, zoom=8)  %>%
      addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
                  fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
                  group="Assessment Regions", label = ~ASSESS_REG) %>% hideGroup('Assessment Regions') %>%
      addPolygons(data= st_as_sf(regionData()$ir2022),  color = 'black', weight = 1,
                  fillColor= 'gray', fillOpacity = 0.5,stroke=0.1,
                  group="Selected Region's VAHU6s", label = ~VAHU6) %>% hideGroup("Selected Region's VAHU6s") %>%
      # Update every two years
      addPolygons(data= st_as_sf(regionData()$ir2022),  color = 'black', weight = 1,
                  fillColor= ~pal2(regionData()$ir2022$`IR Sampling Status`), fillOpacity = 0.5,stroke=0.1,
                  group="IR2022 VAHU6s", label = ~VAHU6, layerId = ~paste0('IR2022_', VAHU6),
                  popup = leafpop::popupTable(regionData()$ir2022, zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR Sampling Status'))) %>% hideGroup("IR2022 VAHU6s") %>%
      # Update every two years
      addPolygons(data= st_as_sf(regionData()$ir2024),  color = 'black', weight = 1,
                  fillColor= ~pal2(regionData()$ir2024$`IR Sampling Status`), fillOpacity = 0.5,stroke=0.1,
                  group="IR2024 VAHU6s", label = ~VAHU6, layerId = ~paste0('IR2024_', VAHU6),
                  popup = leafpop::popupTable(regionData()$ir2024, zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR Sampling Status'))) %>% hideGroup("IR2024 VAHU6s") %>%
      # Update every two years
      addPolygons(data= st_as_sf(regionData()$ir2026),  color = 'black', weight = 1,
                  fillColor= ~pal2(regionData()$ir2026$`IR Sampling Status`), fillOpacity = 0.5,stroke=0.1,
                  group="IR2026 VAHU6s", label = ~VAHU6, layerId = ~paste0('IR2026_', VAHU6),
                  popup = leafpop::popupTable(regionData()$ir2026, zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR Sampling Status'))) %>% hideGroup("IR2026 VAHU6s") %>%
      # Update every two years
      addPolygons(data= st_as_sf(regionData()$ir2028),  color = 'black', weight = 1,
                  fillColor= ~pal2(regionData()$ir2028$`IR Sampling Status`), fillOpacity = 0.5,stroke=0.1,
                  group="IR2028 VAHU6s", label = ~VAHU6, layerId = ~paste0('IR2028_', VAHU6),
                  popup = leafpop::popupTable(regionData()$ir2028, zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR Sampling Status'))) %>% hideGroup("IR2028 VAHU6s") %>%
      
      
      addCircleMarkers(data = st_as_sf(regionData()$x2015), color='gray', fillColor=~stationPal(`Year2015 SPG codes First`), #'gray', 
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2015 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(regionData()$x2015, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6', 
                                                                 'Year2015 Sample n', 'Year2015 SPG codes'))) %>% hideGroup('2015 Stations') %>%
      addCircleMarkers(data = st_as_sf(regionData()$x2016), color='gray', fillColor=~stationPal(`Year2016 SPG codes First`), #color='blue', fillColor='gray', 
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2016 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(regionData()$x2016, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                 'Year2016 Sample n', 'Year2016 SPG codes'))) %>% hideGroup('2016 Stations') %>%
      addCircleMarkers(data = st_as_sf(regionData()$x2017),  color='gray', fillColor=~stationPal(`Year2017 SPG codes First`), #color='blue', fillColor='gray', 
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2017 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(regionData()$x2017, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                 'Year2017 Sample n', 'Year2017 SPG codes'))) %>% hideGroup('2017 Stations') %>%
      addCircleMarkers(data = st_as_sf(regionData()$x2018),  color='gray', fillColor=~stationPal(`Year2018 SPG codes First`), #color='blue', fillColor='gray', 
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2018 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(regionData()$x2018, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                 'Year2018 Sample n', 'Year2018 SPG codes'))) %>% hideGroup('2018 Stations') %>%
      addCircleMarkers(data = st_as_sf(regionData()$x2019), color='gray', fillColor=~stationPal(`Year2019 SPG codes First`), #color='blue', fillColor='gray',  
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2019 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(regionData()$x2019, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                 'Year2019 Sample n', 'Year2019 SPG codes'))) %>% hideGroup('2019 Stations') %>%
      addCircleMarkers(data = st_as_sf(regionData()$x2020),  color='gray', fillColor=~stationPal(`Year2020 SPG codes First`), #color='blue', fillColor='gray',  
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2020 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(regionData()$x2020, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                 'Year2020 Sample n', 'Year2020 SPG codes'))) %>% hideGroup('2020 Stations') %>%
      addCircleMarkers(data = st_as_sf(regionData()$x2021),  color='gray', fillColor=~stationPal(`Year2021 SPG codes First`), #color='blue', fillColor='gray', 
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2021 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(regionData()$x2021, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                 'Year2021 Sample n', 'Year2021 SPG codes'))) %>% hideGroup('2021 Stations') %>%
      # Update every two years
      addCircleMarkers(data = st_as_sf(regionData()$x2022),  color='gray', fillColor=~stationPal(`Year2022 SPG codes First`), #color='blue', fillColor='gray', 
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2022 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(regionData()$x2022, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                              'Year2022 Sample n', 'Year2022 SPG codes'))) %>% hideGroup('2022 Stations') %>%
      
      addCircleMarkers(data = st_as_sf(regionData()$windowSampleInfoFinalSummary), color='blue', fillColor='gray', radius = 4,
                       fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="All Stations",
                       label = ~FDT_STA_ID, layerId = ~FDT_STA_ID,
                       popup = leafpop::popupTable(regionData()$windowSampleInfoFinalSummary, 
                                                   zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                          'IR2022 Sample n', 'IR2024 Sample n', 'IR2026 Sample n',
                                                          'IR2028 Sample n'))) %>% hideGroup('All Stations') %>%
      addLegend(data = regionData()$x2015,'topright', pal = stationPal, values = ~`Year2015 SPG codes First`, title = 'Individual Year SPG Codes') %>% 
      inlmisc::AddSearchButton(group = 'All Stations', zoom = 15, textPlaceholder = "Search by StationID") %>% 
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      # Update every two years
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Assessment Regions", "Selected Region's VAHU6s",
                                         "IR2022 VAHU6s", "IR2024 VAHU6s",  "IR2026 VAHU6s", "IR2028 VAHU6s",
                                         'All Stations', "2015 Stations","2016 Stations","2017 Stations","2018 Stations",
                                         "2019 Stations","2020 Stations", "2021 Stations", "2022 Stations"),
                       #"IR2022 Missing VAHU6's", "IR2024 Missing VAHU6's"),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  })
  
  regionalMap_proxy <- leafletProxy("regionalMap")
  
  # Based on user map click, filter data for review
  tableInfo <- reactive({req(input$regionalMap_marker_click)
    clickName <- input$regionalMap_marker_click$id
    if(is.null(clickName ))
      return()
    if(str_detect(clickName, '/')){
      clickName <- gsub("/.*", "", clickName)    }
    return(filter(regionData()$windowSampleInfoFinalSummary, FDT_STA_ID %in% clickName)%>% 
             dplyr::select(-geometry) %>% 
             dplyr::select(-contains('SPG codes First'))) })
  
  
  ## display information about clicked item in datatable
  output$stationDetails <- DT::renderDataTable({
    DT::datatable(tableInfo(), rownames = F,
                  options = list(dom = 't', scrollX = TRUE, pageLength = 1), selection = 'none') })
  
  
})
