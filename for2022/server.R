source('global.R')

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
assessmentRegionsVAHU6 <- st_read( 'data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  filter(ASSESS_REG == 'BRRO') %>%
  dplyr::select(VAHU6, ASSESS_REG)
## use same layers as weekly pin (where WQM_Stations_Spatial comes from)
vahu6 <- st_read('data/GIS/VA_SUBWATERSHED_6TH_ORDER_STG.shp') # this version of vahu6 layer goes outside state boundary
subbasinConversion <- read_csv('data/subbasinToVAHU6conversion.csv')
unrestrictedAssessmentRegionVAHU6Subbasin <- left_join(vahu6, subbasinConversion, by = c('VAHU6', 'VAHU5'))




# To update application, search for "# Update every two years" and adjust accordingly
# vahu6Layout() also needs to be updated


shinyServer(function(input, output, session) {
  
  # empty reactive objects list
  reactive_objects = reactiveValues() 
  
  # set initial dataset
  observe({
    reactive_objects$tableData <- windowSampleInfoFinalSummary %>% ungroup() 
    reactive_objects$IR2022 <- ir2022
    reactive_objects$IR2024 <- ir2024
    reactive_objects$IR2026 <- ir2026    })
  
  ## map
  output$regionalMap <- renderLeaflet({req(assessmentRegions, ir2022, reactive_objects$tableData)
    pal <- colorFactor(
      palette = rainbow(7),
      domain = assessmentRegions$ASSESS_REG)
    
    pal2 <- colorFactor(
      palette = c('yellow', 'green','red'),
      domain = levels(reactive_objects$IR2022$IR2022))
    
    stationPal <- colorFactor(palette = rainbow(11),
                              domain = unique(reactive_objects$tableData %>% 
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
      addPolygons(data= assessmentRegionsVAHU6,  color = 'black', weight = 1,
                  fillColor= 'gray', fillOpacity = 0.5,stroke=0.1,
                  group="BRRO VAHU6's", label = ~VAHU6) %>% hideGroup("BRRO VAHU6's") %>%
      # Update every two years
      addPolygons(data= reactive_objects$IR2022,  color = 'black', weight = 1,
                  fillColor= ~pal2(reactive_objects$IR2022$IR2022), fillOpacity = 0.5,stroke=0.1,
                  group="IR2022 VAHU6's", label = ~VAHU6, layerId = ~paste0('IR2022_', VAHU6),
                  popup = leafpop::popupTable(reactive_objects$IR2022, zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR2022'))) %>% hideGroup("IR2022 VAHU6's") %>%
      # Update every two years
      addPolygons(data= reactive_objects$IR2024,  color = 'black', weight = 1,
                  fillColor= ~pal2(reactive_objects$IR2024$IR2024), fillOpacity = 0.5,stroke=0.1,
                  group="IR2024 VAHU6's", label = ~VAHU6,
                  popup = leafpop::popupTable(reactive_objects$IR2024, zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR2024'))) %>% hideGroup("IR2024 VAHU6's") %>%
      # Update every two years
      addPolygons(data= reactive_objects$IR2026,  color = 'black', weight = 1,
                  fillColor= ~pal2(reactive_objects$IR2026$IR2026), fillOpacity = 0.5,stroke=0.1,
                  group="IR2026 VAHU6's", label = ~VAHU6,
                  popup = leafpop::popupTable(reactive_objects$IR2026, zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR2026'))) %>% hideGroup("IR2026 VAHU6's") %>%

      addCircleMarkers(data = x2015, color='gray', fillColor=~stationPal(`Year2015 SPG codes First`), #'gray', 
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2015 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(x2015, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6', 
                                                                 'Year2015 Sample n', 'Year2015 SPG codes'))) %>% hideGroup('2015 Stations') %>%
      addCircleMarkers(data = x2016, color='gray', fillColor=~stationPal(`Year2016 SPG codes First`), #color='blue', fillColor='gray', 
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2016 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(x2016, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                 'Year2016 Sample n', 'Year2016 SPG codes'))) %>% hideGroup('2016 Stations') %>%
      addCircleMarkers(data = x2017,  color='gray', fillColor=~stationPal(`Year2017 SPG codes First`), #color='blue', fillColor='gray', 
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2017 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(x2017, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                 'Year2017 Sample n', 'Year2017 SPG codes'))) %>% hideGroup('2017 Stations') %>%
      addCircleMarkers(data = x2018,  color='gray', fillColor=~stationPal(`Year2018 SPG codes First`), #color='blue', fillColor='gray', 
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2018 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(x2018, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                 'Year2018 Sample n', 'Year2018 SPG codes'))) %>% hideGroup('2018 Stations') %>%
      addCircleMarkers(data = x2019, color='gray', fillColor=~stationPal(`Year2019 SPG codes First`), #color='blue', fillColor='gray',  
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2019 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(x2019, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                 'Year2019 Sample n', 'Year2019 SPG codes'))) %>% hideGroup('2019 Stations') %>%
      addCircleMarkers(data = x2020,  color='gray', fillColor=~stationPal(`Year2020 SPG codes First`), #color='blue', fillColor='gray',  
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2020 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(x2020, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                 'Year2020 Sample n', 'Year2020 SPG codes'))) %>% hideGroup('2020 Stations') %>%
      addCircleMarkers(data = x2021,  color='gray', fillColor=~stationPal(`Year2021 SPG codes First`), #color='blue', fillColor='gray', 
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2021 Stations",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(x2021, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                 'Year2021 Sample n', 'Year2021 SPG codes'))) %>% hideGroup('2021 Stations') %>%
      addCircleMarkers(data = reactive_objects$tableData %>% st_as_sf(), color='blue', fillColor='gray', radius = 4,
                       fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="All Stations",
                       label = ~FDT_STA_ID, layerId = ~FDT_STA_ID,
                       popup = leafpop::popupTable(reactive_objects$tableData, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6', 
                                                                                        'IR2022 Sample n', 'IR2024 Sample n', 'IR2026 Sample n'))) %>% hideGroup('All Stations') %>% 
      addLegend(data = x2015,'topright', pal = stationPal, values = ~`Year2015 SPG codes First`, title = 'Individual Year SPG Codes') %>% 
      inlmisc::AddSearchButton(group = 'All Stations', zoom = 15, textPlaceholder = "Search by StationID") %>% 
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      # Update every two years
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Assessment Regions", "BRRO VAHU6's","IR2022 VAHU6's", "IR2024 VAHU6's",  "IR2026 VAHU6's",
                                         'All Stations', "2015 Stations","2016 Stations","2017 Stations","2018 Stations",
                                         "2019 Stations","2020 Stations", "2021 Stations"),
                       #"IR2022 Missing VAHU6's", "IR2024 Missing VAHU6's"),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  })
  
  regionalMap_proxy <- leafletProxy("regionalMap")
  
  # Based on user map click, filter data for review
  tableInfo <- reactive({req(input$regionalMap_marker_click)
    clickName <- input$regionalMap_marker_click$id
    if(is.null(clickName ))
      return()
    if(str_detect(clickName, ' ')){
      clickName <- gsub(" .*", "", clickName)    }
    
    return(filter(reactive_objects$tableData, FDT_STA_ID %in% clickName)) })
  
  
  ## display information about clicked item in datatable
  output$stationDetails <- DT::renderDataTable({
      DT::datatable(tableInfo() %>% dplyr::select(-geometry) %>% dplyr::select(-contains('SPG codes First')), rownames = F,
                  options = list(dom = 't', scrollX = TRUE, pageLength = 1), selection = 'none') })

  
  
  ## User uploads data----------------------------------------------------------------------------------------------------------------------
  
  # uploaded data
  userUpload <- reactive({req(input$userUploadNetwork)
    inFile <- input$userUploadNetwork
    if('Sample Plan' %in% excel_sheets(inFile$datapath)){
      return(read_excel(inFile$datapath, sheet = 'Sample Plan') )
    } else { showNotification("Uploaded Dataset Does Not Contain a Sheet Named 'Sample Plan'", duration = 10)} })
      
  # Process Uploaded Data
  stationPlan <- reactive({req(input$userUploadNetwork, nrow(userUpload()) > 0)
    stationRaw <- dplyr::select(userUpload(), StationID, lat = Latitude, lng = Longitude) %>% # make new lat/lng fields in case user info here
      filter(!is.na(StationID)) %>%  # drop extra excel rows 
      left_join(WQM_Stations_Spatial, by = 'StationID')
    if(nrow(filter(stationRaw, is.na(Latitude) | is.na(Longitude))) > 0 ){
      # Get spatial info for new stations
      fix <- filter(stationRaw, is.na(Latitude) | is.na(Longitude)) %>% 
        st_as_sf(coords = c("lng", "lat"),  # make spatial layer using these columns
                 remove = F, # don't remove these lat/lon cols from df
                 crs = 4326) %>% 
        st_intersection(dplyr::select(unrestrictedAssessmentRegionVAHU6Subbasin, ASSESS_REG, VAHU6)) %>% 
        mutate(ASSESS_REG = ASSESS_REG.1,
               VAHU6 = VAHU6.1,
               Latitude = lat,
               Longitude = lng) %>% 
        dplyr::select(-c(ASSESS_REG.1, VAHU6.1, lat,lng))
      stationPlan <- bind_rows(
        filter(stationRaw, !StationID %in% fix$StationID),
        fix %>% st_drop_geometry()) 
    } else {
      stationPlan <- stationRaw
    }
    return(stationPlan)  })
      
  
  # Reanalyze new monitoring network by VAHU6
  newMonitoring <- reactive({req(nrow(stationPlan()) > 0, windowInfo)
    # add proposed sampling frequency to window info
    windowInfoNew <- fakeDataFunction(stationPlan(), userUpload(), windowInfo)

    # reanalyze monitoring network by vahu6
    return(vahu6Layout(conventionals, windowInfoNew, WQM_Stations_Spatial, stationPlan()) %>% ungroup())    })
  

  # Update data available in review table
  observe({req(nrow(stationPlan()) > 0, windowInfo, nrow(newMonitoring()) > 0)
    reactive_objects$tableData <- newMonitoring()  })
  
  #output$test <- renderPrint({ newIR2024() })
  
  
  
  
  # Update every two years
  newIR2024 <- reactive({req(nrow(newMonitoring()))
    return(left_join(assessmentRegionsVAHU6, 
                     newMonitoring() %>% 
                       filter(!is.na(`IR2024 Sample n`)) %>% 
                       group_by(VAHU6) %>% 
                       summarise(`n Stations` = length(unique(FDT_STA_ID))) %>% 
                       left_join(
                         newMonitoring() %>% 
                           group_by(VAHU6) %>% 
                           summarise(`n Samples` = sum(`IR2024 Sample n`, na.rm = T)) , by = 'VAHU6'  ),
                     by = 'VAHU6') %>% 
             mutate(IR2024 = as.factor(case_when(is.na(`n Stations`) ~ 'Need Station', 
                                                 between(`n Samples`, 1, 9) ~ '< 10 samples',
                                                 `n Samples` >9 ~ 'Fine')))) %>% 
      st_as_sf() })
  # Update every two years
  newIR2026 <- reactive({req(nrow(newMonitoring()))
    return(left_join(assessmentRegionsVAHU6, 
                     newMonitoring() %>% 
                       filter(!is.na(`IR2026 Sample n`)) %>% 
                       group_by(VAHU6) %>% 
                       summarise(`n Stations` = length(unique(FDT_STA_ID))) %>% 
                       left_join(
                         newMonitoring() %>% 
                           group_by(VAHU6) %>% 
                           summarise(`n Samples` = sum(`IR2026 Sample n`, na.rm = T)) , by = 'VAHU6'  ),
                     by = 'VAHU6') %>% 
             mutate(IR2026 = as.factor(case_when(is.na(`n Stations`) ~ 'Need Station', 
                                                 between(`n Samples`, 1, 9) ~ '< 10 samples',
                                                 `n Samples` >9 ~ 'Fine')))) %>% 
      st_as_sf()   })
  
  newYear <- reactive({req(nrow(newMonitoring()) > 0)
    filter_at(newMonitoring(), c(length(newMonitoring())-2), all_vars(!is.na(.))) %>% # grab second to last column, which should be the new year sample counts
      st_as_sf() %>% 
      mutate(UID = paste0(FDT_STA_ID,' ', unique(userUpload()$`Monitoring Year`)))}) 
 
  observe({req(nrow(newMonitoring()) > 0)
    pal2 <- colorFactor(
      palette = c('yellow', 'green','red'),
      domain = levels(reactive_objects$IR2022$IR2022))
    
    stationPal <- colorFactor(palette = rainbow(11),
                              domain = unique(reactive_objects$tableData %>% 
                                                dplyr::select(contains('SPG codes First')) %>% 
                                                pivot_longer(cols = contains('SPG codes First'), names_to = 'year', values_to = 'SPG') %>% 
                                                filter(!is.na(SPG)) %>% 
                                                distinct(SPG) %>% 
                                                pull(SPG)))
    
    
    regionalMap_proxy %>%
      clearGroup('All Stations') %>% clearGroup("IR2024 VAHU6's") %>% clearGroup("IR2026 VAHU6's") %>% 
      #clearMarkers() %>%
      addCircleMarkers(data = newMonitoring() %>% st_as_sf(), color='blue', fillColor='gray', radius = 4,
                       fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="All Stations",
                       label = ~FDT_STA_ID, layerId = ~paste(FDT_STA_ID, 'new'),
                       popup = leafpop::popupTable(newMonitoring(), zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                           'IR2022 Sample n', 'IR2024 Sample n', 'IR2026 Sample n'))) %>% hideGroup('All Stations') %>% 
      addCircleMarkers(data = newYear(),
                       color='black', fillColor=~stationPal(`Year2022 SPG codes First`), # Update every two years 
                       radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="Proposed Monitoring Network",
                       label = ~FDT_STA_ID, layerId = ~UID,
                       popup = leafpop::popupTable(newYear(), zcol=c('FDT_STA_ID','Sta_Desc', 'VAHU6', 
                                                                     'Year2022 Sample n', 'Year2022 SPG codes'))) %>% # Update every two years
      addPolygons(data= newIR2024(),  color = 'black', weight = 1,
                  fillColor= ~pal2(newIR2024()$IR2024), fillOpacity = 0.5,stroke=0.1,
                  group="IR2024 VAHU6's", label = ~VAHU6,
                  popup = leafpop::popupTable(newIR2024(), zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR2024'))) %>% hideGroup("IR2024 VAHU6's") %>%
      addPolygons(data= newIR2026(),  color = 'black', weight = 1,
                  fillColor= ~pal2(newIR2026()$IR2026), fillOpacity = 0.5,stroke=0.1,
                  group="IR2026 VAHU6's", label = ~VAHU6,
                  popup = leafpop::popupTable(newIR2026(), zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR2026'))) %>% hideGroup("IR2026 VAHU6's") %>%
      # Update every two years
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Assessment Regions", "BRRO VAHU6's","IR2022 VAHU6's", "IR2024 VAHU6's",  "IR2026 VAHU6's",
                                         'All Stations', "2015 Stations","2016 Stations","2017 Stations","2018 Stations",
                                         "2019 Stations","2020 Stations", "2021 Stations", "Proposed Monitoring Network"),
                       options=layersControlOptions(collapsed=T),
                       position='topleft') })


  
  
  ## Tab Uploaded Data
  output$uploadedDataset <- DT::renderDataTable({req(userUpload())
    DT::datatable(userUpload(), rownames = F,
                  ist(dom = 'Bit', scrollX = TRUE, scrollY = '350px',
                      pageLength = nrow(userUpload()), buttons=list('copy')), selection = 'none')})
  
  
  
})