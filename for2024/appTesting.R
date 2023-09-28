# set region for testing
regionData <- readRDS('data/BRROregionInfo.RDS')
tableData <- regionData$windowSampleInfoFinalSummary

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
## use same layers as weekly pin (where WQM_Stations_Spatial comes from)
vahu6 <- st_read('data/GIS/VA_SUBWATERSHED_6TH_ORDER_STG.shp') # this version of vahu6 layer goes outside state boundary
subbasinConversion <- read_csv('data/subbasinToVAHU6conversion.csv')
unrestrictedAssessmentRegionVAHU6Subbasin <- left_join(vahu6, subbasinConversion, by = c('VAHU6', 'VAHU5'))


# uploaded data
userUpload <- read_excel('uploadData/Draft  MonPlan 2024EVJ.xlsx', sheet = 'Sample Plan') 


# Process Uploaded Data
stationRaw <- dplyr::select(userUpload, StationID, lat = Latitude, lng = Longitude) %>% # make new lat/lng fields in case user info here
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


# Reanalyze new monitoring network by VAHU6
windowInfoNew <- fakeDataFunction(stationPlan, userUpload, regionData$windowInfo)

newMonitoring <- vahu6Layout(regionData$conventionals, windowInfoNew, WQM_Stations_Spatial, stationPlan) %>% ungroup()


newIR2026 <- left_join(st_as_sf(regionData$ir2024) %>% 
                     dplyr::select(VAHU6, ASSESS_REG), 
                   newMonitoring %>% 
                     filter(!is.na(`IR2026 Sample n`)) %>% 
                     group_by(VAHU6) %>% 
                     summarise(`n Stations` = length(unique(FDT_STA_ID))) %>% 
                     left_join(
                       newMonitoring %>% 
                         group_by(VAHU6) %>% 
                         summarise(`n Samples` = sum(`IR2026 Sample n`, na.rm = T)) , by = 'VAHU6'  ),
                   by = 'VAHU6') %>% 
           mutate(`IR Sampling Status` = as.factor(case_when(is.na(`n Stations`) ~ 'Need Station', 
                                               between(`n Samples`, 1, 9) ~ '< 10 samples',
                                               `n Samples` >9 ~ 'Fine'))) %>% 
    st_as_sf()


newIR2028 <- left_join(st_as_sf(regionData$ir2024) %>% 
                     dplyr::select(VAHU6, ASSESS_REG), 
                   newMonitoring %>% 
                     filter(!is.na(`IR2028 Sample n`)) %>% 
                     group_by(VAHU6) %>% 
                     summarise(`n Stations` = length(unique(FDT_STA_ID))) %>% 
                     left_join(
                       newMonitoring %>% 
                         group_by(VAHU6) %>% 
                         summarise(`n Samples` = sum(`IR2028 Sample n`, na.rm = T)) , by = 'VAHU6'  ),
                   by = 'VAHU6') %>% 
           mutate(`IR Sampling Status` = as.factor(case_when(is.na(`n Stations`) ~ 'Need Station', 
                                               between(`n Samples`, 1, 9) ~ '< 10 samples',
                                               `n Samples` >9 ~ 'Fine'))) %>% 
    st_as_sf() 



newIR2030 <- left_join(st_as_sf(regionData$ir2024) %>% 
                         dplyr::select(VAHU6, ASSESS_REG), 
                       newMonitoring %>% 
                         filter(!is.na(`IR2030 Sample n`)) %>% 
                         group_by(VAHU6) %>% 
                         summarise(`n Stations` = length(unique(FDT_STA_ID))) %>% 
                         left_join(
                           newMonitoring %>% 
                             group_by(VAHU6) %>% 
                             summarise(`n Samples` = sum(`IR2030 Sample n`, na.rm = T)) , by = 'VAHU6'  ),
                       by = 'VAHU6') %>% 
  mutate(`IR Sampling Status` = as.factor(case_when(is.na(`n Stations`) ~ 'Need Station', 
                                                    between(`n Samples`, 1, 9) ~ '< 10 samples',
                                                    `n Samples` >9 ~ 'Fine'))) %>% 
  st_as_sf() 


newYear <- filter_at(newMonitoring, c(length(newMonitoring)-2), all_vars(!is.na(.))) %>% # grab second to last column, which should be the new year sample counts
    st_as_sf() %>% 
    mutate(UID = paste0(FDT_STA_ID,' ', unique(userUpload$`Monitoring Year`)))





# Update map data
  pal2 <- colorFactor(
    palette = c('yellow', 'green','red'),
    domain = levels(regionData$ir2024$`IR Sampling Status`))
  
  stationPal <- colorFactor(palette = rainbow(11),
                            domain = unique(regionData$windowSampleInfoFinalSummary %>%
                                              dplyr::select(contains('SPG codes First')) %>%
                                              pivot_longer(cols = contains('SPG codes First'), names_to = 'year', values_to = 'SPG') %>%
                                              filter(!is.na(SPG)) %>%
                                              distinct(SPG) %>%
                                              pull(SPG)))
  
pal <- colorFactor(
  palette = rainbow(7),
  domain = assessmentRegions$ASSESS_REG)

pal2 <- colorFactor(
  palette = c('yellow', 'green','red'),
  domain = levels(regionData$ir2024$`IR Sampling Status`))

stationPal <- colorFactor(palette = rainbow(11),
                          domain = unique(regionData$windowSampleInfoFinalSummary %>% 
                                            dplyr::select(contains('SPG codes First')) %>% 
                                            pivot_longer(cols = contains('SPG codes First'), names_to = 'year', values_to = 'SPG') %>% 
                                            filter(!is.na(SPG)) %>% 
                                            distinct(SPG) %>% 
                                            pull(SPG)))

regionalMap_proxy <-
CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE,
             options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20,
                                     preferCanvas = TRUE)) %>%
  setView(-79.1, 37.7, zoom=8)  %>%
  addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
              fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
              group="Assessment Regions", label = ~ASSESS_REG) %>% hideGroup('Assessment Regions') %>%
  addPolygons(data= st_as_sf(regionData$ir2024),  color = 'black', weight = 1,
              fillColor= 'gray', fillOpacity = 0.5,stroke=0.1,
              group="Selected Region's VAHU6s", label = ~VAHU6) %>% hideGroup("Selected Region's VAHU6s") %>%
  # Update every two years
  addPolygons(data= st_as_sf(regionData$ir2024),  color = 'black', weight = 1,
              fillColor= ~pal2(regionData$ir2024$`IR Sampling Status`), fillOpacity = 0.5,stroke=0.1,
              group="IR2024 VAHU6s", label = ~VAHU6, layerId = ~paste0('IR2024_', VAHU6),
              popup = leafpop::popupTable(regionData$ir2024, zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR Sampling Status'))) %>% hideGroup("IR2024 VAHU6s") %>%
  # Update every two years
  addPolygons(data= st_as_sf(regionData$ir2026),  color = 'black', weight = 1,
              fillColor= ~pal2(regionData$ir2026$`IR Sampling Status`), fillOpacity = 0.5,stroke=0.1,
              group="IR2026 VAHU6s", label = ~VAHU6, layerId = ~paste0('IR2026_', VAHU6),
              popup = leafpop::popupTable(regionData$ir2026, zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR Sampling Status'))) %>% hideGroup("IR2026 VAHU6s") %>%
  # Update every two years
  addPolygons(data= st_as_sf(regionData$ir2028),  color = 'black', weight = 1,
              fillColor= ~pal2(regionData$ir2028$`IR Sampling Status`), fillOpacity = 0.5,stroke=0.1,
              group="IR2028 VAHU6s", label = ~VAHU6, layerId = ~paste0('IR2028_', VAHU6),
              popup = leafpop::popupTable(regionData$ir2028, zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR Sampling Status'))) %>% hideGroup("IR2028 VAHU6s") %>%
  # Update every two years
  addPolygons(data= st_as_sf(regionData$ir2030),  color = 'black', weight = 1,
              fillColor= ~pal2(regionData$ir2030$`IR Sampling Status`), fillOpacity = 0.5,stroke=0.1,
              group="IR2030 VAHU6s", label = ~VAHU6, layerId = ~paste0('IR2030_', VAHU6),
              popup = leafpop::popupTable(regionData$ir2030, zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR Sampling Status'))) %>% hideGroup("IR2030 VAHU6s") %>%
  
  
  addCircleMarkers(data = st_as_sf(regionData$x2017),  color='gray', fillColor=~stationPal(`Year2017 SPG codes First`), #color='blue', fillColor='gray', 
                   radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2017 Stations",
                   label = ~FDT_STA_ID, layerId = ~UID,
                   popup = leafpop::popupTable(regionData$x2017, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                          'Year2017 Sample n', 'Year2017 SPG codes'))) %>% hideGroup('2017 Stations') %>%
  addCircleMarkers(data = st_as_sf(regionData$x2018),  color='gray', fillColor=~stationPal(`Year2018 SPG codes First`), #color='blue', fillColor='gray', 
                   radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2018 Stations",
                   label = ~FDT_STA_ID, layerId = ~UID,
                   popup = leafpop::popupTable(regionData$x2018, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                          'Year2018 Sample n', 'Year2018 SPG codes'))) %>% hideGroup('2018 Stations') %>%
  addCircleMarkers(data = st_as_sf(regionData$x2019), color='gray', fillColor=~stationPal(`Year2019 SPG codes First`), #color='blue', fillColor='gray',  
                   radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2019 Stations",
                   label = ~FDT_STA_ID, layerId = ~UID,
                   popup = leafpop::popupTable(regionData$x2019, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                          'Year2019 Sample n', 'Year2019 SPG codes'))) %>% hideGroup('2019 Stations') %>%
  addCircleMarkers(data = st_as_sf(regionData$x2020),  color='gray', fillColor=~stationPal(`Year2020 SPG codes First`), #color='blue', fillColor='gray',  
                   radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2020 Stations",
                   label = ~FDT_STA_ID, layerId = ~UID,
                   popup = leafpop::popupTable(regionData$x2020, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                          'Year2020 Sample n', 'Year2020 SPG codes'))) %>% hideGroup('2020 Stations') %>%
  addCircleMarkers(data = st_as_sf(regionData$x2021),  color='gray', fillColor=~stationPal(`Year2021 SPG codes First`), #color='blue', fillColor='gray', 
                   radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2021 Stations",
                   label = ~FDT_STA_ID, layerId = ~UID,
                   popup = leafpop::popupTable(regionData$x2021, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                          'Year2021 Sample n', 'Year2021 SPG codes'))) %>% hideGroup('2021 Stations') %>%
  addCircleMarkers(data = st_as_sf(regionData$x2022),  color='gray', fillColor=~stationPal(`Year2022 SPG codes First`), #color='blue', fillColor='gray', 
                   radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2022 Stations",
                   label = ~FDT_STA_ID, layerId = ~UID,
                   popup = leafpop::popupTable(regionData$x2022, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                          'Year2022 Sample n', 'Year2022 SPG codes'))) %>% hideGroup('2022 Stations') %>%
  # Update every two years
  addCircleMarkers(data = st_as_sf(regionData$x2023), color='gray', fillColor=~stationPal(`Year2023 SPG codes First`), #'gray', 
                   radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="2023 Stations",
                   label = ~FDT_STA_ID, layerId = ~UID,
                   popup = leafpop::popupTable(regionData$x2023, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6', 
                                                                          'Year2023 Sample n', 'Year2023 SPG codes'))) %>% hideGroup('2023 Stations') %>%
  
  
  addCircleMarkers(data = st_as_sf( tableData ), color='blue', fillColor='gray', radius = 4,
                   fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="All Stations",
                   label = ~FDT_STA_ID, layerId = ~FDT_STA_ID,
                   popup = leafpop::popupTable(  tableData , 
                                                 zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                        'IR2024 Sample n', 'IR2026 Sample n',
                                                        'IR2028 Sample n', 'IR2030 Sample n'))) %>% hideGroup('All Stations') %>%
  addLegend(data = regionData$x2017,'topright', pal = stationPal, values = ~`Year2017 SPG codes First`, title = 'Individual Year SPG Codes') %>% 
  inlmisc::AddSearchButton(group = 'All Stations', zoom = 15, textPlaceholder = "Search by StationID") %>% 
  inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
  # Update every two years
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c("Assessment Regions", "Selected Region's VAHU6s",
                                     "IR2024 VAHU6s",  "IR2026 VAHU6s", "IR2028 VAHU6s", "IR2030 VAHU6s", 
                                     'All Stations', "2017 Stations","2018 Stations",
                                     "2019 Stations","2020 Stations", "2021 Stations", "2022 Stations", "2023 Stations"),
                   #"IR2022 Missing VAHU6's", "IR2024 Missing VAHU6's"),
                   options=layersControlOptions(collapsed=T),
                   position='topleft') 
  
 
# updated map 
regionalMap_proxy %>%
    clearGroup('All Stations') %>% clearGroup("IR2026 VAHU6s") %>% clearGroup("IR2028 VAHU6s") %>% clearGroup("IR2030 VAHU6s") %>%
    addCircleMarkers(data = newMonitoring %>% st_as_sf(), color='blue', fillColor='gray', radius = 4,
                     fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="All Stations",
                     label = ~FDT_STA_ID, layerId = ~paste(FDT_STA_ID, '/new'),
                     popup = leafpop::popupTable(newMonitoring, zcol=c('FDT_STA_ID', 'Sta_Desc', 'VAHU6',
                                                                         'IR2024 Sample n', 'IR2026 Sample n',
                                                                         'IR2028 Sample n', 'IR2030 Sample n'))) %>% hideGroup('All Stations') %>%
    addCircleMarkers(data = newYear,
                     color='black', fillColor=~stationPal(`Year2024 SPG codes First`), # Update every two years
                     radius = 4, fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="Proposed Monitoring Network",
                     label = ~FDT_STA_ID, layerId = ~UID,
                     popup = leafpop::popupTable(newYear, zcol=c('FDT_STA_ID','Sta_Desc', 'VAHU6',
                                                                   'Year2024 Sample n', 'Year2024 SPG codes'))) %>% # Update every two years
    addPolygons(data= newIR2026,  color = 'black', weight = 1,
                fillColor= ~pal2(newIR2026$`IR Sampling Status`), fillOpacity = 0.5,stroke=0.1,
                group="IR2026 VAHU6s", label = ~VAHU6,
                popup = leafpop::popupTable(newIR2026, zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR Sampling Status'))) %>% hideGroup("IR2026 VAHU6s") %>%
    addPolygons(data= newIR2028,  color = 'black', weight = 1,
                fillColor= ~pal2(newIR2028$`IR Sampling Status`),  fillOpacity = 0.5,stroke=0.1,
                group="IR2028 VAHU6s", label = ~VAHU6,
                popup = leafpop::popupTable(newIR2028, zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR Sampling Status'))) %>% hideGroup("IR2028 VAHU6s") %>%
    addPolygons(data= newIR2030,  color = 'black', weight = 1,
                fillColor= ~pal2(newIR2030$`IR Sampling Status`),  fillOpacity = 0.5,stroke=0.1,
                group="IR2030 VAHU6s", label = ~VAHU6,
                popup = leafpop::popupTable(newIR2030, zcol=c('VAHU6', 'n Stations', 'n Samples', 'IR Sampling Status'))) %>% hideGroup("IR2030 VAHU6s") %>%
    
    # Update every two years
    addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                     overlayGroups = c("Assessment Regions", "Selected Region's VAHU6s",
                                       "IR2024 VAHU6s",  "IR2026 VAHU6s", "IR2028 VAHU6s", "IR2030 VAHU6s",
                                       'All Stations', "2017 Stations","2018 Stations",
                                       "2019 Stations","2020 Stations", "2021 Stations", "2022 Stations", "2023 Stations",
                                       "Proposed Monitoring Network"),
                     options=layersControlOptions(collapsed=T),
                     position='topleft') 
