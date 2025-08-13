# go through the code and find where the full stations list is
#dput(stations)

stations_df<-stations |> st_drop_geometry()
stations_df$lat_sf<-stations$lat
stations_df$lon_sf<-stations$lon
stations_new<-st_as_sf(stations_df, coords = c("lon_sf", "lat_sf"), crs = 4326)
# adding stations that are not currently present in the data
# new_stations<-data.frame(station_name = c("Douglas Crossing"), map_label = c("Douglas \nCrossing"), lat = c(49.0061439), lon =c(-122.75759020128658),
#                          lat_sf = c(49.0061439), lon_sf =c(-122.75759020128658),hours_of_operation = c("Unknown"), station_type = c("Part-time Inspection Station")) |> 
#   st_as_sf(coords = c("lon_sf", "lat_sf"), crs = 4326) %>%
#   { if ("geometry" %in% names(.)) rename(., geom = geometry) else . } # if the name is geometry, change it to geom
#   
# stations = rbind(stations, new_stations)

stations<-structure(list(station_name = c("Cascade", "Cutts (Hwy 93)", 
                                          "Dawson Creek", "Golden", "Laidlaw", "Mt. Robson", "Olsen", "Osoyoos", 
                                          "Pacific", "Radium", "Salmo", "Yahk", "Balfour", "Castlegar", 
                                          "Elko", "Midway", "Penticton Roving", "Scheduled Inspection", 
                                          "Lower Mainland Roving", "Keremeos", "Sumas Border", "Douglas Crossing"
), map_label = c("Cascade", "Cutts (Hwy 93)", "Dawson Creek", 
                 "Golden", "Laidlaw", "Mt. Robson", "Olsen", "Osoyoos", "Pacific", 
                 "Radium", "Salmo", "Yahk", "Balfour", "Castlegar", "Elko", "Midway", 
                 "Penticton", "Scheduled Inspection", "Lower Mainland", "Keremeos", 
                 "Sumas Border", "Douglas \nCrossing"), 
lat = c(49.002104, 49.203097, 
        55.481347, 51.28151, 49.342155, 53.007403, 49.625793, 49.001, 
        49.002876, 50.618197, 49.17924, 49.0776, 49.62, 49.29, 49.3, 
        49.01, 49.49, 49.49, 49.267, 49.205, 49.002411, 49.0061439), 
lon = c(-118.225024, -115.166461, -120.003456, -116.838613, 
        -121.595674, -119.289523, -114.933554, -119.4625, -122.739866, 
        -116.068788, -117.281471, -116.128006, -116.96, -117.64, 
        -115.11, -118.78, -119.59, -119.59, -121.685, -119.826, -122.265511, 
        -122.757590201287), 
hours_of_operation = c("10 hrs/day", 
                       "10 hrs/day", "10 hrs/day", "24 hr", "dawn to dusk", "10 hrs/day", 
                       "dawn to dusk", "10 hrs/day", "10 hrs/day", "dawn to dusk", 
                       "10 hrs/day", "dawn to dusk", "Unknown", "Unknown", "Unknown", 
                       "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", 
                       "Unknown"), 
station_type = structure(
  c(NA, 2L, 1L, 1L, NA, 
    1L, 1L, 1L, 2L, 2L, NA, 1L, NA, NA, NA, NA, 3L, NA, 3L, NA, 
    2L, 2L), 
  levels = c("Permanent Inspection Station", "Part-time Inspection Station", 
             "Roving Inspection Crew"), class = "factor"), 
lat_sf = c(49.002104, 
           49.203097, 55.481347, 51.28151, 49.342155, 53.007403, 49.625793, 
           49.001, 49.002876, 50.618197, 49.17924, 49.0776, 49.62, 49.29, 
           49.3, 49.01, 49.49, 49.49, 49.267, 49.205, 49.002411, 49.0061439
), 
lon_sf = c(-118.225024, -115.166461, -120.003456, -116.838613, 
           -121.595674, -119.289523, -114.933554, -119.4625, -122.739866, 
           -116.068788, -117.281471, -116.128006, -116.96, -117.64, 
           -115.11, -118.78, -119.59, -119.59, -121.685, -119.826, -122.265511, 
           -122.757590201287)), 
row.names = c(NA, 22L), class = c("tbl_df", 
                                  "tbl", "data.frame"))

new_stations<-data.frame(station_name = c("Hwy 97c"), map_label = c("Hwy 97c"), lat = c(50.312449), lon =c(-120.8171998),
                         lat_sf = c(50.3124495), lon_sf =c(-120.8171998),hours_of_operation = c("Unknown"), station_type = c("Part-time Inspection Station"))
stations = rbind(stations, new_stations)

stations<-st_as_sf(stations, coords = c("lon_sf", "lat_sf"), crs = 4326)
sf::write_sf(stations,paste0(
  my_opts$remote_spatial_data,
  'Projects/ZQMussels/data/inspection_stations.gpkg'))
