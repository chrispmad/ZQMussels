
# Apply station name correction(s)
dat = dat |> 
  dplyr::left_join(stations.to.change) |> 
  dplyr::mutate(Station = ifelse(!is.na(new_name), new_name, Station))

dat_all = dat_all |> 
  dplyr::left_join(stations.to.change) |> 
  dplyr::mutate(Station = ifelse(!is.na(new_name), new_name, Station))


# See which stations we have data for in this year.
stations_active = dat |> 
  # dplyr::mutate(Station = dplyr::case_when(
  #   str_detect(Station,"Lower Mainland") ~ "Lower Mainland Roving",
  #   T ~ Station
  # )) |> 
  dplyr::count(Station) |> 
  dplyr::filter(n >= 2) |> 
  dplyr::pull(Station)



# stations_active<-gsub("Keremeos (Hwy 3)", "Keremeos", stations_active)

#Establish colour scheme for roving stations that we keep.
if(my.year == 2022){
  rovers = data.frame(Station = c("Hwy 97c","Keremeos","Greenwood","Kaleden",
                                  "Lower Mainland","Pacific",
                                  "Penticton"),
                      StationType = "Roving")
}
if(my.year == 2023){
  rovers = data.frame(Station = c("Hwy 97c","Keremeos","Greenwood","Kaleden",
                                  "Lower Mainland","Pacific",
                                  "Sumas",
                                  "Penticton","Cutts (Hwy 93)"),
                      StationType = "Roving")
}
if(my.year == 2024){
  rovers = data.frame(Station = c("Hwy 97c",
                                  "Keremeos","Greenwood","Kaleden",
                                  "Lower Mainland","Pacific",
                                  "Sumas",
                                  "Penticton",
                                  "Cutts (Hwy 93)",
                                  "Douglas Crossing"),
                      StationType = "Roving")
}
if(my.year == 2025){
  rovers = data.frame(Station = c("Hwy 97c","Keremeos","Greenwood","Kaleden",
                                  "Lower Mainland","Pacific",
                                  "Sumas",
                                  "Penticton","Cutts (Hwy 93)"),
                      StationType = "Roving")
}



station_types = bind_rows(rovers,
                          data.frame(Station = dat |> 
                                       select(Station) |> 
                                       filter(!Station %in% rovers$Station) |> 
                                       distinct() |> 
                                       pull(Station),
                                     StationType = "Permanent"))

rm(rovers)

# what is this for?
#Make one more vector of stations that we want to drop in the figures. These are roving stations.
if(my.year == 2022){
  rovers_to_drop = c("Scheduled Inspection","Other","Okanagan","Sumas Border")
} 
if(my.year == 2023){
  rovers_to_drop = c("Other","Okanagan",
                     "Scheduled Inspection (Other Notification)",
                     "Scheduled Inspection (Cbsa Notification)")
}
if(my.year == 2024){
  rovers_to_drop = c("Other","Okanagan",
                     "Scheduled Inspection (Other Notification)",
                     "Scheduled Inspection (Cbsa Notification)")
}
if(my.year == 2025){
  rovers_to_drop = c("Other","Okanagan",
                     "Scheduled Inspection (Other Notification)",
                     "Scheduled Inspection (Cbsa Notification)")
}



# Grab inspection stations shapefile.
stations = read_sf(paste0(
  my_opts$remote_spatial_data,
  'Projects/ZQMussels/data/inspection_stations.gpkg')
) |> 
  dplyr::mutate(
    map_label = ifelse(
      map_label == 'Fraser Valley Roving', 
      'Lower Mainland',
      map_label),
    station_name = ifelse(
      station_name == 'Fraser Valley Roving',
      'Lower Mainland Roving',
      station_name)
  ) |> 
  dplyr::mutate(map_label = ifelse(map_label == 'Penticton Roving','Penticton',map_label)) |> 
  dplyr::mutate(station_type = dplyr::case_when(
    station_name %in% permanent.stations ~ station_type,
    station_name %in% part.time.stations ~ "Part-time Inspection Station",
    station_name %in% roving.stations ~ "Roving Inspection Crew"
  )) |> 
  dplyr::mutate(station_type = factor(station_type, levels = c("Permanent Inspection Station","Part-time Inspection Station","Roving Inspection Crew")))


stations = stations |> 
  mutate(station_name = case_when(station_name == "Keremeos (Hwy 3)" ~ "Keremeos",
                                  station_name == "Penticton Roving" ~ "Penticton",
                                  station_name == "Sumas Border" ~ "Sumas",
                                  station_name == "Lower Mainland Roving" ~ "Lower Mainland",
                                  station_name == "Peace Arch Crossing" ~ "Douglas Crossing",
                             TRUE ~ station_name),
         station_name = str_replace(station_name, "Lower Mainland Roving", "Lower Mainland"))

# Drop stations from leaflet maps that we did not want to keep.
stations_current = stations |> 
  filter(station_name %in% leaflet.stations.to.include) |> 
  dplyr::filter(!station_name %in% rovers_to_drop) 

stations_for_maps = stations_current |> 
  mutate(station_type = replace(station_type, station_type == 'Unknown', 'Roving')) |> 
  mutate(my_text_size = ifelse(str_detect(map_label,"(Roving|Border)+$"), 'small', 'medium')) |> 
  dplyr::filter(station_name %in% stations_active) |> 
  distinct()


source(here::here("02_IMDP_Figure_Generator", "utils", "station_label_offsets.R"))

station_labels = adjust_station_labels(stations_for_maps ,offsets)








# Find which stations have inspections in this year's data.
stations_active_this_year = dat_all |> 
  dplyr::mutate(Station = dplyr::case_when(
    str_detect(Station,"Lower Mainland") ~ "Lower Mainland Roving",
    T ~ Station
  )) |> 
  dplyr::mutate(Station = dplyr::case_when(
    str_detect(Station,"Lower Mainland Roving") ~ "Lower Mainland",
    T ~ Station
  )) |> 
  dplyr::filter(Year == my.year+1) |> 
  dplyr::count(Station) |> 
  dplyr::filter(n > 5) |> 
  dplyr::pull(Station) 

# If we're in 2024, Martina and co. also want Radium and Sumas as part-time stations.
stations_active_this_year = c(stations_active_this_year, "Radium","Sumas Border")
# Also, change Sumas Border from Roving to Part-time inspections, and
#simplify its label to 'Sumas'
stations[stations$station_name == 'Sumas Border',]$map_label = 'Sumas'
stations[stations$station_name == 'Sumas Border',]$station_type = 'Part-time Inspection Station'

# Drop stations from leaflet maps that we did not want to keep.
stations_this_year = stations |> 
  # filter(station_name %in% leaflet.stations.to.include) |> 
  # dplyr::filter(!station_name %in% rovers_to_drop) |> 
  dplyr::filter(station_name %in% stations_active_this_year)

stations_for_maps_this_year = stations_this_year |> 
  # mutate(station_type = replace(station_type, station_type == 'Unknown', 'Roving')) |> 
  mutate(my_text_size = ifelse(str_detect(map_label,"(Roving|Border)+$"), 'small', 'medium'))

# Ugh. Time to make some hand-drawn locations for the labels...
station_labels_this_year = adjust_station_labels(
  stations_for_maps_this_year,
  offsets
)


# Roving stations for the previous year (i.e. not your current year)
stations_roving = stations |> 
  filter(station_type != 'Permanent Inspection Station') |>
  # filter(station_name != 'Cutts (Hwy 93)') |> 
  dplyr::filter(!station_name %in% c('Scheduled Inspection')) |> 
  dplyr::filter(!stringr::str_detect(station_name, 'Notification')) |> 
  dplyr::mutate(map_label = ifelse(map_label == 'Fraser Valley Roving', 
                                   'Lower Mainland Roving',
                                   map_label),
                station_name = ifelse(station_name == 'Fraser Valley Roving',
                                      'Lower Mainland Roving',
                                      station_name))

# Ugh. Time to make some hand-drawn locations for the labels...
station_labels_roving = adjust_station_labels(stations_roving, offsets)
