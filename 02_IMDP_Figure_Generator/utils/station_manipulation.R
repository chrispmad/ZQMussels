# ------------------------------------------------------------------------------
# Apply station name corrections
# ------------------------------------------------------------------------------

dat = dat |> 
  dplyr::left_join(stations.to.change) |> 
  dplyr::mutate(
    Station = ifelse(!is.na(new_name), new_name, Station)
  )

dat_all = dat_all |> 
  dplyr::left_join(stations.to.change) |> 
  dplyr::mutate(
    Station = ifelse(!is.na(new_name), new_name, Station)
  )


# ------------------------------------------------------------------------------
# Identify stations with sufficient data for this year
# ------------------------------------------------------------------------------

stations_active = dat |> 
  # dplyr::mutate(Station = dplyr::case_when(
  #   str_detect(Station,"Lower Mainland") ~ "Lower Mainland Roving",
  #   TRUE ~ Station
  # )) |> 
  dplyr::count(Station) |> 
  dplyr::filter(n >= 2) |> 
  dplyr::pull(Station)

# stations_active <- gsub("Keremeos (Hwy 3)", "Keremeos", stations_active)


# ------------------------------------------------------------------------------
# Define roving stations by year (also used for colour schemes)
# ------------------------------------------------------------------------------

if (my.year == 2022) {
  rovers = data.frame(
    Station = c(
      "Hwy 97c", "Keremeos", "Greenwood", "Kaleden",
      "Lower Mainland", "Pacific", "Penticton"
    ),
    StationType = "Roving"
  )
}

if (my.year == 2023) {
  rovers = data.frame(
    Station = c(
      "Hwy 97c", "Keremeos", "Greenwood", "Kaleden",
      "Lower Mainland", "Pacific", "Sumas",
      "Penticton", "Cutts (Hwy 93)"
    ),
    StationType = "Roving"
  )
}

if (my.year == 2024) {
  rovers = data.frame(
    Station = c(
      "Hwy 97c", "Keremeos", "Greenwood", "Kaleden",
      "Lower Mainland", "Pacific", "Sumas",
      "Penticton", "Cutts (Hwy 93)", "Douglas Crossing"
    ),
    StationType = "Roving"
  )
}

if (my.year == 2025) {
  rovers = data.frame(
    Station = c(
      "Hwy 97c", "Keremeos", "Greenwood", "Kaleden",
      "Lower Mainland", "Pacific", "Sumas",
      "Penticton", "Cutts (Hwy 93)"
    ),
    StationType = "Roving"
  )
}


# ------------------------------------------------------------------------------
# Assign station types (Roving vs Permanent)
# ------------------------------------------------------------------------------

station_types = bind_rows(
  rovers,
  data.frame(
    Station = dat |> 
      dplyr::select(Station) |> 
      dplyr::filter(!Station %in% rovers$Station) |> 
      dplyr::distinct() |> 
      dplyr::pull(Station),
    StationType = "Permanent"
  )
)

rm(rovers)


# ------------------------------------------------------------------------------
# Roving stations to drop from figures (by year)
# ------------------------------------------------------------------------------

if (my.year == 2022) {
  rovers_to_drop = c(
    "Scheduled Inspection",
    "Other",
    "Okanagan",
    "Sumas Border"
  )
}

if (my.year == 2023) {
  rovers_to_drop = c(
    "Other",
    "Okanagan",
    "Scheduled Inspection (Other Notification)",
    "Scheduled Inspection (Cbsa Notification)"
  )
}

if (my.year == 2024) {
  rovers_to_drop = c(
    "Other",
    "Okanagan",
    "Scheduled Inspection (Other Notification)",
    "Scheduled Inspection (Cbsa Notification)"
  )
}

if (my.year == 2025) {
  rovers_to_drop = c(
    "Other",
    "Okanagan",
    "Scheduled Inspection (Other Notification)",
    "Scheduled Inspection (Cbsa Notification)"
  )
}


# ------------------------------------------------------------------------------
# Load and clean inspection station spatial data
# ------------------------------------------------------------------------------

stations = read_sf(
  paste0(
    my_opts$remote_spatial_data,
    "Projects/ZQMussels/data/inspection_stations.gpkg"
  )
) |> 
  dplyr::mutate(
    map_label = ifelse(
      map_label == "Fraser Valley Roving",
      "Lower Mainland",
      map_label
    ),
    station_name = ifelse(
      station_name == "Fraser Valley Roving",
      "Lower Mainland Roving",
      station_name
    )
  ) |> 
  dplyr::mutate(
    map_label = ifelse(
      map_label == "Penticton Roving",
      "Penticton",
      map_label
    )
  ) |> 
  dplyr::mutate(
    station_type = dplyr::case_when(
      station_name %in% permanent.stations ~ station_type,
      station_name %in% part.time.stations ~ "Part-time Inspection Station",
      station_name %in% roving.stations ~ "Roving Inspection Crew"
    )
  ) |> 
  dplyr::mutate(
    station_type = factor(
      station_type,
      levels = c(
        "Permanent Inspection Station",
        "Part-time Inspection Station",
        "Roving Inspection Crew"
      )
    )
  )


# ------------------------------------------------------------------------------
# Standardize station naming
# ------------------------------------------------------------------------------

stations = stations |> 
  mutate(
    station_name = case_when(
      station_name == "Keremeos (Hwy 3)"      ~ "Keremeos",
      station_name == "Penticton Roving"      ~ "Penticton",
      station_name == "Sumas Border"          ~ "Sumas",
      station_name == "Lower Mainland Roving" ~ "Lower Mainland",
      station_name == "Peace Arch Crossing"   ~ "Douglas Crossing",
      TRUE ~ station_name
    ),
    station_name = str_replace(
      station_name,
      "Lower Mainland Roving",
      "Lower Mainland"
    )
  )


# ------------------------------------------------------------------------------
# Stations for maps (all active stations)
# ------------------------------------------------------------------------------

stations_current = stations |> 
  dplyr::filter(!station_name %in% rovers_to_drop)

stations_for_maps = stations_current |> 
  mutate(
    station_type = replace(
      station_type,
      station_type == "Unknown",
      "Roving"
    ),
    my_text_size = ifelse(
      str_detect(map_label, "(Roving|Border)+$"),
      "small",
      "medium"
    )
  ) |> 
  dplyr::filter(station_name %in% stations_active) |> 
  distinct()

source(
  here::here(
    "02_IMDP_Figure_Generator",
    "utils",
    "station_label_offsets.R"
  )
)

station_labels = adjust_station_labels(
  stations_for_maps,
  offsets
)


# ------------------------------------------------------------------------------
# Stations active in *this* year only
# ------------------------------------------------------------------------------

stations_active_this_year = dat_all |> 
  dplyr::mutate(
    Station = dplyr::case_when(
      str_detect(Station, "Lower Mainland") ~ "Lower Mainland Roving",
      TRUE ~ Station
    )
  ) |> 
  dplyr::mutate(
    Station = dplyr::case_when(
      str_detect(Station, "Lower Mainland Roving") ~ "Lower Mainland",
      TRUE ~ Station
    )
  ) |> 
  dplyr::filter(Year == my.year) |> 
  dplyr::count(Station) |> 
  dplyr::filter(n > 5) |> 
  dplyr::pull(Station)

# Special handling for 2024
stations_active_this_year = c(
  stations_active_this_year,
  "Radium",
  "Sumas Border"
)

stations[stations$station_name == "Sumas Border", ]$map_label   = "Sumas"
stations[stations$station_name == "Sumas Border", ]$station_type =
  "Part-time Inspection Station"


# ------------------------------------------------------------------------------
# Stations for maps (this year only)
# ------------------------------------------------------------------------------

stations_this_year = stations |> 
  dplyr::filter(station_name %in% stations_active_this_year)

stations_for_maps_this_year = stations_this_year |> 
  mutate(
    my_text_size = ifelse(
      str_detect(map_label, "(Roving|Border)+$"),
      "small",
      "medium"
    )
  )

station_labels_this_year = adjust_station_labels(
  stations_for_maps_this_year,
  offsets
)


# ------------------------------------------------------------------------------
# Roving stations from previous years
# ------------------------------------------------------------------------------

stations_roving = stations |> 
  filter(station_type != "Permanent Inspection Station") |> 
  dplyr::filter(!station_name %in% c("Scheduled Inspection")) |> 
  dplyr::filter(!stringr::str_detect(station_name, "Notification")) |> 
  dplyr::mutate(
    map_label = ifelse(
      map_label == "Fraser Valley Roving",
      "Lower Mainland Roving",
      map_label
    ),
    station_name = ifelse(
      station_name == "Fraser Valley Roving",
      "Lower Mainland Roving",
      station_name
    )
  )

station_labels_roving = adjust_station_labels(
  stations_roving,
  offsets
)
