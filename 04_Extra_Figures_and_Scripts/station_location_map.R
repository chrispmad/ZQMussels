library(tidyverse)
library(dplyr)
library(sf)
library(readxl)

source(here::here("02_IMDP_Figure_Generator/utils/initialise_plots.R"))

source(here::here("02_IMDP_Figure_Generator", "utils", "load_figure_data.R"))


dat = dat |> 
  mutate(Start_Time = as.POSIXct(
    Start_Time * 86400,
    origin = "1899-12-30",
    tz = "UTC"
  ))

source(here::here("02_IMDP_Figure_Generator", "utils", "station_manipulation.R"))

source(here::here("02_IMDP_Figure_Generator", "utils", "get_basemaps.R"))




stations_for_maps <- stations_for_maps_this_year |>
  filter(station_name != "Radium") |>
  mutate(
    station_type = if_else(
      station_name %in% c("Sumas", "Pacific"),
      "Permanent Inspection Station",
      station_type
    )
  ) |>
  mutate(
    station_name = if_else(
      station_name == "Sumas",
      "Huntington (Sumas)",
      station_name
    )
  )




station_labels = adjust_station_labels(
  stations_for_maps,
  offsets
)

station_labels = station_labels |> 
  mutate(
    map_label = if_else(
      station_name == "Sumas",
      "Huntington (Sumas)",
      station_name
    )
  )

test = stations_for_maps[stations_for_maps$station_name == "Pacific",]

coords <- st_coordinates(test)

lon <- coords[1, "X"]
lat <- coords[1, "Y"]


p1 <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_colour) +
  geom_sf(
    data = st_buffer(stations_for_maps, 20000),
    aes(fill = station_type)
  ) +
  geom_sf_label(
    data = station_labels,
    aes(label = map_label),
    size = 3, fill = "white", color = "black", label.size = 0.1
  ) +
  scale_fill_manual(
    name = "2026 Watercraft \n Inspection Stations",
    breaks = c(
      "Permanent Inspection Station",
      "Part-time Inspection Station",
      "Roving Inspection Crew"
    ),
    values = c(
      "Permanent Inspection Station" = "lightblue",
      "Part-time Inspection Station" = "#E8DE51",
      "Roving Inspection Crew" = "purple"
    )
  ) +
  ggspatial::annotation_scale() +
  ggthemes::theme_map() +
  theme(
    legend.position = c(0.68, 0.72),
    legend.background = element_rect(colour = "black")
  )

p1

ggsave(paste0('./images/2026_Stations.jpg'), p1, width = 7.6, height = 6.4, dpi = 300)


p2<-ggplot()+
  tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_colour)+
  geom_sf(data = st_buffer(stations_for_maps_this_year,20000), aes(fill = as.factor(station_type)))+
  geom_sf_label(data = station_labels_this_year, aes(label = map_label),
                size = 3, fill = "white", color = "black", label.size = 0.1)+
  scale_fill_manual(name = "2025 Watercraft \n Inspection Stations", values = c("Permanent Inspection Station" = "lightblue", "Roving Inspection Crew" = "purple", "Part-time Inspection Station" = "yellow"))+
  ggspatial::annotation_scale()+
  ggthemes::theme_map()+
  theme(legend.position = c(0.68,0.75),
  )

p2
