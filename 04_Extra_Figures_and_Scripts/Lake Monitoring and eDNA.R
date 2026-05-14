library(tidyverse)
library(sf)
library(dplyr)
library(ggplot2)
library(bcdata)
library(bcmaps)
library(readxl)

# source("02_IMDP_Figure_Generator/utils/initialise_plots.R")

source("./02_IMDP_Figure_Generator/utils/get_basemaps.R")

my_opts = read_csv(paste0(here::here(),"/Options.csv"))

my.year = 2025
## Read in data
lab_dat = read_excel(paste0(my_opts$zqm_operations_data_folder,'Lake Monitoring/',my.year,'/Lab results/Final report/BC Veliger Sampling Inventory 2025_FINAL_REPORT.xlsx'))

lab_dat =  lab_dat |> 
  dplyr::mutate(Waterbody = stringr::str_remove_all(Waterbody, " \\(.*")) |> 
  dplyr::mutate(Waterbody = stringr::str_remove_all(Waterbody, "\\,.*")) |> 
  dplyr::mutate(Waterbody = stringr::str_remove_all(Waterbody, "\\'"))

lab_sf = lab_dat |> 
  dplyr::rename(
    lat = `Lat (Decimal degrees)`,
    lng = `Long (Decimal degrees)`
  ) |> 
  mutate(lat = as.numeric(lat),
         lng = as.numeric(lng)) |> 
  dplyr::filter(!is.na(lat)) |> 
  st_as_sf(coords = c("lng","lat"),
           crs = 4326)

# Make excel sheet for updating final report appendix!
lab_sf |> 
  # Join the Natural Resource regions (n = 8) to the lakes.
  st_join(
    sf::read_sf(paste0(my_opts$remote_spatial_data,'shared_data_sets/FLNRO_Fishing_Boundaries.shp')) |>
      st_transform(crs = 4326) |> 
      dplyr::select(Region = REGION_N)
  ) |> 
  st_drop_geometry() |> 
  tidyr::separate_longer_delim(cols = `Type of plankton tow (vertical or horizontal)`, delim = ', ') |> 
  count(Waterbody,Region,`sampling group/agency`,`Type of plankton tow (vertical or horizontal)`,`Zebra/Quagga mussels veligers`) |> 
  dplyr::rename(`Sampling Group/Agency` = `sampling group/agency`) |> 
  dplyr::select(-n) |> 
  dplyr::select(-c(`Type of plankton tow (vertical or horizontal)`)) |> 
  mutate(Waterbody = str_squish(Waterbody)) |> 
  distinct() |> 
  filter(!is.na(Region)) |> 
  dplyr::group_by(Waterbody) |> 
  dplyr::summarise(Region = paste0(Region, collapse = ', '),
                   `Sampling Group/Agency` = paste0(`Sampling Group/Agency`, collapse = ', '),
                   `Zebra/Quagga mussels veligers` = paste0(`Zebra/Quagga mussels veligers`, collapse = ', ')) |> 
  openxlsx::write.xlsx(file = 'lake_monitoring_for_report_appendix.xlsx')

sampling_pal = leaflet::colorFactor(
  palette = 'Set3',
  domain = lab_sf$`sampling group/agency`
)

lab_sf = lab_sf |> 
  mutate(my_colors = sampling_pal(`sampling group/agency`))

lab_sf = lab_sf |> 
  mutate(my_colors = sampling_pal(`sampling group/agency`))


legend_df_lab <- lab_sf |>
  sf::st_drop_geometry() |>
  dplyr::select(`sampling group/agency`, my_colors) |>
  distinct()

#--------------------------------
# We have the plankton tow locations, now lets grab the ZQ eDNA results
# dates to be added to the xlsx 	I:\WFC AEB\General\2 SCIENCE - Invasives\SPECIES\Whirling Disease\Monitoring\2025\Lab results\Filled metadata sheets
edna_results = read_excel("./data/Whirling_Disease_2025_Sample_Tracking.xlsx")

edna_results = edna_results |> 
  filter(!is.na(`Result Zebra Mussels`)) |> 
  mutate(Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude)) |> 
  filter(!is.na(Longitude) & !is.na(Latitude))


edna_results_sf <- st_as_sf(
  edna_results,
  coords = c("Longitude", "Latitude"),
  crs = 4326,        # WGS84
  remove = FALSE    # keep Lon/Lat columns if you want
)

#------------------------
# Add this Z:\2 SCIENCE - Invasives\GENERAL\Budget\Canada Nature fund 2023-2026\Year 2 (2025-2026)\eDNA ZQM Pylet Lab\Sample collection & results
# ZQM data Batch 1 complete.xlsx

pylet_locations<- read_excel("Z:/2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Year 2 (2025-2026)/eDNA ZQM Pylet Lab/Sample collection & results/ZQM data Batch 1 complete_with-WB-TALLY.xlsx",
                             sheet = "COS")

pylet_locations = pylet_locations |> 
  mutate(Lat = str_remove(Lat, " N"), Long = str_remove(Long, " W")) |> 
  mutate(Long = as.numeric(Long),
                       Lat = as.numeric(Lat)) |> 
  mutate(Long = -1*abs(Long)) |> 
  filter(!is.na(Long) & !is.na(Lat) & !is.na(Lake))

pylet_locations_sf <- st_as_sf(
  pylet_locations,
  coords = c("Long", "Lat"),
  crs = 4326,        # WGS84
  remove = FALSE    # keep Lon/Lat columns if you want
  
  
)
#------------


bc_stations_basemap_trimmed <- terra::trim(bc_stations_basemap_white)
raster_bbox_df <- as.list(terra::ext(bc_stations_basemap_trimmed))
bc_bound = bcmaps::bc_bound() |> st_transform(4326)


map_9_ggplot <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_trimmed) +
  geom_sf(data = bc_bound, color = "grey", fill = NA, linewidth = 1) +
  
  # Existing lab points
  geom_sf(
    data = lab_sf,
    aes(fill = `sampling group/agency`),
    shape = 21,
    color = "black",
    size = 3,
    stroke = 0.2
  ) +
  
  # eDNA points (black, different shape)
  geom_sf(
    data = edna_results_sf,
    aes(shape = "eDNA sampling"),
    fill  = "transparent",
    color = "#0e148a",
    size  = 3,
    linewidth = 3
  )+
  geom_sf(
    data = pylet_locations_sf,
    aes(shape = "eDNA sampling"),
    fill  = "transparent",
    color = "#0e148a",
    size  = 3,
    linewidth = 3
  )+
  scale_shape_manual(
    name = NULL,
    values = c("eDNA sampling" = 22)
  )+
  
  scale_fill_manual(
    name = paste0(my.year, " Plankton\n Tow Sampling"),
    values = setNames(
      legend_df_lab$my_colors,
      legend_df_lab$`sampling group/agency`
    )
  ) +
  
  coord_sf(
    xlim = c(raster_bbox_df$xmin, raster_bbox_df$xmax),
    ylim = c(raster_bbox_df$ymin, raster_bbox_df$ymax),
    expand = FALSE
  ) +
  ggspatial::annotation_scale(location = "bl") +
  ggthemes::theme_map() +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(
      fill = alpha("white", 0.7),
      color = "black"
    ),
    legend.key = element_blank()
  )

map_9_ggplot


  # tmap_save(tm = map_9_tmap, filename = 'Map9_Map_of_Lake_Sampling.jpg', width = 7.6, height = 6.4, dpi = 300)
ggsave(filename = './images/Map9_Map_of_Lake_Sampling_andeDNA.jpg',map_9_ggplot, width = 7.6, height = 6.4, dpi = 300)

# 
# lab_sf
# 
# edna_results_sf
# 
# pylet_locations_sf


lab_sf_reduced <- lab_sf %>%
  mutate(
    Date_Collected = as.character(`Date Collected (yyyy-mm-dd)`),   # force character
    Date_Collected = case_when(
      grepl("^\\d+$", Date_Collected) ~ as.Date(as.numeric(Date_Collected), origin = "1899-12-30"),           # Excel integer
      grepl("^\\d+\\.\\d+$", Date_Collected) ~ as.Date(floor(as.numeric(Date_Collected)), origin = "1899-12-30"),  # Excel fractional
      TRUE ~ parse_date_time(Date_Collected, orders = c("ymd", "mdy", "dmy"))  # proper text dates
    ),
    Date_Collected = as.Date(Date_Collected)  # ensure final class
  ) |> 
  mutate(long = sf::st_coordinates(geometry)[,1],
         lat = sf::st_coordinates(geometry)[,2]) |> 
  select(Waterbody,Date_Collected, long, lat) |> 
  st_drop_geometry() |> 
  mutate(across(everything(), as.character))

lab_sf_reduced$Type = "Plankton Tow"

edna_results_sf_reduced = edna_results_sf |> 
  st_drop_geometry() |> 
  select(`Waterbody Name`, Latitude, Longitude) |> 
  mutate(across(everything(), as.character)) |> 
  rename(Waterbody = `Waterbody Name`, long = Longitude, lat = Latitude)

edna_results_sf_reduced$Type = "eDNA"
edna_results_sf_reduced$Date_Collected = "NA"


## we need waterbodies 
waterb = read_sf(paste0(my_opts$base_dir, "/01_DataCleaning/data/summarized_bc_waterbodies_same_gnis_joined.gpkg"))

names(pylet_locations_sf) <- janitor::make_clean_names(names(pylet_locations_sf))

pylet_locations_sf_reduced = pylet_locations_sf |> 
  mutate(Date_Collected = as.character(date_yyyy_mm_dd),
         Date_Collected = case_when(
           grepl("^\\d+$", Date_Collected) ~ as.Date(as.numeric(Date_Collected), origin = "1899-12-30"),           # Excel integer
           grepl("^\\d+\\.\\d+$", Date_Collected) ~ as.Date(floor(as.numeric(Date_Collected)), origin = "1899-12-30"),  # Excel fractional
           TRUE ~ parse_date_time(Date_Collected, orders = c("ymd", "mdy", "dmy"))  # proper text dates
         ),
         Date_Collected = as.Date(Date_Collected)
         ) |> 
  # we need waterbody names for each of the geoms in this list, from waterb
  select(lake, Date_Collected, lat, long) |> 
  st_drop_geometry() |> 
  rename(Waterbody = lake) |> 
  mutate(Waterbody = paste0(Waterbody, " Lake")) |> 
  mutate(across(everything(), as.character))

pylet_locations_sf_reduced$Type = "eDNA"


full_data = bind_rows(lab_sf_reduced, edna_results_sf_reduced, pylet_locations_sf_reduced)

#write to excel
library(openxlsx)
write.xlsx(full_data,"output/sample_locations2025.xlsx")



#-----------------------
# we need spatial files for the samples taken - which ones are relevant?


lab_sf




lab_sf_report <- lab_sf %>%
  mutate(
    Date_Collected = as.character(`Date Collected (yyyy-mm-dd)`),   # force character
    Date_Collected = case_when(
      grepl("^\\d+$", Date_Collected) ~ as.Date(as.numeric(Date_Collected), origin = "1899-12-30"),           # Excel integer
      grepl("^\\d+\\.\\d+$", Date_Collected) ~ as.Date(floor(as.numeric(Date_Collected)), origin = "1899-12-30"),  # Excel fractional
      TRUE ~ parse_date_time(Date_Collected, orders = c("ymd", "mdy", "dmy"))  # proper text dates
    ),
    Date_Collected = as.Date(Date_Collected)  # ensure final class
  ) |> 
  mutate(long = sf::st_coordinates(geometry)[,1],
         lat = sf::st_coordinates(geometry)[,2]) 

lab_sf_report$Type = "Plankton Tow"

lab_sf_report = lab_sf_report |> 
  select(Date_Collected, Waterbody, `Location (site name)`, `Type of plankton tow (vertical or horizontal)`, 
         `Zebra/Quagga mussels veligers`, `sampling group/agency`, long, lat, geometry)


edna_results_sf_report = edna_results_sf |> 
  select(`Date Collected`, `Waterbody Name`, `Final sample site name`, Longitude, Latitude,
  `Sampling organization`, `Sampling method (eDNA or fish or both)`, geometry) |> 
  dplyr::mutate(
    `Sampling organization` = dplyr::case_when(
      `Sampling organization` == "ONA (Eleanor)" ~ "ONA",
      `Sampling organization` == "ONA (Sam)" ~ "ONA",
      TRUE ~ `Sampling organization`
    )
  )

pylet_locations_report = pylet_locations_sf |> 
  select(lake, sample_name_site_name_replicate_number_batch_number, date_yyyy_mm_dd, lat, long, geometry)


CSISS = lab_sf_report |> 
  filter(`sampling group/agency` == "CSISS")
FVISS = lab_sf_report |> 
  filter(`sampling group/agency` == "FVISS")
CKISS = lab_sf_report |> 
  filter(`sampling group/agency` == "CKISS")
ISCBC = lab_sf_report |> 
  filter(`sampling group/agency` == "ISCBC")
BISS = lab_sf_report |> 
  filter(`sampling group/agency` == "BISS")
CLSS = lab_sf_report |> 
  filter(`sampling group/agency` == "CLSS")
OASISS = lab_sf_report |> 
  filter(`sampling group/agency` == "OASISS")
EKISC = lab_sf_report |> 
  filter(`sampling group/agency` == "EKISC")
ONA = lab_sf_report |> 
  filter(`sampling group/agency` == "ONA")

## got them all bar LRISS - where are these points?
ONA_2 = edna_results_sf_report |> 
  filter(`Sampling organization` == "ONA") |> 
  rename(Date_Collected = `Date Collected`, Waterbody = `Waterbody Name`, `Location (site name)` = `Final sample site name`,
         long = Longitude, lat = Latitude, `sampling group/agency` = `Sampling organization`)

final_list_sf = dplyr::bind_rows(CSISS, FVISS, CKISS, ISCBC, BISS, CLSS, OASISS, EKISC, ONA, ONA_2)

final_list_sf = final_list_sf |> 
  select(-`Zebra/Quagga mussels veligers`)

final_list_sf <- final_list_sf %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(
    geometry = sf::st_sfc(
      lapply(sf::st_geometry(.), function(pt) {
        coords <- sf::st_coordinates(pt)
        coords[1] <- -abs(coords[1])   # force longitude negative
        sf::st_point(coords)
      }),
      crs = 4326
    )
  ) |> 
  mutate(long = sf::st_coordinates(geometry)[,1],
         lat = sf::st_coordinates(geometry)[,2])



write_sf(final_list_sf, "./output/funding_request_locations.gpkg")
write.csv(final_list_sf |> st_drop_geometry(), "./output/funding_request_locations.csv")



map_10_ggplot <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_trimmed) +
  geom_sf(data = bc_bound, color = "grey", fill = NA, linewidth = 1) +
  
  # Existing lab points
  geom_sf(
    data = final_list_sf,
    aes(fill = `sampling group/agency`),
    shape = 21,
    color = "black",
    size = 3,
    stroke = 0.2
  ) +
  
  scale_shape_manual(
    name = NULL,
    values = c("eDNA sampling" = 22)
  )+
  
  scale_fill_manual(
    name = paste0(my.year, " Plankton\n Tow Sampling"),
    values = setNames(
      legend_df_lab$my_colors,
      legend_df_lab$`sampling group/agency`
    )
  ) +
  
  coord_sf(
    xlim = c(raster_bbox_df$xmin, raster_bbox_df$xmax),
    ylim = c(raster_bbox_df$ymin, raster_bbox_df$ymax),
    expand = FALSE
  ) +
  ggspatial::annotation_scale(location = "bl") +
  ggthemes::theme_map() +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(
      fill = alpha("white", 0.7),
      color = "black"
    ),
    legend.key = element_blank()
  )

map_10_ggplot


# tmap_save(tm = map_9_tmap, filename = 'Map9_Map_of_Lake_Sampling.jpg', width = 7.6, height = 6.4, dpi = 300)
ggsave(filename = './images/Funded_Map.jpg',map_10_ggplot, width = 7.6, height = 6.4, dpi = 300)
