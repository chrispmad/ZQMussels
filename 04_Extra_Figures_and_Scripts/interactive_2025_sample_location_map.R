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

my.year = 2024

lab_dat = read_excel(paste0(my_opts$zqm_operations_data_folder,'Lake Monitoring/',my.year,'/Lab results/Final report/BC Veliger Sampling Inventory 2024_FINAL REPORT.xlsx'))

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


lab_sf_report = lab_sf_report |> 
  select(Date_Collected, Waterbody, `Location (site name)`, `Type of plankton tow (vertical or horizontal)`, 
         `Zebra/Quagga mussels veligers`, `sampling group/agency`, long, lat, geometry)


lab_sf_report = lab_sf_report |>
  filter(!(`sampling group/agency` %in% c("WLRS", "MLWRS")))


write_sf(lab_sf_report, "./output/funding_request_locations_2024.gpkg")
write.csv(lab_sf_report |> st_drop_geometry(), "./output/funding_request_locations_2024.csv")
