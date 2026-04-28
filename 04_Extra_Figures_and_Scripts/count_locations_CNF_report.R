## get the veliger samples taken this year, then count the locations and the agencies
# For Rocky Mountain Ridged Mussel, they are only in a couple places - count those ones and add to the report
# location are Okanagan Lake, Skaha Lake, Osoyoos Lake

library(tidyverse)
library(sf)
library(bcdata)
library(bcmaps)
library(readxl)
library(leaflet)
library(leafpop)
library(leaflet.extras)
library(janitor)
library(here)
library(openxlsx)
library(htmlwidgets)

lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/shared_data_sets/"


dat<-openxlsx::read.xlsx(here::here("./data/BC Veliger Sampling Inventory 2025_FINAL_REPORT.xlsx"))

dat_edna = read.csv(here::here("./data/2025_sample_locations_final.csv")) |> 
  filter(sample_type == "eDNA")

dat <- dat |> 
  clean_names()

dat<- dat |> 
  rename(count =  x1)

dat<- dat |> 
  mutate(date_collected_yyyy_mm_dd = as.Date(as.numeric(date_collected_yyyy_mm_dd), origin = "1899-01-01"))

dat_complete<-dat |> 
  mutate(long_decimal_degrees = as.numeric(long_decimal_degrees),
         lat_decimal_degrees = as.numeric(lat_decimal_degrees)) |> 
  filter(!is.na(lat_decimal_degrees) | !is.na(long_decimal_degrees))
dat_missing<-dat |> 
  mutate(long_decimal_degrees = as.numeric(long_decimal_degrees),
         lat_decimal_degrees = as.numeric(lat_decimal_degrees)) |> 
  filter(is.na(lat_decimal_degrees) | is.na(long_decimal_degrees))


#fix wrong longs - all should be negative
dat_complete<-dat_complete |> 
  mutate(long_decimal_degrees = ifelse(long_decimal_degrees > 0, long_decimal_degrees * -1, long_decimal_degrees))


dat_sf<-st_as_sf(dat_complete, coords = c("long_decimal_degrees", "lat_decimal_degrees"), crs = 4326)


############## 
# --------- we also want the eDNA samples taken in 2025 as part of WD 
dat_edna = dat_edna |> 
  rename(count = X) |> 
  mutate(long_decimal_degrees = ifelse(long_decimal_degrees > 0, long_decimal_degrees * -1, long_decimal_degrees))
dat_edna_sf = st_as_sf(dat_edna, coords = c("long_decimal_degrees", "lat_decimal_degrees"), crs = 4326)


ridge = dat_edna_sf |> 
  dplyr::filter(waterbody %in% c("Okanagan Lake", "Skaha Lake", "Osoyoos Lake"))

ridge2 = dat_sf |>
  dplyr::filter(waterbody %in% c("Okanagan Lake", "Skaha Lake", "Osoyoos Lake"))


ridge2 |>
  dplyr::distinct(waterbody, geometry) |>
  dplyr::count(waterbody, name = "n_locations")


ridge2_locations <- ridge2 |>
  distinct(waterbody, location_site_name, geometry)



leaflet(ridge2_locations) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  
  addCircleMarkers(
    radius = 5,
    stroke = TRUE,
    weight = 1,
    color = "black",
    fillColor = "#2b8cbe",
    fillOpacity = 0.8,
    
    label = ~location_site_name,
    labelOptions = labelOptions(
      direction = "auto",
      textsize = "12px"
    ),
    
    popup = ~paste0(
      "<strong>Site:</strong> ", location_site_name, "<br>",
      "<strong>Waterbody:</strong> ", waterbody
    )
  )
