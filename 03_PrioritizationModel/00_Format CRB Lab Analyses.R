#2021 PSFC Mussel Monitoring Data Call
rm(list=ls())

library(readxl) 
library(tidyverse)
library(sf)
library(lubridate)
library(openxlsx)

my_opts = read_csv("I:/Admin/R_Scripts/Options.csv") %>% 
  as.data.frame()

#Load in waterbody sampling data from this 2021.
dat = read_excel(paste0("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/",
                        my_opts$year,
                        "/Lab Analysis/Final report and data/BC Veliger Sampling Inventory 2022_FinalReport.xlsx"))

#Load in the template
templ = read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2021/Lab Analysis/Final report/CRB_Mussel_Monitoring_2021.xlsx",
                   sheet = "Data (2021)")

watsh = read_sf("W:/CMadsen/shared_data_sets/WatershedGroups.shp")
waterbodies = read_sf("W:/CMadsen/shared_data_sets/summarized_bc_waterbodies_same_gnis_joined.shp")

#Correct column names and drop empty first rows in template.
names(templ) = templ[4,]
templ = templ[5,]

#Add a column that unique identifies each sample.
colnames(dat)[1] <- "UniqID"

# Make column names nicer to work with.
dat = dat %>% 
  setNames(snakecase::to_snake_case(names(.)))

# Add watershed name and waterbody type to dat.
dat_sf = dat %>% 
  mutate(lat = as.numeric(str_remove(lat_decimal_degrees,'°')),
         lon = as.numeric(str_remove_all(str_remove(str_remove(long_decimal_degrees,"^\\."),'°'),' '))) %>% 
  st_as_sf(coords = c('lon','lat'), crs = 4326) %>% 
  st_transform(crs = 3005)

dat_sf = dat_sf %>% 
  st_join(watsh %>% dplyr::select(WATERSHED_, WATERSHE_1), st_intersects) %>% 
  st_join(waterbodies, st_intersects)

# # Check that the waterbody we matched with spatially is the same as reported.  
# dat_sf %>% 
#   st_drop_geometry() %>% 
#   dplyr::select(waterbody,GNIS_NA) %>% 
#   filter(waterbody != GNIS_NA) %>% distinct()

# Correct some naming differences.
dat_sf = dat_sf %>% 
  mutate(waterbody = case_when(
    waterbody == 'Arrow Lake, Lower' ~ 'Lower Arrow Lake',
    waterbody == 'Arrow Lake, Upper' ~ 'Upper Arrow Lake',
    waterbody == 'Deka Lake' ~ 'Bridge Lake',
    waterbody == 'Francois Lake' ~ 'François Lake',
    waterbody == 'Kinbasket Reservoir' ~ 'Kinbasket Lake',
    waterbody == 'Koocanusa  Lake' ~ 'Lake Koocanusa',
    waterbody == 'Kootenay River (Nelson)' ~ 'Kootenay River',
    waterbody == 'Lac La Hache' ~ 'Lac la Hache',
    waterbody == "Pend d'Oreille River" ~ "Pend-d'Oreille River",
    waterbody == 'Premier Lake' ~ 'Whiteswan Lake',
    waterbody == 'Revelstoke Reservoir' ~ 'Revelstoke Lake',
    waterbody == 'Shuswap Lake' ~ 'Little Shuswap Lake',
    waterbody == 'Wahleach' ~ 'Wahleach Lake',
    T ~ waterbody
  ))

# Correct date column.
dat_sf = dat_sf %>% 
  filter(str_detect(date_collected_yyyy_mm_dd, '4[0-9]{4}.*')) %>% 
  mutate(date_collected_yyyy_mm_dd = convertToDate(date_collected_yyyy_mm_dd)) %>% 
  bind_rows(
    dat_sf %>% 
      filter(!str_detect(date_collected_yyyy_mm_dd, '4[0-9]{4}.*')) %>% 
      mutate(date_collected_yyyy_mm_dd = ymd(date_collected_yyyy_mm_dd))
  )

# Make dat like template.
data_to_export = dat_sf %>% 
  st_drop_geometry() %>% 
  summarise(`Collecting Agency` = sampling_group_agency,
            `Water System Name` = WATERSHE_1,
            `Water Body Name` = GNIS_NA,
            `Sampling Location Description` = location_site_name,
            `State, Province` = "BC",
            `Date Sampled` = format(date_collected_yyyy_mm_dd,"%m-%d-%y"),
            `Latitude \r\n(in decimal degrees)` = str_remove(lat_decimal_degrees,'°'),
            `Longitude\r\n (in decimal degrees)` = str_remove_all(str_remove(str_remove(long_decimal_degrees,"^\\."),'°'),' '),
            `Type of tow` = type_of_plankton_tow_vertical_or_horizontal,
            `Length of tow` = depth_length_of_tow_m,
            `Calculated: Volume of tow (cu m)` = total_volume_sampled_l,
            `Datum latitude and longitude` = "WGS84",
            `Sample Collection Method` = "Plankton Tow")

data_to_export = templ[0,] %>% 
  bind_rows(data_to_export)

openxlsx::write.xlsx(data_to_export,
                     paste0(my_opts$zqm_operations_data_folder,"Lake Monitoring/",
                     my_opts$year,
                     "/Lab Analysis/Final report and data/BC Veliger Sampling Inventory 2022_FinalReport_CRB_format.xlsx"),
                     overwrite = T)
