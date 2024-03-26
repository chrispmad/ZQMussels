#2023 PSFC Mussel Monitoring Data Call

library(readxl) 
library(tidyverse)
library(sf)
library(lubridate)
library(openxlsx)

my_opts = read_csv("C:/Users/CMADSEN/Downloads/LocalR/Options.csv") %>% 
  as.data.frame()

my_opts$year = 2023

#Load in waterbody sampling data from 2023.
dat = read_excel(paste0(my_opts$zqm_operations_data_folder,
                        "/Lake Monitoring/",
                        my_opts$year,
                        "/Lab results/BC Veliger Sampling Inventory 2023_11-20_Final Report.xlsx"))

#Load in the template
templ = read_excel(paste0(my_opts$zqm_operations_data_folder,"Lake Monitoring/2022/CRB request/CRB_Mussel_Monitoring_2022.xlsx"),
                   sheet = "Data (2022)")
templ = templ |> 
  dplyr::filter(!is.na(`Collecting Agency`))

watsh = read_sf(paste0(my_opts$remote_spatial_data,"shared_data_sets/WatershedGroups.shp"))
waterbodies = read_sf(paste0(my_opts$remote_spatial_data,"shared_data_sets/summarized_bc_waterbodies_same_gnis_joined.shp"))

# #Correct column names and drop empty first rows in template.
# names(templ) = templ[4,]
# templ = templ[5,]

#Add a column that unique identifies each sample.
colnames(dat)[1] <- "UniqID"

# Make column names nicer to work with.
dat = dat |>
  setNames(snakecase::to_snake_case(names(dat)))

dat_sf = dat |> 
  dplyr::filter(!is.na(lat_decimal_degrees) & !is.na(long_decimal_degrees)) |> 
  mutate(lat = as.numeric(str_remove(lat_decimal_degrees,'°')),
         lon = as.numeric(str_remove_all(str_remove(str_remove(long_decimal_degrees,"^\\."),'°'),' '))) |> 
  st_as_sf(coords = c('lon','lat'), crs = 4326) %>% 
  st_transform(crs = 3005)


# Add watershed name and waterbody type to dat.
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
  reframe(`Collecting Agency` = sampling_group_agency,
            `Water System Name` = WATERSHE_1,
            `Water Body Name` = GNIS_NA,
            `Sampling Location Description` = location_site_name,
            `State, Province` = "BC",
            `Date Sampled` = lubridate::ymd(date_collected_yyyy_mm_dd),
            `Latitude \r\n(in decimal degrees)` = str_remove(lat_decimal_degrees,'°'),
            `Longitude\r\n (in decimal degrees)` = str_remove_all(str_remove(str_remove(long_decimal_degrees,"^\\."),'°'),' '),
            `Type of tow` = type_of_plankton_tow_vertical_or_horizontal,
            `Length of tow \r\n(m)` = depth_length_of_tow_m,
            `Calculated: Volume of tow  (cu m)` = total_volume_sampled_l,
            `Datum latitude and longitude` = "WGS84",
            `Sample Collection Method` = "Plankton Tow") |> 
  as_tibble() |> 
  dplyr::mutate(`Latitude \r\n(in decimal degrees)` = as.numeric(`Latitude \r\n(in decimal degrees)`),
                `Longitude\r\n (in decimal degrees)` = as.numeric(`Longitude\r\n (in decimal degrees)`))
  

# Do we have all of the same colum names in the data to be exported
# in the CRB format?

data_into_crb_format = templ[0,] |> 
  dplyr::mutate(across(everything(), as.character)) |> 
  dplyr::mutate(`Calculated: Volume of tow  (cu m)` = as.numeric(`Calculated: Volume of tow  (cu m)`)) |> 
  dplyr::mutate(`Date Sampled` = as.Date(`Date Sampled`),
                `Latitude \r\n(in decimal degrees)` = as.numeric(`Latitude \r\n(in decimal degrees)`),
                `Longitude\r\n (in decimal degrees)` = as.numeric(`Longitude\r\n (in decimal degrees)`)) |> 
  dplyr::bind_rows(data_to_export)

all_crb_names = names(templ)
all_d_export_names = names(data_into_crb_format)

names_in_common = all_crb_names[all_crb_names %in% all_d_export_names]
missing_names = all_crb_names[!all_crb_names %in% all_d_export_names]

# All column names are now the same! Woot.

openxlsx::write.xlsx(data_into_crb_format,
                     '04_Extra_Figures_and_Scripts/CRB/output/2023_Provincial_PlanktonTow_LabResults_CRB_Format.xlsx',
                     overwrite = T)
