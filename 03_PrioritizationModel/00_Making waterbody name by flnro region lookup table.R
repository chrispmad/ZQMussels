library(tidyverse)
library(sf)

my_opts = jsonlite::read_json("C:/Users/CMADSEN/Downloads/LocalRWork/Options.json") %>% 
  as.data.frame()

wats = read_sf(paste0(my_opts$local_data_folder,"shapefiles/all_bcg_waterbodies_cleaned.shp"))

flnro = read_sf(paste0(my_opts$remote_spatial_data,"shared_data_sets/FLNRO_Fishing_Boundaries.shp"))

name_list = wats %>% 
  filter(!is.na(GNIS_NAME_)) %>% 
  st_join(flnro, st_intersects) %>% 
  select(GNIS_NAME_,REGION_G,REGION_N) %>% 
  st_drop_geometry()

openxlsx::write.xlsx(x = name_list,
                     paste0(my_opts$remote_spatial_data,"shared_data_sets/waterbody_name_flrno_region_lookup_table.xlsx"),
                     overwrite = T)
