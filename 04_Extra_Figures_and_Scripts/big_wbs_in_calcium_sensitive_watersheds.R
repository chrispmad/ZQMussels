library(sf)
library(tidyverse)

setwd(here::here())

my_opts = read_csv("Options.csv")

waterb = read_sf(paste0(my_opts$remote_spatial_data,"Projects/ZQMussels/data/waterb_with_data.gpkg"))

calc = read_sf(paste0(my_opts$base_dir,'01_DataCleaning/output/ZQM_RiskAssessment_ssp370_10C_threshold_Subwatershed.gpkg'))

high_calc = calc |> 
  dplyr::filter(Watershed_Mean > 30)

waterb_in_high_calc = waterb |> 
  dplyr::filter(st_intersects(geom, high_calc, sparse = F))