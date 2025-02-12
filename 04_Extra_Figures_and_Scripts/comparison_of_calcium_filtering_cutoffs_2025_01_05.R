# Which lakes / rivers did NOT make the cut using the new 
# calcium cut-off of 100 mg/L?
library(tidyverse)

setwd(here::here())

my_opts = read_csv("Options.csv")

lan_dat = "J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2025/Prioritization model"

library(readxl)

d_old = read_excel(paste0(lan_dat,"/Final waterbody list based on risk estimates_2025_300_calc_limit.xlsx"))
d_new = read_excel(paste0(lan_dat,"/Final waterbody list based on risk estimates_2025.xlsx"))

# Temporary addition! Calcium levels with 300 mg/L max filter applied, 
# averaged at the waterbody scale. Join in to explore!

watersheds = sf::read_sf(paste0(my_opts$remote_spatial_data,"shared_data_sets/WatershedGroups_lowres.shp"))

wb_calc = read_csv("01_DataCleaning/output/calcium_only_filtered_for_sub_300_and_100_mg_L_at_waterbody_scale.csv") |> 
  # dplyr::select(-...1) |> 
  dplyr::left_join(sf::st_drop_geometry(watersheds) |> dplyr::select(WATERSHED_,WATERSHE_1)) |> 
  dplyr::rename(WatershedName = WATERSHE_1,
                Waterbody = GNIS_NA)
ws_calc = read_csv("01_DataCleaning/output/calcium_only_filtered_for_sub_300_and_100_mg_L_at_watershed_scale.csv")

# Join to d_old
d_old = d_old |> 
  dplyr::left_join(wb_calc) |> 
  dplyr::left_join(ws_calc) |> 
  dplyr::select(WatershedName:SummedDamCapacity,
                mean_calcium_wb:number_calcium_data_points_100_f_wb,
                mean_calcium_ws:max_calcium_100_f_ws,
                calcium_bin:Sampling_Frequency)

wbs_in_common = d_old[d_old$Waterbody %in% d_new$Waterbody,]$Waterbody
wbs_in_common = wbs_in_common[order(wbs_in_common)]

wbs_dropped = d_old[!d_old$Waterbody %in% d_new$Waterbody,]$Waterbody
wbs_dropped = wbs_dropped[order(wbs_dropped)]

wbs_in_common

wbs_dropped

d_old |> 
  dplyr::filter(Waterbody %in% wbs_dropped) |> 
  View()

d_old |> 
  dplyr::filter(Waterbody %in% wbs_dropped) |> 
  dplyr::select(WatershedName, Waterbody, calcium_bin, mean_calcium_wb:calcium_data) |> 
  View()
# Since we have included more recent calcium data for this year's prioritization model, 
# it is conceivable that the wb list with 142 wbs was including some calcium
# outliers between 100 and 300 mg/L.


# Take the mean calcium at the waterbody filtering out 100+ mg/L values; also maybe include min, max, median.

# Also, show the mean subwatershed calcium value pre-binning.

# Is Elk Lake super high calcium?
