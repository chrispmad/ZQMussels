#library(rjson)
library(tidyverse)

options = data.frame(#base_dir = "C:/Users/CMADSEN/Downloads/LocalRWork/",
  base_dir = "C:/Users/CMADSEN/Downloads/LocalR/ZQMussels/",
  zqm_risk_climate_model = "ssp370",
  zqm_risk_temp_limit = 10,
  zqm_risk_calc_param = "all",
  remote_spatial_data = "W:/CMadsen/",
  # What year of data are we working on?
  year = 2022,
  zqm_operations_data_folder = "J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/",
  zqm_figure_output_remote_folder = "J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Communications/Inspection data reporting/Final report/",
  zqm_figure_local_folder = "C:/Users/CMADSEN/Downloads/LocalR/ZQMussels/02_IMDP_Figure_Generator/",
  local_data_folder = "C:/Users/CMADSEN/Downloads/LocalR/ZQMussels/data/")

write_csv(options, paste0(options$base_dir,"/Options.csv"))
