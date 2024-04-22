library(tidyverse)
library(openxlsx)

dat = readxl::read_excel(paste0(my_opts$zqm_operations_data_folder,'Lake Monitoring/2023/Lab results/BC Veliger Sampling Inventory 2023_11-20_Final Report.xlsx'))

dat = dat |> 
  set_names(snakecase::to_snake_case) 

dat = dat |> 
  dplyr::select(date_collected_yyyy_mm_dd,
                waterbody,
                containers,
                of_plankton_tows,
                sampling_group_agency)

dat = dat |> 
  as_tibble()
  
dat = dat |> 
  mutate(across(-date_collected_yyyy_mm_dd, str_squish))
  
dat |> 
  count(waterbody) |> 
  pull(waterbody)
# 85 different lakes have been sampled.

dat |> 
  select(containers) |> 
  summarise(total = sum(as.numeric(containers)))
# Looks like 996 containers

dat |> 
  select(of_plankton_tows) |> 
  summarise(total = sum(as.numeric(of_plankton_tows), na.rm=T))
# 2,996 plankton tows.