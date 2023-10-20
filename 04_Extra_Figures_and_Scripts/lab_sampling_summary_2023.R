library(tidyverse)
library(openxlsx)

dat = openxlsx::read.xlsx('04_Extra_Figures_and_Scripts/data/BC Veliger Sampling Inventory 2023.xlsx')

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
# 82 different lakes have been sampled.

dat |> 
  select(containers) |> 
  summarise(total = sum(as.numeric(containers)))
# Looks like 404 containers

dat |> 
  select(of_plankton_tows) |> 
  summarise(total = sum(as.numeric(of_plankton_tows), na.rm=T))
# 1,240 plankton tows.