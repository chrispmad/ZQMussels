library(readxl)
library(tidyverse)

dat = read_csv("J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years/metabase_2022.csv")

# Previous WB other details
output = dat |> 
  rename(prev_wb = `Previous Waterbody 1 Other Details`) |> 
  filter(!is.na(prev_wb)) |> 
  dplyr::count(prev_wb, sort = T) |> 
  dplyr::mutate(wb_name = str_remove_all(prev_wb, '(,|WA|Washington|Bc|BC).*')) |> 
  dplyr::mutate(prev_wb = str_remove(str_remove(prev_wb, wb_name),',[ ]?')) |> 
  dplyr::mutate(wb_state = str_extract(prev_wb, '^[A-Za-z]*')) |> 
  mutate(prev_wb = str_squish(str_remove(str_remove(prev_wb, wb_state),',[ ]?'))) |> 
  mutate(wb_country = str_extract(prev_wb, '^[A-Za-z]*')) |> 
  mutate(wb_name = stringr::str_to_title(wb_name)) |> 
  mutate(wb_name = case_when(
    str_detect(wb_name, '^Lake') ~ str_squish(paste0(str_remove(wb_name,"^Lake"), " Lake")),
    str_detect(wb_name, '^River') ~ str_squish(paste0(str_remove(wb_name,"^River"), " River")),
    T ~ str_squish(wb_name)
  )) |> 
  slice(1:50) |> 
  mutate(wb_state = case_when(
    wb_name == 'Amber Lake' ~ 'WA',
    wb_state %in% c('WA','WAS',"Washington") ~ "WA",
    wb_state %in% c("Bc","BC") ~ "BC",
    wb_state %in% c("Alberta") ~ "AB",
    wb_state %in% c("Manitoba") ~ "MB",
    wb_state %in% c('OR","Or') ~ "OR",
    wb_state %in% c("Colorado","CO") ~ "CO",
    T ~ wb_state
  )) |> 
  dplyr::mutate(wb_country = case_when(
    wb_country == 'OR' ~ 'USA',
    wb_state %in% c("WA","OR","CO","AZ") ~ 'USA',
    wb_state %in% c("BC","MB","AB","ON") ~ 'Canada',
    T ~ wb_country
  )) |> 
  arrange(wb_name,desc(wb_state)) |> 
  select(-prev_wb) |> 
  add_count(wb_name) |> 
  filter(!is.na(wb_country) | nn == 1) |> 
  select(-nn) |> 
  mutate(wb_name = str_remove(wb_name, ' Usa$')) |> 
  group_by(wb_name, wb_state, wb_country) |> 
  summarise(n = sum(n)) |> 
  arrange(desc(n))

# Renaming
output

output |> 
  summarise(
    water_body_name = wb_name,
    country_code = ifelse(wb_country == 'USA',"USA","CAN"),
    province_code = wb_state)

iwrite_csv(output, '04_Extra_Figures_and_Scripts/output/top_50_source_waterbodies.csv')
# Destination wb names
dat |> 
  dplyr::count(`Destination Waterbody 1 Other Details`, sort = T)