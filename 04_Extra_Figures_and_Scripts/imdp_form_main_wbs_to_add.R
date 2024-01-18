library(readxl)
library(tidyverse)

dat_2020 = read_csv("J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years/metabase_2020.csv")
dat_2021 = read_csv("J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years/metabase_2021.csv")
dat_2022 = read_csv("J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years/metabase_2022.csv")
dat_2023 = read_csv("J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years/metabase_2023.csv")

dat = list(dat_2020,dat_2021,dat_2022,dat_2023)

dat = lapply(dat, \(x) {
  x |> 
    dplyr::select(prev_wb = `Previous Waterbody 1 Other Details`,
                  dest_wb = `Destination Waterbody 1 Other Details`)
}) |> 
  bind_rows() |> 
  dplyr::filter(!is.na(prev_wb) | !is.na(dest_wb))

# Previous WB other details
sources = dat |> 
  filter(!is.na(prev_wb)) |> 
  dplyr::count(prev_wb, sort = T) |> 
  # dplyr::filter(n > 1) |> 
  # Remove things in parentheses, e.g. (Caroline).
  dplyr::mutate(prev_wb = str_remove_all(prev_wb, '[ ]?\\(.*\\)[ ]?')) |> 
  dplyr::mutate(wb_name = str_remove_all(prev_wb, '(,|WA|Washington|Bc|BC).*')) |> 
  # dplyr::mutate(prev_wb = str_remove_all(prev_wb, "\\([a-zA-Z]*[\\)]?")) |> 
  dplyr::mutate(prev_wb = str_remove(str_remove_all(prev_wb, wb_name),',( |)')) |>
  # Remove excess location information that is not a state.
  dplyr::mutate(prev_wb = case_when(
    stringr::str_count(prev_wb, ',') >= 2 ~ str_remove(prev_wb, '^[a-zA-Z ]*,[ ]?'),
    T ~ str_remove(prev_wb, '^\\, ')
  )) |> 
  dplyr::mutate(wb_state = str_extract(prev_wb, '^[A-Za-z]*')) |> 
  mutate(prev_wb = str_squish(str_remove(str_remove(prev_wb, wb_state),',[ ]?'))) |> 
  mutate(wb_country = str_extract(prev_wb, '^[A-Za-z]*')) |> 
  mutate(wb_name = stringr::str_to_title(wb_name)) |> 
  mutate(wb_name = case_when(
    str_detect(wb_name, '^Lake') ~ str_squish(paste0(str_remove(wb_name,"^Lake"), " Lake")),
    str_detect(wb_name, '^River') ~ str_squish(paste0(str_remove(wb_name,"^River"), " River")),
    T ~ str_squish(wb_name)
  )) |> 
  count(wb_name,wb_state, wt = n) |> 
  slice(1:50) |>
  mutate(wb_state = case_when(
    wb_name == 'Amber Lake' ~ 'WA',
    wb_name == 'Fraser River' ~ 'BC',
    wb_name == 'Ghost Lake' ~ 'AB',
    wb_name == 'Astoria River' ~ 'AB',
    wb_name == 'Police Outpost Lake' ~ 'AB',
    wb_name == 'Sea Of Cortez' ~ 'BC',
    wb_name == 'Athabaskan River' ~ 'AB',
    wb_state %in% c('WA','WAS',"Washington") ~ "WA",
    wb_state %in% c("Bc","BC") ~ "BC",
    wb_state %in% c("Alberta",'Edmonton','Calgary') ~ "AB",
    wb_state %in% c("Manitoba") ~ "MB",
    wb_state %in% c("Saskatchewan") ~ "SK",
    wb_state %in% c('OR","Or') ~ "OR",
    wb_state %in% c("Colorado","CO") ~ "CO",
    wb_state %in% c("Arizona") ~ "AZ",
    T ~ wb_state
  )) |> 
  dplyr::mutate(wb_country = case_when(
    wb_country == 'OR' ~ 'USA',
    wb_name == 'Sea Of Cortez' ~ 'MEX',
    wb_state %in% c("WA","OR","CO","AZ") ~ 'USA',
    wb_state %in% c("BC","MB","AB","ON") ~ 'CAN',
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
  arrange(desc(n)) |> 
  ungroup() |> 
  dplyr::mutate(wb_country = case_when(
    wb_country == 'USA' ~ "USA",
    wb_country == 'MEX' ~ "MEX",
    T ~ "CAN"))

sources

destinations = dat |> 
  filter(!is.na(dest_wb)) |> 
  dplyr::count(dest_wb, sort = T) |> 
  dplyr::filter(n > 1) |> 
  # Remove things in parentheses, e.g. (Caroline).
  dplyr::mutate(dest_wb = str_remove_all(dest_wb, '[ ]?\\(.*\\)[ ]?')) |> 
  dplyr::mutate(wb_name = str_remove_all(dest_wb, '(,|WA|Washington|Bc|BC).*')) |> 
  # dplyr::mutate(dest_wb = str_remove_all(dest_wb, "\\([a-zA-Z]*[\\)]?")) |> 
  dplyr::mutate(dest_wb = str_remove(str_remove_all(dest_wb, wb_name),',( |)')) |>
  # Remove excess location information that is not a state.
  dplyr::mutate(dest_wb = case_when(
    stringr::str_count(dest_wb, ',') >= 2 ~ str_remove(dest_wb, '^[a-zA-Z ]*,[ ]?'),
    T ~ str_remove(dest_wb, '^\\, ')
  )) |> 
  dplyr::mutate(wb_state = str_extract(dest_wb, '^[A-Za-z]*')) |> 
  mutate(dest_wb = str_squish(str_remove(str_remove(dest_wb, wb_state),',[ ]?'))) |> 
  mutate(wb_country = str_extract(dest_wb, '^[A-Za-z]*')) |> 
  mutate(wb_name = stringr::str_to_title(wb_name)) |> 
  mutate(wb_name = case_when(
    str_detect(wb_name, '^Lake') ~ str_squish(paste0(str_remove(wb_name,"^Lake"), " Lake")),
    str_detect(wb_name, '^River') ~ str_squish(paste0(str_remove(wb_name,"^River"), " River")),
    T ~ str_squish(wb_name)
  )) |> 
  slice(1:50) |> 
  mutate(wb_state = case_when(
    wb_name == 'Amber Lake' ~ 'WA',
    wb_name == 'Fraser River' ~ 'BC',
    wb_name == 'Ghost Lake' ~ 'AB',
    wb_name == 'Astoria River' ~ 'AB',
    wb_name == 'Police Outpost Lake' ~ 'AB',
    wb_name == 'Sea Of Cortez' ~ 'BC',
    wb_name == 'Athabaskan River' ~ 'AB',
    wb_state %in% c('WA','WAS',"Washington") ~ "WA",
    wb_state %in% c("Bc","BC") ~ "BC",
    wb_state %in% c("Alberta",'Edmonton','Calgary') ~ "AB",
    wb_state %in% c("Manitoba") ~ "MB",
    wb_state %in% c("Saskatchewan") ~ "SK",
    wb_state %in% c('OR","Or') ~ "OR",
    wb_state %in% c("Colorado","CO") ~ "CO",
    wb_state %in% c("Arizona") ~ "AZ",
    T ~ wb_state
  )) |> 
  dplyr::mutate(wb_country = case_when(
    wb_country == 'OR' ~ 'USA',
    wb_name == 'Sea Of Cortez' ~ 'MEX',
    wb_state %in% c("WA","OR","CO","AZ") ~ 'USA',
    wb_state %in% c("BC","MB","AB","ON") ~ 'CAN',
    T ~ wb_country
  )) |> 
  arrange(wb_name,desc(wb_state)) |> 
  select(-dest_wb) |> 
  add_count(wb_name) |> 
  filter(!is.na(wb_country) | nn == 1) |> 
  select(-nn) |> 
  mutate(wb_name = str_remove(wb_name, ' Usa$')) |> 
  group_by(wb_name, wb_state, wb_country) |> 
  summarise(n = sum(n)) |> 
  arrange(desc(n)) |> 
  ungroup() |> 
  dplyr::mutate(wb_country = case_when(
    wb_country == 'USA' ~ "USA",
    wb_country == 'MEX' ~ "MEX",
    T ~ "CAN"))

destinations

# openxlsx::write.xlsx(sources, paste0('04_Extra_Figures_and_Scripts/output/top_source_waterbodies.xlsx'))
# openxlsx::write.xlsx(destinations, paste0('04_Extra_Figures_and_Scripts/output/top_destination_waterbodies.xlsx'))



# Another way of finding the most numerously reported lakes:
prev_wbs_by_name = dat |> 
  filter(!is.na(prev_wb)) |> 
  dplyr::count(prev_wb, sort = T) |> 
  # Remove things in parentheses, e.g. (Caroline).
  dplyr::mutate(prev_wb = str_remove_all(prev_wb, '[ ]?\\(.*\\)[ ]?')) |> 
  dplyr::mutate(wb_name = str_remove_all(prev_wb, '(,|WA|Washington|Bc|BC).*')) |> 
  # dplyr::mutate(prev_wb = str_remove_all(prev_wb, "\\([a-zA-Z]*[\\)]?")) |> 
  dplyr::mutate(prev_wb = str_remove(str_remove_all(prev_wb, wb_name),',( |)')) |>
  # Remove excess location information that is not a state.
  dplyr::mutate(prev_wb = case_when(
    stringr::str_count(prev_wb, ',') >= 2 ~ str_remove(prev_wb, '^[a-zA-Z ]*,[ ]?'),
    T ~ str_remove(prev_wb, '^\\, ')
  )) |> 
  dplyr::mutate(wb_state = str_extract(prev_wb, '^[A-Za-z]*')) |> 
  mutate(prev_wb = str_squish(str_remove(str_remove(prev_wb, wb_state),',[ ]?'))) |> 
  mutate(wb_country = str_extract(prev_wb, '^[A-Za-z]*')) |> 
  mutate(wb_name = stringr::str_to_title(wb_name)) |> 
  mutate(wb_name = case_when(
    str_detect(wb_name, '^Lake') ~ str_squish(paste0(str_remove(wb_name,"^Lake"), " Lake")),
    str_detect(wb_name, '^River') ~ str_squish(paste0(str_remove(wb_name,"^River"), " River")),
    T ~ str_squish(wb_name)
  )) |> 
  # Count by wb name and state, weight by n from first count.
  count(wb_name,wb_state, wt = n) |> 
  arrange(desc(n)) |> 
  # Calculate summary N by waterbody name.
  group_by(wb_name) |>
  mutate(name_total = sum(n)) |> 
  filter(name_total > 1) |> 
  arrange(desc(name_total))

# To be in this list, wb's must have been referenced more than once.
unique(prev_wbs_by_name$wb_name)
# N unique wb names: 204.

# Same thing for destinations.
dest_wbs_by_name = dat |> 
  filter(!is.na(dest_wb)) |> 
  dplyr::count(dest_wb, sort = T) |> 
  # Remove things in parentheses, e.g. (Caroline).
  dplyr::mutate(dest_wb = str_remove_all(dest_wb, '[ ]?\\(.*\\)[ ]?')) |> 
  dplyr::mutate(wb_name = str_remove_all(dest_wb, '(,|WA|Washington|Bc|BC).*')) |> 
  # dplyr::mutate(dest_wb = str_remove_all(dest_wb, "\\([a-zA-Z]*[\\)]?")) |> 
  dplyr::mutate(dest_wb = str_remove(str_remove_all(dest_wb, wb_name),',( |)')) |>
  # Remove excess location information that is not a state.
  dplyr::mutate(dest_wb = case_when(
    stringr::str_count(dest_wb, ',') >= 2 ~ str_remove(dest_wb, '^[a-zA-Z ]*,[ ]?'),
    T ~ str_remove(dest_wb, '^\\, ')
  )) |> 
  dplyr::mutate(wb_state = str_extract(dest_wb, '^[A-Za-z]*')) |> 
  mutate(dest_wb = str_squish(str_remove(str_remove(dest_wb, wb_state),',[ ]?'))) |> 
  mutate(wb_country = str_extract(dest_wb, '^[A-Za-z]*')) |> 
  mutate(wb_name = stringr::str_to_title(wb_name)) |> 
  mutate(wb_name = case_when(
    str_detect(wb_name, '^Lake') ~ str_squish(paste0(str_remove(wb_name,"^Lake"), " Lake")),
    str_detect(wb_name, '^River') ~ str_squish(paste0(str_remove(wb_name,"^River"), " River")),
    T ~ str_squish(wb_name)
  )) |> 
  # Count by wb name and state, weight by n from first count.
  count(wb_name,wb_state, wt = n) |> 
  arrange(desc(n)) |> 
  # Calculate summary N by waterbody name.
  group_by(wb_name) |>
  mutate(name_total = sum(n)) |> 
  filter(name_total > 1) |> 
  arrange(desc(name_total))

# To be in this list, wb's must have been referenced more than once.
unique(dest_wbs_by_name$wb_name)
# N unique wb names: 29.

openxlsx::write.xlsx(prev_wbs_by_name, paste0('04_Extra_Figures_and_Scripts/output/top_source_waterbodies.xlsx'))
openxlsx::write.xlsx(dest_wbs_by_name, paste0('04_Extra_Figures_and_Scripts/output/top_destination_waterbodies.xlsx'))
