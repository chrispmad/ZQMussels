# Load in the lake monitoring results from all years(?)

library(readxl)
library(tidyverse)

setwd("J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring")

l23 = readxl::read_excel('2023/Lab results/BC Veliger Sampling Inventory 2023_11-20_Final Report.xlsx') |> 
  dplyr::select(date = `Date Collected (yyyy-mm-dd)`,
                waterbody = Waterbody,
                location = `Location (site name)`,
                lat = `Lat (Decimal degrees)`,
                long = `Long (Decimal degrees)`,
                others = others) |> 
  dplyr::mutate(lat = as.numeric(lat),
                long = as.numeric(long))

l23 = l23 |> 
  mutate(date = case_when(
    stringr::str_detect(date,'\\.[0-9]+') ~ as.character(lubridate::ymd(openxlsx::convertToDate(date))),
    stringr::str_detect(date,'^4') ~ as.character(lubridate::ymd(openxlsx::convertToDateTime(date))),
    T ~ as.character(lubridate::ymd(date))
  ))

l22 = readxl::read_excel('2022/Lab Analysis/Final report and data/BC Veliger Sampling Inventory 2022_FinalReport.xlsx') |> 
  dplyr::select(date = `Date Collected (yyyy-mm-dd)`,
                waterbody = Waterbody,
                location = `Location (site name)`,
                lat = `Lat (Decimal degrees)`,
                long = `Long (Decimal degrees)`,
                others = others)

l22 = l22 |> 
  mutate(date = case_when(
    stringr::str_detect(date,'\\.[0-9]+') ~ as.character(lubridate::ymd(openxlsx::convertToDate(date))),
    stringr::str_detect(date,'^4') ~ as.character(lubridate::ymd(openxlsx::convertToDateTime(date))),
    T ~ as.character(lubridate::ymd(date))
  ))


l21 = readxl::read_excel('2021/Lab Analysis/Final Report/BC Veliger Sampling Inventory 2021.xlsx') |> 
  dplyr::select(date = `Date Collected (yyyy-mm-dd)`,
                waterbody = Waterbody,
                location = `Location (site name)`,
                lat = `Lat (Decimal degrees)`,
                long = `Long (Decimal degrees)`,
                others = others) |> 
  dplyr::mutate(lat = as.numeric(lat),
                long = as.numeric(long))

l21 = l21 |> 
  mutate(date = case_when(
    stringr::str_detect(date,'\\.[0-9]+') ~ as.character(lubridate::ymd(openxlsx::convertToDate(date))),
    stringr::str_detect(date,'^4') ~ as.character(lubridate::ymd(openxlsx::convertToDateTime(date))),
    T ~ as.character(lubridate::ymd(date))
  ))

l20 = readxl::read_excel('2020/Lab analysis/954 - BC Veliger Sampling Inventory 2020_Final Report.xlsx') |> 
  dplyr::select(date = `Date Collected (yyyy-mm-dd)`,
                waterbody = Waterbody,
                location = `Location (site name)`,
                lat = `Lat (Decimal degrees)`,
                long = `Long (Decimal degrees)`,
                others = others) |> 
  dplyr::mutate(lat = as.numeric(lat),
                long = as.numeric(long))

l20 = l20 |> 
  mutate(date = case_when(
    stringr::str_detect(date,'\\.[0-9]+') ~ as.character(lubridate::ymd(openxlsx::convertToDate(date))),
    stringr::str_detect(date,'^4') ~ as.character(lubridate::ymd(openxlsx::convertToDateTime(date))),
    T ~ as.character(lubridate::ymd(date))
  ))


l19 = readxl::read_excel('2019/Lab Sample Analysis/Viliger Analysis/Weekly Veliger Analysis Results/BC Veliger Sampling Inventory 2019 FINAL.xlsx') |> 
  dplyr::select(date = `Date Collected (yyyy-mm-dd)`,
                waterbody = Waterbody,
                location = `Location (site name)`,
                lat = `Lat (Decimal degrees)`,
                long = `Long (Decimal degrees)`,
                others = others) |> 
  dplyr::mutate(date = openxlsx::convertToDateTime(date)) |> 
  dplyr::mutate(lat = as.numeric(lat),
                long = as.numeric(long),
                date = as.character(lubridate::ymd(date)))

# l18 = readxl::read_excel('2018/Lab sample analysis/2019-06-20 2018 BC Dreissenid mussel sampling_full_final.xlsx') |> 
#   dplyr::select(date = Date,
#                 waterbody = Waterbody,
#                 lat = Lat,
#                 long = long,
#                 others = others) |> 
#   dplyr::mutate(date = openxlsx::convertToDateTime(date))
l18 = readxl::read_excel('2018/Lab sample analysis/2018 Lab final results.xlsx',
                         sheet = 'Raw Data - Flat',
                         skip = 5)

l18 = l18 |> 
  dplyr::select(col1 = `250-494-7553`,
                col2 = ...3) |> 
  dplyr::filter(!is.na(col1)) |> 
  dplyr::filter(col1 != 'Total:')

l18$group_number = 1
group_count = 0

for(i in 1:nrow(l18)){
  if(stringr::str_detect(l18[i,1],'Site')){
    group_count = group_count + 1
  } 
  l18$group_number[i] = group_count
}

groups_with_corbicula = l18 |> 
  dplyr::filter(stringr::str_detect(col1, '(Corbicula)')) |> 
  dplyr::pull(group_number)

l18 |> 
  dplyr::filter(group_number %in% groups_with_corbicula) |> 
  dplyr::filter(stringr::str_detect(col1, '(Site|Corbicula)')) |> 
  group_by(group_number) |> 
  dplyr::mutate(corbicula_count = paste0(col2, collapse = '')) |> 
  dplyr::mutate(corbicula_count = as.numeric(stringr::str_remove_all(corbicula_count, 'NA'))) |> 
  dplyr::ungroup() |> 
  dplyr::filter(stringr::str_detect(col1,'Site')) |> 
  dplyr::select(-col2)

# No Corbicula detections for 2018!

l17 = readxl::read_excel('2017/Lab analysis/Final Veliger Data Report November 2017.xlsx',
                         sheet = 'Raw Data - Flat',
                         skip =  5)

l17 = l17 |> 
  dplyr::select(col1 = `250-494-7553`,
                col2 = ...3) |> 
  dplyr::filter(!is.na(col1)) |> 
  dplyr::filter(col1 != 'Total:')

l17$group_number = 1
group_count = 0

for(i in 1:nrow(l17)){
  if(stringr::str_detect(l17[i,1],'Site')){
    group_count = group_count + 1
  } 
  l17$group_number[i] = group_count
}

groups_with_corbicula = l17 |> 
  dplyr::filter(stringr::str_detect(col1, '([c,C]orbicula)')) |> 
  dplyr::pull(group_number)

# No Corbicula detected in 2017!

alldat = dplyr::bind_rows(l19,l20,l21,l22,l23)

alldat |> 
  filter(str_detect(others, '[c,C]orbicula')) |> 
  filter(stringr::str_detect(waterbody, 'Pend')) |> 
  dplyr::mutate(date = as.character(date)) |> 
  write.csv('C:/Users/CMADSEN/Downloads/LocalR/long_term_projects/ZQMussels/04_Extra_Figures_and_Scripts/output/Corbicula_Detections_in_Pend_dOreille.csv')
