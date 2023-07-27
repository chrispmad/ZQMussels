#libraries
library(tidyverse)
library(readxl)
library(rvest)
library(sf)

#This script compares three lists of lakes:
# 1. Our priority list of lakes from the ZQM risk model work.
# 2. A list of lakes sent to us by Mike Sokal: https://www2.gov.bc.ca/gov/content/environment/research-monitoring-reporting/monitoring/lake-monitoring/bc-lake-monitoring-network/list-of-lake-monitoring-network-sites
# 3. A list of waterbodies approved for sampling this year by HCTF

#List 1.
zqm = read_excel("J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2023/Prioritization model/Final waterbody list based on risk estimates_2023.xlsx")

zqm = zqm |> 
  mutate(Waterbody = case_when(
    Waterbody == 'Lake Koocanusa' ~ 'Koocanusa Lake',
    T ~ Waterbody
  ))

#List 2.
mikelist = rvest::read_html(x = 'https://www2.gov.bc.ca/gov/content/environment/research-monitoring-reporting/monitoring/lake-monitoring/bc-lake-monitoring-network/lake-monitoring-network-sites')

ml_sf = mikelist |> 
  # Get tables from the web page.
  rvest::html_table() |> 
  # Put tables (vertically) together.
  bind_rows() |> 
  # Simplify column names.
  set_names(snakecase::to_snake_case) |> 
  # Select subset of columns.
  dplyr::select(lake_name_ems_site_name, region, site_information) |> 
  # Keep only the first two words in the lake_name column (drop anything in brackets)
  mutate(lake_name_ems_site_name = str_extract(lake_name_ems_site_name, '[a-zA-Z]+ [a-zA-Z]+')) |> 
  # Pull the latitude and longitude info out of the site information column.
  mutate(lat = str_extract(site_information, '[0-9]+\\.[0-9]+'),
         lon = str_extract(site_information, '[0-9]{3}\\.[0-9]{4}')) |> 
  # Convert lat and long data to numeric type.
  mutate(across(c('lat','lon'), as.numeric)) |> 
  # Ensure that longitude is negative.
  mutate(lon = -1*lon) |> 
  # Convert table to spatial table.
  st_as_sf(coords = c("lon","lat"), crs = 4326) |> 
  # Simplify lake_name column name.
  dplyr::rename(lake_name = lake_name_ems_site_name) |> 
  # Retain only lake_name, region and geometry columns.
  dplyr::select(lake_name, region)

#List 3.
hctf = read_excel("J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2023/HCTF Grants/Grant applications/Copy of 2023 waterbodies list for TRC DRAFT 2.xlsx")

regions_for_match = bcmaps::nr_regions() |> 
  dplyr::select(region = REGION_NAME) |> 
  mutate(region = stringr::str_remove_all(region, ' Natural.*')) |> 
  mutate(region = ifelse(region == 'Thompson-Okanagan', 'Thompson', region)) |> 
  st_transform(crs = 4326)

hctf_sf = hctf |> 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)

hctf_comp = hctf_sf |> 
  st_join(regions_for_match, st_intersects) |> 
  rename(waterbody = `Waterbody name`) %>% 
  st_drop_geometry() |> 
  dplyr::select(waterbody,region) |> 
  mutate(HCTF = "on list") |> 
  mutate(waterbody = case_when(
    str_detect(waterbody, "^Kootenay Lake") ~ "Kootenay Lake",
    str_detect(waterbody, "^Kettle River") ~ "Kettle River",
    waterbody == 'Lac La Hache' ~ 'Lac la Hache',
    waterbody == 'Norbury Lake' ~ 'Norbury Lakes',
    waterbody == "Pend d'Oreille" ~ "Pend-d'Oreille River",
    waterbody == 'Arrow Lake, Upper' ~ 'Upper Arrow Lake',
    waterbody == 'Arrow Lake, Lower' ~ 'Lower Arrow Lake',
    T ~ waterbody
  )) |> 
  dplyr::select(-region) |> 
  distinct()

mike_comp = ml_sf |> 
  st_drop_geometry() |> 
  mutate(Mike = "on list") |> 
  dplyr::select(-region)|>
  rename(waterbody = lake_name) |> 
  distinct()

zqm_comp = zqm %>% 
  select(Region, Waterbody, Risk_bin) %>% 
  #Split commas into separate rows.
  mutate(Region = strsplit(Region, ", ")) %>% 
  unnest(Region) %>% 
  #Replace 'Thompson-Nicola' into just 'Thompson' to match formatting of Mike's list.
  mutate(Region = replace(Region, Region == "Thompson-Nicola", "Thompson")) %>% 
  mutate(ZQM = "on list") |> 
  dplyr::rename(region = Region, waterbody = Waterbody) |> 
  dplyr::select(-region) |>
  distinct()

lakes_comparison = zqm_comp |> 
  full_join(mike_comp) |>  
  full_join(hctf_comp) |>  
  # arrange(waterbody,region) %>% 
  arrange(waterbody) |> 
  distinct()

lakes_comparison = lakes_comparison |> 
  mutate(sample_request = case_when(
    ZQM == 'on list' & Mike == 'on list' & is.na(HCTF) ~ 'request sample',
    Risk_bin == 3 ~ 'request sample',
    T ~ 'no sample required'
  ))

openxlsx::write.xlsx(lakes_comparison, "04_Extra_Figures_and_Scripts/output/2023_veliger_lake_sampling_comparison.xlsx",
                     overwrite = T)
