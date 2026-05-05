library(tidyverse)
library(sf)
library(bcdata)
library(bcmaps)
library(readxl)
library(leaflet)
library(leafpop)
library(leaflet.extras)
library(janitor)
library(here)
library(openxlsx)
library(htmlwidgets)



sample_locs<- read_excel("Z:/2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Year 2 (2025-2026)/eDNA ZQM Pylet Lab/All ZQM work related to RMRM/all eDNA_RESULTS_2025_ZQM-GM-WD.xlsx")


dfo_sar = sf::read_sf(paste0(onedrive_wd,"CNF/dfo_sara_and_crit_hab_bulltrout_and_sockeye_data.gpkg")) |> sf::st_transform(4326)


species_list <- c("Cultus Pygmy Sculpin", "Westslope Cutthroat Trout",
                  "Bull Trout", "Rocky Mountain Ridged Mussel",
                  "Sockeye Salmon")



cnf_sar = dfo_sar |> 
  filter(Common_Name_EN %in% species_list)

named_wbs <- read_rds(
  here::here("../AIS_SAR_prioritization_model/data/named_lakes_and_rivers.rds")
)


# named_wbs_overlap <- named_wbs %>%
#   filter(lengths(st_intersects(geometry, cnf_sar)) > 0) |> 
#   st_transform(3005) |> 
#   st_buffer(10000) |> 
#   st_transform(4326)

named_wbs_overlap <- named_wbs %>%
  filter(lengths(st_intersects(geometry, cnf_sar)) > 0) |>
    st_transform(3005) |>
    st_buffer(7000) |> 
  st_transform(4326) |> 
  mutate(
    species = sapply(st_intersects(geometry, cnf_sar), function(idx) {
      paste(cnf_sar$Common_Name_EN[idx], collapse = ", ")
    })
  )


sample_locs_sf = st_as_sf(sample_locs, coords = c("Long", "Lat"), crs = 4326, remove = F)

sample_locs_sf <- sample_locs_sf |>
  mutate(
    SAR_species = sapply(st_intersects(geometry, named_wbs_overlap), function(idx) {
      if (length(idx) == 0) return(NA)
      paste(unique(unlist(strsplit(named_wbs_overlap$species[idx], ", "))), collapse = ", ")
    })
  )


write.csv(sample_locs_sf, "./output/sample_locs_sara_combined.csv")
