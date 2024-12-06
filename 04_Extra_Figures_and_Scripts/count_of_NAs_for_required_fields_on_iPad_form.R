library(tidyverse)
library(plotly)
library(ggrepel)

mdat = read_csv('J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years/metabase_2024.csv')
mfdat = readxl::read_excel("02_IMDP_Figure_Generator/data/figure_dat_mf.xlsx")
alldat = read_csv("02_IMDP_Figure_Generator/data/figure_dat_all.csv")

# cdat = readxl::read_excel("02_IMDP_Figure_Generator/data/figure_dat.xlsx")

mdat = mdat |> 
  dplyr::filter(!str_detect(`Shift Start Comment`,'(Test|test|TEST)'))

mdat = mdat |> 
  dplyr::mutate(PreviousJourneyInfo = coalesce(`Previous Waterbody 1 Name`,
                                               `Previous Waterbody 1 Closest City`,
                                               `Previous Waterbody 1 Other Details`,
                                               `Previous Major City`),
                DestinationJourneyInfo = coalesce(`Destination Waterbody 1 Name`,
                                                  `Destination Waterbody 1 Closest City`,
                                                  `Destination Waterbody 1 Other Details`,
                                                  `Destination Major City`))

req_f = c("Start Time","Inspection Time","Number Of People In Party",
          "Commercially Hauled Ind","Previous AIS Knowledge Ind",
          "Previous Inspection Ind","PreviousJourneyInfo",
          "DestinationJourneyInfo","K9 Inspection Ind","Clean Drain Dry After Inspection Ind",
          "Watercraft Has Drainplugs Ind")

highrisk_reqf = c("Decontamination Performed Ind","Decontamination order issued Ind",
                  "Seal Issued Ind","Quarantine Period Issued Ind","High Risk AIS Ind")

#Note: watercraft complexity is not required.

# Inspection Form Required Fields
mdat |> 
  dplyr::select(any_of(req_f)) |> 
  dplyr::mutate(`Number Of People In Party` = ifelse(`Number Of People In Party` == 0, NA, `Number Of People In Party`)) |> 
  dplyr::mutate(DestinationJourneyInfo = ifelse(DestinationJourneyInfo == 'None', NA, DestinationJourneyInfo),
                PreviousJourneyInfo = ifelse(PreviousJourneyInfo == 'None', NA, PreviousJourneyInfo)) |> 
  dplyr::mutate(across(everything(), \(x) !is.na(x))) |> 
  pivot_longer(cols = everything()) |> 
  dplyr::count(name,value) |> 
  dplyr::mutate(value = ifelse(value, "Has Data", "Data Missing")) |> 
  pivot_wider(names_from = value, values_from = n)

# High Risk Form Required Fields
mdat |> 
  # dplyr::rename(high_risk = "High Risk AIS Ind") |> 
  # dplyr::filter(`High Risk AIS Ind`) |> 
  dplyr::select(all_of(highrisk_reqf)) |> 
  dplyr::mutate(across(all_of(highrisk_reqf[-5]), \(x) !is.na(x))) |> 
  pivot_longer(cols = -`High Risk AIS Ind`) |> 
  dplyr::count(`High Risk AIS Ind`,name,value) |> 
  dplyr::mutate(value = ifelse(value, "Has Data", "Data Missing")) |> 
  pivot_wider(names_from = value, values_from = n)

# 9 inspections are not flagged as high risk, yet have decons, seal, quarantine etc.