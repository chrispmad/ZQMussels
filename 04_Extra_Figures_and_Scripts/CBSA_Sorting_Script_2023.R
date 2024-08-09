# Libraries

library(tidyverse)
library(readxl)

d = readxl::read_excel("J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Communications/Inspection data reporting/Final report/2023/GIS Maps and Excel Figures/2023 COS inbox notifications master list.xlsx")

# Remove rows with 'ENV:EX'

d_no_env_ex = d |> 
  dplyr::filter(!str_detect(From, "ENV:EX")) |> 
  filter(!From %in% c("Microsoft Outlook","uarantine@messaging.microsoft.com"))

d_sorted = d_no_env_ex |> 
  dplyr::mutate(Categories = case_when(
    Subject == '' | is.na(Subject) | str_count(From, " ") < 1 ~ 'public',
    str_detect(From, 'ENV$') ~ 'Saskatchewan',
    str_detect(Subject, "(REF|AIS2023)") ~ 'Alberta',
    str_detect(From, "[a-zA-Z]+, [a-zA-Z]+") & !str_detect(From, 'ENV$') ~ 'CBSA',
    T ~ 'unknown'
  ))

d_sorted |> 
  ggplot() + 
  geom_histogram(aes(Categories), stat = 'count')

openxlsx::write.xlsx(d_sorted,
                     "04_Extra_Figures_and_Scripts/output/2023_CBSA_sorting_results.xlsx")

# Part 2! Martina's asked for a little count summary.

d = readxl::read_excel("J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Communications/Inspection data reporting/Final report/2023/GIS Maps and Excel Figures/2023_CBSA_sorting_results_MB.xlsx")

d |> 
  mutate(for_count = case_when(
    Categories == 'public' ~ 'public',
    str_detect(Categories,'^CBSA') ~ 'CBSA',
    Categories == 'IMDP partner' ~ 'IMDP partner',
    Categories %in% c("Alberta","Saskatchewan","Idaho","Montana") ~ 'State_or_Prov',
    Categories == 'New boat shipment' ~ 'New boat shipment',
    T ~ NA
  )) |> 
  count(for_count, sort = T)
