## BC lake monitoring comparisons: Mike Sokall list, 
## provincial priority water bodies,  and HCTF sampling list.

library(sf)
library(tidyverse)

rm(list = ls())

## Mike Sokall list:
mike = st_read("W:/CSilverio/File Dump for Cass/data/BCLake_Monitoring_Network_Sites.shp")

## Provincial priority water body list:
prov = openxlsx::read.xlsx("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2021/Updates to protocol_waterbody ranking/2021 priority waterbodies for sampling_Jan 27 2021 review .xlsx",
                           sheet = "2021 FINAL LIST")

## HCTF sampling list:
hctf = read_csv("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2021/Updates to protocol_waterbody ranking/2021_Invasive_Mussel_Mapping_location list_bvs2 (final from HCTF).csv")

## Prepping: 
## 1. Which HCTF waterbodies are sampled in at least one location?
hctf_yes = hctf %>% filter(`TRC Recommended Or FC approval` == "Yes")

## 2. Which HCTF waterbodies are not sampled in at least one location?
hctf_not_yes = hctf %>% 
  filter(`TRC Recommended Or FC approval` != "Yes") %>% 
#And filter out any of those 'no' waterbodies that have another station 'yes'
  filter(!`Waterbody name` %in% hctf_yes$`Waterbody name`)

hctf_not_yes
#Results in Shuswap River, Elk Lake and Jim Smith Lake.

## Questions:
# 1. Which water bodies are present in the provincial list but are NOT being sampled
#    by the HCTF sampling?
head(prov$Waterbody)
head(hctf_yes$`Waterbody name`)
#Orthography of the naming variables is similar.

prov_not_hctf = prov %>% 
  filter(!Waterbody %in% hctf_yes$`Waterbody name`) %>% 
  distinct(Waterbody)
#68 waterbodies

# 2. Which of those 68 waterbodies are shown in the 'mike' list?
mike_list = as.character(mike$Gazetted_N)

opportunistic_lakes_to_sample = prov_not_hctf %>% 
  filter(Waterbody %in% mike_list)
#11 waterbodies: Bowron, Charlie, Cluculz, Elk, Fraser, Moberly, Nadsilnich,
#                Nicola, Stuart, Swan and Tabor lakes.