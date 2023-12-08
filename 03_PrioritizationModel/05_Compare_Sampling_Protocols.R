# Load libraries

library(tidyverse)
library(readxl)

setwd("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/")

# Load in year 1
year_1 = read_excel('2022/Prioritization model/Final waterbody list with frequency added.xlsx')

# Load in year 2
year_2 = read_excel("2022/Prioritization model/Final waterbody list based on risk estimates_2022.xlsx")

# Compare!
comp_table = setdiff(year_1 %>% dplyr::select(Region, Waterbody, Risk_bin),
        year_2 %>% dplyr::select(Region, Waterbody, Risk_bin))

year_1 %>% mutate(year = '2021') %>% 
  bind_rows(year_2 %>% mutate(year = '2022')) %>% 
  filter(Region %in% comp_table$Region,
         Waterbody %in% comp_table$Waterbody)

# Old waterbodyes in year 1
waterbodies_dropped_from_year1 = year_1 %>% 
  mutate(my_key = paste0(Region,Waterbody)) %>% 
  filter(!my_key %in% paste0(year_2$Region,year_2$Waterbody))

# New waterbodies in year 2
waterbodies_added_from_year2 = year_2 %>% 
  mutate(my_key = paste0(Region,Waterbody)) %>% 
  filter(!my_key %in% paste0(year_1$Region,year_1$Waterbody))
