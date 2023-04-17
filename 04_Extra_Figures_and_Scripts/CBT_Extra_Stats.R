library(tidyverse)

library(readxl)

rm(list = ls())

d = read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/Multiyear data/WatercraftInspectionData_AllYears_Selected_Columns.xlsx")

d %>% 
  filter(Year == 2021) %>% 
  filter(!is.na(Destination_Waterbody_1_Name)) %>% 
  count(Destination_Waterbody_1_Name, sort = T) %>% 
  mutate(total_n = sum(n)) %>% 
  mutate(prop = 100*n/total_n) %>% 
  slice(1:10) %>% 
  select(-total_n)

d %>% 
  filter(Year == 2021) %>% 
  filter(High_Risk_AIS_Ind == T) %>% 
  count(Station, sort = T) %>% 
  slice(1:10)

#Number of high risk boats by source prov/state.
d %>% 
  filter(Year == 2021) %>% 
  filter(Previous_Major_City != "None" | !is.na(Previous_Waterbody_1_Province_Or_State)) %>% 
  filter(High_Risk_AIS_Ind == T) %>% 
  select(state.name = Previous_Major_City,
         pwat = Previous_Waterbody_1_Province_Or_State) %>% 
  mutate(state.name = str_extract(state.name, "(?<=, )[a-zA-Z]*(?=,)")) %>% 
  left_join(data.frame(state.abb, state.name)) %>% 
  mutate(source_state_prov = coalesce(pwat,state.abb)) %>%
  filter(!is.na(source_state_prov)) %>% 
  #mutate(source_state_prov = fct_lump_prop(source_state_prov, prop = 0.05)) %>% 
  count(source_state_prov, sort = T) %>% 
  mutate(total_n = sum(n)) %>% 
  mutate(prop = 100*n/total_n) %>% 
  filter(prop < 5) %>% 
  arrange(source_state_prov)
  

  count(Previous_Major_City, sort = T) %>% 
  mutate(total_n = sum(n))
