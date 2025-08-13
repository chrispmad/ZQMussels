## BC lake monitoring comparisons: 2020 and 2021 lists of lakes.

library(sf)
library(tidyverse)
library(readxl)
library(bcdata)

rm(list = ls())

bc = read_sf("//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/SpatialData/bc_simple.shp")

flnro = read_sf("//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/SpatialData/FLNRO_Fishing_Boundaries.shp")

cities = read_csv("C:/Users/CMADSEN/Downloads/LocalRWork/data/CityTableForMussels.csv") %>% 
  filter(Province == "British Columbia") %>% 
  st_as_sf(coords = c("City Longitude","City Latitude"), crs = 4326)

d2020 = read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2020/Lab analysis/954 - BC Veliger Sampling Inventory 2020_Final Report.xlsx")

d2020_sf = d2020 %>% 
  rename(long = `Long (Decimal degrees)`,
         lat = `Lat (Decimal degrees)`) %>% 
  st_as_sf(coords = c("long","lat"),
           crs = 4326)

ggplot() + 
  geom_sf(data = flnro) + 
  geom_sf(data = d2020_sf)

#Add FLNRORD region 
d2020_sf = d2020_sf %>% 
  st_join(flnro %>% 
            st_transform(crs = 4326) %>% 
            select(REGION_N))

#Just names... from sheet 2 of the excel sheet for 2020.
# d2020_2 = read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2020/Lab analysis/954 - BC Veliger Sampling Inventory 2020_Final Report.xlsx",
#                    sheet = "Sheet2")

# Read in 2021 data.
d2021 = read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2021/Lab Analysis/900 - BC Veliger Sampling Inventory 2021.xlsx")

#Some name corrections for plankton tow sampling.
d2021 = d2021 %>% 
  mutate(Waterbody = case_when(
    Waterbody == "Little Shuswap lake" ~ "Little Shuswap Lake",
    Waterbody == "Lake Revelstoke" ~ "Revelstoke Lake",
    Waterbody == "Saint Mary's Lake" ~ "St. Mary Lake",
    T ~ Waterbody))

# 2021 Substrate sampling.
d2021_sub = read_excel("data/21-22_CompB. Appendix 1 2021 List of Sampling Sites.xlsx")

#Martina wants me to double-check the numbers using this version of the appendix.
d2021_recheck = read_excel("data/Appendix 1 for MoE report_2021-Final Draft.xlsx")

#I did some manual checking of waterbody name matching. The follow don't match initially:
  # Kinbasket Lake, Lake Koocanusa, Lake Windermere, Lilloett Lake,
  # Pend d'oreille River, Revelstoke Lake, and St. Mary's Lake.

#Therefore, I correct these names to whatever we have used in the d2021 dataset.
d2021_sub = d2021_sub %>% 
  mutate(Waterbody = case_when(
    Waterbody == "Kinbasket Lake" ~ "Kinbasket Reservoir",
    Waterbody == "Lake Koocanusa" ~ "Koocanusa Lake",
    Waterbody == "Lake Windermere" ~ "Windermere Lake",
    Waterbody == "Lilloett Lake" ~ "Lillooet Lake",
    Waterbody == "Pend d'oreille River" ~ "Pend d'Oreille River",
    Waterbody == "St. Mary's Lake" ~ "St. Mary Lake",
    T ~ Waterbody
  ))
  
#Make substrate sampling true / false
d2021_sub = d2021_sub %>% 
  mutate(Substrate = `# Substrate samplers` > 0)

#Make a sampling method column; populate with plankton tow for all; left join substrate samples.
d2021 = d2021 %>% 
  mutate(`Sampling Method(s)` = "Plankton tow") %>% 
  left_join(d2021_sub %>% select(Waterbody, Substrate)) %>% 
  mutate(`Sampling Method(s)` = replace(`Sampling Method(s)`, Substrate == TRUE, "Plankton tow / Substrate sampling"))

#Are we lacking any of the substrate sampler waterbodies?
d2021_sub[which(d2021_sub$Waterbody %in% d2021$Waterbody),]$Waterbody

#Make into a spatial object.
d2021_sf = d2021 %>% 
  rename(long = `Long (Decimal degrees)`,
         lat = `Lat (Decimal degrees)`) %>% 
  st_as_sf(coords = c("long","lat"),
           crs = 4326)

ggplot() + 
  geom_sf(data = flnro) + 
  geom_sf(data = d2021_sf, aes(col = `Sampling Method(s)`))

d2021_sf = d2021_sf %>% 
  st_join(flnro %>% 
            st_transform(crs = 4326) %>% 
            select(REGION_N))

#Narrow down fields.
d2020_sf = d2020_sf %>% 
  select(Waterbody, `sampling group/agency`, REGION_N) %>% 
  rename(Samplers = `sampling group/agency`) %>% 
  distinct() %>% 
  arrange(Waterbody)

d2021_sf = d2021_sf %>% 
  select(Waterbody, `sampling group/agency`, REGION_N, `Sampling Method(s)`) %>% 
  rename(Samplers = `sampling group/agency`) %>% 
  distinct() %>% 
  arrange(Waterbody)

d2020_sf
d2021_sf

n2020 = d2020_sf %>% 
  st_drop_geometry() %>% 
  distinct()

n2021 = d2021_sf %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  select(Waterbody, REGION_N, Samplers, `Sampling Method(s)`)

#Make table of 2021 data.
n2021 %>% 
  openxlsx::write.xlsx("C:/Users/CMADSEN/Downloads/LocalRWork/output/2021_lake_monitoring_locations.xlsx",
                       overwrite = T)

#Make table of 2021 data, but just with lat and long as one field.
d2021_sf %>% 
  mutate(latlon = paste0(round(matrix(unlist(test), ncol = 2, byrow = T)[,2],3),
                         ", ",
                         round(matrix(unlist(test), ncol = 2, byrow = T)[,1],3))) %>% 
  st_join(cities %>% select(`City Name`), st_nearest_feature) %>% 
  st_drop_geometry() %>% 
  group_by(Waterbody) %>% 
  slice(1) %>% 
  select(Waterbody, latlon, `City Name`) %>% 
  openxlsx::write.xlsx("C:/Users/CMADSEN/Downloads/LocalRWork/output/2021_lake_monitoring_locations_with_coords.xlsx",
                       overwrite = T)

#==========================================
#  Superfluous comparisons of 2020 and 2021...
#==========================================
# 
# #Which lakes were sampled in 2020 but not in 2021?
# dat_combo = n2020 %>% 
#   select(Waterbody, REGION_N) %>% 
#   distinct() %>% 
#   mutate(Sample_2020 = T) %>% 
#   full_join(n2021 %>% 
#               select(Waterbody, REGION_N) %>% 
#               mutate(Sample_2021 = T) %>% 
#               distinct())
# 
# dat_combo %>% 
#   filter(is.na(Sample_2021))
# 
# #Which lakes were sampled in 2021 but not in 2020?
# dat_combo %>% 
#   filter(is.na(Sample_2020))
# 
# 
# names_2020 = d2020 %>% 
#   select(Waterbody, `sampling group/agency`) %>% 
#   rename(Samplers = `sampling group/agency`) %>% 
#   distinct() %>% 
#   arrange(Waterbody)
# 
# names_2021 = d2021 %>% 
#   select(Waterbody, `sampling group/agency`) %>% 
#   rename(Samplers = `sampling group/agency`) %>% 
#   distinct() %>% 
#   arrange(Waterbody)
# 
# bcdata_names = bcdata::bcdc_list()
# bcdata_names[str_detect(bcdata_names, "fresh")]
# #Permanent name of freshwater atlas lakes: 'cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6'
# lakes = bcdc_query_geodata('cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6', crs = 4326) %>% 
#   filter(GNIS_NAME_1 %in% all_of(c(names_2021$Waterbody, names_2020$Waterbody))) %>% 
#   select(GNIS_NAME_1, WATERBODY_KEY, WATERSHED_GROUP_ID) %>% 
#   collect()
# 
# names_2020 %>% 
#   mutate(Sampled = 2020) %>% 
#   pivot_wider(names_from = Samplers, values_from = Sampled)
#   rename(Samplers_2020 = Samplers) %>% 
#   left_join(names_2021 %>% 
#               rename(Samplers_2021 = Samplers))
# #Add a field to lakes if they have the same waterbody name as a lake that
# #  was sampled in 2020.
# lakes = lakes %>% 
#   left_join(d2020 %>% 
#               mutate(sampled = "2020") %>% 
#               rename(GNIS_NAME_1 = Lake_Name))
# 
# #134 lakes out of total of 154 match with sampling names from 2020, even though
# # there should only be 89... Maybe some lakes are split into multiple shapefiles,
# # or they are lakes in different parts of the province with the same name, and 
# # probably only one (the largest?) has actually been sampled. Regardless, should be fine.
# 
# ggplot() + 
#   geom_sf(data = bc) +
#   geom_sf(data = lakes, aes(fill = sampled))
#   
# #Add another field based on name matching with sampling names from 2021.
# justnames = lakes %>% 
#   left_join(names_2021 %>% 
#               mutate(sampled_2 = "2021")) %>% 
#   select(WATERSHED_GROUP_ID, GNIS_NAME_1, AREA_HA, sampled, sampled_2) %>% st_drop_geometry()
# 
# justnames %>% View()
# 
# #Names NOT sampled in 2021 but sampled in 2020: Buntzen Lake, Burns Lake, 
# dropped_from_2020_to_2021 = justnames %>% 
#   filter(is.na(sampled_2)) %>% 
#   select(GNIS_NAME_1) %>% 
#   distinct()
#   
# added_from_2020_to_2021 =  justnames %>% 
#   filter(is.na(sampled)) %>% 
#   select(GNIS_NAME_1) %>% 
#   distinct()
# 
# #Which lakes are in 2020 and also in 2021?
# d2020[d2020$Lake_Name %in% all_of(names_2021),]
# #56 lakes are shared immediately.
# 
# #Which lakes are in 2020 and are NOT in 2021?
# d2020[!d2020$Lake_Name %in% all_of(names_2021),]
# #33 lakes! Might be typos...
# 
# 
# ## Prepping: 
# ## 1. Which HCTF waterbodies are sampled in at least one location?
# hctf_yes = hctf %>% filter(`TRC Recommended Or FC approval` == "Yes")
# 
# ## 2. Which HCTF waterbodies are not sampled in at least one location?
# hctf_not_yes = hctf %>% 
#   filter(`TRC Recommended Or FC approval` != "Yes") %>% 
#   #And filter out any of those 'no' waterbodies that have another station 'yes'
#   filter(!`Waterbody name` %in% hctf_yes$`Waterbody name`)
# 
# hctf_not_yes
# #Results in Shuswap River, Elk Lake and Jim Smith Lake.
# 
# ## Questions:
# # 1. Which water bodies are present in the provincial list but are NOT being sampled
# #    by the HCTF sampling?
# head(prov$Waterbody)
# head(hctf_yes$`Waterbody name`)
# #Orthography of the naming variables is similar.
# 
# prov_not_hctf = prov %>% 
#   filter(!Waterbody %in% hctf_yes$`Waterbody name`) %>% 
#   distinct(Waterbody)
# #68 waterbodies
# 
# # 2. Which of those 68 waterbodies are shown in the 'mike' list?
# mike_list = as.character(mike$Gazetted_N)
# 
# opportunistic_lakes_to_sample = prov_not_hctf %>% 
#   filter(Waterbody %in% mike_list)
# #11 waterbodies: Bowron, Charlie, Cluculz, Elk, Fraser, Moberly, Nadsilnich,
# #                Nicola, Stuart, Swan and Tabor lakes.