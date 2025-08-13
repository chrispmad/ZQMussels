#2021 PSFC Mussel Monitoring Data Call
rm(list=ls())

library(readxl) 
library(tidyverse)
library(sf)

#Load in waterbody sampling data from this 2021.
dat = read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2021/Lab Analysis/Final report/BC Veliger Sampling Inventory 2021.xlsx")

#Lat/long corrections (we found this using this script, that section is now 
#commented out)
#latlon_corr = read_excel("data/2021_waterbody_sampling_latlong_corrections.xlsx")

#Load in the template
templ = read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2021/Lab Analysis/Final report/CRB_Mussel_Monitoring_2021.xlsx",
                   sheet = "Data (2021)")

watsh = read_sf("//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/shared_data_sets/WatershedGroups.shp")
waterbodies = read_sf("//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/shared_data_sets/summarized_bc_waterbodies_same_gnis_joined.shp")

#Correct column names and drop empty first rows in template.
names(templ) = templ[4,]
templ = templ[5,]

#Add a column that unique identifies each sample.
colnames(dat)[1] <- "UniqID"

#Correct date column in our data.
dat = dat %>% 
  mutate(`Date Collected (yyyy-mm-dd)` = case_when(
    str_length(`Date Collected (yyyy-mm-dd)`) == 5 ~ as.Date(as.numeric(`Date Collected (yyyy-mm-dd)`), origin = "1899-12-30"),
    str_detect(`Date Collected (yyyy-mm-dd)`, "-") ~ lubridate::ymd(`Date Collected (yyyy-mm-dd)`),
    str_detect(`Date Collected (yyyy-mm-dd)`, "\\.") ~ as.Date(as.numeric(str_extract(`Date Collected (yyyy-mm-dd)`,"^[0-9]{5}")), origin = "1899-12-30")))

# Brief aside - correct some lat/long coordinates.
# dat = dat %>% 
#   left_join(latlon_corr) %>%
#   mutate(`Lat (Decimal degrees)` = case_when(
#           !is.na(lat_replace) ~ as.character(lat_replace),
#           T ~ `Lat (Decimal degrees)`),
#         `Long (Decimal degrees)` = case_when(
#           !is.na(lon_replace) ~ as.character(lon_replace),
#           T ~ `Long (Decimal degrees)`)
#   )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Spatially locate each sampling location from our 2021 data.   #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Step 1. 
# Make our tabular data into a spatial object ("a layer").
dat_sf = dat %>% 
  # select(UniqID,
  #        `Date Collected (yyyy-mm-dd)`,
  #        Waterbody,
  #        `Location (site name)`,
  #        `Lat (Decimal degrees)`,
  #        `Long (Decimal degrees)`) %>% 
  mutate(`Long (Decimal degrees)` = str_remove_all(`Long (Decimal degrees)`,"\\?"),
         `Lat (Decimal degrees)` = str_remove_all(`Lat (Decimal degrees)`, "\\?")) %>% 
  mutate(`Long (Decimal degrees)` = str_remove_all(`Long (Decimal degrees)`, " "),
         `Lat (Decimal degrees)` = str_remove_all(`Lat (Decimal degrees)`, " ")) %>% 
  mutate(`Long (Decimal degrees)` = as.numeric(`Long (Decimal degrees)`), 
         `Lat (Decimal degrees)` = as.numeric(`Lat (Decimal degrees)`)) %>% 
  mutate(lon = `Long (Decimal degrees)`,
         lat = `Lat (Decimal degrees)`) %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  st_transform(crs = 3005)

#Correct place names from our 2021 sampling data. These were identified by hand.
dat_sf = dat_sf %>% 
  mutate(Waterbody = case_when(
    Waterbody == "Alouette" ~ "Alouette Lake",
    Waterbody == "Wahleach" ~ "Wahleach Lake",
    Waterbody == "Kawkawa lake" ~ "Kawkawa Lake",
    Waterbody == "Little Shuswap lake" ~ "Little Shuswap Lake",
    Waterbody == "Kootenay River (Nelson)" ~ "Kootenay River",
    Waterbody == "Arrow Lake, Upper" ~ "Upper Arrow Lake",
    Waterbody == "Arrow Lake, Lower" ~ "Lower Arrow Lake",
    Waterbody == "Pend d'Oreille River" ~ "Pend-d'Oreille River",
    Waterbody == "Koocanusa" ~ "Lake Koocanusa",
    Waterbody == "Koocanusa Lake" ~ "Lake Koocanusa",
    Waterbody == "Windemere Lake" ~ "Windermere Lake",
    Waterbody == "Norbury Lake" ~ "Norbury Lakes",
    Waterbody == "St. Marys Lake" ~ "St. Mary Lake",
    Waterbody == "Saint Mary's Lake" ~ "St. Mary Lake",
    Waterbody == "Lac La Hache" ~ "Lac la Hache",
    Waterbody == "Lac Des Roches" ~ "Lac des Roches",
    Waterbody == "Lake Revelstoke" ~ "Revelstoke Lake",
    Waterbody == "Kinbasket Reservoir" ~ "Kinbasket Lake",
    Waterbody == "Whitetail lake" ~ "Whitetail Lake",
    Waterbody == "Francois Lake" ~ "Fran?ois Lake",
    T ~ Waterbody))

#Correct a simple mix-up in data entry between Anderson and Lillooet lakes.
dat_sf = dat_sf %>% 
  mutate(Waterbody = case_when(
    Waterbody == "Anderson Lake" & `Lat (Decimal degrees)` < 50.3 ~ "Lillooet Lake",
    Waterbody == "Lillooet Lake" & `Lat (Decimal degrees)` > 50.3 ~ "Anderson Lake",
    T ~ Waterbody))

# Step 2: QA/QC
# Here, we verify that all sampling locations are matching 
# to the correct waterbody.

# i) Join the subwatershed unique ID onto our sampling info.
dat_sf = st_join(dat_sf, watsh %>% select(WATERSHE_1))

#Did all samples match to a subwatershed?
dat_sf %>% filter(is.na(WATERSHE_1))
#Yes!

# iii) Join the waterbody name and unique ID from the BCG Warehouse 
#      waterbody layers (lakes + rivers + man-made waterbodies) to our sampling data.
dat_sf = st_join(dat_sf, waterbodies %>% select(GNIS_NAME_, WATERBOD_1))

#How many samples matched to a waterbody in the BCG Warehouse layers?
match_ws = dat_sf %>% 
  filter(!is.na(WATERBOD_1))
#667 of 900 received a code; they spatially matched 
#sampling data and waterbody polygons!

#How many of the dat_sf did not spatially match to any waterbody?
not_match_ws = dat_sf %>% 
  filter(is.na(WATERBOD_1))
#233 matched NO waterbody! 

#Zoom in on Sugar Lake points.
ggplot() + 
  geom_sf(data = not_match_ws %>% filter(Waterbody == "Sugar Lake"), col = "red", size = 5) + geom_sf(data = match_ws %>% filter(Waterbody == "Sugar Lake"), col = "green") + 
  geom_sf(data = waterbodies %>% filter(GNIS_NAME_ == "Sugar Lake")) + 
  coord_sf(xlim = c(1530000,1530500), ylim = c(623600,624000))

for(i in 1:nrow(not_match_ws)){
  print(i)
  
  #Look for another sampling record at the same waterbody and location.
  replacement = match_ws %>% 
    filter(Waterbody == not_match_ws[i,]$Waterbody) %>% 
    filter(`Location (site name)` == not_match_ws[i,]$`Location (site name)`) %>% 
    slice(1)
  
  #If we found something, use the first row to replace the lat/long of our #
  #non-matching record.
  if(nrow(match_ws %>% 
          filter(Waterbody == not_match_ws[i,]$Waterbody) %>% 
          filter(`Location (site name)` == not_match_ws[i,]$`Location (site name)`)) > 0){
    st_geometry(not_match_ws[i,]) = st_geometry(replacement)
  }
  #   not_match_ws[i,]$`Lat (Decimal degrees)` = replacements$`Lat (Decimal degrees)`
  #   not_match_ws[i,]$`Long (Decimal degrees)` = replacements$`Long (Decimal degrees)`
  #   
  #   st_geometry(not_match_ws[i,]) = st_geometry(st_as_sf(not_match_ws[i,],
  #                                            coords = c("Long (Decimal degrees)","Lat (Decimal degrees)"),
  #                                            crs = 4326) %>% st_transform(crs = 3005))
  #}
}

ggplot() + 
  geom_sf(data = not_match_ws %>% filter(Waterbody == "Sugar Lake"), col = "red", size = 5) + geom_sf(data = match_ws %>% filter(Waterbody == "Sugar Lake"), col = "green") + 
  geom_sf(data = waterbodies %>% filter(GNIS_NAME_ == "Sugar Lake")) + 
  coord_sf(xlim = c(1530000,1530500), ylim = c(623600,624000))

#Replace the "fixed" rows that did not originally match.
fin_sf = match_ws %>% 
  bind_rows(not_match_ws)

#Try the spatial match again.
fin_sf$GNIS_NAME_ = NULL
fin_sf$WATERBOD_1 = NULL

fin_sf = st_join(fin_sf, waterbodies %>% select(GNIS_NAME_, WATERBOD_1))

#Second attempt: how many samples matched to a waterbody 
#in the BCG Warehouse layers?
match_ws = fin_sf %>% 
  filter(!is.na(WATERBOD_1))
#776, so we've gained 109 matches.

not_match_ws = fin_sf %>% 
  filter(is.na(WATERBOD_1))
#124 still don't match spatially.

ggplot() + 
  geom_sf(data = not_match_ws, fill = "red") +
  geom_sf(data = waterbodies %>% 
            filter(GNIS_NAME_ %in% not_match_ws$Waterbody) %>% 
            st_crop(not_match_ws))

#Format our data to fit their format.
data_to_export = templ[0,] %>% 
  mutate(`Date Sampled` = as.Date(`Date Sampled`)) %>% 
  mutate(`Date Sampled` = format(`Date Sampled`, "%m-%d-%y")) %>% 
  mutate(across(-`Date Sampled`, as.character)) %>% 
  mutate(across(c(`Latitude \r\n(in decimal degrees)`,
                  `Longitude\r\n (in decimal degrees)`), as.numeric)) %>% 
  bind_rows(fin_sf %>% 
  st_drop_geometry() %>% 
  summarise(`Collecting Agency` = `sampling group/agency`,
         `Water System Name` = WATERSHE_1,
         `Water Body Name` = Waterbody,
         `Sampling Location Description` = `Location (site name)`,
         `Date Sampled` = format(`Date Collected (yyyy-mm-dd)`,"%m-%d-%y"),
         `Latitude \r\n(in decimal degrees)` = `Lat (Decimal degrees)`,
         `Longitude\r\n (in decimal degrees)` = `Long (Decimal degrees)`,
         `Type of tow` = `Type of plankton tow (vertical or horizontal)`,
         `Length of tow` = `Depth/Length of tow (m)`,
         `Calculated: Volume of tow (cu m)` = `Total Volume Sampled (L)`) %>% 
  mutate(`State, Province` = "BC",
         `Datum latitude and longitude` = "WGS84",
         `Sample Collection Method` = "Plankton Tow"))
  
openxlsx::write.xlsx(data_to_export,
                     "I:/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2021/Lab Analysis/Final report/BC Veliger Sampling Inventory_CRB format_2021.xlsx",
                     overwrite = T)

#Maybe add some code to write this out as a simple feature?
crb = read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2021/Lab Analysis/Final report/BC Veliger Sampling Inventory_CRB format_2021.xlsx")

colnames(crb)[c(1:7,9,10,11,12,
                13,17,18,19,20,21,22)] = c("Agency","WSName","WBCat","WBName",
                                           "Desc","StateProv",
                                           "Date","Lat","Lon","GNIS_ID",
                                        "SrceLocationID","SrceLocComments",
                                        "DiamNet_cm","RadNet_m","LenTow_m",
                                        "VolTow_L","VoleDNA_L","Comments")

crb = crb %>% 
  st_as_sf(coords = c("Lon","Lat"),
           crs = 4326) %>% 
  st_transform(crs = 3005)

write_sf(crb, "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/SpatialData/BC_Veliger_Sampling_CRB_format.shp")
