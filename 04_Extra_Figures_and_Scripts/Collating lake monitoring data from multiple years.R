### 2020 Lake monitoring plankton tow sampling locations

### This script compiles data gathered from 2015-2020 at "Veliger sampling sites" throughout BC, performs a few checks of the data,
### and exports the data as shapefiles for 

library(raster)
library(sf)
library(ggthemes)
library(ggspatial)
library(ggpattern)
library(rgdal)
library(tidyverse)
library(readxl)
library(lubridate)

rm(list=ls())
gc()

#==========================================================================

#Where is your R project located? This becomes the 'root' folder
mywd = "C:/Users/CMADSEN/Downloads/LocalRWork/"

#Set the working directory (which folder R is 'looking' at) to your 'mywd'.
setwd(mywd)

#This assumes you have a folder called 'data' in your root folder, in which you place your data files.
data.path = paste0(mywd,"data/")

#IF you don't yet have a folder called 'output' in your root folder, this command makes such a folder.
if(!dir.exists(paste0(mywd,"output/"))) {
  dir.create(paste0(mywd,"output/"))
}

# Download BC shapefile (first have to download shapefile for all of Canada, then we subset it for just BC), 
# Worldclim data (temperature), and calcium data from EMS database (downloaded manually using Microsoft Edge and URLs 
# shared with me by Cass.

#2. Download the shapefile for Canada, then just select the BC portion.
#bc_shp = getData(name = "GADM", country = "CAN", level = 1)
#bc_shp = bc_shp[bc_shp$NAME_1=="British Columbia",]
bc_shp = read_sf("//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/SpatialData/bc_shapefile.shp")

#==========================================================================

#Bring in the data files. I've had to select these 'by hand', since many of the files are named in a distinct way, have
# "FINAL" as part of their name, or are in a different folder arrangement.
setwd("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/")

dat2013 = read_excel("2013/BC Veliger Sampling Inventory 2013.xlsx", 
                     col_types = c("text", "text", "text", 
                                   "numeric", "numeric", "text", "text", 
                                   "text", "text", "text", "text", "text"), 
                     skip = 4)


dat2014 = read_excel("2014/Sample inventory/BC Veliger Sampling Inventory 2014_FINAL.xlsx", 
                     col_types = c("text", "text", "text", 
                                   "text", "text", "text", "numeric", 
                                   "numeric", "date", "text", "text", 
                                   "text", "text", "text", "text", "text"), 
                     skip = 4)

dat2015 = read_excel("2015/Sample inventory/2015 BC Veliger Sampling Inventory 2016_07_07_corrected.xlsx",
                     sheet = "Report Table FINAL")

dat2016 = read_excel("2016/Veliger sample sites/2016 Lake monitoring Locations Final.xlsx", 
                     col_types = c("text", "text", "numeric", 
                                   "numeric", "date", "text", "text"))

dat2017 = read_excel("2017/Lab analysis/Final Veliger Data Report November 2017_cleaned.xlsx")

dat2018 = read_excel("2018/Lab sample analysis/2019-06-20 2018 BC Dreissenid mussel sampling_full_final.xlsx", 
                     col_types = c("text", "text", "text", 
                                   "text", "text", "numeric", "numeric", 
                                   "text", "text", "text", "numeric", 
                                   "numeric", "numeric", "text"))
  
dat2019 = read_excel("2019/Lab Sample Analysis/Viliger Analysis/Weekly Veliger Analysis Results/BC Veliger Sampling Inventory 2019 FINAL.xlsx",
                     col_types = c("numeric","text","text","text","numeric",
                                   "numeric","numeric","numeric","numeric","numeric","numeric","numeric","text",
                                   "numeric","text","numeric","numeric","text","text","text","text","text","text"))

dat2020 = read_excel("2020/Lab analysis/954 - BC Veliger Sampling Inventory 2020_Final Report.xlsx", 
                                                                             col_types = c("skip", "date", "text", 
                                                                                           "text", "numeric", "text", "numeric", 
                                                                                           "numeric", "numeric", "numeric", 
                                                                                           "numeric", "numeric", "numeric", 
                                                                                           "text", "numeric", "text", "numeric", 
                                                                                           "numeric", "text", "text", "text", 
                                                                                           "text", "text"))

dat2021 = read_excel("2021/Lab Analysis/Final report/BC Veliger Sampling Inventory - report 2021.xlsx",
                     col_types = "text")

#==========================================================================
#2020

#column names.
dat2020 = dat2020 %>% 
  rename(CollectDate = `Date Collected (yyyy-mm-dd)`,
         Location = `Location (site name)`,
         NumContainers = `# Containers`,
         VolContainers = `container volume`,
         Lat = `Lat (Decimal degrees)`,
         Lon = `Long (Decimal degrees)`,
         WaterTemp = `Water temperature (?C)*`,
         WaterpH = `pH (in water column)`,
         SecchiDepth = `Secchi Depth (m)`,
         NumPlanktonTows = `# of Plankton tows`,
         TowDepthOrLength = `Depth/Length of tow (m)`,
         PlanktonTowType = `Type of plankton tow (vertical or horizontal)`,
         TotalVolume_L = `Total Volume Sampled (L)`,
         Buffered = `Buffered (Y/N)`,
         SamplepH_pres = `Sample pH (at the time preserved)`,
         SamplepH_lab = `Sample pH (at arrival in the lab)`,
         PresTypeConc = `Preservative Type and Concentration`,
         Notes = `Description of Location/Notes`,
         ZQM_veligers = `Zebra/Quagga mussels veligers`,
         SamplingGroup = `sampling group/agency`)

#Remove rows that have 'NA' in the coordinates.
dat2020 = dat2020 %>%
  filter(is.na(Lat) == F,
         is.na(Lon) == F)
#About 20 rows were lost!

#Some rows (~ 5) have latitude and longitude swapped. Let's swap these to be correct.
dat2020[dat2020$Lat < 0,c(6,7)] <- dat2020[dat2020$Lat < 0,c(7,6)]

#And some longitude values need a '-' sign.
dat2020[dat2020$Lon > 0,c(7)] <- -1*dat2020[dat2020$Lon > 0,c(7)]

#And one longitude value is much too negative...
dat2020[dat2020$Lon < -154,]$Lon = -115.8386

dat2020 = dat2020 %>%
  mutate(VolContainers = as.numeric(str_extract(VolContainers, "[0-9]+"))) %>%
  rename(VolContainers_mL = VolContainers) %>%
  filter(WaterTemp > 2,
         NumPlanktonTows < 20) %>%
  mutate(PlanktonTowType = replace(PlanktonTowType, PlanktonTowType == "horizontal", "Horizontal"),
         PlanktonTowType = replace(PlanktonTowType, PlanktonTowType == "Horizontal, Vertical", "Vertical, Horizontal"),
         PlanktonTowType = replace(PlanktonTowType, PlanktonTowType == "horitzontal", "Horizontal"),
         PlanktonTowType = replace(PlanktonTowType, PlanktonTowType == "shoreline toss", "Shoreline toss"),
         PlanktonTowType = replace(PlanktonTowType, PlanktonTowType == "vertical", "Vertical"),
         PlanktonTowType = replace(PlanktonTowType, PlanktonTowType == "Vertical (1) and horizontal (2)", "Vertical, Horizontal"),
         PlanktonTowType = replace(PlanktonTowType, PlanktonTowType == "Vertica", "Vertical")) %>%
  mutate(Buffered = replace(Buffered, str_detect(Buffered, "[y,Y]"), "Y"),
         Buffered = replace(Buffered, str_detect(Buffered, "[n,N]"), "N")) %>%
  filter(SamplepH_lab < 20) %>%
  mutate(PresTypeConc = replace(PresTypeConc, str_detect(PresTypeConc, "[i,I][s,S][o,O]"), "99% Isopropyl"),
         PresTypeConc = replace(PresTypeConc, str_detect(PresTypeConc, "[e,E]th"), "95% Ethanol"))

dat2020$Year = 2020
dat2020$SampleMethod = "PlanktonTow"
dat2020 = dat2020[,c(23,1:7,24,8:22)]

#And let's cast our dat2020 data as a spatial file.
dat2020_sf = st_as_sf(SpatialPointsDataFrame(coords = dat2020[,c(8,7)],
                                    data = dat2020[,-c(7,8)],
                                    proj4string = crs(bc_shp)))

#==========================================================================

# Some data cleaning is needed.

#==========================================================================
#             2013
#correct date field.
dat2013 = dat2013 %>%
  mutate(Waterbody = str_remove(`Sampling Location Name, State, Province`, ", BC")) %>%
  select(-`Sampling Location Name, State, Province`, -`UTM North`, -`UTM East`) %>%
  mutate(CollectDate = `Date sampled`,
         CollectDate = replace(CollectDate, CollectDate == "2013", "01.01.2013"),
         CollectDate = dmy(CollectDate)) %>%
  select(-`Date sampled`, -`Have samples from this date yet to be analyzed (Y/N)`) %>%
  rename(Lat = `Lat (Decimal degrees)`,
         Lon = `Long (decimal degrees)`,
         SamplingGroup = `Name/location of analytical lab`,
         ZQM_veligers = veliger) %>%
  mutate(ZQM_veligers = replace(ZQM_veligers, ZQM_veligers == "-", "no"),
         others = replace(others, others == "-", NA)) %>%
  mutate(Year = 2013)

dat2013 = dat2013[,c(10,1:9)]

dat2013_sf = st_as_sf(SpatialPointsDataFrame(coords = dat2013[,c(3,2)],
                       data = dat2013[,-c(2,3)],
                       proj4string = crs(bc_shp)))

#==========================================================================
#             2014
dat2014 = dat2014 %>% 
  rename(ZQM_veligers = veliger) %>%
  mutate(Year = 2014)

dat2014 <- dat2014[,c(17,1:16)]

dat2014_Zone11 = dat2014 %>% 
  filter(is.na(`UTM Easting`)==F) %>%
  filter(`UTM Zone` == "11")
  
dat2014_Zone10 = dat2014 %>% 
  filter(is.na(`UTM Easting`)==F) %>%
  filter(`UTM Zone` == "10")

dat2014_Zone11_sf = st_transform(st_as_sf(SpatialPointsDataFrame(coords = dat2014_Zone11[,c(8,9)],
                         data = dat2014_Zone11[,-c(8,9)],
                         proj4string = CRS("+proj=utm +zone=11 +datum=WGS84"))),
                         crs(bc_shp))

dat2014_Zone10_sf = st_transform(st_as_sf(SpatialPointsDataFrame(coords = dat2014_Zone10[,c(8,9)],
                                                    data = dat2014_Zone10[,-c(8,9)],
                                                    proj4string = CRS("+proj=utm +zone=10 +datum=WGS84"))),
                        crs(bc_shp))

dat2014_sf = rbind(dat2014_Zone10_sf, dat2014_Zone11_sf); rm(dat2014_Zone10); rm(dat2014_Zone10_sf); rm(dat2014_Zone11); rm(dat2014_Zone11_sf);

#Figure out which fields can be used for sampling group.
dat2014_sf = dat2014_sf %>% 
  rename(SamplingGroup = `Name/location of analytical lab`)

#Looks like at least one data point lies below the border, which is really unlikely. We'll have to check out the projection...

#==========================================================================
#             2015
dat2015 = dat2015 %>%
  rename(Waterbody = Lake,
         SamplingGroup = `Sampling Agency`,
         Lat = `Lat (Decimal degrees)`,
         Lon = `Long (decimal degrees)`,
         CollectDate = `Date sampled (mm/dd/yyyy)`,
         ZQM_veligers = `ZQM Veliger Detected? (YES/NO)`,
         others = `Native Species Detected`) %>%
  mutate(Lon = ifelse(Lon > 0, -1*Lon, Lon)) %>% 
  mutate(CollectDate = mdy(CollectDate),
         ZQM_veligers = str_to_lower(ZQM_veligers),
         others = replace(others, others == "-", NA)) %>%
  filter(is.na(Lat)==F) %>%
  mutate(Year = 2015)

dat2015 = dat2015[,c(8,1:7)]

dat2015_sf = st_as_sf(SpatialPointsDataFrame(coords = dat2015[,c(5,4)],
                                                          data = dat2015[,-c(5,4)],
                                                          proj4string = crs(bc_shp)))

#==========================================================================
#             2016
dat2016 = dat2016 %>%
  rename(Waterbody = Lake,
  SamplingGroup = `Sampling Agency`,
  Lat = `Lat (Decimal degrees)`,
  Lon = `Long (Decimal degrees)`,
  CollectDate = `Date sampled (YYYY-MM-DD)`,
  ZQM_veligers = `ZQM Veligers Detected (Yes/No)`,
  others = `Native Species Detected`) %>%
  mutate(Lon = ifelse(Lon > 0, -1*Lon, Lon)) %>% 
  mutate(ZQM_veligers = str_to_lower(ZQM_veligers),
         others = replace(others, others == "-", NA)) %>%
  filter(is.na(Lat)==F) %>%
  mutate(Year = 2016)

dat2016 = dat2016[,c(8,1:7)]

dat2016_sf = st_as_sf(SpatialPointsDataFrame(coords = dat2016[,c(5,4)],
                                             data = dat2016[,-c(5,4)],
                                             proj4string = crs(bc_shp)))


#==========================================================================
#             2017
dat2017$Year = 2017

#First of all, some rows are missing coord data. Let's remove these rows.
dat2017 = dat2017 %>%
  filter(is.na(coords) == F)

#Some of the EMS data (coordinates) are in UTMs. Make into spatial file. 
dat2017_utms = dat2017 %>%
  filter(str_detect(coords, "^[10,11]")) %>% 
  mutate(zone = str_extract(coords, "^[0-9]{2}")) %>% 
  mutate(coords = str_squish(str_remove(coords, "^[0-9]{2}[A-Z]{1}(,)?"))) %>% 
  separate(coords, into = c("Northing", "Easting"), sep = "\\,") %>% 
  mutate(Northing = str_remove(Northing, "^0")) %>% 
  mutate(Easting = str_squish(Easting)) %>% 
  mutate(Northing = str_remove_all(Northing, "[A-Z]"),
         Easting = str_remove_all(Easting, "[A-Z]")) %>% 
  mutate(Easting = case_when(
    CC == "CC180511" ~ "5556973",
    T ~ Easting
  )) %>% 
  mutate(Northing = case_when(
    CC == "CC181114" ~ "509965",
    T ~ Northing
  ))

#Zone 10...
utms = dat2017_utms %>% 
  filter(zone == "10") %>% 
  st_as_sf(coords = c("Northing","Easting"),
            crs = 3157) %>% 
  st_transform(crs = 4326) %>% 
  #Join to zone 11...
  bind_rows(dat2017_utms %>% 
              filter(zone == "11") %>% 
              st_as_sf(coords = c("Northing","Easting"),
                       crs = 26911) %>% 
              st_transform(crs = 4326))

#Check for incorrect coordinates.
# ggplot() + 
#   geom_sf(data = st_as_sf(bc_shp)) +
#   geom_sf(data = utms) + 
#   geom_sf_label(data = utms, aes(label = CC))

#normal lat/long data.
dat2017_sf = dat2017 %>% 
  filter(!str_detect(coords, "^[10,11]")) %>% 
  separate(coords, into = c("lat","long"), sep = ",") %>%
  mutate(long = str_squish(long)) %>% 
  mutate(long = case_when(
    CC == "CC181138" ~ "-115.180866",
    CC == "CC180515" ~ "-117.936536",
    !str_detect(long, "^-") ~ paste0("-",long),
    T ~ long
  )) %>%
  mutate(lat = case_when(
    CC == "CC180307" ~ "49.3394",
    CC == "CC180515" ~ "50.4342",
    T ~ lat
  )) %>% 
  mutate(lat = as.numeric(lat),
         long = as.numeric(long)) %>% 
  filter(!is.na(long),
         !is.na(lat)) %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  #Join the UTM records from above...
  bind_rows(utms)

#Look for outliers in lat/long data points. There's one outside BC, drop it.
# ggplot() + 
#   geom_sf(data = st_as_sf(bc_shp)) +
#   geom_sf(data = dat2017_sf) +
#   geom_sf_label(data = dat2017_sf, aes(label = CC))

dat2017_sf = dat2017_sf %>% 
  filter(!(Site == "MOE" & Sample == "Trout" & Date == "20-Sep-17"))

dat2017_sf = dat2017_sf %>% 
  mutate(NoDrMusFound = "no") %>% 
  mutate(Date = lubridate::as_date(Date)) %>% 
  rename(CollectDate = Date,
         Waterbody = Sample,
         Location = CC,
         SamplingGroup = Site,
         ZQM_veligers = NoDrMusFound)
#==========================================================================
#             2018

#column names.
dat2018 = dat2018 %>%
  rename(SamplingGroup = `Sampling Agency`,
         WaterbodyType = `Waterbody type`,
         Lon = long,
         CollectDate = Date,
         SamplingMethod = `Sampling method`,
         PlanktonTowType = `Type of tow`,
         LifeStage = `target life stage`,
         TowDepthOrLength = `Depth (m)`,
         NetMeshSize_um = `net mesh size (?m)`,
         TotalVolume_L = `Minimum volume of water sampled (L)`) %>%
  select(-`...14`)

dat2018 = dat2018 %>% 
  mutate(CollectDate = case_when(
    str_detect(CollectDate,"^2018") ~ lubridate::as_date(CollectDate),
    str_detect(CollectDate,"^4") ~ janitor::excel_numeric_to_date(as.numeric(CollectDate)))) %>% 
  mutate(Year = year(CollectDate)) %>% 
  mutate(Year = replace_na(Year, 2018))

dat2018 = dat2018 %>% 
  mutate(ZQM_veligers = "no", others = "NA")

dat2018_sf = dat2018 %>% 
  st_as_sf(coords = c("Lon","Lat"), 
           crs = crs(bc_shp))

library(ggsflabel)
# ggplot() +
#   geom_sf(data = st_as_sf(bc_shp)) + 
#   geom_sf(data = dat2018_sf) +
#   geom_sf_text_repel(data = dat2018_sf, 
#                      aes(label = Waterbody))

#==========================================================================
#             2019

#column names.
dat2019 = dat2019 %>% 
  rename(CollectDate = `Date Collected (yyyy-mm-dd)`,
         Location = `Location (site name)`,
         NumContainers = `# Containers`,
         Lat = `Lat (Decimal degrees)`,
         Lon = `Long (Decimal degrees)`,
         WaterTemp = `Water temperature (?C)*`,
         WaterpH = `pH (in water column)`,
         SecchiDepth = `Secchi Depth (m)`,
         NumPlanktonTows = `# of Plankton tows`,
         TowDepthOrLength = `Depth/Length of tow (m)`,
         PlanktonTowType = `Type of plankton tow (vertical or horizontal)`,
         TotalVolume_L = `Total Volume Sampled (L)`,
         Buffered = `Buffered (Y/N)`,
         SamplepH_pres = `Sample pH (at the time preserved)`,
         SamplepH_lab = `Sample pH (at arrival in the lab)`,
         PresTypeConc = `Preservative Type and Concentration`,
         Notes = `Description of Location/Notes`,
         ZQM_veligers = `Zebra/Quagga mussels veligers`,
         SamplingGroup = `sampling group/agency`)

dat2019 = dat2019 %>%  
  mutate(CollectDate = case_when(
    ...1 == 233 ~ as_date("2019-09-24"),
    str_detect(CollectDate,"^2019") ~ lubridate::as_date(CollectDate),
    str_detect(CollectDate,"^4") ~ janitor::excel_numeric_to_date(as.numeric(CollectDate)))) %>% 
  mutate(Year = year(CollectDate))

dat2019 = dat2019 %>%
  filter(is.na(Lat) == F,
         is.na(Lon) == F,
         Lat != 0)

dat2019[dat2019$Lat < 0,c(6,7)] <- dat2019[dat2019$Lat < 0,c(7,6)]
dat2019[dat2019$Lon > 0,7] <- (-1)*dat2019[dat2019$Lon > 0,7]

dat2019_sf = st_as_sf(SpatialPointsDataFrame(coords = dat2019[,c(7,6)],
                                             dat = dat2019[,-c(7,6)],
                                             proj4string = crs(bc_shp)))

#==========================================================================
#             2021

dat2021_m = dat2021 %>% 
  janitor::clean_names() %>% 
  select(-x1) %>% 
  mutate(CollectDate = janitor::excel_numeric_to_date(as.numeric(date_collected_yyyy_mm_dd))) %>% 
  mutate(CollectDate = replace_na(CollectDate,lubridate::as_date("2021-07-08"))) %>%
  mutate(Year = year(CollectDate)) %>% 
  mutate(lat_decimal_degrees = str_remove(lat_decimal_degrees, "?"),
         long_decimal_degrees = str_remove(long_decimal_degrees, "?")) %>% 
  mutate(lat_decimal_degrees = str_remove(lat_decimal_degrees, "(?<=\\.) ")) %>% 
  mutate(Lat = as.numeric(lat_decimal_degrees),
         Lon = as.numeric(long_decimal_degrees)) %>% 
  mutate(Lon = ifelse(Lon > 0, -1*Lon, Lon)) %>% 
  mutate(number_containers = as.numeric(number_containers)) %>% 
  mutate(water_temperature_c = as.numeric(water_temperature_c)) %>% 
  mutate(p_h_in_water_column = as.numeric(p_h_in_water_column)) %>% 
  mutate(secchi_depth_m = as.numeric(secchi_depth_m)) %>% 
  mutate(number_of_plankton_tows = as.numeric(number_of_plankton_tows)) %>% 
  mutate(depth_length_of_tow_m = as.numeric(depth_length_of_tow_m)) %>% 
  mutate(total_volume_sampled_l = as.numeric(total_volume_sampled_l)) %>% 
  mutate(sample_p_h_at_the_time_preserved = as.numeric(sample_p_h_at_the_time_preserved)) %>% 
  rename(Location = location_site_name,
         NumContainers = number_containers,
         Waterbody = waterbody,
         WaterTemp = water_temperature_c,
         WaterpH = p_h_in_water_column,
         SecchiDepth = secchi_depth_m,
         NumPlanktonTows = number_of_plankton_tows,
         TowDepthOrLength = depth_length_of_tow_m,
         TotalVolume_L = total_volume_sampled_l,
         Buffered = buffered_y_n,
         SamplepH_pres = sample_p_h_at_the_time_preserved,
         PresTypeConc = preservative_type_and_concentration,
         ZQM_veligers = zebra_quagga_mussels_veligers,
         SamplingGroup = sampling_group_agency)

dat2021_sf = dat2021_m %>% 
  st_as_sf(coords = c("Lon","Lat"), crs = 4326)

#Check for incorrect coordinates.
# ggplot() +
#   geom_sf(data = st_as_sf(bc_shp)) +
#   geom_sf(data = dat2021_sf) +
#   geom_sf_label(data = dat2021_sf, aes(label = Waterbody))

#==========================================================================
#             
#                        Let's combine these shapefiles!
#
#==========================================================================

alldat_sf = dat2020_sf %>%
  bind_rows(dat2021_sf) %>% 
  bind_rows(dat2019_sf) %>%
  bind_rows(dat2018_sf) %>%
  bind_rows(dat2017_sf) %>%
  bind_rows(dat2016_sf) %>%
  bind_rows(dat2015_sf) %>%
  bind_rows(dat2014_sf) %>%
  bind_rows(dat2013_sf)

alldat_sf = alldat_sf[,-c(24:ncol(alldat_sf))]

alldat_sf$Lon = sf::st_coordinates(alldat_sf)[,1]
alldat_sf$Lat = sf::st_coordinates(alldat_sf)[,2]

#Some minor corrections to SamplingGroup. First, let's see which sampling groups have very few observations;
#These are likely typos.
alldat_sf %>% 
  st_drop_geometry() %>% 
  count(SamplingGroup) %>% 
  arrange(n) %>% View()
#The top 10 or 11 are probably typos.

#Corrections of sampling groups... I'm favouring the full names here,
#instead of the acronyms.
alldat_sf = alldat_sf %>% 
  mutate(SamplingGroup = case_when(
    SamplingGroup == "BIS" ~ "Boundary Invasive Species Society",
    SamplingGroup == "BISS" ~ "Boundary Invasive Species Society",
    SamplingGroup == "Boundary ISS" ~ "Boundary Invasive Species Society",
    SamplingGroup == "CSISC" ~ "CSISS",
    str_detect(SamplingGroup, "MOE") ~ "ENV",
    SamplingGroup == "LN" ~ "CSISS",
    SamplingGroup == "Central Kootenay ISS" ~ "Central Kootenay Invasive Species Society",
    SamplingGroup == "CKISS" ~ "Central Kootenay Invasive Species Society",
    SamplingGroup == "Christina Lake SS" ~ "Christina Lake Stewardship Society",
    SamplingGroup == "CLSS" ~ "Christina Lake Stewardship Society",
    SamplingGroup == "Columbia/Shuswap ISS" ~ "Columbia Shuswap Invasive Species Society",
    SamplingGroup == "CSISS" ~ "Columbia Shuswap Invasive Species Society",
    SamplingGroup == "East Kootenay ISC" ~ "East Kootenay Invasive Species Council",
    SamplingGroup == "EKISC" ~ "East Kootenay Invasive Species Council",
    SamplingGroup == "FVISS" ~ "Fraser Valley Invasive Species Society",
    SamplingGroup == "ISCBC" ~ "Invasive Species Council of British Columbia",
    SamplingGroup == "Lakes Network- LN" ~ "Lakes Network",
    SamplingGroup == "Lillooet Regional ISS" ~ "Lillooet Regional Invasive Species Society",
    SamplingGroup == "LRISS" ~ "Lillooet Regional Invasive Species Society",
    SamplingGroup == "MFLNRO" ~ "FLNRORD",
    SamplingGroup == "Ministry of Forests, Lands, Natural Resource Operations and Rural Development" ~ "FLNRORD",
    SamplingGroup == "FLNRO" ~ "FLNRORD",
    SamplingGroup == "Ministry of Environment and Climate Change Strategy" ~ "MOE",
    SamplingGroup == "Northwest IPC" ~ "Northwest Invasive Plant Council",
    SamplingGroup == "NWIPC" ~ "Northwest Invasive Plant Council",
    SamplingGroup %in% c("Okanagan and SimilkameeInvasive Species Society",
                         "Okanagan and Similkameen Invasive Species Society (OASISS)",
                         "OASSIS") ~ "Okanagan and Similkameen Invasive Species Society",
    SamplingGroup %in% c("Sea to Sky",
                         "SSISC") ~ "Sea to Sky Invasive Species Council",
    SamplingGroup == "ONA" ~ "Okanagan Nation Alliance",
    T ~ SamplingGroup
  ))

ggplot() +
  geom_sf(data = st_as_sf(bc_shp)) +
  geom_sf(data = alldat_sf, aes(col = Year)) + 
  facet_wrap(~ Year)

#How many dots fall outside of BC right now? About 30.
alldat_sf = alldat_sf %>% 
  st_join(st_as_sf(bc_shp) %>% select(GID_1), 
          st_intersects) %>% 
  filter(!is.na(GID_1))


write_sf(alldat_sf, 
         "//sfp.idir.bcgov/s140/S40203/Ecosystems/Conservation Science/Invasive Species/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/Combined Years/shapefile/LakeMonitoringdata_2013_2021_Combined.gpkg",
         overwrite = T)

write.csv(alldat_sf %>% st_drop_geometry(), 
          "//sfp.idir.bcgov/s140/S40203/Ecosystems/Conservation Science/Invasive Species/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/Combined Years/LakeMonitoringdata_2013_2021_Combined.csv",
          row.names = F)

