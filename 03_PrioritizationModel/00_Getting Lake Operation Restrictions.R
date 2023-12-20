## This script grabs tables from https://laws-lois.justice.gc.ca/eng/Regulations/SOR-2008-120/FullText.html
## to describe which lakes are not to be accessed by boaters.

library(rvest)
library(tidyverse)
library(sf)

setwd("C:/Users/CMADSEN/Downloads/LocalRWork/")

my_opts = read_csv("C:/Users/CMADSEN/Downloads/LocalR/long_term_projects/ZQMussels/Options.csv") %>% 
  as.data.frame()

sched2 = read_html('https://laws-lois.justice.gc.ca/eng/Regulations/SOR-2008-120/page-5.html#h-743207')
sched3 = read_html('https://laws-lois.justice.gc.ca/eng/Regulations/SOR-2008-120/page-6.html#h-743255')
sched4 = read_html('https://laws-lois.justice.gc.ca/eng/Regulations/SOR-2008-120/page-7.html#h-743315')
sched5 = read_html('https://laws-lois.justice.gc.ca/eng/Regulations/SOR-2008-120/page-9.html#docCont')

prohibited = sched2 %>% 
  html_table() %>% 
  .[[2]] %>% 
  as_tibble(.) %>% 
  setNames(str_extract(str_replace_all(.[2,]," ","_"),"[a-zA-Z]*_[a-zA-Z]*(?=_)")) %>% 
  .[-c(1,2),-1] %>% 
  dplyr::select(Name_Given, Location_Reference) %>% 
  distinct() %>% 
  filter(!str_detect(Name_Given, "Repealed")) %>%
  filter(!str_detect(Location_Reference, "^[0-9]{3}")) %>% 
  filter(!str_detect(Name_Given,'That part of')) |> 
  filter(!str_detect(Name_Given,'The waters of')) |> 
  filter(!str_detect(Name_Given,'at coordinates')) |> 
  #Pull the latitude and longitude degree-minute-seconds out of 
  #the 'Location_Reference' field. Clean them up.
  mutate(lat = str_extract(Location_Reference, ".*(?= )"),
         lng = str_extract(Location_Reference, "(?<= )1.*")) %>% 
  #Remove any rows where we have latitude repeated instead of latitude and longitude...
  filter(!is.na(lng)) %>% 
  mutate(lat = str_squish(str_replace_all(lat, "[^0-9]", " ")),
         lng = str_squish(str_replace_all(lng, "[^0-9]", " "))) %>% 
  # mutate(lat = replace(lat, str_detect(Name_Given,"During any event"), "48 32 04"),
  #        lng = replace(lng, str_detect(Name_Given,"During any event"), "123 23 33")) %>% 
  # mutate(lat = replace(lat, str_detect(Name_Given, "Kempenfelt Bay"), "44 23 24"),
  #        lng = replace(lng, str_detect(Name_Given, "Kempenfelt Bay"), "79 36 21")) %>% 
  #Convert lat and long from degree-minute-seconds to decimal degrees.
  separate(lat, paste("lat",c("d","m","s"), sep="_") ) %>%
  separate(lng, paste("lng",c("d","m","s"), sep="_" ) ) %>% 
  mutate(lat_s = replace_na(lat_s, "0"),
         lng_s = replace_na(lng_s, "0")) %>% 
  mutate(across(contains("lat_"), as.numeric)) %>%
  mutate(across(contains("lng_"), as.numeric)) %>% 
  mutate(lat_dec=lat_d + lat_m/60 + lat_s/60^2,
         lng_dec=-(lng_d + lng_m/60 + lng_s/60^2)) %>% 
  dplyr::select(Name_Given, lat_dec, lng_dec) %>% 
  rename(lat = lat_dec, lng = lng_dec) %>%
  #Make into a spatial layer.
  st_as_sf(coords = c("lng","lat"), crs = 4326) %>% 
  mutate(OperRes = "All Vessels Prohibited")

no_power_or_elec = sched3 %>% 
  html_table() %>% 
  .[[1]] %>% 
  as_tibble(.) %>% 
  setNames(str_extract(str_replace_all(.[2,]," ","_"),"[a-zA-Z]*_[a-zA-Z]*(?=_)")) %>% 
  .[-c(1,2),-1] %>% 
  dplyr::select(Name_Given, Location_Reference) %>% 
  distinct() %>% 
  filter(!str_detect(Name_Given, "Repealed")) %>%
  filter(!str_detect(Name_Given,'That part of')) |> 
  filter(!str_detect(Name_Given,'The waters of')) |> 
  filter(!str_detect(Name_Given,'at coordinates')) |> 
  filter(!str_detect(Location_Reference, "^[0-9]{3}")) %>% 
  #Pull the latitude and longitude degree-minute-seconds out of 
  #the 'Location_Reference' field. Clean them up.
  mutate(lat = str_extract(Location_Reference, ".*(?= )"),
         lng = str_extract(Location_Reference, "(?<= ).*")) %>% 
  mutate(lat = str_squish(str_replace_all(lat, "[^0-9]", " ")),
         lng = str_squish(str_replace_all(lng, "[^0-9]", " "))) %>% 
  filter(!is.na(lat)) %>% 
  #Convert lat and long from degree-minute-seconds to decimal degrees.
  separate(lat, paste("lat",c("d","m"), sep="_") ) %>%
  separate(lng, paste("lng",c("d","m"), sep="_" ) ) %>% 
  mutate(across(contains("lat_"), as.numeric)) %>%
  mutate(across(contains("lng_"), as.numeric)) %>%
  mutate(lat_dec=lat_d + lat_m/60,
         lng_dec=-(lng_d + lng_m/60)) %>% 
  #Replace lat and long for Sayres Lake
  mutate(lat_dec = replace(lat_dec, Name_Given == "Sayres Lake", 49.32120070885363),
         lng_dec = replace(lng_dec, Name_Given == "Sayres Lake", -122.32597931074996)) %>% 
  dplyr::select(Name_Given, lat_dec, lng_dec) %>% 
  rename(lat = lat_dec, lng = lng_dec) %>%
  #Make into a spatial layer.
  st_as_sf(coords = c("lng","lat"), crs = 4326) %>% 
  mutate(OperRes = "Power/Electric Prohibited")

#Two rows actually code for multiple lakes. We can find these by filtering
#for Names containing "All" at the start.

#I've made polygons for both areas indicated by these rows. Let's use
#the big waterb layer to identify all of waterbodies indicated.
waterb = read_sf(paste0(my_opts$remote_spatial_data,"shared_data_sets/waterb_with_data.shp")) 
strath = read_sf(paste0(my_opts$remote_spatial_data,"shared_data_sets/Strathcona_Park.shp")) %>% 
  mutate(id = "Strathcona")
bowlake = read_sf(paste0(my_opts$remote_spatial_data,"shared_data_sets/BowronLakePark.shp")) %>% 
  mutate(id = "Bowlake")

strath_waterbodies = waterb[as.data.frame(st_intersects(waterb, strath))$row.id,] %>% 
  filter(!GNIS_NA %in% c("Gold Lake","Buttle Lake",
                         "Upper Lake","Donner Lake",
                         "Campbell Lake"))

ggplot() + 
  geom_sf(data = strath, fill = "antiquewhite") + 
  geom_sf(data = strath_waterbodies, fill = "red")

#Isaac Lake, Lanezi Lake, Sandy Lake, Unna Lake, Babcock Lake,
#Skoi Lake, Spectacle Lakes, Swan Lake, Kibbee Lake, Thompson Lake, 
#Indianpoint Lake, Isaac River, McLeary Lake, Cariboo River.

no_power = sched4 %>% 
  html_table() %>% 
  .[[1]] %>% 
  as_tibble(.) %>% 
  setNames(str_extract(str_replace_all(.[2,]," ","_"),"[a-zA-Z]*_[a-zA-Z]*(?=_)")) %>% 
  .[-c(1,2),] %>% 
  dplyr::select(Name_Given, Location_Reference) %>% 
  distinct() %>% 
  filter(!str_detect(Name_Given,"Repealed")) %>% 
  filter(!str_detect(Name_Given,'That part of')) |> 
  filter(!str_detect(Name_Given,'The waters of')) |> 
  filter(!str_detect(Name_Given,'at coordinates')) |> 
  filter(!str_detect(Location_Reference, "^[0-9]{3}")) %>% 
  #Pull the latitude and longitude degree-minute-seconds out of 
  #the 'Location_Reference' field. Clean them up.
  mutate(lat = str_replace_all(Location_Reference, "[^0-9|^ ]", " "),
         lng = str_replace_all(Location_Reference, "[^0-9|^ ]", " "),
         lat = str_trim(str_extract(lat, ".*(?=[0-9]{3})")),
         lng = str_trim(str_extract(lng, "[0-9]{3}.*")),
         lat = case_when(
           str_length(lat) < 8 ~ paste0(lat," 00"),
           str_length(lat) >= 10 ~ str_extract(lat, "^.{8}"),
           T ~ lat
           ), 
         lng = case_when(
           str_length(lng) < 9 ~ paste0(lng," 00"),
           str_length(lng) >= 11 ~ str_extract(lng, "^.{9}"),
           T ~ lng
         )) %>% 
  filter(!is.na(lat)) %>% 
  #Convert lat and long from degree-minute-seconds to decimal degrees.
  separate(lat, paste("lat",c("d","m","s"), sep="_") ) %>%
  separate(lng, paste("lng",c("d","m","s"), sep="_" ) ) %>%
  mutate(across(contains("lat_"), as.numeric)) %>%
  mutate(across(contains("lng_"), as.numeric)) %>%
  mutate(lat_dec=lat_d + lat_m/60 + lat_s/60^2,
         lng_dec=-(lng_d + lng_m/60 + lng_s/60^2)) %>% 
  dplyr::select(Name_Given, lat_dec, lng_dec) %>% 
  rename(lat = lat_dec, lng = lng_dec) %>% 
  mutate(lat = case_when(
    Name_Given == "Sayres Lake" ~ 49.32156,
    T ~ lat
  )) %>% 
  #Make into a spatial layer.
  st_as_sf(coords = c("lng","lat"), crs = 4326) %>% 
  mutate(OperRes = "Power Vessels Prohibited")

#Take a glance at each of the three schedules.
bc = read_sf(paste0(my_opts$remote_spatial_data,"shared_data_sets/bc_shapefile.shp"))
prohibited = prohibited %>% st_transform(crs = 3005)
okanagan = waterb %>% filter(GNIS_NA == "Okanagan Lake")

ggplot() + 
  geom_sf(data = bc) +
  geom_sf(data = okanagan,
          fill = "blue") +
  geom_sf(data = prohibited) + 
  coord_sf(xlim = st_bbox(okanagan)[c(1,3)],
           ylim = st_bbox(okanagan)[c(2,4)])


# Schedule 5.
speed_limit = sched5 %>% 
  html_table() %>% 
  .[[1]] %>% 
  as_tibble(.) %>% 
  setNames(str_extract(str_replace_all(.[2,]," ","_"),"[a-zA-Z]*_[a-zA-Z]*(?=_)")) %>% 
  .[-c(1,2),-1] %>% 
  dplyr::select(Name_Given, Location_Reference) %>% 
  distinct() %>% 
  filter(!str_detect(Name_Given, "Repealed")) %>%
  filter(!str_detect(Location_Reference, "^[0-9]{3}")) %>% 
  filter(!str_detect(Name_Given,'That part of')) |> 
  filter(!str_detect(Name_Given,'The waters of')) |> 
  filter(!str_detect(Name_Given,'at coordinates')) |> 
  #Pull the latitude and longitude degree-minute-seconds out of 
  #the 'Location_Reference' field. Clean them up.
  mutate(lat = str_extract(Location_Reference, ".*(?= )"),
         lng = str_extract(Location_Reference, "(?<= )1.*")) %>% 
  #Remove any rows where we have latitude repeated instead of latitude and longitude...
  filter(!is.na(lng)) %>% 
  mutate(lat = str_squish(str_replace_all(lat, "[^0-9]", " ")),
         lng = str_squish(str_replace_all(lng, "[^0-9]", " "))) %>% 
  # mutate(lat = replace(lat, str_detect(Name_Given,"During any event"), "48 32 04"),
  #        lng = replace(lng, str_detect(Name_Given,"During any event"), "123 23 33")) %>% 
  # mutate(lat = replace(lat, str_detect(Name_Given, "Kempenfelt Bay"), "44 23 24"),
  #        lng = replace(lng, str_detect(Name_Given, "Kempenfelt Bay"), "79 36 21")) %>% 
  #Convert lat and long from degree-minute-seconds to decimal degrees.
  separate(lat, paste("lat",c("d","m","s"), sep="_") ) %>%
  separate(lng, paste("lng",c("d","m","s"), sep="_" ) ) %>% 
  mutate(lat_s = replace_na(lat_s, "0"),
         lng_s = replace_na(lng_s, "0")) %>% 
  mutate(across(contains("lat_"), as.numeric)) %>%
  mutate(across(contains("lng_"), as.numeric)) %>% 
  mutate(lat_dec=lat_d + lat_m/60 + lat_s/60^2,
         lng_dec=-(lng_d + lng_m/60 + lng_s/60^2)) %>% 
  dplyr::select(Name_Given, lat_dec, lng_dec) %>% 
  rename(lat = lat_dec, lng = lng_dec) %>%
  #Make into a spatial layer.
  st_as_sf(coords = c("lng","lat"), crs = 4326) %>% 
  mutate(OperRes = "All Vessels Prohibited")


#Something is up with how we are currently using the complete restriction data.
write_sf(prohibited, paste0(my_opts$base_dir,"Projects/ZQMussels/03_PrioritizationModel/data/complete_watercraft_restrictions.gpkg"))

#Combine the three spatial layers.
restr = prohibited %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(no_power) %>% 
  bind_rows(no_power_or_elec) %>% 
  bind_rows(speed_limit) |> 
  arrange(Name_Given)

unique(restr$Name_Given)

restr = restr %>% 
  st_transform(crs = 3005)

#Check that no fields are too long.
str_length(restr$Name_Given)
#Check that we don't have any errors in coordinates.

ggplot() + 
  geom_sf(data = bc, fill = "grey") + 
  geom_sf(data = restr, #%>% filter(OperRes == "All Vessels Prohibited"), 
          aes(col = OperRes)) + 
  geom_sf_label_repel(data = restr %>% filter(OperRes == "Power/Electric Prohibited"),
                      aes(label = Name_Given))
  # geom_sf_label_repel(data = restr, #%>% filter(OperRes == "All Vessels Prohibited"),
  #                     aes(label = Name_Given),
  #                     force = 100)

#All looks good!
write_sf(restr,
         paste0(my_opts$base_dir,"03_PrioritizationModel/data/Waterbody_operation_restrictions.gpkg"))

