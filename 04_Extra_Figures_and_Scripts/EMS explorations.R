#EMS dataset explorations

library(tidyverse)
library(lubridate)
library(data.table)
library(sf)
library(gmodels)
library(ggthemes)
library(terra)

bc = read_sf("W:/CMadsen/SpatialData/bc_simple.shp")
subw = read_sf("W:/CMadsen/SpatialData/WatershedGroups.shp")
ems = fread("C:/Users/CMADSEN/Downloads/ems_sample_results_historic_expanded.csv")

keep_n_top_parameters = function(x,number_parameters = 50){
  x %>% 
    filter(PARAMETER %in% (
      x %>% 
        count(PARAMETER, sort = T) %>% 
        slice(1:number_parameters) %>% filter(PARAMETER != "") %>% 
        pull(PARAMETER)
    )
    )
}

ems_top_50 = keep_n_top_parameters(ems, number_parameters = 50)

#Explore! Tabular.
ems_top_50 %>% 
  count(PARAMETER, sort = T)

#Make spatial
ems_spatial = ems %>% 
  select(LONGITUDE,LATITUDE,PARAMETER,RESULT) %>% 
  filter(!is.na(LATITUDE)) %>% 
  filter(!is.na(RESULT)) %>% 
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326)
rm(ems)

#Clean results. If below 0, make 0. Limit high values to twice the upper 95% confidence 
#interval.
ems_spatial = ems_spatial %>% 
  group_by(PARAMETER) %>% 
  mutate(group_ci_h = ci(RESULT)[3]) %>% 
  mutate(res_cleaned = case_when(
    RESULT < 0 ~ 0,
    RESULT > 2*group_ci_h ~ 2*group_ci_h,
    T ~ RESULT
  ))

#Remove any dots that fall outside of BC.
ems_spatial = ems_spatial %>% 
  st_transform(crs = 3005)
  
ems_spatial = ems_spatial %>% 
  st_join(bc %>% select(NAME_0, geometry))

#Filter out any rows that have NA for NAME_0
ems_spatial = ems_spatial %>% 
  filter(!is.na(NAME_0))

#Add column that shows RESULT relative to the max value, grouped by parameter.
ems_spatial = ems_spatial %>% 
  group_by(PARAMETER) %>% 
  mutate(res_rel = res_cleaned/max(res_cleaned)) %>% 
  ungroup()

#Take top 5 most numerous parameters - let's plot these.
ems_spatial_top_5 = ems_spatial %>%
  mutate(PARAMETER = fct_lump(PARAMETER,5)) %>% 
  filter(PARAMETER != "Other")
  
#Plot
ggplot() + 
  geom_sf(data = bc) + 
  geom_sf(data = ems_spatial_top_5, aes(col = res_rel)) + 
  facet_wrap(~ PARAMETER)


# ------ Just temperature! ------- #
#Check for just temperature.
temp = ems %>% 
  filter(str_detect(PARAMETER, "temp|Temp|TEMP"))
rm(ems)
gc()

#Keep only winter records.
temp = temp %>% 
  mutate(date_start = ymd_hms(COLLECTION_START),
         date_end = ymd_hms(COLLECTION_END))

#Keep only records from October to February.
temp_winter = temp %>% 
  filter(month(date_start) %in% c(10:12,1,2))

#Make spatial
temp_winter = temp_winter %>% 
  select(LONGITUDE,LATITUDE,PARAMETER,RESULT,date_start,date_end) %>% 
  filter(!is.na(LATITUDE)) %>% 
  filter(!is.na(RESULT)) %>% 
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326)

#Clean results. If below -40, make -40, and if above 50, make 50. 
#Limit high values to four times the upper 95% confidence interval.
temp_winter = temp_winter %>% 
  mutate(RESULT = case_when(
    RESULT > 50 ~ 50,
    RESULT < -40 ~ -40,
    T ~ RESULT)
  ) %>% 
  group_by(PARAMETER) %>% 
  mutate(group_ci_h = ci(RESULT)[3]) %>% 
  mutate(res_cleaned = case_when(
    RESULT > 2*group_ci_h ~ 2*group_ci_h,
    T ~ RESULT
  ))

#Remove any dots that fall outside of BC.
temp_winter = temp_winter %>% 
  st_transform(crs = 3005) %>% 
  st_join(bc %>% select(NAME_0, geometry)) %>% 
  filter(!is.na(NAME_0))

ggplot() + 
  geom_sf(data = bc, fill = "grey") + 
  geom_sf(data = temp_winter %>% filter(RESULT <= -2), aes(col = RESULT)) + 
  scale_color_gradient2(low = "blue", mid = "white", high = "red") + 
  theme_map()

#Calculate mean winter temperatures by month for subwatersheds.
temps_at_subw_scale = temp_winter %>% 
  st_join(subw %>% select(WATERSHE_1), st_intersects)

subw_with_temp = subw %>% 
  left_join(temps_at_subw_scale %>% 
              st_drop_geometry() %>% 
              mutate(Month = month(date_start, abbr = F, label = T)) %>% 
              group_by(Month, WATERSHE_1) %>% 
              filter(!is.na(res_cleaned)) %>% 
              summarise(mean_temp = mean(res_cleaned,na.rm=T)))

subw_with_temp %>%
  filter(!is.na(mean_temp)) %>% 
  ggplot() + 
  geom_sf(aes(fill = mean_temp), col = "black") + 
  scale_fill_gradient2(low = "blue", high = "white", mid = "white") +
  facet_wrap(~ Month) + 
  theme_map()
#Very few subwatersheds seem to have mean temperatures in the winter months below 0... 
#Not so useful.

#How about if we try interpolating temperature, say by seeing if we can explain temperature
#with elevation?

#Get elevation.
library(raster)
canada_elevation = raster::getData(name = "alt", country = "CAN", mask = T)
plot(canada_elevation)

bc_elevation = mask(canada_elevation, bc %>% st_transform(crs = 4326))
bc_elevation = crop(bc_elevation, bc %>% st_transform(crs = 4326))
bc_elevation = terra::rast(bc_elevation)
plot(bc_elevation)

#Find the elevation for each of our mean temperatures.
temp_winter

temp_winter$elevation = as.vector(terra::extract(bc_elevation, vect(temp_winter %>% st_transform(crs = 4326)))[,2])

ggplot(temp_winter, aes(elevation, res_cleaned)) + 
  geom_point() + 
  geom_smooth()
#Not looking so clean...

elev_model = lm(data = temp_winter, res_cleaned ~ elevation)

summary(elev_model)


