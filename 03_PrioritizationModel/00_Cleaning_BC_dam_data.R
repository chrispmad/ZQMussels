library(tidyverse)
rm(list = ls())

setwd("C:/Users/CMADSEN/Downloads/LocalRWork/Projects/ZQMussels/03_PrioritizationModel/data")

# Dams

fortis = openxlsx::read.xlsx("Fortis_facilities.xlsx") %>% 
  rename(Name = Plant.Name,lng = Dec.Long, lat = Dec.Lat, Capacity = Entitlement.Capacity) %>% 
  mutate(Capacity = as.numeric(str_extract(Capacity, "[0-9]*"))) %>% 
  dplyr::select(Name, Capacity, lng, lat) %>% 
  as.tibble(.)

damcalcs = openxlsx::read.xlsx("Hydro Calculations april 13.xlsx") %>% 
  filter(Type %in% c("Non-Storage Hydro","Storage Hydro")) %>% 
  rename(Name = Project.Name, Capacity = `Capacity.(MW)`) %>% 
  dplyr::select(Name, Capacity, Location) %>% 
  as.tibble(.)

bchydro = read_csv("BC_Hydro_facilitiesGIS.csv") %>% 
  rename(lng = `DEC Lon`,lat = `DEC Lat`,Capacity = `Capacity (MW)`) %>% 
  dplyr::select(Name, Capacity, lng, lat) %>% 
  as.tibble(.)

damcalcs$lng = 0
damcalcs$lat = 0

for(i in 1:nrow(damcalcs)){
    # Pull out place name.
    my.loc = damcalcs[i,]$Location
    #Replace spaces with how the URL codes spaces.
    my.loc = str_replace_all(my.loc, " ", "%20")
    
    url = paste0('https://geocoder.api.gov.bc.ca/addresses.json?addressString=',my.loc,'&maxResults=1&outputSRS=4326')
    
    my.coords = jsonlite::fromJSON(url)$features$geometry %>% 
      summarise(lng = str_extract(coordinates, "(?<=c\\().*(?=\\,)"),
                lat = str_extract(coordinates, "(?<=\\,).*(?=\\))"))
    
    damcalcs[i,]$lng = as.numeric(my.coords$lng)
    damcalcs[i,]$lat = as.numeric(my.coords$lat)
}


damcalcs = damcalcs %>% 
  dplyr::select(names(fortis)) %>% 
  mutate(Capacity = replace(Capacity, Capacity == "< 1", "1")) %>% 
  mutate(Capacity = as.numeric(Capacity))

dams = bchydro %>% 
  bind_rows(damcalcs) %>% 
  bind_rows(fortis)

dams = dams %>% 
  mutate(Name = str_remove(Name,"(?<=Shrum).{1}"))

openxlsx::write.xlsx(dams,
                     "C:/Users/CMADSEN/Downloads/LocalRWork/Projects/ZQMussels/03_PrioritizationModel/output/dams_cleaned.xlsx",
                     overwrite = T)
