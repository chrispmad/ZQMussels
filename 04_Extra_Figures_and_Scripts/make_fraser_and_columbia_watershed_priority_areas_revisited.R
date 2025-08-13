library(bcdata)
library(tidyverse)
library(sf)

fras = sf::read_sf("//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/shared_data_sets/Fraser_River_Big_Watershed.shp")
col = sf::read_sf("//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/shared_data_sets/Columbia_River_Big_Watershed.shp")

ggplot() + geom_sf(data = fras)
ggplot() + geom_sf(data = col)

unique(col$MAJOR_WA_1)

unique(fras$MAJOR_WA_1)

col2 = bcdc_query_geodata('bc-major-watersheds') |> 
  filter(MAJOR_WATERSHED_SYSTEM %in% col$MAJOR_WA_1) |> 
  collect()

ggplot() + geom_sf(data = col2, fill = 'red')
