### Finding the property values of privately owned land in BC

# Load libraries

library(readr)
library(raster)
library(sf)
library(ggthemes)
library(ggspatial)
library(ggpattern)
library(rgdal)
library(viridis)
library(lubridate)
library(tidyverse)
#remotes::install_github('coolbutuseless/ggpattern')
library("ggpattern")
library(rvest)

#This next line clears out all objects that are currently loaded into the 'global environment'. If you are 
#okay clearing out this cache, it frees up some RAM on your computer, speeding up subsequent steps.

rm(list=ls())

#==========================================================================

#Where is your R project located? This becomes the 'root' folder
mywd = "D:/ENV/R/RWork/"

#Set the working directory (which folder R is 'looking' at) to your 'mywd'.
setwd(mywd)

#This assumes you have a folder called 'data' in your root folder, in which you place your data files.
data.path = paste0(mywd,"data/")

#IF you don't yet have a folder called 'output' in your root folder, this command makes such a folder.
if(!dir.exists(paste0(mywd,"output/"))) {
  dir.create(paste0(mywd,"output/"))
}

#==========================================================================

# ogrListLayers("D:/ENV/R/RWork/data/Default.gdb")
# #The layer we want is called 'PrivateOwnershipData'
# priv = readOGR("D:/ENV/R/RWork/data/Default.gdb",
#                layer = "PrivateOwnershipData")
# #This is a HUGE data file. 4.4 gigabytes. It takes a long time to read in (2:07 PM -> 2:50 PM), and your RAM will likely
# #be almost completely swallowed up by it. I'm going to try to split it into like 5 pieces.
# #Let' see if we can read in the rivers, lakes, man-made waterbodies in...
# 
# #Number of rows for each chunk. It's approximately the total divided by 5.
# nr = 50000
# #About 50,000
# 
# setwd(paste0(data.path,"shapefiles"))
# 
# #Save chunks of this shapefile (privately owned land in BC), each chunk being 50,000 parcels of land.
# shapefile(priv[c(1:300000),], "PrivatelyOwnedLand_chunk1.shp", overwrite = T)
# shapefile(priv[c(300001:600000),], "PrivatelyOwnedLand_chunk2.shp", overwrite = T)
# shapefile(priv[c(600001:900000),], "PrivatelyOwnedLand_chunk3.shp", overwrite = T)
# shapefile(priv[c(900001:1200000),], "PrivatelyOwnedLand_chunk4.shp", overwrite = T)
# shapefile(priv[c(1200001:nrow(priv)),], "PrivatelyOwnedLand_chunk5.shp", overwrite = T)
# 
# rm(priv)
# gc()

setwd(paste0(data.path,"shapefiles"))

chunk.names = list.files(path = getwd(), pattern = "_chunk[0-9].shp")

#Read in the waterbody shapefiles (4: waterbody groups, lakes, rivers and streams)
watershedshp = rgdal::readOGR("WatershedGroups.shp")
lakeshp = sf::read_sf("LakePoly.shp") #Subdivide points by 'lake or pond'
rivershp = sf::read_sf("RiverPoly.shp")
manmadeshp = sf::read_sf("ManmadePoly.shp")

#Combine lakes and man-made waterbodies
lakeshp = rbind(lakeshp, manmadeshp)
rm(manmadeshp)

#Buffer the rivers and lakes by x meters
x = 50
riverbuff = sf::st_buffer(rivershp, dist = x)
lakebuff = sf::st_buffer(lakeshp, dist = x)

#Read in an empty 'container' shapefile that we'll add the waterfront parcels to.
waterfront_priv = sf::read_sf("PrivatelyOwnedParcels_container.shp")

for(chunk in chunk.names){

print(paste0("Starting work on: ", chunk))
priv = sf::read_sf(chunk)

#Make a buffer around the river shapefile.
river.intersections = sf::st_intersects(priv, riverbuff)
lake.intersections = sf::st_intersects(priv, lakebuff)

river.int.df = as.data.frame(river.intersections)
lake.int.df = as.data.frame(lake.intersections)

#Some parcels are close to more than one river or lake... the next lines take only the FIRST intersection
#, i.e. just one point of intersection between a private land parcel and a waterbody.
river.int.df = river.int.df[!duplicated(river.int.df$row.id),]
lake.int.df = lake.int.df[!duplicated(lake.int.df$row.id),]

#The row.id is the private parcels of land. Perhaps they are the row number.
#The col.id is the identifier of the river.

#Let's show this result in action by zooming in on one river.
# eg.river = 4514
# eg.priv = river.int.df %>% filter(col.id == eg.river)
# 
# eg.overlap = priv[eg.priv$row.id,]
# 
# ggplot() +
#   geom_sf(data = eg.overlap) + 
#   geom_sf(data = rivershp[eg.river,]) + 
#   geom_sf(data = riverbuff[eg.river,])

priv$RiverNumber = 0
priv$LakeNumber = 0
priv$WatershedName = "No Watershed"

priv[river.int.df$row.id,]$RiverNumber = rivershp[river.int.df$col.id,]$WATERBODY_
priv[river.int.df$row.id,]$WatershedName = rivershp[river.int.df$col.id,]$WATERSHE_1

priv[lake.int.df$row.id,]$LakeNumber = lakeshp[lake.int.df$col.id,]$WATERBODY_
priv[lake.int.df$row.id,]$WatershedName = lakeshp[lake.int.df$col.id,]$WATERSHE_1

waterfront_priv = rbind(waterfront_priv, priv %>% 
  filter(RiverNumber != 0 | LakeNumber != 0))
}

#Let's get rid of duplicated rows for a given PID!
waterfront_parcels = waterfront_priv[!duplicated(waterfront_priv$PID),]

st_write(waterfront_parcels, "Waterfront_Parcels_50m.shp")

waterfront_parcels = shapefile(paste0(data.path,"shapefiles/Waterfront_Parcels_50m.shp"))

PIDs = waterfront_parcels$PID

write.csv(PIDs, "D:/ENV/R/RWork/output/PIDs_of_interest_50m.csv")

#Let's take a look at the waterfront parcels and waterbodies in one sub-watershed.
watershedshp$WATERSHE_1

watershed.of.interest = "KOTL"

ggplot() + 
  geom_sf(data = lakeshp %>% filter(WATERSHE_1 == watershed.of.interest),
          col = "darkblue", fill = "blue") + 
  geom_sf(data = rivershp %>% filter(WATERSHE_1 == watershed.of.interest),
          col = "darkgrey", fill = "grey") +
  geom_sf(data = waterfront_parcels %>% filter(WatershedName == watershed.of.interest),
          col = "black", fill = "darkred")
  

for(i in 1:length(waterfront_parcels$PID))
  print(paste0("Property ",i," of ",length(waterfront_parcels$PID)))

#The next section would work as a webscrape of the property values for each of the waterfront properties...
#However, I'm not going to run the code for now!

waterfront_parcels$TotalValue = 0
waterfront_parcels$LandValue = 0
waterfront_parcels$BuildingValue = 0
waterfront_parcels$unique.code = "Unknown"
waterfront_parcel_info = data.frame(X1 = "Variable", X2 = "Value", PropID = 0)

first_urls = paste0("https://www.bcassessment.ca/Property/Search/GetByPid/",waterfront_parcels$PID,"?PID=",waterfront_parcels$PID)

for(i in 507:1000) {
my.pid = waterfront_parcels$PID[i]

if(is.na(my.pid)) {next}
print(paste0("Property ",i," of ",length(waterfront_parcels$PID)))

webpage <- read_html(first_urls[i]) %>%
  html_text()

waterfront_parcels$unique.code[i] = str_extract(webpage, "QTA.*==")

Sys.sleep(rnorm(1, mean = 5, sd = 0.8))
}

write.csv(waterfront_parcels@data, paste0(data.path,"waterfront_parcels_50m_partial_data.csv"), row.names = F)

second_urls = paste0("https://www.bcassessment.ca//Property/Info/",waterfront_parcels$unique.code)

for(i in 1:10){
url = paste0("https://www.bcassessment.ca//Property/Info/",unique.code)

webpage = read_html(second_urls[i])

webpage %>%
  html_node(".propInfo") %>%
  html_table() %>%
  mutate(PropID = my.pid)

total.value = webpage %>%
  html_nodes(".total-value") %>%
  html_text() %>% 
  str_remove(",") %>%
  str_extract("[0-9]+")

land.value = webpage %>%
  html_node(".land-building-value") %>%
  html_text() %>% 
  str_remove(",") %>%
  str_extract("[0-9]+")

building.value = values %>%
  html_node(".land-building-value") %>%
  html_text() %>% 
  str_remove_all(",") %>%
  str_remove("[0-9]+") %>%
  str_extract("[0-9]+")

waterfront_parcels[i,]$TotalValue = as.numeric(total.value)
waterfront_parcels[i,]$LandValue = as.numeric(land.value)
waterfront_parcels[i,]$BuildingValue = as.numeric(building.value)

#Sys.sleep("0.1")
}

