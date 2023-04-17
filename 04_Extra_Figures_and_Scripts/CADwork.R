library(raster)
library(sp)
library(sf)
library(rmapshaper)
library(tidyverse)

rm(list = ls())
gc()


recalculate.wb.development = T

#BCG Warehouse waterbodies (rivers, lakes and man-made waterbodies)
wbs = read_sf("data/shapefiles/all_bcg_waterbodies_cleaned.shp")

#Make buffer of X meters from waterbodies.
X = 20

#Permanently buffer 20 meters on each waterbody.
wbs = st_buffer(wbs %>% 
                  dplyr::select(WATERBOD_1,WATERSHED_,GNIS_NAME_),
                dist = X)

#If we already have the cadastral parcels filtered by the X meter buffer from waterbodies and merged together, load it in.
if(file.exists("C:/Users/CMADSEN/Downloads/LocalRWork/data/shapefiles/Cadastral_layer_for_wbs_merged.shp")){
  merged_cads = read_sf("data/shapefiles/Cadastral_layer_for_wbs_merged.shp")
}
#If not, we have to make that shapefile. Takes a couple hours.
if(!file.exists("C:/Users/CMADSEN/Downloads/LocalRWork/data/shapefiles/Cadastral_layer_for_wbs_merged.shp")){
  #Full Cadastral file. 
  cad = read_sf("C:/Users/CMADSEN/Downloads/LocalRWork/data/shapefiles/CadastralLayer_complete.shp") %>% 
    filter(!str_detect(OWNERSHIP_, "CROWN"))
  
  #Find intersection of cadastral parcels with waterbody buffer layer.
  my.intersection = st_intersects(waterb_buffer,cad)
  
  #Turn results into a table for indexing.
  my.cads = as.data.frame(my.intersection) %>% 
    rename(WB_id = row.id, CAD_id = col.id)
  
  #Write out our mid-processing results, just in case.
  openxlsx::write.xlsx(my.cads,
                       paste0("C:/Users/CMADSEN/Downloads/LocalRWork/output/waterbody_cadastral_",X,"_m_match.xlsx"),overwrite = T)
  
  #Initialize a new column 
  cad$Waterbody = "None"
  
  #Which are the unique waterbodies for which we found at least
  # one intersection with a cadastral bit?
  wbs_uniq = wbs[unique(my.cads$WB_id),]
  
  #Set up a new table 'merged_cads' - we're going to save the 
  #output of the loop below to this table. Each row is one of the waterbodies that had at least 1 intersection with a cadastral bit.
  merged_cads = data.frame(wb_rownumber = unique(my.cads$WB_id),
                           wb_id = wbs_uniq$WATERBOD_1) %>% 
    filter(!duplicated(wb_id))
  
  #Initialize geometry column of new table 'merged_cads'...
  st_geometry(merged_cads) = rep(st_sfc(st_point(c(1,1))),nrow(merged_cads))
  
  # Iterate through each of these waterbodies, 
  # find the cadastral bits close to it and merging those together. 
  # Put each shapefile in the row with its waterbody.
  for(i in 1:nrow(merged_cads)){
    print(paste0("On row ",i," of ",nrow(merged_cads)," total."))
    
    wb_id_number = merged_cads[i,]$wb_id
    
    st_geometry(merged_cads[i,]) = cad[my.cads %>% filter(WB_id == merged_cads[i,]$wb_rownumber) %>% 
                                         pull(CAD_id),] %>% 
      mutate(Waterbody = wb_id_number) %>% 
      group_by(Waterbody) %>% 
      summarise() %>% 
      pull(geometry)
  }
  
  #Set CRS of the new geometry column.
  sf::st_crs(merged_cads) <- sp::CRS("EPSG:3005")
  
  #Take out an unnecessary column.
  merged_cads = merged_cads %>% select(-wb_rownumber)
  
  #Save to disk.
  write_sf(merged_cads,
           "C:/Users/CMADSEN/Downloads/LocalRWork/data/shapefiles/Cadastral_layer_for_wbs_merged.shp")
}

#Langford Lake example

testw = wbs %>% filter(GNIS_NAME_ == "Langford Lake")
testc = merged_cads %>% filter(wb_id == testw$WATERBOD_1)

#Langford lake with privately owned land within 20 meters highlighted in red.
ggplot() + 
  geom_sf(data = testw, alpha = 0.8) + 
  geom_sf(data = st_buffer(testw, dist = 20), fill = "blue",alpha=0.3) +
  geom_sf(data = testc, fill = "red")

#Calculate the perimeter of the lake that is developed.
teststring = st_cast(st_buffer(testw, dist = 20), "LINESTRING")
teststring_c = st_crop(teststring, testc)

st_length(teststring_c)/st_length(teststring)


#Developed shoreline:

ggplot() + 
  geom_sf(data = testw %>% mutate(id = "Langford Lake"), 
          aes(col = id, fill = id), alpha = 0.5) + 
  geom_sf(data = st_buffer(testw, dist = 20), alpha=0.1) +
  geom_sf(data = testc %>% mutate(id = "Land parcels"), 
          aes(col = id, fill = id)) + 
  geom_sf(data = teststring_c %>% mutate(id = "Developed shoreline"), 
          aes(col = id, fill = id), size = 2) +
  geom_sf_text(data = testw, aes(label = GNIS_NAME_)) +
  geom_sf_text(data = testc %>% mutate(label = "Developed shoreline"),
               aes(label = label), vjust = 13, hjust = 0.8) +
  ggtitle("77% of Langford Lake's shoreline is non-Crown land",
  subtitle = "(most is privately owned land)") +
  labs(fill = "Legend", col = "") +
  scale_fill_manual(values = c("Langford Lake" = "#2ea0c5",
                               "Land parcels" = "#eab676",
                               "Developed shoreline" = "#F31424")) +
  scale_color_manual(values = c("Langford Lake" = "#2ea0c5",
                                "Land parcels" = "#eab676",
                                "Developed shoreline" = "#F31424"),
                     guide = "none") +
  theme_classic() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )


# merged_cads$dev_total_m = 0
# merged_cads$undev_total_m = 0
# merged_cads$dev_prop = 0

if(recalculate.wb.development == T){
# for(i in 12664:nrow(merged_cads)){
#   print(paste0("On row ",i," of ",nrow(merged_cads)," total."))
#   
#   #Get the waterbody polygon.
#   waterbody.focus = wbs %>% filter(WATERBOD_1 == merged_cads[i,]$wb_id) %>% 
#     summarise(WATERBOD_1 = first(WATERBOD_1))
#   
#   #Get the merged cadastral polygon.
#   cad.focus = merged_cads %>% filter(wb_id == merged_cads[i,]$wb_id)
#   
#   ggplot() +
#     geom_sf(data = waterbody.focus, fill = "blue") +
#     geom_sf(data = cad.focus, fill = "red", 
#             col = "pink", alpha = 0.5)
# 
#   #Get the waterbody perimeter, and the perimeter that is developed.
#   waterbody.string = st_cast(waterbody.focus, "LINESTRING")
#   waterbody.string.cut = st_crop(waterbody.string, cad.focus)
#   
#   #Measure lengths of waterbody perimeter lines.
#   developed_m = as.numeric(st_length(waterbody.string.cut))
#   undevel_m = as.numeric(st_length(waterbody.string))
#   
#   #If there are multiple line segments, add them together.
#   if(length(developed_m > 1)) {developed_m = sum(developed_m)}
#   if(length(undevel_m > 1)) {undevel_m = sum(undevel_m)}
#   
#   #If the waterbody is completely inside the cadastral bit, set the 
#   #undeveloped to 0.
#   if(!is.na(as.numeric(st_contains(cad.focus,waterbody.focus)))){
#     undevel_m = 0
#   }
#   
#   #Conversely, if the cadastral bit is completely inside the waterbody,
#   #which probably is a data cleanliness issue, we need to set the developed
#   #perimeter to 0.
#   
#   if(is_empty(developed_m)){ developed_m = 0 }
#   #Another possible situation is when a lake BARELY pokes out of a 
#   #cadastral bit. If the developed and undeveloped measurements
#   #are identical, we can assume this to be the case, and change
#   #the undeveloped measurement to 0.
#   if(developed_m == undevel_m){ undevel_m = 0 }
#   
#   #Add the perimeters to the merged_cads table.
#   merged_cads[i,]$dev_total_m = developed_m
#   merged_cads[i,]$undev_total_m = undevel_m
#   merged_cads[i,]$dev_prop = developed_m/(developed_m+undevel_m)
#   
#   #Periodically save progress...
#   if(i %% 1000 == 0){
#     write_sf(merged_cads,"output/merged_cads_with_dev_measure.shp")
#     }
#   }
 }

#What if we try to vectorize this, without subsetting
#all the massive dataframe each time?
list_of_wbs = unique(merged_cads$wb_id)

list_of_wbs_polys = wbs[wbs$WATERBOD_1%in%list_of_wbs,]
rm(wbs)
gc()

list_of_wbs_polys = list_of_wbs_polys %>% 
  filter(!duplicated(WATERBOD_1))

#make polygons into lines.
list_of_wbs_lines = st_cast(list_of_wbs_polys,"MULTILINESTRING")
rm(list_of_wbs_polys)

#Measure length of complete waterbody polygons.
list_of_wbs_lines$perimeter = st_length(list_of_wbs_lines)

#Mask out waterbodies with the merged cad polygons.
for(i in 1:nrow(list_of_wbs_lines)){
  print(i)
  list_of_wbs_lines$geometry[i] <- ms_erase(list_of_wbs_lines$geometry[i], 
                                            merged_cads[i,])
}

list_of_wbs_lines$undeveloped = st_length(list_of_wbs_lines)

View(list_of_wbs_lines[i,])
st_set_geometry(list_of_wbs_lines, ms_erase(list_of_wbs_lines$geometry, 
                                            merged_cads))
#4:58 PM

wbs_test = list_of_wbs_lines[20152,]
cad_test = merged_cads[20152,]

ggplot() +
  geom_sf(data = cad_test) +
  geom_sf(data = st_difference(wbs_test, cad_test))

st_set_geometry(wbs_test, st_difference(wbs_test, cad_test))

ggplot() +
  geom_sf(data = cad_test) +
  geom_sf(data = wbs_test)

which(merged_cads$wb_id == 329676318)
which(list_of_wbs_lines$GNIS_NAME_ == "Langford Lake")

ggplot() + 
  geom_sf(data = merged_cads[20152,]) +
  geom_sf(data = list_of_wbs_lines_cut %>% 
            filter(GNIS_NAME_ == "Langford Lake"))
  
