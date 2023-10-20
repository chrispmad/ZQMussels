#This script takes a whopping great .CSV file that is produced
# by the R script "Combining Past Watercraft Inspection Data",
# cleans it, then adds it to water body shapefiles and uploads
# several products to my (CMadsen) W: drive and the I: drive.

library(httr)
library(lubridate)
library(openxlsx)
library(bcdata)
library(jsonlite)
library(readxl)
library(sf)
library(tidyverse)
library(sp)

rm(list = ls())
gc()

#=====================================================
#                       OPTIONS 
#=====================================================
setwd('C:/Users/CMADSEN/Downloads/LocalR/ZQMussels/')

my_opts = read_csv("Options.csv") %>% 
  as.data.frame()

#Which folder should we put specific output files in?
my.output.folder = paste0(my_opts$base_dir,"01_DataCleaning/output/")
my.external.output.folder = paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Multiyear data/")
 
#Data folders
my.data.folder = paste0(my_opts$base_dir,"01_DataCleaning/data/")
my.external.data.folder = "W:/CMadsen/shared_data_sets/"

#Write out intermediate results as excel files? 
write.out.excel = F
#=====================================================
#                     END OF OPTIONS
#=====================================================

# A custom function to grab the number of rows of total inspections,
# high-risk inspections and mussel-fouled inspections.
numb_in = function(x, Step = "NA", Var_used = "NA") {
  
  TotalInspection = nrow(x) + 
    nrow(x %>% filter(Destination_Waterbody_2_Name != "NA")) + 
    nrow(x %>% filter(Destination_Waterbody_3_Name != "NA"))

  HighRisk = nrow(x %>% filter(High_Risk_AIS_Ind == "true" | High_Risk_AIS_Ind == T))
  
  MusselFouled = nrow(x %>% filter(MusselsFound_Ind == T))
  
  z = data.frame(Step = Step, Variable = Var_used, 
                 TotalInspection, HighRisk, MusselFouled)
  return(z)
}

setwd(paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Multiyear data/"))

#Load in the (not yet cleaned) data. This comes from the R script "Combining Past Watercraft Inspection Data.R".
#It should be in I:\SPECIES\Zebra_Quagga_Mussel\Operations\Watercraft Inspection Data\Multiyear data\
dat = readxl::read_xlsx("WatercraftInspectionData_AllYears_PreCleaning.xlsx",
                        col_types = "text")

# Clean up the High_Risk_AIS_Ind field so that it either says 'true' or 'false' (no NA, no "TRUE", etc.)
dat = dat %>% 
  mutate(High_Risk_AIS_Ind = case_when(
    is.na(High_Risk_AIS_Ind) ~ "false",
    High_Risk_AIS_Ind == "TRUE" ~ "true",
    High_Risk_AIS_Ind == "FALSE" ~ "false",
    T ~ High_Risk_AIS_Ind))


#Get the first entry in our little 'tracking' table. This looks at how many inspections
#we have at each filtering step for total inspections, high-risk inspections, and mussel-fouled inspections.
filter_tracker = numb_in(dat, Step = "Inspection data of many years combined")

# ===========================================================
# Basic inspection data cleaning

# Destination Water Body Name cleaning - 
# some names have typos. Add spaces and homogenize capitalization.
dat = dat %>% 
  arrange(Watercraft_Risk_Assessment_ID) %>% 
  #Clean up lake names.
  mutate(Destination_Waterbody_1_Name = str_replace(Destination_Waterbody_1_Name,
                                                    "([0-9]+)([a-zA-Z]+)", 
                                                    "\\1 \\2")) %>%
  mutate(Destination_Waterbody_1_Name = str_to_title(Destination_Waterbody_1_Name))

#There are some more errors in destination waterbody names. Here is a brief
#correction block. It is not exhaustive, but should help matching destinations.
dat = dat %>% 
  pivot_longer(cols = c("Destination_Waterbody_1_Name",
                        "Destination_Waterbody_2_Name",
                        "Destination_Waterbody_3_Name")) %>% 
  mutate(value = str_remove(value, ", Bc")) %>% 
  mutate(value = str_to_title(value)) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  arrange(desc(Year),Watercraft_Risk_Assessment_ID)

#Homogenize station name spellings.
dat = dat %>%
  mutate(Station = str_to_title(Station)) %>%
  mutate(Station = case_when(
    str_detect(Station, "Cascade") ~ "Cascade Border",
    str_detect(Station, "Cutts") ~ "Cutts (Hwy 93)",
    str_detect(Station, "Christina Lake") ~ "Christina Lake",
    str_detect(Station, "Dawson Creek") ~ "Dawson Creek",
    str_detect(Station, "Dry") ~ "Dry Gulch",
    str_detect(Station, "Olsen") ~ "Olsen",
    str_detect(Station, "Pacific") ~ "Pacific",
    str_detect(Station, "Paulson") ~ "Paulson Summit",
    str_detect(Station, "Penticton") ~ "Penticton Roving",
    str_detect(Station, "Salmo") ~ "Salmo",
    str_detect(Station, 'Sumas') ~ 'Fraser Valley Roving',
    TRUE ~ Station)
  )

# Read in a document that helps us correct Stations from 
# Penticton Roving to other station names.

# station_name_changer = read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/2021 data/Transient stations with few inspections.xlsx") %>% 
#   select(Year, Watercraft_Risk_Assessment_ID, Station, `New station name`) %>% 
#   rename(NewStationName = `New station name`)
# 
# dat = dat %>% 
#   left_join(station_name_changer) %>% 
#   mutate(Station = case_when(
#     !is.na(NewStationName) ~ NewStationName,
#     is.na(NewStationName) ~ Station
#   )) %>% 
#   select(-NewStationName)

# ==============================================================
# Update the older data's watercraft type to the 4 
# classifications used in recent years.

#Split watercraft type for older data at each comma, put those in 
#new rows. Convert watercraft type to one of the four types used
#in later years. Bring those up into one row per inspection.

#Grabbing a helpful excel doc that classifies boats into new categories.
boattypes = read_excel("UniqueWCTypes.xlsx") %>% 
  left_join(read_excel("watercraft categories_06Aug2021.xlsx") %>% 
              rename(BoatType = `watercraft type`,
                     NewCat = `New Category`) %>% select(-Comments)) %>% 
  add_row(WatercraftType = "Unknown",
          BoatType = "Unknown",
          NewCat = "Unknown")

dat_recent = dat %>% 
  filter(Year %in% c(2019:max(Year))) %>% 
  select(-WatercraftType) 

#Get the older data with many watercraft types
dat_older = dat %>% 
  filter(Year %in% 
           c("2015","2016","2017","2018")) 

#Split any entries for watercraft type that listed multiple boats
#into separate columns.
dat_older = dat_older %>% 
  separate(WatercraftType, c("Type1","Type2","Type3"), ",")

# Join Martina's boat classifier excel sheet and convert boat type to 
# how it's recorded in more recent years.
dat_older = dat_older %>% 
  select(-Non_Motorized_Counter,-Simple_Counter,
         -Complex_Counter,-Very_Complex_Counter) %>% 
  #Label any NA values for first boat type column - we want to keep these.
  mutate(Type1 = replace_na(Type1, "Unknown")) %>% 
  mutate(Type1 = replace(Type1, Type1 == "other", "Unknown")) %>% 
  #Join on boat type table for first boat type column.
  left_join(boattypes %>% 
              rename(Type1 = WatercraftType,
                     NewCat1 = NewCat) %>% 
              select(-BoatType)) %>% 
  left_join(boattypes %>% 
              rename(Type2 = WatercraftType,
                     NewCat2 = NewCat) %>% 
              select(-BoatType)) %>% 
  left_join(boattypes %>% 
              rename(Type3 = WatercraftType,
                     NewCat3 = NewCat) %>% 
              select(-BoatType)) %>% 
  pivot_longer(cols = starts_with("NewCat")) %>%
  filter(is.na(value)==F) %>% 
  rename(BoatType = value) %>% 
  group_by(Watercraft_Risk_Assessment_ID,BoatType) %>% 
  mutate(NumberCraft = as.numeric(n())) %>% 
  select(-name) %>%
  distinct() %>% 
  pivot_wider(names_from = "BoatType",values_from = "NumberCraft", values_fill = 0) %>% 
  select(-Type1, -Type2, -Type3) %>% 
  rename(Non_Motorized_Counter = `Non-motorized`,
         Simple_Counter = Simple,
         Complex_Counter = Complex,
         Very_Complex_Counter = `Very complex`)

#Recombine older and more recent datasets
dat = dat_recent %>% 
  mutate(Non_Motorized_Counter = as.numeric(Non_Motorized_Counter),
         Simple_Counter = as.numeric(Simple_Counter),
         Complex_Counter = as.numeric(Complex_Counter),
         Very_Complex_Counter = as.numeric(Very_Complex_Counter)) %>% 
  bind_rows(dat_older)

rm(dat_recent);rm(dat_older);rm(boattypes)

#Add second row to filter tracking table...
filter_tracker = rbind(filter_tracker,
                       numb_in(dat,
                               Var_used = "WatercraftType",
                               Step = "Converted older records to current boat types (e.g. non-motorized, simple)"))


# ====================================================================
#  Correct Destination Water bodies that were incorrectly spelled and
#  classify inspections where the boat is actually headed outside of 
#  BC. I prepared this excel sheet by exporting a list of destination
#  waterbody names that did not match with any waterbody in the BC water body shapefile.
#  (the shapefile I refer to is an amalgamation I made of lakes, rivers, 
#  and man-made waterbodies in BC)

name_corr = read_excel("WatercraftInspections_no_WB_NameMatch.xlsx") %>% 
  select(-WATERSHED)

#Add a column with the corrected destination wb names.
dat = dat %>% 
  left_join(name_corr %>% 
              rename(Destination_Waterbody_1_Name = Non_Matching_Name)) %>% 
  left_join(name_corr %>% 
              rename(Destination_Waterbody_2_Name = Non_Matching_Name,
                     Corrected_Name2 = Corrected_Name)) %>% 
  left_join(name_corr %>% 
              rename(Destination_Waterbody_3_Name = Non_Matching_Name,
                     Corrected_Name3 = Corrected_Name))

#Replace names for those waterbodies with incorrect names.
dat = dat %>% 
  mutate(Destination_Waterbody_1_Name = case_when(
    is.na(Corrected_Name) == F ~ Corrected_Name,
    is.na(Corrected_Name) == T ~ Destination_Waterbody_1_Name
  )) %>% 
  mutate(Destination_Waterbody_2_Name = case_when(
    is.na(Corrected_Name2) == F ~ Corrected_Name2,
    is.na(Corrected_Name2) == T ~ Destination_Waterbody_2_Name
  )) %>% 
  mutate(Destination_Waterbody_3_Name = case_when(
    is.na(Corrected_Name3) == F ~ Corrected_Name3,
    is.na(Corrected_Name3) == T ~ Destination_Waterbody_3_Name
  ))

#I'm going to write out our data file at this point so that 
#I can add this year's data (which is fairly clean at this point) to
#the IMDP figures that Martina needs for her September presentation
#and for a data request from DFO.
# dat %>%
#   ungroup() %>%
#   summarise(YearOfInspection = str_extract(Watercraft_Risk_Assessment_ID,"^[0-9]{4}"),
#             Previous_Waterbody_1_Name,
#             Previous_Waterbody_1_Closest_City,
#             Previous_Waterbody_1_Province_Or_State,
#             Destination_Waterbody_1_Name,
#             Destination_Waterbody_1_Closest_City,
#             Destination_Waterbody_1_Province_Or_State) %>%
#   write.csv(paste0(my.output.folder,"BC_Watercraft_Inspection_data_for_DFO.csv"), row.names = F)

#Flag boats that have unknown source province/state.
dat = dat %>% 
  mutate(UnknownSourceBoat = case_when(
    is.na(Province_Code) & is.na(coalesce(Previous_Waterbody_1_Province_Or_State,
                                          Previous_Waterbody_2_Province_Or_State,
                                          Previous_Waterbody_3_Province_Or_State)) ~ T,
    T ~ F
  ))

dat_UnknownSourceBoats = dat %>% 
  filter(UnknownSourceBoat == T)

# 2. Filter out inspections where the destination WB province is 
#    not BC (but keep records with 'unknown' as WB province/state).
# In addition, ~ 8,000 boats are headed to waterbodies that have been 
# incorrectly classified as BC water bodies. These are flagged by "No Match"
# in the Destination 1 Name field; let's remove these as well.

bc_waterbodies = name_corr[name_corr$Corrected_Name != "No Match",]$Corrected_Name

# Quick correction of province/state for some known BC waterbodies.
dat[dat$Destination_Waterbody_1_Name %in% bc_waterbodies,]$Destination_Waterbody_1_Province_Or_State = "BC"
dat[dat$Destination_Waterbody_1_Closest_City %in% c("Chemainus,",
                                                    "Vernon",
                                                    "Qualicum Beach",
                                                    "Langley") ,]$Destination_Waterbody_1_Province_Or_State = "BC"

dat[dat$Destination_Waterbody_1_Closest_City %in% c("Chemainus,",
                                                    "Qualicum Beach",
                                                    "Langley") ,]$Destination_Waterbody_1_Name = "Pacific Ocean"

# Data partition 2: Boats heading for WBs not in BC.
dat = dat %>% 
  mutate(DestNotBC = case_when(
    Destination_Waterbody_1_Province_Or_State != "BC" |
      Destination_Waterbody_1_Name == "No Match" ~ T,
    T ~ F
  ))

dat_DestNotBC = dat %>% filter(DestNotBC == T)

# Data partition 3 part 1: boats headed for dry storage.
dat = dat %>% 
  mutate(DryStorage = case_when(
    Destination_Waterbody_1_Name == "Dry Storage" | Destination_Dry_Storage_Ind == "true" ~ T,
    T ~ F
  ))

dat_DryStorage = dat %>% filter(DryStorage == T)

# Data partition 3 part 2: Unknown destination waterbody.
dat = dat %>% mutate(UnknownDest = case_when(
  Unknown_Destination_Waterbody_Ind == "true" ~ T,
  T ~ F))

dat_Unknown = dat %>% filter(UnknownDest == T)

# Data partition 4: Boats headed for the Pacific (or Arctic) Ocean
dat = dat %>% 
  mutate(OceanBoat = case_when(
    str_detect(Destination_Waterbody_1_Name, "Ocean") ~ T,
    T ~ F
  ))

dat_PacificBoats = dat %>% filter(OceanBoat == T)

#Correct some columns from character ("true" or "false") to actual T or F.
dat = dat %>% 
  mutate(across(ends_with("Ind"), ~.x == T | .x == "true"))

#Clean up start and end times for shifts.
dat = dat %>% 
  mutate(Start_Time = case_when(
    Year == "2021" & !is.na(StartTimeComments) ~ StartTimeComments,
    Year == "2021" & is.na(StartTimeComments) ~ StartTimeAuto,
    T ~ Start_Time
  )) %>% 
  mutate(End_Time = case_when(
    Year == "2021" & !is.na(EndTimeComments) ~ EndTimeComments,
    Year == "2021" & is.na(EndTimeComments) ~ EndTimeAuto,
    T ~ End_Time
  ))

#Homogenize the start time, end time, and raw timestamp columns. 
dat = dat %>% 
  mutate(Start_Time = case_when(
    str_detect(Start_Time, "T") ~ as_datetime(Start_Time),
    str_detect(Start_Time,"^[0-9]{5}") ~ convertToDateTime(Start_Time),
    str_detect(Start_Time,"^[0-9]{4}-") ~ convertToDateTime(Start_Time))) %>% 
  mutate(End_Time = case_when(
    str_detect(End_Time, "T") ~ as_datetime(End_Time),
    str_detect(End_Time,"^[0-9]{5}") ~ convertToDateTime(End_Time),
    str_detect(End_Time,"^[0-9]{4}-") ~ convertToDateTime(End_Time))) %>% 
  mutate(TimeOfInspection = convertToDateTime(TimeOfInspection))

#Write out the inspections at this point - this is the most inclusive, clean
#dataset of inspection records possible, though I have limited the columns.
#We use this dataset to create the excel-type figures and the GIS data 
#for the IMDP final reports.

dat %>% 
  select(Year,
         Watercraft_Risk_Assessment_ID,
         MusselsFound_Ind,
         High_Risk_AIS_Ind,
         High_Risk_Area_Ind,
         Station,
         Start_Time,
         End_Time,
         TimeOfInspection,
         Shift_Start_Comment,
         Shift_End_Comment,
         Previous_Waterbody_1_Name,
         Previous_Waterbody_1_Closest_City,
         Previous_Waterbody_1_Province_Or_State,
         Unknown_Previous_Water_Body_Ind,
         Previous_Major_City,
         Destination_Waterbody_1_Name,
         Destination_Waterbody_1_Closest_City,
         Destination_Waterbody_1_Province_Or_State,
         Destination_Major_City,
         Non_Motorized_Counter,
         Simple_Counter,
         Complex_Counter,
         Very_Complex_Counter,
         Non_Motorized_Blow_Bys_Counter,
         Motorized_Blow_Bys_Counter,
         Total_BlowBys,
         Province_Code,
         Decontamination_Performed_Ind,
         Decontamination_order_issued_Ind,
         Commercially_Hauled_Ind,
         New_Passport_Issued_Ind,
         Passport_Holder_Ind,
         Previous_Inspection_Ind,
         Previous_Inspection_Days_Count,
         Previous_AIS_Knowledge_Ind,
         Previous_AIS_Knowledge_Source_Code_ID,
         Clean_Drain_Dry_After_Inspection_Ind,
         Seal_Issued_Ind,
         Quarantine_Period_Issued_Ind,
         General_Comment,
         UnknownSourceBoat,
         DestNotBC,
         DryStorage,
         Unknown_Destination_Waterbody_Ind,
         OceanBoat) %>% 
  #Clean up columns that should be T or F.
  openxlsx::write.xlsx(.,paste0(my.external.output.folder,"WatercraftInspectionData_AllYears_Selected_Columns.xlsx"),
                       overwrite = T)

#We want to make sure that we don't lose any of these boats...
#i) backup of all dat.
dat_b = dat

#ii) backup of all high-risk boats.
highrisk_dat = dat %>% 
  filter(High_Risk_AIS_Ind == T)

#iii) backup of all mussel-fouled boats.
musselfouled_dat = dat %>% 
  filter(MusselsFound_Ind == T)

# Do the actual filtering:
# I)
dat = dat %>%
  filter(UnknownSourceBoat == F) %>%
  select(-UnknownSourceBoat)

filter_tracker = rbind(filter_tracker,
                        numb_in(dat, Var_used = "Province_Code, Previous_Waterbody_1_Province_Or_State (and 2 and 3)",
                                Step = "Removed boats with unknown source province / state."))

#II)
dat = dat %>% 
  filter(DestNotBC == F) %>% 
  select(-DestNotBC)

filter_tracker = rbind(filter_tracker, 
                       numb_in(dat, 
                               Var_used = "Destination_Waterbody_1_Province_Or_State, Destination_Waterbody_1_Name",
                               Step = "Removed boats heading to non-BC waterbodies or waterbodies whose names didn't match a BC waterbody"))

#III) 
dat = dat %>% 
  filter(DryStorage == F) %>% 
  select(-DryStorage)

filter_tracker = rbind(filter_tracker, 
                       numb_in(dat,
                               Var_used = "Destination_Waterbody_1_Name, Destination_Dry_Storage_Ind",
                               "Removed boats heading to dry storage"))

#IV) 
dat = dat %>% filter(UnknownDest == F)

filter_tracker = rbind(filter_tracker, 
                       numb_in(dat, 
                               Var_used = "Unknown_Destination_Waterbody_Ind",
                               "Removed boats with unknown destination waterbody 1"))

#V)
dat = dat %>% filter(OceanBoat == F) %>% 
  select(-OceanBoat)

filter_tracker = rbind(filter_tracker,
                       numb_in(dat, 
                               Var_used = "Destination_Waterbody_1_Name",
                               "Removed boats headed to the Pacific Ocean"))

# ===========================================================
# We need to bring in lakes, rivers and man-made water bodies, 
# drop nameless ones, then summarize any that have multiple 
# polygons into a single multipolygon.

#Add various summaries of inspections to lake simple features by name.
if(file.exists(paste0(my.external.data.folder,"summarized_bc_waterbodies_same_gnis_joined.shp"))) {
  wbs_m = sf::read_sf(paste0(my.external.data.folder,"summarized_bc_waterbodies_same_gnis_joined.shp"))
}
if(!file.exists(paste0(my.external.data.folder,"summarized_bc_waterbodies_same_gnis_joined.shp"))){
  #The code below in this if chunk is untested.
  manmade_noNA = bcdata::bcdc_get_data("freshwater-atlas-manmade-waterbodies") %>% 
    select(-id) %>% filter(is.na(GNIS_NAME_1)==F)
  lakes_noNA = bcdata::bcdc_get_data("freshwater-atlas-lakes") %>% 
    select(-id) %>% filter(is.na(GNIS_NAME_1)==F)
  rivers_noNA = bcdata::bcdc_get_data("freshwater-atlas-rivers") %>% 
    select(-id) %>% filter(is.na(GNIS_NAME_1)==F)
  
  #Put these water body shapefiles together.
  wbs = lakeshp_noNA %>% mutate(WaterBodyType = "Lake") %>% 
    bind_rows(rivershp_noNA %>% mutate(WaterBodyType = "River"),
              manmadeshp_noNA %>% mutate(WaterBodyType = "Manmade"))
  
  wbs_m = wbs %>% 
    group_by(WATERSHED_,GNIS_NAME_) %>% 
    summarise() %>% 
    rename(WATERSH = WATERSHED_,
           GNIS_NA = GNIS_NAME_)
  
  sf::write_sf(wbs_m,paste0(my.external.data.folder,"summarized_bc_waterbodies_same_gnis_joined.shp"))
}

## We now have a layer of lake/river/manmade waterbody polygons for BC.
## We also have a bunch of watercraft inspection data with data on
## the destination waterbody (1, maybe 2 and 3) and the closest cities 
## thereof. To help join these tables, it would be helpful to have
## the coordinates of the nearest city reported in the inspection data.

## If we have run this script before, and have this data on file, 
## we'll load it now. If not, the code below will rerun the BC
## geocoder.

if(file.exists(paste0(my.data.folder,"DestinationPlacenames.xlsx"))) {
  DestinationPlacenames = read_excel("DestinationPlacenames.xlsx")
}
if(!file.exists(paste0(my.data.folder,"DestinationPlacenames.xlsx"))) {
  
  #List of all unique place names to which boats are headed.
  DestinationPlacenames = data.frame(names = unique(c(unique(dat$Destination_Waterbody_1_Closest_City),
                                                      unique(dat$Destination_Waterbody_2_Closest_City),
                                                      unique(dat$Destination_Waterbody_3_Closest_City),
                                                      unique(dat$Destination_Major_City))))
  
  #Create new variables that we will fill with the loop below.
  DestinationPlacenames$lon = 0
  DestinationPlacenames$lat = 0
  
  #This loop uses the BC geocoder to find the most likely coordinates
  # for each of the unique place names.
  for(i in 1:nrow(DestinationPlacenames)){
    # Pull out place name.
    my.name = DestinationPlacenames[i,]$names
    #Clean up names. Remove anything in brackets.
    my.name = str_remove_all(my.name, " \\(.*\\)")
    #Add spaces to names.
    my.name = str_replace_all(my.name, " ", "%20")
    
    url = paste0('https://geocoder.api.gov.bc.ca/addresses.json?addressString=',
                 my.name,'&maxResults=1&outputSRS=4326')
    
    my.coords = fromJSON(url)$features$geometry %>% 
      summarise(lon = str_extract(coordinates, "(?<=c\\().*(?=\\,)"),
                lat = str_extract(coordinates, "(?<=\\,).*(?=\\))"))
    
    DestinationPlacenames[i,]$lon = my.coords$lon
    DestinationPlacenames[i,]$lat = my.coords$lat
  }
  
  openxlsx::write.xlsx(DestinationPlacenames,
                       paste0(my.data.folder,"DestinationPlacenames.xlsx"),
                       overwrite = T)
}

# # This is a good stage to write out cleaned data out to disk.
# #Write out data...
if(write.out.excel == T){
  openxlsx::write.xlsx(dat,
                       paste0(my.external.output.folder,"IMDP_Cleaned_Inspections.xlsx"),
                       overwrite = T)
  
  #Read data back in... add a metadata sheet.
  my.wb = openxlsx::loadWorkbook(paste0(my.external.output.folder,"IMDP_Cleaned_Inspections.xlsx"))
  openxlsx::addWorksheet(wb = my.wb, sheetName = "Metadata")
  openxlsx::writeDataTable(wb = my.wb, sheet = "Metadata", 
                           x = data.frame(Variable = c("Entire Sheet","``","``"),
                                          Provenance = c(paste0("This excel sheet was made on ",Sys.Date(),", using the `Cleaning Past Watercraft Inspection Data` R script, in my LocalRWork folder."),
                                                         "The script takes the excel file `WatercraftInspectionData_AllYears_PreCleaning.xlsx`, cleans it of boats headed to ocean, local BC boats, etc.,",
                                                         "and then writes it out. To update these data, download the current year's inspection records and use the `Combining Past Watercraft Inspection Data` R script.")),
                           withFilter = F)
  openxlsx::saveWorkbook(my.wb, 
                         paste0(my.external.output.folder,"IMDP_Cleaned_Inspections.xlsx"),
                         overwrite = T)
}

# Expand out the 3 possible destination waterbodies so that
# we can match each of the 3 to their waterbodies.
dat_join = dat %>% 
  rename(GNIS_NAME_ = Destination_Waterbody_1_Name,
         Closest_City = Destination_Waterbody_1_Closest_City) %>% 
  bind_rows(dat %>% 
              mutate(GNIS_NAME_ = Destination_Waterbody_2_Name,
                     Closest_City = Destination_Waterbody_2_Closest_City) %>% 
              filter(is.na(GNIS_NAME_)==F)) %>% 
  bind_rows(dat %>% 
              mutate(GNIS_NAME_ = Destination_Waterbody_3_Name,
                     Closest_City = Destination_Waterbody_3_Closest_City) %>% 
              filter(is.na(GNIS_NAME_)==F)) %>% 
  select(Year,Watercraft_Risk_Assessment_ID, GNIS_NAME_, Closest_City,
         Simple_Counter, Complex_Counter, Very_Complex_Counter, Non_Motorized_Counter) %>% 
  rename(GNIS_NA = GNIS_NAME_) %>% 
  mutate(GNIS_NA = replace_na(GNIS_NA, "NA")) %>% 
  arrange(desc(Watercraft_Risk_Assessment_ID))

# Add the coordinates of the closest city that we got using the 
# BC geocoder. These coordinates will be used in the case of a record
# matching names with multiple lakes.
dat_join = dat_join %>% 
  filter(GNIS_NA != "NA") %>% 
  left_join(DestinationPlacenames %>% 
              rename(Closest_City = names))

#Here ends the "Pre-join work"

if(file.exists(paste0(my.external.output.folder,"InspectionSummaries_GNISNA_WATERSH.xlsx"))){
  results_table = read_excel("InspectionSummaries_GNISNA_WATERSH.xlsx")
}

if(!file.exists(paste0(my.external.output.folder,"InspectionSummaries_GNISNA_WATERSH.xlsx"))) {
  
  #The code below is my as-of-yet unsuccessful attempt to write some code that would be much faster joining inspection data to waterbodies...
  # watersheds = read_sf(paste0(my.external.data.folder,"WatershedGroups_LowRes.shp"))
  # # 
  # #Add subwatershed numbers for each inspection, based on the inspection's lat and long.
  # dat_join = dat_join %>%
  #   filter(!is.na(lon)) %>%
  #   mutate(lon = as.numeric(lon),
  #          lat = as.numeric(lat)) %>%
  #   st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
  #   st_join(watersheds %>%
  #             select(WATERSHED_) %>%
  #             rename(watershed = WATERSHED_) %>%
  #             st_transform(crs = 4326), st_intersects)
  # 
  # #Some of our records' info for closest city was imperfect - add in subwatershed IDs
  # # for such records. There may be double matches...
  # leftover_wbs = dat_join %>% 
  #   #Zoom in on records for which we weren't able to extract subwatershed based on intersection with coordinates...
  #   filter(is.na(watershed)) %>% 
  #   st_drop_geometry() %>% 
  #   distinct(GNIS_NA) %>% 
  #   pull(GNIS_NA)
  #   
  # leftover_lakes = bcdc_query_geodata('freshwater-atlas-lakes') %>% 
  #   filter(GNIS_NAME_1 %in% leftover_wbs) %>% 
  #   collect() %>% 
  #   group_by(GNIS_NAME_1) %>% 
  #   slice_max(order_by = AREA_HA)
  # 
  # leftover_rivers = bcdc_query_geodata('freshwater-atlas-rivers') %>% 
  #   filter(GNIS_NAME_1 %in% leftover_wbs) %>% 
  #   collect() %>% 
  #   group_by(GNIS_NAME_1) %>% 
  #   slice_max(order_by = AREA_HA)
  # 
  # dat_join = dat_join %>% 
  #   left_join(
  #     bind_rows(leftover_lakes, leftover_rivers) %>% 
  #       st_drop_geometry() %>% 
  #       rename(leftover_watershed = WATERSHED_GROUP_ID,
  #              GNIS_NA = GNIS_NAME_1) %>% 
  #       select(leftover_watershed, GNIS_NA)
  #   ) %>% 
  #   mutate(watershed = case_when(
  #     !is.na(watershed) ~ as.character(watershed),
  #     is.na(watershed) ~ as.character(leftover_watershed)))
  # 
  # wbs_m %>% 
  #   rename(watershed = WATERSH) %>% 
  #   mutate(watershed = as.character(watershed)) %>% 
  #   inner_join(dat_join %>% st_drop_geometry()) %>% 
  #   select(-leftover_watershed) %>% 
  #   st_drop_geometry() %>% 
  #   group_by(watershed,GNIS_NA) %>%
  #   summarise(across(ends_with("_Counter"), sum, na.rm=T)) %>%
  #   ungroup()
  
  #Do the join! 
  for(i in 1:nrow(wbs_m)){
    print(paste0("Working on waterbody #",i))
    
    #This round of the loop's GNIS_NAME.
    wb_name = wbs_m[i,]$GNIS_NA
    
    #Get the chunk of the inspection data for a given GNIS_NAME
    dat_piece = dat_join %>% filter(GNIS_NA == wb_name)
    
    #Add fields for mussel-fouled or high-risk.
    dat_piece = dat_piece %>% 
      mutate(msfouled = case_when(
        paste0(Year,Watercraft_Risk_Assessment_ID) %in% 
          paste0(musselfouled_dat$Year,
                 musselfouled_dat$Watercraft_Risk_Assessment_ID) ~ T,
        T ~ F
      ),
      highrisk = case_when(
        paste0(Year,Watercraft_Risk_Assessment_ID) %in% 
          paste0(highrisk_dat$Year,
                 highrisk_dat$Watercraft_Risk_Assessment_ID) ~ T,
        T ~ F
      )
      )
    
    #This is the potential list of waterbodies with a given name.
    list_of_wbs_m = wbs_m[wbs_m$GNIS_NA == wb_name,]
    
    #If any of the records in the dat piece couldn't find coordinates for the
    #nearest city, we have to drop them.
    dat_piece = dat_piece %>% filter(!is.na(lat),
                         !is.na(lon))
    
    #Spatialize the inspection data chunk for the same GNIS wb name.
    dat_piece_sf = st_as_sf(dat_piece,
                            coords = c("lon","lat"),
                            crs = 4326) %>% 
      st_transform(crs = raster::crs(list_of_wbs_m))
    
    #Find the subwatershed of the nearest identically named
    #waterbody to the closest city of each inspection.
    dat_piece_sf = st_join(dat_piece_sf, list_of_wbs_m, 
                           join = st_nearest_feature) %>% 
      select(-GNIS_NA.y) %>% rename(GNIS_NA = GNIS_NA.x)
    
    #Summarise to subwatershed, waterbody name.
    join_piece = dat_piece_sf %>% 
      st_drop_geometry() %>% 
      group_by(WATERSH,GNIS_NA,Year) %>% 
      summarise(TotalInspections = n()) %>% 
      full_join(
        dat_piece_sf %>%
          st_drop_geometry() %>% 
          filter(paste0(Year,Watercraft_Risk_Assessment_ID) %in% 
                   paste0(musselfouled_dat$Year,
                          musselfouled_dat$Watercraft_Risk_Assessment_ID)) %>% 
          group_by(WATERSH,GNIS_NA,Year) %>% 
          summarise(NumberMusselFouled = n())
      ) %>% 
      full_join(
        dat_piece_sf %>%
          st_drop_geometry() %>% 
          filter(paste0(Year,Watercraft_Risk_Assessment_ID) %in% 
                   paste0(highrisk_dat$Year,
                          highrisk_dat$Watercraft_Risk_Assessment_ID)) %>% 
          group_by(WATERSH,GNIS_NA,Year) %>% 
          summarise(NumberHighRisk = n())
      ) %>% 
      #All inspections, split into 4 categories of boat 'complexity'
      full_join(
        dat_piece_sf %>%
          st_drop_geometry() %>% 
          mutate(across(ends_with("Counter"), as.numeric)) %>% 
          filter(!paste0(Year,Watercraft_Risk_Assessment_ID) %in% 
                   paste0(highrisk_dat$Year,
                          highrisk_dat$Watercraft_Risk_Assessment_ID),
                 !paste0(Year,Watercraft_Risk_Assessment_ID) %in% 
                   paste0(musselfouled_dat$Year,
                          musselfouled_dat$Watercraft_Risk_Assessment_ID)) %>% 
          pivot_longer(ends_with("Counter"),
                       names_to = "BoatType", values_to = "BoatTypeNumber") %>% 
          mutate(BoatTypeNumber = replace(BoatTypeNumber, BoatTypeNumber > 1, 1)) %>% 
          filter(BoatTypeNumber > 0) %>% 
          group_by(WATERSH,GNIS_NA,Year,BoatType) %>% 
          summarise(TotalInsp = n()) %>% 
          pivot_wider(names_from = BoatType, values_from = TotalInsp,
                      names_prefix = "LowRisk_", values_fill = 0)
      ) %>% 
      #High-risk inspections, split into 4 categories of boat 'complexity'
      full_join(
        dat_piece_sf %>%
          st_drop_geometry() %>% 
          mutate(across(ends_with("Counter"), as.numeric)) %>% 
          filter(paste0(Year,Watercraft_Risk_Assessment_ID) %in% 
                   paste0(highrisk_dat$Year,
                          highrisk_dat$Watercraft_Risk_Assessment_ID)) %>% 
          pivot_longer(ends_with("Counter"),
                       names_to = "BoatType", values_to = "BoatTypeNumber") %>% 
          mutate(BoatTypeNumber = replace(BoatTypeNumber, BoatTypeNumber > 1, 1)) %>% 
          filter(BoatTypeNumber > 0) %>% 
          group_by(WATERSH,GNIS_NA,Year,BoatType) %>% 
          summarise(NumberHighRisk = n()) %>% 
          pivot_wider(names_from = BoatType, values_from = NumberHighRisk,
                      names_prefix = "HR_", values_fill = 0)
      ) %>% distinct()
    
    if(i == 1){
      results_table = join_piece
    }
    if(i > 1){
      results_table = rbind(results_table, join_piece)
    }
  }
  
  #Make a backup - just in case!
  results_table_backup = results_table
  
  results_table = results_table %>% ungroup() %>% distinct()
  
  #Replace NAs with 0.
  results_table = results_table %>% 
    mutate(across(c(TotalInspections:HR_Simple_Counter), ~replace_na(.x, 0)))
  
  #Check totals for MF (and HR?)
  results_table %>% 
    group_by(Year) %>% 
    summarise(TotalMF = sum(NumberMusselFouled,na.rm=T))
  
  results_table %>% 
    summarise(TotalMF = sum(NumberMusselFouled,na.rm=T))
  
  #Write out these by-year results.
  openxlsx::write.xlsx(results_table,
                       paste0(my.data.folder,"InspectionSummaries_GNISNA_WATERSH_Year_with_2022.xlsx"),
                       overwrite = T)
  
  #Summarise by waterbody name and subwatershed (drop year grouping variable)
  results_table = results_table %>% 
    group_by(GNIS_NA,WATERSH) %>% 
    summarise(across(c(TotalInspections:HR_Simple_Counter), ~sum(.x,na.rm=T)))
  
  #Write out results to computer.
  openxlsx::write.xlsx(results_table,
                       paste0(my.data.folder,"InspectionSummaries_GNISNA_WATERSH_with_2022.xlsx"),
                       overwrite = T)
}

left_join0 <- function(x, y, fill = 0L, ...){
  z <- left_join(x, y, ...)
  new_cols <- setdiff(names(z), names(x))
  z <- replace_na(z, setNames(as.list(rep(fill, length(new_cols))), new_cols))
  z
}

lakes = wbs_m %>% 
  left_join0(results_table)

lakes = lakes %>% filter(TotalInspections > 0)

#How do the summaries compare to the numbers we had before joining
#the inspection data to the waterbody polygons? 
filter_tracker = filter_tracker %>% 
  add_row(Step = "Joined inspection data to waterbody polygons",
          Variable = "spatial join using name of waterbody, closest city",
          TotalInspection = sum(lakes$TotalInspections),
          HighRisk = sum(lakes$NumberHighRisk),
          MusselFouled = sum(lakes$NumberMusselFouled))

openxlsx::write.xlsx(filter_tracker,
                     "Inspection_filter_tracker.xlsx",
                     overwrite = T)

write_sf(lakes, paste0(my_opts$remote_spatial_data,"Projects/ZQMussels/data/Waterbodies_with_Inspection_Data_Summaries.gpkg"))
