### This script cleans (if necessary) and combines watercraft inspection data
### from 2015 - whatever year you choose. The script assumes
### that data from 2020 onwards are coming from Metabase, i.e. are 
### in that format (about 100 columns in a .csv file). 

### The data files read in at the start of the script are located in the following folder:
### I:/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years.
### The file(s) produced are located in the Operations folder of the Zebra-Quagga Mussel folder 
### of the BC Public Services' I: drive.

#First, load in libraries we will use.
library(readxl)
library(sf)
library(tidyverse)
library(sp)
library(lubridate)
library(openxlsx)

rm(list=ls())
gc()

setwd("C:/Users/CMADSEN/Downloads/LocalR/ZQMussels/")

#Read in options
my_opts = read_csv("Options.csv") %>% 
  as.data.frame()

#Set working directory.
#Directory:
setwd(paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years"))

#Load in the 7 excel sheets.
dat15 = read_excel("2015 watercraft inspection data clean.xlsx", sheet = "All inspections", 
                   skip = 1, col_types = "text")
dat16 = read_excel("2016 watercraft inspection data clean.xlsx", sheet = "All inspections", 
                             skip = 1, col_types = "text")
dat17 = read_excel("2017 watercraft inspection data clean.xlsx", sheet = "All inspections", 
                           skip = 1, col_types = "text")
dat18 = read_excel("2018 watercraft inspection data clean.xlsx", sheet = "All inspections", 
                           skip = 1, col_types = "text")
dat19 = read_excel("2019 watercraft inspection data clean.xlsx", sheet = "All inspections", 
                           skip = 1, col_types = "text")

#Look for all excel files in our data folder that follow the pattern "metabase_year.csv"
files.to.read = list.files(pattern = "metabase_")
#files.to.read = files.to.read[files.to.read != "metabase_2021_backup.csv"]

#Read in each metabase csv file and put them all together in a list.
metabase_data_list = lapply(files.to.read, read_csv, col_types = cols(.default = "c"))

#Add names to each element (i.e. dataframe) in this list.
metabase_data_list = setNames(metabase_data_list, 2020:(2020+length(metabase_data_list)-1))

metabase_dat = metabase_data_list %>% 
  bind_rows(.id = "Year") %>% 
  mutate(Year = as.numeric(Year))

rm(metabase_data_list)

#Quick check: are the mussel-fouled inspections in that tab the only inspections
#in the 'all records' tab that are flagged as mussel-fouled?
dat15_mf = read_excel("2015 watercraft inspection data clean.xlsx", sheet = "Mussel Fouled", 
                           skip = 1, col_types = "text")
dat16_mf = read_excel("2016 watercraft inspection data clean.xlsx", sheet = "Mussel Fouled", 
                           skip = 1, col_types = "text")
dat17_mf = read_excel("2017 watercraft inspection data clean.xlsx", sheet = "Mussel Fouled", 
                           skip = 1, col_types = "text")
dat18_mf = read_excel("2018 watercraft inspection data clean.xlsx", sheet = "Mussel fouled", 
                           skip = 1, col_types = "text")
dat19_mf = read_xlsx("2019 watercraft inspection data clean.xlsx", sheet = 5, 
                           skip = 1, col_types = "text")

dat20_mf = read_xlsx(paste0(my_opts$zqm_operations_data_folder,
                            "Watercraft Inspection Data/2020 data/Mussel Fouled boats filtered from raw flat file.xlsx")) %>% 
  slice(1:16) %>% 
  setNames(snakecase::to_snake_case(colnames(.)))

dat21_mf = read_xlsx(paste0(my_opts$zqm_operations_data_folder,
                            "Watercraft Inspection Data/2021 data/Mussel fouled boats tracking sheet 2021-08-20.xlsx")) %>% 
  slice(1:17) %>% 
  setNames(snakecase::to_snake_case(colnames(.)))

dat22_mf = read_xlsx(paste0(my_opts$zqm_operations_data_folder,
                            "Watercraft Inspection Data/2022 data/2022 mussel fouled boats tracking sheet.xlsx")) %>% 
  slice(1:14) %>% 
  setNames(snakecase::to_snake_case(colnames(.))) %>% 
  filter(!is.na(watercraft_risk_assessment_id))


#Quick fix for date/time columns in some of the years.
dat15 = dat15 %>%  
  mutate(TimeOfInspection = coalesce(`Time of High Risk Inspection`,
                                     `Timestamp of inspection`,
                                     `Shift Start Time`)) %>% 
  #Some date/times are recorded in the excel format still (looks like this: "42577.02391").
  #This line converts that to a readable format (e.g. "2016-07-28 10:53:00)"
  mutate(TimeOfInspection = ifelse(str_detect(TimeOfInspection, "^[0-9]{5}"),
                                   as.character(convertToDateTime(TimeOfInspection)),
                                   TimeOfInspection)) %>% 
  #Sometimes records are missing the seconds column. These 2 lines add in 00 for seconds.
  mutate(TimeOfInspection = str_replace_all(TimeOfInspection, " ([0-9])(:[0-9]{2})$", " 0\\1\\2:00")) %>% 
  mutate(TimeOfInspection = str_replace_all(TimeOfInspection, " ([0-9]{2})(:[0-9]{2})$", " \\1\\2:00")) %>% 
  mutate(TimeOfInspection = lubridate::ymd_hms(TimeOfInspection)) %>% 
  mutate(across(c(`Time of High Risk Inspection`,`Timestamp of inspection`,
         `Shift Start Time`,`Shift End Time`), ~as_datetime(.x)))

dat16 = dat16 %>%   
  #A few of the timestamps quote 1970 as their year of entry! Crazy. Replace with 
  #Shift start time.
  mutate(`Timestamp of inspection` = case_when(
    str_extract(`Timestamp of inspection`,"^[0-9]{4}") != "2016" ~ `Shift Start Time`,
    T ~ `Timestamp of inspection`)) %>% 
  mutate(TimeOfInspection = coalesce(`Date + Time of High Risk Inspection`,
                                     `Timestamp of inspection`, 
                                     `Shift Start Time`)) %>% 
  #Some date/times are recorded in the excel format still (looks like this: "42577.02391").
  #This line converts that to a readable format (e.g. "2016-07-28 10:53:00)"
  mutate(TimeOfInspection = ifelse(str_detect(TimeOfInspection, "^[0-9]{5}"),
                                   as.character(convertToDateTime(TimeOfInspection)),
                                   TimeOfInspection)) %>% 
  #Sometimes records are missing the seconds column. These 2 lines add in 00 for seconds.
  mutate(TimeOfInspection = str_replace_all(TimeOfInspection, " ([0-9])(:[0-9]{2})$", " 0\\1\\2:00")) %>% 
  mutate(TimeOfInspection = str_replace_all(TimeOfInspection, " ([0-9]{2})(:[0-9]{2})$", " \\1\\2:00")) %>% 
  mutate(TimeOfInspection = ymd_hms(TimeOfInspection)) %>% 
  mutate(across(c(`Date + Time of High Risk Inspection`,`Timestamp of inspection`,
                  `Shift Start Time`,`Shift End Time`), ~as_datetime(.x))) %>% 
  mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`),origin = "1899-12-30")) %>% 
  filter(year(TimeOfInspection) == 2016)

#When we have time of inspection (as opposed to just raw_timestamp), use that instead of raw timestamp.
#We'll need to combine the hour/minute/second info from time of inspection with the date info from raw timestamp.
dat17 = dat17 %>% 
  rename(time_of_insp = `Time of inspection`,
         shift_start = `Shift Start Time`) %>% 
  #If the raw timestamp field is blank, coalesce with shift start.
  #Convert data types into date-times and periods.
  mutate(Timestamp = convertToDateTime(coalesce(Timestamp, shift_start))) %>% 
  mutate(time_of_insp = hms(time_of_insp)) %>% 
  #If the hour field was 24, it got changed to 0 and automatically dropped. Add it back in.
  mutate(time_of_insp = case_when(
    hour(time_of_insp) == 0 ~ period(paste0("24H ", as.character(time_of_insp))),
    T ~ time_of_insp)) %>% 
  #If the Time of Inspection field is totally blank, stick to raw timestamp. Best we can do.
  mutate(Timestamp = case_when(
    !is.na(time_of_insp) ~ ymd_hms(paste(date(Timestamp), time_of_insp)),
    T ~ Timestamp
  )) %>% 
  rename(TimeOfInspection = Timestamp) %>% 
  select(-time_of_insp) %>% 
  mutate(across(c(`Date + Time of Inspection`,
                  shift_start,`Shift End Time`), ~convertToDateTime(as.numeric(.x)))) %>% 
  mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`),origin = "1899-12-30"))

dat18 = dat18 %>% 
  rename(time_of_insp = `Time of Inspection`,
         shift_start = `Shift Start Time`) %>% 
  #If the raw timestamp field is blank, coalesce with shift start.
  #Convert data types into date-times and periods.
  mutate(Timestamp = convertToDateTime(coalesce(Timestamp, shift_start))) %>% 
  mutate(time_of_insp = convertToDateTime(time_of_insp)) %>% 
  mutate(time_of_insp = hms(str_extract(as.character(time_of_insp), "(?<= ).*"))) %>% 
  #If the hour field was 24, it got changed to 0 and automatically dropped. Add it back in.
  mutate(time_of_insp = case_when(
    hour(time_of_insp) == 0 ~ period(paste0("24H ", as.character(time_of_insp))),
    T ~ time_of_insp)) %>% 
  #If the Time of Inspection field is totally blank, stick to raw timestamp. Best we can do.
  mutate(Timestamp = case_when(
    !is.na(time_of_insp) ~ ymd_hms(paste(date(Timestamp), time_of_insp)),
    T ~ Timestamp
  )) %>% 
  rename(TimeOfInspection = Timestamp) %>% 
  select(-time_of_insp) %>% 
  mutate(across(c(shift_start,`Shift End Time`), ~convertToDateTime(as.numeric(.x)))) %>% 
  mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`),origin = "1899-12-30"))

dat19 = dat19 %>% 
  rename(time_of_insp = `Time of Inspection`,
         shift_start = `Shift Start Time`) %>% 
  #If the raw timestamp field is blank, coalesce with shift start.
  #Convert data types into date-times and periods.
  mutate(Timestamp = convertToDateTime(coalesce(Timestamp, shift_start))) %>% 
  mutate(time_of_insp = convertToDateTime(time_of_insp)) %>% 
  mutate(time_of_insp = hms(str_extract(as.character(time_of_insp), "(?<= ).*"))) %>%
  #If the hour field was 24, it got changed to 0 and automatically dropped. Add it back in.
  mutate(time_of_insp = case_when(
    hour(time_of_insp) == 0 ~ period(paste0("24H ", as.character(time_of_insp))),
    T ~ time_of_insp)) %>% 
  #If the Time of Inspection field is totally blank, stick to raw timestamp. Best we can do.
  mutate(Timestamp = case_when(
    !is.na(time_of_insp) ~ ymd_hms(paste(date(Timestamp), time_of_insp)),
    T ~ Timestamp
  )) %>% 
  rename(TimeOfInspection = Timestamp) %>% 
  select(-time_of_insp) %>% 
  mutate(across(c(shift_start,`Shift End Time`), ~convertToDateTime(as.numeric(.x)))) %>% 
  mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`),origin = "1899-12-30"))

#Make same corrections to each metabase dataset.
metabase_dat = metabase_dat %>% 
  mutate(across(c(`Start Time`,`End Time`,raw_timestamp),
                ~as_datetime(str_replace(.x, "T", "-")))) %>% 
  mutate(`Inspection Date` = format(raw_timestamp, "%Y-%m-%d"))

# for(i in 1:length(metabase_data_list)){
#   metabase_data_list[[i]] = metabase_data_list[[i]] %>% 
#     mutate(across(c(`Start Time`,`End Time`,raw_timestamp),
#                   ~as_datetime(str_replace(.x, "T", "-")))) %>% 
#     mutate(`Inspection Date` = format(raw_timestamp, "%Y-%m-%d"))
# }

#Custom Functions
#Write a function that removes spaces in column names so that they are easier
#to work with.
ExcelNameImprover = function(x){
  names = colnames(x)
  names = str_replace_all(names," ","_")
  names = str_replace_all(names,"\\/","_")
  names = str_remove_all(names, "\\?")
  names = str_remove_all(names, "\\#")
  colnames(x) <- names
  x
}

dat15 = ExcelNameImprover(dat15)
dat16 = ExcelNameImprover(dat16)
dat17 = ExcelNameImprover(dat17)
dat18 = ExcelNameImprover(dat18)
dat19 = ExcelNameImprover(dat19)
metabase_dat = ExcelNameImprover(metabase_dat)

# #dat20 and dat21 (and maybe later years?) 
# #have a typo where AIS is written Ais. Find the column and fix it.
# metabase_dat = metabase_dat %>% 
#   setNames(str_replace(colnames(metabase_dat),"Ais","AIS"))

#Make a function that replaces long form names with the 2-letter abbreviations.
name_shortner = function(x,y) {
            if(str_detect(x[,y], "Ontario")) {x[,y] = "ON"}
            if(str_detect(x[,y], "British")) {x[,y] = "BC"}
            if(str_detect(x[,y], "Alberta")) {x[,y] = "AB"}
            if(str_detect(x[,y], "Indiana")) {x[,y] = "IN"}
            if(str_detect(x[,y], "Manitoba")) {x[,y] = "MB"}
            if(str_detect(x[,y], "Washington")) {x[,y] = "WA"}
            if(str_detect(x[,y], "Califor")) {x[,y] = "CA"}
            if(str_detect(x[,y], "Sask")) {x[,y] = "SK"}
            if(str_detect(x[,y], "Oregon")) {x[,y] = "OR"}
            if(str_detect(x[,y], "Nova")) {x[,y] = "NS"}
            if(str_detect(x[,y], "Montana")) {x[,y] = "MT"}
            if(str_detect(x[,y], "Idaho")) {x[,y] = "ID"}
            if(str_detect(x[,y], "Nebraska")) {x[,y] = "NE"}
            if(str_detect(x[,y], "Colorado")) {x[,y] = "CO"}
            if(str_detect(x[,y], "Minnesota")) {x[,y] = "MN"}
            if(str_detect(x[,y], "Texas")) {x[,y] = "TX"}
            if(str_detect(x[,y], "Alaska")) {x[,y] = "AK"}
            if(str_detect(x[,y], "New York")) {x[,y] = "NY"}
            if(str_detect(x[,y], "North Dakota")) {x[,y] = "ND"}
            if(str_detect(x[,y], "Tennessee")) {x[,y] = "TN"}
            if(str_detect(x[,y], "Michigan")) {x[,y] = "MI"}
            if(str_detect(x[,y], "Nevada")) {x[,y] = "NV"}
            if(str_detect(x[,y], "Florida")) {x[,y] = "FL"}
            if(str_detect(x[,y], "Quebec")) {x[,y] = "QC"}
            if(str_detect(x[,y], "South Dakota")) {x[,y] = "SD"}
            if(str_detect(x[,y], "West Virginia")) {x[,y] = "WV"}
            if(str_detect(x[,y], "Arizona")) {x[,y] = "AZ"}
            if(str_detect(x[,y], "Wyoming")) {x[,y] = "WY"}
  x
}


#Shorten home province/state into 2-letter codes.
dat15 = name_shortner(dat15,y="State_Province_of_Boat_Residence")
dat16 = name_shortner(dat16,y="State_Province_of_Boat_Residence")
dat17 = name_shortner(dat17,y="State_Province_of_Boat_Residence")
dat18 = name_shortner(dat18,y="State_Province_of_Boat_Residence")
dat19 = name_shortner(dat19,y="State_Province_of_Boat_Residence")

#Shorten destination province/state into 2-letter codes.
dat15 = name_shortner(dat15,y="Destination_Province_State")
dat16 = name_shortner(dat16,y="Province_State_of_Destination_Waterbody")
dat17 = name_shortner(dat17,y="Province_State_of_Destination_Waterbody")
dat18 = name_shortner(dat18,y="State_Province_of_Destination_Waterbody")
dat19 = name_shortner(dat19,y="State_Province_of_Destination_Waterbody")


# Brief aside - Martina asked me to quantify the "Other" destination
# and source water bodies for each year. Also, for metabase.
# other_waterbody_info_for_excel = dat15 %>% 
#   mutate(Year = 2015) %>% 
#   select(Year, Timestamp_of_inspection, Other_Previous_Waterbody_1, 
#          Other_City_State_Province_of_Previous_waterbody_1, 
#          Other_Destination_Waterbody,
#          Other_Closest_City_Destination_Waterbody) %>% 
#   bind_rows(
#     dat16 %>% 
#       mutate(Year = 2016) %>% 
#       select(Year, Timestamp_of_inspection, 
#              Other_waterbody_1, Other_closest_city_to_waterbody_1,
#              Other_destination_waterbody,
#              Other_closest_city_to_destination_waterbody) %>% 
#       rename(Other_Previous_Waterbody_1 = Other_waterbody_1,
#              Other_City_State_Province_of_Previous_waterbody_1 = Other_closest_city_to_waterbody_1,
#              Other_Destination_Waterbody = Other_destination_waterbody,
#              Other_Closest_City_Destination_Waterbody = Other_closest_city_to_destination_waterbody)
#   ) %>% 
#   bind_rows(
#     dat17 %>% 
#       mutate(Year = 2017) %>% 
#       select(Year, TimeOfInspection, Other_waterbody_1, Other_closest_city_to_waterbody_1,
#              Other_destination_waterbody, Other_closest_city_to_destination_waterbody) %>% 
#       rename(Timestamp_of_inspection = TimeOfInspection,
#              Other_Previous_Waterbody_1 = Other_waterbody_1,
#              Other_City_State_Province_of_Previous_waterbody_1 = Other_closest_city_to_waterbody_1,
#              Other_Destination_Waterbody = Other_destination_waterbody,
#              Other_Closest_City_Destination_Waterbody = Other_closest_city_to_destination_waterbody)
#   )  %>% 
#   bind_rows(
#     dat18 %>% 
#       mutate(Year = 2018) %>% 
#       select(Year, TimeOfInspection, Other_Previous_Waterbody_1, Other_Closest_City_Previous_Waterbody_1,
#              Other_Destination_Waterbody_1, Other_Closest_City_to_Destination_Waterbody) %>% 
#       rename(Timestamp_of_inspection = TimeOfInspection,
#              Other_City_State_Province_of_Previous_waterbody_1 = Other_Closest_City_Previous_Waterbody_1,
#              Other_Destination_Waterbody = Other_Destination_Waterbody_1,
#              Other_Closest_City_Destination_Waterbody = Other_Closest_City_to_Destination_Waterbody)
#   ) %>% 
#   bind_rows(
#     dat19 %>% 
#       mutate(Year = 2019) %>% 
#       select(Year, TimeOfInspection, Other_Previous_Waterbody_1, Other_Closest_City_Previous_Waterbody_1,
#              Other_Destination_Waterbody_1, Other_Closest_City_to_Destination_Waterbody) %>% 
#       rename(Timestamp_of_inspection = TimeOfInspection,
#              Other_City_State_Province_of_Previous_waterbody_1 = Other_Closest_City_Previous_Waterbody_1,
#              Other_Destination_Waterbody = Other_Destination_Waterbody_1,
#              Other_Closest_City_Destination_Waterbody = Other_Closest_City_to_Destination_Waterbody)
#   ) %>% 
#   #Combine the water body name with the extra info about nearest city / province etc.
#   mutate(Other_Previous_Waterbody_1 = case_when(
#     !is.na(Other_City_State_Province_of_Previous_waterbody_1) ~ paste0(Other_Previous_Waterbody_1, ";", Other_City_State_Province_of_Previous_waterbody_1),
#     is.na(Other_City_State_Province_of_Previous_waterbody_1) ~ Other_Previous_Waterbody_1
#   )) %>% 
#   mutate(Other_Previous_Waterbody_1 = case_when(
#     !is.na(Other_City_State_Province_of_Previous_waterbody_1) ~ paste0(Other_Previous_Waterbody_1, ";", Other_City_State_Province_of_Previous_waterbody_1),
#     is.na(Other_City_State_Province_of_Previous_waterbody_1) ~ Other_Previous_Waterbody_1
#   )) %>% 
#   pivot_longer(c(Other_Destination_Waterbody, Other_Previous_Waterbody_1)) %>% 
#   #Get rid of empty values
#   filter(!is.na(value)) %>% 
#   #group_by(Year) %>% 
#   count(value) %>% 
#   arrange(desc(n)) %>% 
#   slice(1:100) %>%
#   mutate(value = str_split(value, ";")) %>% 
#   unnest_wider(value) %>% 
#   rename(other_waterbody_name = ...1,
#          location_info = ...2) %>%
#   mutate(location_info = coalesce(location_info, ...3)) %>% 
#   select(-...3)
# 
# other_waterbody_info_for_excel = metabase_dat %>% 
#   select(Year, raw_timestamp, Other_Previous_Waterbody_1, 
#          Other_City_State_Province_of_Previous_waterbody_1, 
#          Other_Destination_Waterbody,
#          Other_Closest_City_Destination_Waterbody) %>% 
#   #Combine the water body name with the extra info about nearest city / province etc.
#   mutate(Other_Previous_Waterbody_1 = case_when(
#     !is.na(Other_City_State_Province_of_Previous_waterbody_1) ~ paste0(Other_Previous_Waterbody_1, ";", Other_City_State_Province_of_Previous_waterbody_1),
#     is.na(Other_City_State_Province_of_Previous_waterbody_1) ~ Other_Previous_Waterbody_1
#   )) %>% 
#   mutate(Other_Previous_Waterbody_1 = case_when(
#     !is.na(Other_City_State_Province_of_Previous_waterbody_1) ~ paste0(Other_Previous_Waterbody_1, ";", Other_City_State_Province_of_Previous_waterbody_1),
#     is.na(Other_City_State_Province_of_Previous_waterbody_1) ~ Other_Previous_Waterbody_1
#   )) %>% 
#   pivot_longer(c(Other_Destination_Waterbody, Other_Previous_Waterbody_1)) %>% 
#   #Get rid of empty values
#   filter(!is.na(value)) %>% 
#   #group_by(Year) %>% 
#   count(value) %>% 
#   arrange(desc(n)) %>% 
#   slice(1:100) %>%
#   mutate(value = str_split(value, ";")) %>% 
#   unnest_wider(value) %>% 
#   rename(other_waterbody_name = ...1,
#          location_info = ...2) %>%
#   mutate(location_info = coalesce(location_info, ...3)) %>% 
#   select(-...3)
# 
# write.xlsx(other_waterbody_info_for_excel,
#            "I:/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/Common 'Other waterbody' names and location info.xlsx", overwrite = T)

#I have to try to homogenize column names before binding the data...
dat15_clean = dat15 %>% 
  summarise(Year = 2015,
            Watercraft_Risk_Assessment_ID = Inspection_ID,
            Station = Station_Name,
            Start_Time = Shift_Start_Time,
            End_Time = Shift_End_Time,
            TimeOfInspection = TimeOfInspection,
            Total_BlowBys = `Blow_By's_(Non-compliant)`,
            Inspection_Date = format(TimeOfInspection,"%Y-%m-%d"),
            Inspection_Hour = str_extract(TimeOfInspection, "[0-9]{2}(?=:[0-9]{2}:[0-9]{2})"),
            Province_Code = Province_State_Previous_Waterbody,
            Country_Code = case_when(Province_Code %in% c("AB","BC","MB",
                                                          "NB","NL","NT",
                                                          "NS","NS","NU",
                                                          "PE","QC","ON",
                                                          "SK","YT") ~ "CAN",
                                     TRUE ~ "USA"),
            WatercraftType = case_when(Watercraft_Type == "Other" ~ Other_Watercraft_Type,
                                       Watercraft_Type != "Other" ~ Watercraft_Type),
            Number_Of_People_In_Party = Number_of_People_Interacted_with,
            Previous_AIS_Knowledge_Ind = Previous_Knowledge_of_CDD_AIS,
            Previous_AIS_Knowledge_Source = case_when(Source_of_Previous_Knowledge_AIS_CDD != "Other" ~ Source_of_Previous_Knowledge_AIS_CDD,
                                                      Source_of_Previous_Knowledge_AIS_CDD == "Other" ~ Other_Source_of_Knowledge),
            Previous_Inspection_Ind = Has_the_watercraft_been_previously_inspected,
            Previous_Inspection_Days_Count = Days_Since_Previous_Inspection,
            Previous_Inspection_Source_Name = Location_of_Previous_Inspection,
            Previous_Waterbody_1_Name = Previous_Waterbody_1,
            Previous_Waterbody_1_Closest_City = case_when(City_State_Province_of_Previous_Waterbody_1 != "Other" ~ str_extract(City_State_Province_of_Previous_Waterbody_1, "[A-Za-z]*(?= )"),
                                                          City_State_Province_of_Previous_Waterbody_1 == "Other" ~ str_extract(Other_City_State_Province_of_Previous_waterbody_1, "\\,.*")),
            Previous_Waterbody_1_Province_Or_State = Province_State_Previous_Waterbody,
            Previous_Waterbody_1_Days_Out_Of_Water = Days_out_of_waterbody_1,
            Previous_Waterbody_2_Name = Previous_Waterbody_2,
            Destination_Waterbody_1_Name = Destination_Waterbody_1,
            Destination_Waterbody_1_Closest_City = str_remove_all(Closest_City_to_Destination_Waterbody,"[A-Z]{2}"),
            Destination_Waterbody_1_Province_Or_State = str_extract(Closest_City_to_Destination_Waterbody,"[A-Z]{2}"),
            Destination_Dry_Storage_Ind = case_when(str_detect(Destination_Waterbody_1, "Dry") ~ "true",
                                                    str_detect(Other_Destination_Waterbody, "Dry") ~ "true",
                                                    TRUE ~ "false"),
            Unknown_Destination_Waterbody_Ind = case_when(str_detect(Destination_Waterbody_1, "Unknown") ~ "true",
                                                          str_detect(Other_Destination_Waterbody, "Unknown") ~ "true",
                                                          TRUE ~ "false"),
            High_Risk_AIS_Ind = case_when(Is_the_watercraft_high_risk_for_Dreissenid_mussels_AIS == "yes" ~ "true",
                                          Is_the_watercraft_high_risk_for_Dreissenid_mussels_AIS == "no" ~ "false"),
            Adult_Dressenidae_Found_Ind = case_when(Reason_for_High_Risk_Inspection2 == "Dreissenid Mussels Found" ~ "true",
                                                    TRUE ~ "false"),
            Standing_Water_Present_Ind = case_when(Reason_for_High_Risk_Inspection2 == "Standing Water" ~ "true",
                                                   TRUE ~ "false"),
            Adult_Dreissenidae_Mussel_Found_Ind = case_when(Reason_for_High_Risk_Inspection2 == "Dreissenid Mussels Found" ~ "true",
                                                            TRUE ~ "false"))

dat16_clean = dat16 %>% 
  summarise(Year = 2016,
            Watercraft_Risk_Assessment_ID = Inspection_ID,
            Station = Station_Name,
            Start_Time = Shift_Start_Time,
            End_Time = Shift_End_Time,
            Shift_Duration = `Shift_Duration_(hrs)`,
            Motorized_Blow_Bys_Counter = `Blow_By's_(Motorized_Boats)`,
            Non_Motorized_Blow_Bys_Counter = `Blow_By's_(Non-Motorized)`,
            TimeOfInspection = TimeOfInspection,
            Inspection_Date = format(Shift_Date,"%Y-%m-%d"),
            Inspection_Hour = str_extract(TimeOfInspection, "[0-9]{2}(?=:[0-9]{2}:[0-9]{2})"),
            Province_Code = State_Province_of_Previous_Waterbody,
            Country_Code = case_when(Province_Code %in% c("AB","BC","MB",
                                                          "NB","NL","NT",
                                                          "NS","NS","NU",
                                                          "PE","QC","ON",
                                                          "SK","YT") ~ "CAN",
                                     TRUE ~ "USA"),
            WatercraftType = case_when(Watercraft_type == "Other" ~ Other_watercraft_type,
                                       Watercraft_type != "Other" ~ Watercraft_type),
            Number_Of_People_In_Party = Number_of_people_in_party,
            Commercially_Hauled_Ind = case_when(Is_the_watercraft_equipment_commercially_hauled == "yes" ~ "true",
                                                TRUE ~ "false"),
            Previous_AIS_Knowledge_Ind = case_when(`Previous_knowledge_of_AIS_or_Clean,_Drain,_Dry` == "yes" ~ "true",
                                                   `Previous_knowledge_of_AIS_or_Clean,_Drain,_Dry` == "no" ~ "false"),
            Previous_AIS_Knowledge_Source = case_when(`Source_of_AIS_Clean,_Drain,_Dry_Knowledge` != "Other" ~ Other_source_of_AIS_knowledge,
                                                      `Source_of_AIS_Clean,_Drain,_Dry_Knowledge` == "Other" ~ Other_source_of_AIS_knowledge),
            Previous_Inspection_Ind = case_when(Previous_inspection == "yes" ~ "true",
                                                Previous_inspection == "no" ~ "false"),
            Previous_Inspection_Days_Count = Number_of_days_since_previous_inspection,
            Previous_Inspection_Source_Name = Location_of_previous_inspection,
            Previous_Waterbody_1_Name = case_when(`Previous_waterbody_(1)` == "Other" ~ Other_waterbody_1,
                                                  `Previous_waterbody_(1)` != "Other" ~ `Previous_waterbody_(1)`),
            Previous_Waterbody_1_Closest_City = case_when(Closest_City_to_Previous_Waterbody_1 == "Other" ~ Other_closest_city_to_waterbody_1,
                                                          Closest_City_to_Previous_Waterbody_1 != "Other" ~ Closest_City_to_Previous_Waterbody_1),
            Previous_Waterbody_1_Province_Or_State = State_Province_of_Previous_Waterbody,
            Previous_Waterbody_1_Days_Out_Of_Water = Number_of_days_out_of_waterbody_1,
            Previous_Waterbody_2_Name = case_when(`Previous_waterbody_(2)` == "Other" ~ Other_waterbody_2,
                                                  `Previous_waterbody_(2)` != "Other" ~ `Previous_waterbody_(2)`),
            Previous_Waterbody_2_Closest_City = Closest_City_to_Previous_Waterbody_2,
            Previous_Waterbody_2_Province_Or_State = State_Province_of_Previous_Waterbody_2,
            Previous_Waterbody_2_Days_Out_Of_Water = Number_of_days_out_of_waterbody_2,
            
            Previous_Dry_Storage_Ind = case_when(str_detect(`Previous_waterbody_(1)`, "Dry Storage") ~ "true",
                                                 TRUE ~ "false"),
            Unknown_Previous_Water_Body_Ind = case_when(str_detect(`Previous_waterbody_(1)`, "Unknown") ~ "true",
                                                        TRUE ~ "false"),
            Destination_Waterbody_1_Name = case_when(Destination_waterbody_1 == "Other" ~ Other_destination_waterbody,
                                                     Destination_waterbody_1 != "Other" ~ Destination_waterbody_1),
            Destination_Waterbody_1_Closest_City = Closest_City_to_Destination_Waterbody,
            Destination_Waterbody_1_Province_Or_State = Province_State_of_Destination_Waterbody,
            Destination_Dry_Storage_Ind = case_when(str_detect(Destination_waterbody_1, "Dry Storage") ~ "true",
                                                    TRUE ~ "false"),
            Unknown_Destination_Waterbody_Ind = case_when(str_detect(Destination_waterbody_1, "Unknown") ~ "true",
                                                          TRUE ~ "false"),
            High_Risk_Area_Ind = case_when(str_detect(Reason_for_Inspection, "contaminated") ~ "true",
                                           TRUE ~ "false"),
            High_Risk_AIS_Ind = case_when(Is_the_watercraft_equipment_high_risk_for_Dreissenid_Mussels_or_AIS == "yes" ~ "true",
                                          Is_the_watercraft_equipment_high_risk_for_Dreissenid_Mussels_or_AIS == "no" ~ "false"),
            Adult_Dressenidae_Found_Ind = case_when(str_detect(Watercraft_Inspection_Outcome, "Dreissenid Mussels Found") ~ "true",
                                                    TRUE ~ "false"),
            Standing_Water_Present_Ind = case_when(str_detect(Watercraft_Inspection_Outcome, "Standing Water") ~ "true",
                                                   TRUE ~ "false"),
            Adult_Dreissenidae_Mussel_Found_Ind = case_when(str_detect(Watercraft_Inspection_Outcome, "Dreissenid Mussels Found") ~ "true",
                                                            TRUE ~ "false"),
            Decontamination_Performed_Ind = case_when(is.na(Decontamination_Order_Reference_Number)==F ~ "true",
                                                      TRUE ~ "false"),
            Decontamination_Reference = Decontamination_Order_Reference_Number,
            Decontamination_order_issued_Ind = case_when(str_detect(Actions_Taken, "Decontamination Order Issued") ~ "true",
                                                         TRUE ~ "false"),
            Seal_Number = Tag_Seal_Number,
            Quarantine_Period_Issued_Ind = case_when(str_detect(Actions_Taken, "30 Day Quarantine Order Issued") ~ "true",
                                                     TRUE ~ "false"))

dat17_clean = dat17 %>% 
  summarise(Year = 2017,
            Watercraft_Risk_Assessment_ID = Inspection_ID,
            Station = Station_Name,
            Start_Time = shift_start,
            End_Time = Shift_End_Time,
            Shift_Duration = `Shift_duration_(hrs)`,
            Motorized_Blow_Bys_Counter = `Blow_By's_(Motorized_Boats)`,
            Non_Motorized_Blow_Bys_Counter = `Blow_By's_(Non-Motorized)`,
            New_Passport_Issued_Ind = Was_a_passport_issued,
            Passport_Holder_Ind = case_when(str_detect(Passport_number, "[0-9]+") ~ "true",
                                            TRUE ~ "false"),
            Passport_Number = Passport_number,
            TimeOfInspection = TimeOfInspection,
            Inspection_Date = format(Shift_Date,"%Y-%m-%d"),
            Inspection_Hour = format(TimeOfInspection, "%H"),
            Province_Code = State_Province_of_Previous_Waterbody,
            Country_Code = case_when(Province_Code %in% c("AB","BC","MB",
                                                          "NB","NL","NT",
                                                          "NS","NS","NU",
                                                          "PE","QC","ON",
                                                          "SK","YT") ~ "CAN",
                                     TRUE ~ "USA"),
            K9_Inspection_Ind = case_when(str_detect(Was_a_K9_inspection_performed, "yes") ~ "true",
                                          TRUE ~ "false"),
            WatercraftType = case_when(Watercraft_type == "Other" ~ Other_watercraft_type,
                                       Watercraft_type != "Other" ~ Watercraft_type),
            Number_Of_People_In_Party = Number_of_people_in_party,
            Commercially_Hauled_Ind = case_when(Is_the_watercraft_equipment_commercially_hauled == "yes" ~ "true",
                                                TRUE ~ "false"),
            Previous_AIS_Knowledge_Ind = case_when(`Previous_knowledge_of_AIS_or_Clean,_Drain,_Dry` == "yes" ~ "true",
                                                   `Previous_knowledge_of_AIS_or_Clean,_Drain,_Dry` == "no" ~ "false"),
            Previous_AIS_Knowledge_Source = case_when(`Source_of_AIS_Clean,_Drain,_Dry_Knowledge` != "Other" ~ Other_source_of_AIS_knowledge,
                                                      `Source_of_AIS_Clean,_Drain,_Dry_Knowledge` == "Other" ~ Other_source_of_AIS_knowledge),
            Previous_Inspection_Ind = case_when(Previous_inspection == "yes" ~ "true",
                                                Previous_inspection == "no" ~ "false"),
            Previous_Inspection_Days_Count = Number_of_days_since_previous_inspection,
            Previous_Inspection_Source_Name = Location_of_previous_inspection,
            Previous_Waterbody_1_Name = case_when(`Previous_waterbody_(1)` == "Other" ~ Other_waterbody_1,
                                                  `Previous_waterbody_(1)` != "Other" ~ `Previous_waterbody_(1)`),
            Previous_Waterbody_1_Closest_City = case_when(Closest_City_to_Previous_Waterbody_1 == "Other" ~ Other_closest_city_to_waterbody_1,
                                                          Closest_City_to_Previous_Waterbody_1 != "Other" ~ Closest_City_to_Previous_Waterbody_1),
            Previous_Waterbody_1_Province_Or_State = State_Province_of_Previous_Waterbody,
            Previous_Waterbody_1_Days_Out_Of_Water = Number_of_days_out_of_waterbody_1,
            Previous_Waterbody_2_Name = case_when(`Previous_waterbody_(2)` == "Other" ~ Other_waterbody_2,
                                                  `Previous_waterbody_(2)` != "Other" ~ `Previous_waterbody_(2)`),
            Previous_Waterbody_2_Closest_City = Closest_City_to_Previous_Waterbody_2,
            Previous_Waterbody_2_Province_Or_State = State_Province_of_Previous_Waterbody_2,
            Previous_Waterbody_2_Days_Out_Of_Water = Number_of_days_out_of_waterbody_2,
            Previous_Dry_Storage_Ind = case_when(str_detect(`Previous_waterbody_(1)`, "Dry Storage") ~ "true",
                                                 TRUE ~ "false"),
            Unknown_Previous_Water_Body_Ind = case_when(str_detect(`Previous_waterbody_(1)`, "Unknown") ~ "true",
                                                        TRUE ~ "false"),
            Destination_Waterbody_1_Name = case_when(Destination_waterbody_1 == "Other" ~ Other_destination_waterbody,
                                                     Destination_waterbody_1 != "Other" ~ Destination_waterbody_1),
            Destination_Waterbody_1_Closest_City = Closest_City_to_Destination_Waterbody,
            Destination_Waterbody_1_Province_Or_State = Province_State_of_Destination_Waterbody,
            Destination_Dry_Storage_Ind = case_when(str_detect(Destination_waterbody_1, "Dry [s,S]torage") ~ "true",
                                                    TRUE ~ "false"),
            Unknown_Destination_Waterbody_Ind = case_when(str_detect(Destination_waterbody_1, "Unknown") ~ "true",
                                                          TRUE ~ "false"),
            Aquatic_Plants_Found_Ind = case_when(Is_the_watercraft_free_of_any_aquatic_plants == "no" ~ "true",
                                                 str_detect(Watercraft_Inspection_Outcome, "Aquatic vegetation found") ~ "true",
                                                 TRUE ~ "false"),
            High_Risk_Area_Ind = case_when(str_detect(Reason_for_High_Risk_Inspection, "contaminated") ~ "true",
                                           TRUE ~ "false"),
            High_Risk_AIS_Ind = case_when(Is_the_watercraft_equipment_high_risk_for_Dreissenid_mussels_or_AIS == "yes" ~ "true",
                                          Is_the_watercraft_equipment_high_risk_for_Dreissenid_mussels_or_AIS == "no" ~ "false"),
            Adult_Dressenidae_Found_Ind = case_when(str_detect(Watercraft_Inspection_Outcome, "Dreissenid mussels and") ~ "true",
                                                    str_detect(Watercraft_Inspection_Outcome, "Dreissenid Mussels Only") ~ "true",
                                                    str_detect(Watercraft_Inspection_Outcome, "No Dreissenid") ~ "false",
                                                    TRUE ~ "false"),
            Standing_Water_Present_Ind = case_when(str_detect(Watercraft_Inspection_Outcome, "and standing water") ~ "true",
                                                   str_detect(Watercraft_Inspection_Outcome, "Standing Water") ~ "true",
                                                   TRUE ~ "false"),
            Adult_Dreissenidae_Mussel_Found_Ind = case_when(str_detect(Watercraft_Inspection_Outcome, "Dreissenid mussels and") ~ "true",
                                                            str_detect(Watercraft_Inspection_Outcome, "Dreissenid Mussels Only") ~ "true",
                                                            str_detect(Watercraft_Inspection_Outcome, "No Dreissenid") ~ "false",
                                                            TRUE ~ "false"),
            Decontamination_Performed_Ind = case_when(Was_a_decontamination_performed == "yes" ~ "true",
                                                      TRUE ~ "false"),
            Decontamination_Reference = Record_of_Decontamination_Reference_Number,
            Decontamination_order_issued_Ind = case_when(Was_a_decontamination_order_issued == "yes" ~ "true",
                                                         TRUE ~ "false"),
            Decontamination_order_number = Decontamination_Order_Reference_Number,
            Seal_Issued_Ind = case_when(Did_you_seal_the_boat == "yes" ~ "true", 
                                        TRUE ~ "false"),
            Seal_Number = Tag_Seal_Number,
            Quarantine_Period_Issued_Ind = case_when(Was_a_quarantine_period_issued == "yes" ~ "true",
                                                     TRUE ~ "false"))

dat18_clean = dat18 %>% 
  summarise(Year = 2018,
            Watercraft_Risk_Assessment_ID = Inspection_ID,
            Station = Station_Name,
            Start_Time = shift_start,
            End_Time = Shift_End_Time,
            Shift_Duration = `Shift_Duration_(Hrs)`,
            Motorized_Blow_Bys_Counter = `Blow_By's_(motorized_boats)`,
            Non_Motorized_Blow_Bys_Counter = `Blow_By's_(Non-motorized)`,
            K9_On_Shift_Ind = K9_On_Shift,
            New_Passport_Issued_Ind = case_when(Was_an_AIS_passport_issued == "yes" ~ "true",
                                                TRUE ~ "false"),
            Passport_Number = AIS_Passport_Number_Issued,
            TimeOfInspection = TimeOfInspection,
            Inspection_Date = format(Shift_Date,"%Y-%m-%d"),
            Inspection_Hour = format(TimeOfInspection, "%H"),
            Province_Code = State_Province_for_Previous_Waterbody_1,
            Country_Code = case_when(Province_Code %in% c("AB","BC","MB",
                                                          "NB","NL","NT",
                                                          "NS","NS","NU",
                                                          "PE","QC","ON",
                                                          "SK","YT") ~ "CAN",
                                     TRUE ~ "USA"),
            K9_Inspection_Ind = case_when(str_detect(K9_Inspection, "yes") ~ "true",
                                          TRUE ~ "false"),
            WatercraftType = case_when(Watercraft_Type == "Other" ~ Other_Watercraft_Type,
                                       Watercraft_Type != "Other" ~ Watercraft_Type),
            Number_Of_People_In_Party = Total_People_Interacted_with,
            Commercially_Hauled_Ind = case_when(Is_the_Watercraft_Commercially_Hauled == "yes" ~ "true",
                                                TRUE ~ "false"),
            Previous_AIS_Knowledge_Ind = case_when(Previous_knowledge_of_CDD_or_AIS == "yes" ~ "true",
                                                   Previous_knowledge_of_CDD_or_AIS == "no" ~ "false"),
            Previous_AIS_Knowledge_Source = case_when(Source_of_Previous_Knowledge_of_CDD_AIS != "Other" ~ Other_Source_of_CDD_AIS_knowledge,
                                                      Source_of_Previous_Knowledge_of_CDD_AIS == "Other" ~ Other_Source_of_CDD_AIS_knowledge),
            Previous_Inspection_Ind = case_when(Was_the_watercraft_previously_inspected == "yes" ~ "true",
                                                Was_the_watercraft_previously_inspected == "no" ~ "false"),
            Previous_Inspection_Found_Dressenidae_Mussels = case_when(Dreissenid_Mussels_Found_on_Previous_Inspection == "yes" ~ "true",
                                                                      TRUE ~ "false"),
            Previous_Inspection_Source_Name = Location_of_Previous_Inspection,
            Previous_Waterbody_1_Name = case_when(Previous_Waterbody_1 == "Other" ~ Other_Previous_Waterbody_1,
                                                  Previous_Waterbody_1 != "Other" ~ Previous_Waterbody_1),
            Previous_Waterbody_1_Closest_City = case_when(Closest_City_to_Previous_Waterbody_1 == "Other" ~ Other_Closest_City_Previous_Waterbody_1,
                                                          Closest_City_to_Previous_Waterbody_1 != "Other" ~ Closest_City_to_Previous_Waterbody_1),
            Previous_Waterbody_1_Province_Or_State = State_Province_for_Previous_Waterbody_1,
            Previous_Waterbody_1_Days_Out_Of_Water = `Days_out_of_water_(previous_waterbody1)`,
            Previous_Waterbody_2_Name = case_when(Previous_Waterbody_2 == "Other" ~ Other_Previous_Waterbody_2,
                                                  Previous_Waterbody_2 != "Other" ~ Previous_Waterbody_2),
            Previous_Waterbody_2_Closest_City = Closest_City_to_Previous_Waterbody_2,
            Previous_Waterbody_2_Province_Or_State = State_Province_for_Previous_Waterbody_2,
            Previous_Waterbody_2_Days_Out_Of_Water = `Day_out_of_water_(previous_waterbody_2)`,
            Previous_Dry_Storage_Ind = case_when(str_detect(Previous_Waterbody_1, "Dry Storage") ~ "true",
                                                 TRUE ~ "false"),
            Unknown_Previous_Water_Body_Ind = case_when(str_detect(Previous_Waterbody_1, "Unknown") ~ "true",
                                                        TRUE ~ "false"),
            Destination_Waterbody_1_Name = case_when(Destination_Waterbody_1 == "Other" ~ Other_Destination_Waterbody_1,
                                                     Destination_Waterbody_1 != "Other" ~ Destination_Waterbody_1),
            Destination_Waterbody_1_Closest_City = Closest_City_to_Destination_Waterbody,
            Destination_Waterbody_1_Province_Or_State = State_Province_of_Destination_Waterbody,
            Destination_Dry_Storage_Ind = case_when(str_detect(Destination_Waterbody_1, "Dry [s,S]torage") ~ "true",
                                                    TRUE ~ "false"),
            Unknown_Destination_Waterbody_Ind = case_when(str_detect(Destination_Waterbody_1, "Unknown") ~ "true",
                                                          TRUE ~ "false"),
            Aquatic_Plants_Found_Ind = case_when(Other_inspection_outcomes == "Aquatic plants found" ~ "true",
                                                 TRUE ~ "false"),
            Marine_Mussel_Found_Ind = case_when(Other_inspection_outcomes == "Marine mussels found" ~ "true",
                                                TRUE ~ "false"),
            High_Risk_AIS_Ind = case_when(Is_the_watercraft_high_risk_for_AIS_Dreissenid_mussels == "yes" ~ "true",
                                          Is_the_watercraft_high_risk_for_AIS_Dreissenid_mussels == "no" ~ "false"),
            Adult_Dressenidae_Found_Ind = case_when(Dreissenid_Mussels_Found...54 == "yes" ~ "true",  
                                                    TRUE ~ "false"),
            Standing_Water_Present_Ind = case_when(Standing_water_found == "yes" ~ "true",
                                                   TRUE ~ "false"),
            Adult_Dreissenidae_Mussel_Found_Ind = case_when(Dreissenid_Mussels_Found...54 == "yes" ~ "true",  
                                                            TRUE ~ "false"),
            Adult_Mussels_Location_Name = Location_of_mussels_or_standing_water_found,
            Decontamination_Performed_Ind = case_when(Decontamination_Performed == "yes" ~ "true",
                                                      TRUE ~ "false"),
            Decontamination_Reference = Record_of_Decontamination_Number,
            Decontamination_order_issued_Ind = case_when(Decontamination_Order_Issued == "yes" ~ "true",
                                                         TRUE ~ "false"),
            Decontamination_order_number = Decontamination_Order_number,
            Seal_Issued_Ind = case_when(Was_a_seal_applied_to_the_watercraft == "yes" ~ "true", 
                                        TRUE ~ "false"),
            Seal_Number = Seal_tag_number,
            Quarantine_Period_Issued_Ind = case_when(Quarantine_period_issued_for_30_day_drying_time == "yes" ~ "true",
                                                     TRUE ~ "false"))

dat19_clean = dat19 %>% 
  #Coalesce columns for watercraft type...
  mutate(Single_watercraft_type = coalesce(Single_watercraft_type,Watercraft_Type_1)) %>% 
  summarise(Year = 2019,
            Watercraft_Risk_Assessment_ID = Inspection_ID,
            Station = Station_Name,
            Start_Time = shift_start,
            End_Time = Shift_End_Time,
            Shift_Duration = `Shift_Duration_(hrs)`,
            Motorized_Blow_Bys_Counter = `Blow_By_(Motorized_Boats)`,
            Non_Motorized_Blow_Bys_Counter = `Blow_By_(Non-Motorized_Boats)`,
            K9_On_Shift_Ind = K9_on_Shift,
            New_Passport_Issued_Ind = case_when(Was_an_AIS_passport_issued == "yes" ~ "true",
                                                TRUE ~ "false"),
            Passport_Number = AIS_Passport_Number_Issued,
            TimeOfInspection = TimeOfInspection,
            Inspection_Date = format(Shift_Date,"%Y-%m-%d"),
            Inspection_Hour = format(TimeOfInspection, "%H"),
            Province_Code = State_Province_for_Previous_Waterbody_1,
            Country_Code = case_when(Province_Code %in% c("AB","BC","MB",
                                                          "NB","NL","NT",
                                                          "NS","NS","NU",
                                                          "PE","QC","ON",
                                                          "SK","YT") ~ "CAN",
                                     TRUE ~ "USA"),
            K9_Inspection_Ind = case_when(str_detect(K9_Inspection, "yes") ~ "true",
                                          TRUE ~ "false"),
            Non_Motorized_Counter = case_when(Single_watercraft_type == "Non-motorized / hand launched watercraft" ~ Number_watercraft,TRUE ~ "0"),
            Simple_Counter = case_when(Single_watercraft_type == "Simple watercraft" ~ Number_watercraft, TRUE ~ "0"),
            Complex_Counter = case_when(Single_watercraft_type == "Complex watercraft" ~ Number_watercraft, TRUE ~ "0"),
            Very_Complex_Counter = case_when(Single_watercraft_type == "Very complex watercraft" ~ Number_watercraft, TRUE ~ "0"),
            Number_Of_People_In_Party = Total_People_Interacted_with,
            Commercially_Hauled_Ind = case_when(Is_the_Watercraft_Commercially_Hauled == "yes" ~ "true",
                                                TRUE ~ "false"),
            Previous_AIS_Knowledge_Ind = case_when(Previous_knowledge_of_CDD_or_AIS == "yes" ~ "true",
                                                   Previous_knowledge_of_CDD_or_AIS == "no" ~ "false"),
            Previous_AIS_Knowledge_Source = case_when(Source_of_Previous_Knowledge_of_CDD_AIS != "Other" ~ Other_Source_of_CDD_AIS_knowledge,
                                                      Source_of_Previous_Knowledge_of_CDD_AIS == "Other" ~ Other_Source_of_CDD_AIS_knowledge),
            Previous_Inspection_Ind = case_when(Was_the_watercraft_previously_inspected == "yes" ~ "true",
                                                Was_the_watercraft_previously_inspected == "no" ~ "false"),
            Previous_Inspection_Found_Dressenidae_Mussels = case_when(Dreissenid_Mussels_Found_on_Previous_Inspection == "yes" ~ "true",
                                                                      TRUE ~ "false"),
            Previous_Inspection_Source_Name = Location_of_Previous_Inspection,
            Previous_Waterbody_1_Name = case_when(Previous_Waterbody_1 == "Other" ~ Other_Previous_Waterbody_1,
                                                  Previous_Waterbody_1 != "Other" ~ Previous_Waterbody_1),
            Previous_Waterbody_1_Closest_City = case_when(Closest_City_to_Previous_Waterbody_1 == "Other" ~ Other_Closest_City_Previous_Waterbody_1,
                                                          Closest_City_to_Previous_Waterbody_1 != "Other" ~ Closest_City_to_Previous_Waterbody_1),
            Previous_Waterbody_1_Province_Or_State = State_Province_for_Previous_Waterbody_1,
            Previous_Waterbody_1_Days_Out_Of_Water = `Days_out_of_water_(previous_waterbody1)`,
            Previous_Waterbody_2_Name = case_when(Previous_Waterbody_2 == "Other" ~ Other_Previous_Waterbody_2,
                                                  Previous_Waterbody_2 != "Other" ~ Previous_Waterbody_2),
            Previous_Waterbody_2_Closest_City = Closest_City_to_Previous_Waterbody_2,
            Previous_Waterbody_2_Province_Or_State = State_Province_for_Previous_Waterbody_2,
            Previous_Waterbody_2_Days_Out_Of_Water = `Day_out_of_water_(previous_waterbody_2)`,
            Previous_Dry_Storage_Ind = case_when(str_detect(Previous_Waterbody_1, "Dry Storage") ~ "true",
                                                 TRUE ~ "false"),
            Unknown_Previous_Water_Body_Ind = case_when(str_detect(Previous_Waterbody_1, "Unknown") ~ "true",
                                                        TRUE ~ "false"),
            Destination_Waterbody_1_Name = case_when(Destination_Waterbody_1 == "Other" ~ Other_Destination_Waterbody_1,
                                                     Destination_Waterbody_1 != "Other" ~ Destination_Waterbody_1),
            Destination_Waterbody_1_Closest_City = Closest_City_to_Destination_Waterbody,
            Destination_Waterbody_1_Province_Or_State = State_Province_of_Destination_Waterbody,
            Destination_Dry_Storage_Ind = case_when(str_detect(Destination_Waterbody_1, "Dry [s,S]torage") ~ "true",
                                                    TRUE ~ "false"),
            Unknown_Destination_Waterbody_Ind = case_when(str_detect(Destination_Waterbody_1, "Unknown") ~ "true",
                                                          TRUE ~ "false"),
            Aquatic_Plants_Found_Ind = case_when(Aquatic_plants_found_on_inspection == "yes" ~ "true",
                                                 TRUE ~ "false"),
            Marine_Mussel_Found_Ind = case_when(Other_inspection_outcomes == "Marine mussels found" ~ "true",
                                                TRUE ~ "false"),
            Marine_Species_Found_Ind = case_when(Marine_species_found_on_inspection == "yes" ~ "true",
                                                 TRUE ~ "false"),
            High_Risk_AIS_Ind = case_when(Is_the_watercraft_high_risk_for_AIS_Dreissenid_mussels == "yes" ~ "true",
                                          Is_the_watercraft_high_risk_for_AIS_Dreissenid_mussels == "no" ~ "false"),
            Adult_Dressenidae_Found_Ind = case_when(Dreissenid_Mussels_Found...62 == "yes" ~ "true",  
                                                    TRUE ~ "false"),
            Standing_Water_Present_Ind = case_when(Standing_water_found == "yes" ~ "true",
                                                   TRUE ~ "false"),
            Adult_Dreissenidae_Mussel_Found_Ind = case_when(Dreissenid_Mussels_Found...62 == "yes" ~ "true",  
                                                            TRUE ~ "false"),
            Adult_Mussels_Location_Name = Location_of_mussels_or_standing_water_found,
            Decontamination_Performed_Ind = case_when(Decontamination_Performed == "yes" ~ "true",
                                                      TRUE ~ "false"),
            Decontamination_Reference = Record_of_Decontamination_Number,
            Decontamination_order_issued_Ind = case_when(Decontamination_Order_Issued == "yes" ~ "true",
                                                         TRUE ~ "false"),
            Decontamination_order_number = Decontamination_Order_number,
            Seal_Issued_Ind = case_when(Was_a_seal_applied_to_the_watercraft == "yes" ~ "true", 
                                        TRUE ~ "false"),
            Seal_Number = Seal_tag_number,
            Quarantine_Period_Issued_Ind = case_when(Quarantine_period_issued_for_30_day_drying_time == "yes" ~ "true",
                                                     TRUE ~ "false"))

rm(dat15);rm(dat16);rm(dat17);rm(dat18);rm(dat19)

# Making sure mussel-fouled records line up between the raw data and Martina's mussel-fouled 
#   tracking sheets. For each year's inspection data, we verify that inspection ID's of 
#   mussel-fouled boats line up between Martina's sheet and the raw data. If there are 
#   discrepancies, we trust Martina's data over all else!

dat16_clean = dat16_clean %>% 
  mutate(Adult_Dressenidae_Found_Ind = case_when(
          str_remove(Watercraft_Risk_Assessment_ID, "2016-") %in% dat16_mf$`Inspection ID` ~ "true",
          TRUE ~ "false"),
         Adult_Dreissenidae_Mussel_Found_Ind = case_when(
          str_remove(Watercraft_Risk_Assessment_ID, "2016-") %in% dat16_mf$`Inspection ID` ~ "true",
          TRUE ~ "false"))

dat17_clean = dat17_clean %>% 
  mutate(Adult_Dressenidae_Found_Ind = case_when(
    str_remove(Watercraft_Risk_Assessment_ID, "2017-") %in% dat17_mf$`Inspection ID` ~ "true",
    TRUE ~ "false"),
    Adult_Dreissenidae_Mussel_Found_Ind = case_when(
      str_remove(Watercraft_Risk_Assessment_ID, "2017-") %in% dat17_mf$`Inspection ID` ~ "true",
      TRUE ~ "false"))

dat18_clean = dat18_clean %>% 
  mutate(Adult_Dressenidae_Found_Ind = case_when(
      str_remove(Watercraft_Risk_Assessment_ID, "2018-") %in% dat18_mf$`Inspection ID` ~ "true",
      TRUE ~ "false"),
         Adult_Dreissenidae_Mussel_Found_Ind = case_when(
      str_remove(Watercraft_Risk_Assessment_ID, "2018-") %in% dat18_mf$`Inspection ID` ~ "true",
      TRUE ~ "false"))

dat19_clean = dat19_clean %>% 
  mutate(Adult_Dressenidae_Found_Ind = case_when(
    str_remove(Watercraft_Risk_Assessment_ID, "2019-") %in% dat19_mf$`Inspection ID` ~ "true",
    TRUE ~ "false"),
    Adult_Dreissenidae_Mussel_Found_Ind = case_when(
      str_remove(Watercraft_Risk_Assessment_ID, "2019-") %in% dat19_mf$`Inspection ID` ~ "true",
      TRUE ~ "false"))

metabase_dat = metabase_dat %>% 
  mutate(Adult_Dressenidae_Found_Ind = case_when(
    str_remove(Watercraft_Risk_Assessment_ID,"202[0-2]{1}-") %in% c(dat20_mf$watercraft_risk_assessment_id,
                                                                    dat21_mf$watercraft_risk_assessment_id,
                                                                    dat22_mf$watercraft_risk_assessment_id) ~ "true",
    TRUE ~ "false"),
    Adult_Dreissenidae_Mussel_Found_Ind = case_when(
      str_remove(Watercraft_Risk_Assessment_ID,"202[0-2]{1}-") %in% c(dat20_mf$watercraft_risk_assessment_id,
                                                                      dat21_mf$watercraft_risk_assessment_id,
                                                                      dat22_mf$watercraft_risk_assessment_id) ~ "true",
      TRUE ~ "false"))

#Make sure dates for metabase data are all good. E.g. change timezones to PDT.
metabase_dat = metabase_dat %>% 
  mutate(TimeOfInspection = force_tz(raw_timestamp, "America/Los_Angeles")) %>% 
  select(-raw_timestamp)

#Combine the datasets.
dat = bind_rows(metabase_dat) %>% 
  bind_rows(dat19_clean %>% mutate(TimeOfInspection = force_tz(TimeOfInspection, "America/Los_Angeles")),
            dat18_clean %>% mutate(TimeOfInspection = force_tz(TimeOfInspection, "America/Los_Angeles")), 
            dat17_clean %>% mutate(TimeOfInspection = force_tz(TimeOfInspection, "America/Los_Angeles")), 
            dat16_clean %>% mutate(TimeOfInspection = force_tz(TimeOfInspection, "America/Los_Angeles")), 
            dat15_clean %>% mutate(TimeOfInspection = force_tz(TimeOfInspection, "America/Los_Angeles")))

dat %>% 
  ggplot() + 
  geom_histogram(aes(hour(TimeOfInspection))) + 
  facet_wrap(~ year(TimeOfInspection), scales = "free")

dat = dat %>% 
  #Clean up the mussel-fouled columns by combining them.
  mutate(MusselsFound_Ind = case_when(
    Adult_Dressenidae_Found_Ind == "true" | Adult_Dreissenidae_Mussel_Found_Ind == "true" ~ T,
    T ~ F)) %>% 
  select(-Adult_Dressenidae_Found_Ind, -Adult_Dreissenidae_Mussel_Found_Ind)

openxlsx::write.xlsx(dat, paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Multiyear data/WatercraftInspectionData_AllYears_PreCleaning.xlsx"),
                     overwrite = T)

