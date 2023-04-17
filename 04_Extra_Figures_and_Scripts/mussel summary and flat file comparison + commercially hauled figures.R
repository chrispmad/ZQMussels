
# Load in libraries
library(raster)
library(tidyverse)
library(sf)
library(readxl)
library(openxlsx)
library(lubridate)

rm(list = ls())

#Test to see which rows are the rows that are NOT found in the mussel_summary but ARE found in 
# the mussel summary flat file that the team has been using. Look for something that identifies
# those rows, then remove them from the flat file if necessary. Once those are gone, check numbers
# calculated below and see if the figures in the 2020 IMDP report need to be reviewed.
flat = read_csv("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/2020 data/2020 Mussel Summary flat file.csv")
# summary = read_csv("C:/Users/CMADSEN/Downloads/All row from mussel_summary to check for row number with mussel flat file.csv")
# #wra = read_csv("C:/Users/CMADSEN/Downloads/watercraft_risk_assessment.csv")
# 
# #Arrange by watercraft risk assessment id
# flat = flat %>% arrange((`Watercraft Risk Assessment ID`))
# summary = summary %>% arrange((watercraft_risk_assessment_id))
# #wra = wra %>% arrange(`Watercraft Risk Assessment ID`)
# 
# length(unique(flat$`Watercraft Risk Assessment ID`))
# length(unique(summary$watercraft_risk_assessment_id))
# #length(unique(wra$`Watercraft Risk Assessment ID`))
# #Yes, all rows are unique wra ids.
# 
# #Same date range?
# max(flat$`Start Time`) #2020-11-04
# max(summary$start_time) #2021-04-26
# #max(wra$`Created At`) #2021-04-28
# 
# #Nope, I need to filter out 2021 dates for the summary table.
# summary = summary %>% filter(lubridate::year(start_time) < 2021)
# #wra = wra %>% filter(lubridate::year(`Created At`) < 2021)
# #wra and flat file are now the same. This means that my code to create the flat file 
# #does not contain errors that artificially adds extra rows. Good!
# 
# #Quick check: Can we just use the vector of unique watercraft risk assessment IDs 
# #from the Metabase table "summary" to get the filtered rows for the flat file?
# flat[flat$`Watercraft Risk Assessment ID`%in%summary$watercraft_risk_assessment_id,]
# 
# #Yes! So, we can just alter the query on metabase that produces the excel flat file
# # so that it uses "mussel_summary" instead of "wra". That should solve our problems.
# 
# #Which rows are in the flat file but not in the mussel_summary on metabase?
# onlyflat = flat %>%
#   filter(!`Watercraft Risk Assessment ID`%in%summary$watercraft_risk_assessment_id)
# 
# onlysummary = summary %>%
#   filter(!watercraft_risk_assessment_id%in%flat$`Watercraft Risk Assessment ID`)
# 
# # onlywra = wra %>%
# #   filter(!`Watercraft Risk Assessment ID`%in%summary$watercraft_risk_assessment_id)
# # rm(onlywra)
# #We actually have 2221 WRA IDs that are only in the flat file, and no records only in the summary file.
# 
# #Are the 2221 records duplicates of the 29943 records in the mussel_summary table?
# 
# nrow(flat[duplicated(flat$raw_timestamp),])
# #2,189 records have exactly the same timestamp. If we remove those from the flat file,
# #do we get the same number of records as in the summary table of Metabase?
# #If we can remove the correct duplicated rows, we will have 29975 rows (very close to summary)
# 
# flat_desc = flat %>% arrange(desc(`Watercraft Risk Assessment ID`))
# 
# flat_flag = flat_desc %>% 
#   #select(`Watercraft Risk Assessment ID`, raw_timestamp) %>%
#   rename(wraid = `Watercraft Risk Assessment ID`) %>%
#   group_by(raw_timestamp) %>%  
#   mutate(NumberIdenticalRows = n(), 
#          GroupNumber = row_number()) %>%
#   mutate(RemoveMe = case_when(
#     NumberIdenticalRows == 1 ~ "Keep",
#     NumberIdenticalRows > 1 & wraid >= 2000 & wraid <= 3000 & GroupNumber != 1 ~ "Remove",
#     NumberIdenticalRows > 1 & wraid < 2000 & GroupNumber != 2 ~ "Remove",
#     NumberIdenticalRows > 1 & wraid > 3000 & GroupNumber != 2 ~ "Remove",
#     TRUE ~ "Keep"
#     )
#   )
# 
# flat_flag %>% filter(wraid%in%c(1873,2245,2263,5987,31163,31245)) %>% View(.)
# 
# flat_clean = flat_flag %>% filter(RemoveMe == "Keep") %>% 
#   rename(`Watercraft Risk Assessment ID` = wraid)
# 
# #Remove any records with "test" in the comments.
# flat_clean = flat_clean %>% filter(!str_detect(`Shift Start Comment`,"Test"))
# 
# #Remove any rows with Cass' email address, or the shift start comment "app learning"
# flat_clean = flat_clean %>% filter(!str_detect(Email, "cassandra"),
#                                       !str_detect(`Shift Start Comment`, "app learning"))
# 
# #How many records do we have in only the "flat_clean" table, and how many only in the 
# #summary?
# onlyflat = flat_clean %>%
#   filter(!`Watercraft Risk Assessment ID`%in%summary$watercraft_risk_assessment_id)
# 
# onlysummary = summary %>%
#   filter(!watercraft_risk_assessment_id%in%flat_clean$`Watercraft Risk Assessment ID`)
# #There are 366 only in this trimmed version of the flat file, and 334 that are 
# #only in the summary file! This is bad... it means I have filtered out 334 records that
# #the Metabase team didn't filter out.
# 
# 
# #What do the last few extra records in "flat_minus_timestampdups" look like?
# flat_minus_timestampdups[!flat_minus_timestampdups$`Watercraft Risk Assessment ID`%in%summary$watercraft_risk_assessment_id,]
# 
# 
# #Are the extra records identifiable with the email address column?
# unique(summary$email) #37 email addresses in summary file.
# unique(flat$Email) #42 email addresses in flat file (i.e. table with extra records)
# unique(onlyflat$Email) #25 email addresses in the extra flat file records.
# 
# #Which email addresses are only found in the extra flat file records?
# unique(onlyflat[!onlyflat$Email%in%summary$email,]$Email) #Martina, Michael, Cassandra, istest3, roland.stens
# #But, the above email addresses only make up 28 of the 2221 extra rows... so, something
# # else is going on here!
# 
# #How many records per email address in the onlyflat table?
# onlyflat %>% group_by(Email) %>% summarise(NumberOfRows = n()) %>% arrange(desc(NumberOfRows))
# 
# apply(onlyflat %>% select(-`General Comment`), 2, length(unique(.)))
# 
# onlyflat %>% group_by(`Shift Start Comment`) %>% summarise(numbrows = n()) %>% arrange(desc(numbrows))
# 
# names(onlyflat)

#Summarise number of commercially hauled watercraft by station and source province/state, then
# write out to an excel file.

dat <- read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/2020 data/Commercially hauled boats checked by hand.xlsx", 
                  sheet = "CHBoats_Source")

dat_wb = openxlsx::loadWorkbook("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/2020 data/Commercially hauled boats checked by hand.xlsx")

addWorksheet(dat_wb, sheetName = "Source ProvState")
addWorksheet(dat_wb, sheetName = "Insp per Station")

dat_sps = dat %>% 
  group_by(`Hand-picked Source`) %>%
  summarise(NumberInspections = n()) %>%
  arrange(desc(NumberInspections)) %>%
  mutate(TotalCommHaul = sum(NumberInspections))

dat_sps[c(2:26),3] <- NA

#And by station...
dat_st = dat %>% 
  mutate(Station = str_replace(Station,"Scheduled.*", "Scheduled Insp")) %>% 
  group_by(Station) %>%
  summarise(NumberInspections = n()) %>%
  arrange(desc(NumberInspections)) %>%
  mutate(TotalCommHaul = sum(NumberInspections))

dat_st[c(2:8),3] <- NA

dat_st

#Add info to sheets...
writeData(dat_wb, "Source ProvState", dat_sps)
writeData(dat_wb, "Insp per Station", dat_st)

saveWorkbook(dat_wb,
             "I:/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/2020 data/Commercially hauled boats post R processing.xlsx",
             overwrite = T)
