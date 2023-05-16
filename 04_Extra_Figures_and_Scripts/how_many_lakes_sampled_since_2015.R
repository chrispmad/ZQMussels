library(tidyverse)
library(sf)
library(openxlsx)

my_opts = read_csv('Options.csv')

dat2015 = read.xlsx(paste0(my_opts$zqm_operations_data_folder,'Lake Monitoring/2015/Sample inventory/2015 BC Veliger Sampling Inventory 2016_07_07_corrected.xlsx')) %>% as_tibble()

lakes_2015 = unique(na.omit(dat2015[,1])[-1,])

dat2016 = read.xlsx(paste0(my_opts$zqm_operations_data_folder,'Lake Monitoring/2016/Veliger sample sites/2016 Lake monitoring Locations Final.xlsx')) %>% as_tibble()

lakes_2016 = unique(dat2016$Lake)

dat2017 = read.xlsx(paste0(my_opts$zqm_operations_data_folder,'Lake Monitoring/2017/Lab analysis/Final Veliger Data Report November 2017_cleaned.xlsx')) %>% as_tibble()

lakes_2017 = unique(dat2017$Sample)

dat2018 = read_csv(paste0(my_opts$zqm_operations_data_folder,'Lake Monitoring/2018/Lab sample analysis/2018_ZQM_plankton_sites (lat long updates).csv'))

lakes_2018 = unique(dat2018$Waterbody...2)

dat2019 = read.xlsx(paste0(my_opts$zqm_operations_data_folder,'Lake Monitoring/2019/Lab Sample Analysis/Viliger Analysis/Weekly Veliger Analysis Results/BC Veliger Sampling Inventory 2019 FINAL.xlsx')) %>% as_tibble()

lakes_2019 = unique(dat2019$Waterbody)

dat2020 = read.xlsx(paste0(my_opts$zqm_operations_data_folder,'Lake Monitoring/2020/Lab analysis/954 - BC Veliger Sampling Inventory 2020_Final Report.xlsx')) %>% as_tibble()

lakes_2020 = unique(dat2020$Waterbody)

dat2021 = read.xlsx(paste0(my_opts$zqm_operations_data_folder,'Lake Monitoring/2021/Lab Analysis/Final report/BC Veliger Sampling Inventory - report 2021.xlsx')) %>% as_tibble()

lakes_2021 = unique(dat2021$Waterbody)

dat2022 = read.xlsx(paste0(my_opts$zqm_operations_data_folder,'Lake Monitoring/2022/Lab Analysis/Final report and data/BC Veliger Sampling Inventory 2022_FinalReport.xlsx')) %>% as_tibble()

lakes_2022 = unique(dat2022$Waterbody)

all_lakes = unique(c(lakes_2015,lakes_2016,lakes_2017,lakes_2018,lakes_2019,lakes_2020,
              lakes_2021,lakes_2022))

all_lakes = unlist(all_lakes)

all_lakes = all_lakes[order(all_lakes)]

all_lakes = unique(str_squish(all_lakes))

all_lakes = str_to_title(all_lakes)

# Remove anything from the end of a name that's not letters.
all_lakes = str_squish(str_remove_all(all_lakes, "(#[0-9]+|\\.|/.*$|\\'|\\(.*|[0-9]{1}[a-z]{2}$| upper| Upper| lower| Lower| E$| W$| R$| L$| South$| North$)"))

# # Remove numbers (a la '#[0-9]')
# all_lakes = str_squish(str_remove(all_lakes, '#[0-9]+'))

# Replace 'Lk' with 'Lake'
all_lakes = str_replace(all_lakes, 'Lk$', 'Lake')

# Remove an s from the end (e.g. 'Lakes') 
all_lakes = str_remove(all_lakes, '(?<=Lake)s')

# Remove commas from the end of the name.
all_lakes = str_remove(all_lakes, '\\,')

# If the name is missing 'Lake', add in 'Lake'
for(i in 1:length(all_lakes)){
  
  this_wb = all_lakes[i]
  
  if(sum(str_detect(all_lakes, paste0(this_wb,' Lake'))) > 0){
    this_wb = paste0(this_wb, " Lake")
  }
  
  all_lakes[i] = this_wb
}

# Same thing but for 'River'
for(i in 1:length(all_lakes)){
  
  this_wb = all_lakes[i]
  
  if(sum(str_detect(all_lakes, paste0(this_wb,' River'))) > 0){
    this_wb = paste0(this_wb, " River")
  }
  
  all_lakes[i] = this_wb
}

all_lakes = unique(all_lakes)

length(all_lakes)

write.csv(all_lakes, '04_Extra_Figures_and_Scripts/output/unique_lake_list.csv', row.names = F)
