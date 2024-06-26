# Automatic updater of IMDP data. Run once a week.

# Read in options
my_opts = readr::read_csv("C:/Users/CMADSEN/Downloads/LocalR/long_term_projects/ZQMussels/Options.csv")

# Remove 'old' version of this year's IMDP inspection data.
this_year = stringr::str_extract(Sys.time(),'^[0-9]{4}')

old_file = paste0(my_opts$zqm_operations_data_folder,'Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years/metabase_',this_year,'.csv')

if(file.exists(old_file)){
  file.remove(old_file)
}
  
# Copy the new version in to that folder.
file.copy(from = paste0("C:/Users/CMADSEN/Downloads/metabase_",this_year,".csv"),
          to = old_file)

if(file.exists(paste0("C:/Users/CMADSEN/Downloads/metabase_",this_year,".csv"))){
  file.remove(paste0("C:/Users/CMADSEN/Downloads/metabase_",this_year,".csv"))
}

imdp::update_all_imdp_data(year = my_opts$year)