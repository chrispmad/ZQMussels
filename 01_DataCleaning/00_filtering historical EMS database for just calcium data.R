# # Title: Filtering EMS database for calcium and pH data
#
# Date: 2024-01-29
#
# Author(s): Chris Madsen (chris.madsen@gov.bc.ca)
# 
# Description: This script takes the whopping-huge historical file of EMS database,
# which you can download on the BC Data Catalogue website 
# (https://catalogue.data.gov.bc.ca/dataset/bc-environmental-monitoring-system-results/resource/6aa7f376-a4d3-4fb4-a51c-b4487600d516), 
# and just keeps rows that tell us about total and dissolved calcium.

# Once the giant EMS database file has been filtered for calcium data, I would recommend deleting it (it is 5+ GB in size!)

library(tidyverse)
library(readr)
library(bcdata)

#Options file - this allows us to set our working directories for all scripts in just one file.
my_opts = read_csv("Options.csv") %>% 
  as.data.frame()

dat = read_csv_chunked(paste0(my_opts$base_dir,"01_DataCleaning/data/ems_sample_results_historic_expanded.csv"), 
                       callback = DataFrameCallback$new(function(x, pos) subset(x, PARAMETER %in% c("Calcium Total",'Calcium Dissolved','pH'))))

qs::qsave(dat, paste0(my_opts$base_dir,"01_DataCleaning/data/mammoth_dataset_filtered_for_total_and_dissolved_calcium_and_pH.qs"))
