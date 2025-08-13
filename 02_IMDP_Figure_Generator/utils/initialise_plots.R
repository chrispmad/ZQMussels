
# NOTE: this script uses tmap version 3.3. That is no longer the version available by default when you use install.packages(). The new version completely alters the plots produced.
# You may need to manually install tmap version 3.3.
# install.packages("C:/Users/CMADSEN/Downloads/tmap_3.3.tar.gz", repos = NULL, type = "source")
# This may require that you also install: {widgetframe}.

pacman::p_load(
  readxl,
  ggpubr,
  ggrepel,
  plotly,
  ggExtra,
  ggtext,
  tidyverse,
  leaflet,
  sf,
  RColorBrewer,
  openxlsx,
  scales,
  lubridate,
  plotrix,
  basemaps,
  ggplot2)


#=====================================================
#                       OPTIONS 
#=====================================================
# setwd("C:/Users/CMADSEN/Downloads/LocalR/long_term_projects/ZQMussels")
my_opts = read_csv(paste0(here::here(),"/Options.csv"))

#Which year should we focus on?
my.year = 2024
# my.year = 2025

#Update GIS maps? We probably want this turned on unless you are making fine adjustments to some figures.
update.gis = FALSE

# Are there any station names we want to change / correct?
stations.to.change = data.frame(
  Station = c('Fraser Valley Roving'),
  new_name = c('Lower Mainland Roving')
)

# Are there any station names we want to change / correct?
stations.to.change = data.frame(
  Station = c('Fraser Valley Roving'),
  new_name = c('Lower Mainland Roving')
)

#Are there any stations we would like to exclude from the excel-type figures?
#In 2021, we will exclude:
if(my.year == 2022){
  stations.to.include = c("Golden","Dawson Creek","Mt. Robson",
                          "Osoyoos","Olsen","Penticton Roving",
                          "Radium","Pacific","Fraser Valley Roving",
                          "Keremeos")
  
  stations.to.put.aside = c("Scheduled Inspection","Boat Launch - Okanagan",
                            "Okanagan",
                            "Penticton Roving - Hwy 33", 
                            "Penticton Roving - Inspection Event")
}
if(my.year == 2023){
  stations.to.include = c("Golden","Radium","Olsen","Yahk",
                          "Pacific","Osoyoos","Hwy 97c","Mt. Robson",
                          "Keremeos","Dawson Creek","Lower Mainland Roving",
                          "Sumas Border","Cutts (Hwy 93)","Penticton Roving")
  stations.to.put.aside = c("Scheduled Inspection","Boat Launch - Okanagan",
                            "Okanagan",
                            "Penticton Roving - Hwy 33", 
                            "Penticton Roving - Inspection Event")
}
if(my.year == 2024){
  stations.to.include = c("Golden","Radium","Olsen","Yahk",
                          "Pacific","Osoyoos","Hwy 97c","Mt. Robson",
                          "Keremeos (Hwy 3)","Dawson Creek","Lower Mainland Roving",
                          "Sumas Border","Cutts (Hwy 93)","Penticton Roving",
                          "Douglas Crossing")
  stations.to.put.aside = c("Scheduled Inspection","Boat Launch - Okanagan",
                            "Okanagan",
                            "Penticton Roving - Hwy 33", 
                            "Penticton Roving - Inspection Event")
}
permanent.stations = c("Golden","Olsen","Yahk",
                       "Osoyoos","Mt. Robson",
                       "Dawson Creek")

roving.stations = c("Penticton Roving","Lower Mainland Roving",
                    "Keremeos (Hwy 3)","Sumas Border","Pacific",
                    "Cutts (Hwy 93)","Douglas Crossing", "Hwy 97c")

part.time.stations = c("Sumas Border","Pacific","Cutts (Hwy 93)","Radium","Douglas Crossing")
# Just in case it's different, which stations do we want to show in the 
# leaflet maps?
if(my.year == 2022){
  leaflet.stations.to.include = c("Golden","Radium","Olsen","Dawson Creek",
                                  "Mt. Robson","Penticton Roving","Osoyoos","Fraser Valley Roving")
} 
if(my.year == 2023){
  leaflet.stations.to.include = c("Golden","Olsen","Dawson Creek","Mt. Robson",
                                  "Penticton Roving","Osoyoos","Lower Mainland Roving",
                                  "Yahk")
}
if(my.year == 2024){
  leaflet.stations.to.include = c(
    "Golden","Olsen","Dawson Creek","Mt. Robson",
    "Radium","Sumas Border",
    "Penticton Roving","Osoyoos","Lower Mainland Roving",
    "Yahk","Pacific",
    "Keremeos","Hwy 97c","Cutts (Hwy 93)","Douglas Crossing")
}

#Data folders
my.data.folder = paste0(my_opts$zqm_figure_local_folder,"data/")
my.external.data.folder = paste0(my_opts$remote_spatial_data,"Projects/ZQMussels/data/")

#Which folder should we put specific output files in?
my.output.folder = paste0(my_opts$zqm_figure_local_folder,"output/")
my.external.output.folder = paste0(my_opts$zqm_figure_output_remote_folder,my.year)
zqm.operations.folder = my_opts$zqm_operations_data_folder
this.years.report.folder = paste0(my_opts$remote_spatial_data,"Projects/ZQMussels/",my.year," IMDP Final Report/")

#Where is the mussel-fouled tracking sheet?
if(my.year == 2022){
  MusselFouledTracker = paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/", my.year," data/2022 mussel fouled boats tracking sheet.xlsx")
}
if(my.year == 2023){
  MusselFouledTracker = paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/", my.year," data/2023_mussel_fouled_details.xlsx")
}
if(my.year == 2024){
  MusselFouledTracker = paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/", my.year," data/mussel_fouled_summary.xlsx")
}

#Where can we find the CBSA notifications excel sheet for this year?
if(file.exists(paste0(my.external.output.folder,"/GIS Maps and Excel Figures/",my.year," COS inbox notifications.xlsx"))){
  cbsa_dat = read_excel(paste0(my.external.output.folder,"/GIS Maps and Excel Figures/",my.year," COS inbox notifications.xlsx"),
                        sheet = "Filtered list")
}


#These lines below set options for all of the code for the rest of this script. You probably don't need to change any of these.
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = F, fig.width = 8, fig.height = 4)
knitr::opts_knit$set(root.dir = paste0(my.external.output.folder,"/GIS Maps and Excel Figures/ExcelFigures"))

#Colour for some of the figures:
my.grey = "#5f6a6f"

#=====================================================
#                     END OF OPTIONS
#=====================================================

#If there is no folder for excel figures for the target year in the I: drive, make it now.
if(!dir.exists(paste0(my.external.output.folder,"/GIS Maps and Excel Figures/ExcelFigures"))){
  dir.create(paste0(my.external.output.folder,"/GIS Maps and Excel Figures/ExcelFigures/"),
             recursive = T)
}


