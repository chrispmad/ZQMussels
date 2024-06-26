library(tidyverse)
library(openxlsx)
library(sf)

my_opts = read_csv("C:/Users/CMADSEN/Downloads/LocalR/long_term_projects/ZQMussels/Options.csv")

#Data folders
my.data.folder = paste0(my_opts$zqm_figure_local_folder,"data/")
my.external.data.folder = paste0(my_opts$remote_spatial_data,"Projects/ZQMussels/data/")

# Load in data
if(!exists('dat_all')){
  dat_all = read.xlsx(paste0(my.data.folder,'figure_dat_all.xlsx')) |>
    mutate(TimeOfInspection = openxlsx::convertToDateTime(TimeOfInspection)) |> 
    as_tibble()
}

mf = dat_all |> 
  dplyr::filter(MusselsFound_Ind)

ont_mf = mf |> 
  dplyr::filter(Previous_Waterbody_1_Province_Or_State == 'ON') |> 
  dplyr::select(Year,
                starts_with('Previous_Waterbody_1'),
                Previous_Major_City)

# Coalesce to a single column
ont_mf = ont_mf |> 
  dplyr::mutate(Previous_Waterbody_1_Name = stringr::str_squish(Previous_Waterbody_1_Name)) |> 
  dplyr::mutate(Previous_Waterbody_1_Name = case_when(
    Previous_Waterbody_1_Name %in% c("Other","Dry Storage","None","Unknown") ~ NA,
    T ~ Previous_Waterbody_1_Name
  )) |> 
  dplyr::mutate(city = coalesce(#Previous_Waterbody_1_Name, 
                                Previous_Waterbody_1_Closest_City, 
                                # Previous_Waterbody_1_Other_Details, 
                                Previous_Major_City))

# Clean up coalesced column
ont_mf = ont_mf |> 
  mutate(city = str_remove(city, "\\(.*")) |> 
  mutate(city = str_remove(city, ',.*')) |> 
  mutate(city = str_squish(city)) |> 
  mutate(city = str_to_title(city))

ont_mf = ont_mf |> 
  dplyr::count(city, sort = T, name = 'Number of Mussel-fouled Boats') |> 
  dplyr::filter(!city %in% c('None','Unknown','','Personal Property Storage','Previously Owned')) |> 
  filter(!is.na(city))

openxlsx::write.xlsx(ont_mf,
                     "04_Extra_Figures_and_Scripts/output/Reported Closest Major City of Mussel-fouled Boats from Ontario entering British Columbia.xlsx")
