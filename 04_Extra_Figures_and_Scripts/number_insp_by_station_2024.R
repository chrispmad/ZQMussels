# To run this script, first run just up to the section "numbers_for_exec_summary"
# in the script "02_IMDP_FinalReport_Figures.Rmd"

perm_stations = c("Golden","Yahk","Osoyoos","Pacific",
                  "Olsen","Dawson Creek","Mt. Robson","Radium")
s = dat |> 
  # Roll up all roving stations into one 'Roving'
  # dplyr::mutate(Station = ifelse(!Station %in% perm_stations, 'Roving', Station)) |> 
  dplyr::mutate(Station = ifelse(stringr::str_detect(Station, "^Scheduled"), "Scheduled Inspection", Station)) |> 
  dplyr::select(Year,Station) |> 
  dplyr::mutate(StationType = ifelse(Station %in% perm_stations,
                                     "Permanent",
                                     "Roving")) |> 
  dplyr::count(Year,Station,StationType, sort = T)

roving_rolled_s = dat |> 
  # Roll up all roving stations into one 'Roving'
  dplyr::mutate(Station = ifelse(!Station %in% perm_stations, 'Roving', Station)) |>
  dplyr::mutate(Station = ifelse(stringr::str_detect(Station, "^Scheduled"), "Scheduled Inspection", Station)) |> 
  dplyr::select(Year,Station) |> 
  dplyr::mutate(StationType = ifelse(Station %in% perm_stations,
                                     "Permanent",
                                     "Roving")) |> 
  dplyr::count(Year,Station,StationType, sort = T)

my_wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(my_wb, "By_Station")
openxlsx::addWorksheet(my_wb, "By_Station_Roving_RolledUp")
openxlsx::writeData(my_wb, "By_Station", s)
openxlsx::writeData(my_wb, "By_Station_Roving_RolledUp", roving_rolled_s)
openxlsx::saveWorkbook(my_wb, file = '04_Extra_Figures_and_Scripts/output/number_of_inspections_by_station_in_2024.xlsx',
                       overwrite=T)
