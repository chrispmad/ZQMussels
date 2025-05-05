# To run this script, first run just up to the section "numbers_for_exec_summary"
# in the script "02_IMDP_FinalReport_Figures.Rmd"

perm_stations = c("Golden","Yahk","Osoyoos","Pacific",
                  "Olsen","Dawson Creek","Mt. Robson")

s = dat |> 
  # Roll up all roving stations into one 'Roving'
  dplyr::mutate(Station = ifelse(!Station %in% perm_stations, 'Roving', Station)) |> 
  dplyr::select(Year,Station,TimeOfInspection) |> 
  dplyr::mutate(day_of_week = lubridate::wday(TimeOfInspection,abbr = T, label = T),
                the_date = lubridate::ymd(stringr::str_extract(TimeOfInspection,"^.*(?= )"))) |> 
  dplyr::select(-TimeOfInspection) |> 
  dplyr::distinct() |> 
  dplyr::mutate(week_day = ifelse(!day_of_week %in% c("Sat","Sun"),"Weekday Shifts","Weekend Shifts")) |> 
  dplyr::mutate(StationType = ifelse(Station %in% perm_stations,
                                     "Permanent",
                                     "Roving"))


days_active_count = s |> 
  dplyr::count(Station,StationType, name = 'days_active', sort = T)

wdays_active_count = s |> 
  dplyr::count(Station,StationType,week_day, name = 'days_active', sort = T) |>
  dplyr::group_by(Station) |> 
  dplyr::mutate(total_count = sum(days_active)) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(dplyr::desc(total_count), Station, dplyr::desc(days_active)) |> 
  dplyr::select(-total_count) |> 
  tidyr::pivot_wider(names_from = week_day, values_from = days_active, values_fill = 0)

my_wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(my_wb, "Summary_by_Station")
openxlsx::addWorksheet(my_wb, "Summary_by_Station_Weekday")
openxlsx::writeData(my_wb, "Summary_by_Station", days_active_count)
openxlsx::writeData(my_wb, "Summary_by_Station_Weekday", wdays_active_count)
openxlsx::saveWorkbook(my_wb, file = '04_Extra_Figures_and_Scripts/output/number_of_shifts_in_2024.xlsx',
                       overwrite=T)
