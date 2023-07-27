shift_ids = read_csv('C:/Users/CMADSEN/Downloads/query_result_2023-07-17T18_16_03.866Z.csv')

shift_ids = shift_ids |> 
  filter(!str_detect(shift_start_comment,'[t,T]est'))

shift_ids |> 
  count(boats_inspected_ind)

shift_ids |> 
  mutate(Day = lubridate::day(start_time),
         Month = lubridate::month(start_time)) |> 
  mutate(unique_date = paste0(Month,'-',Day)) |> 
  filter(!duplicated(unique_date))
# From the observer workflow table, we have 232 unique dates in 2022 
# on which a shift occurred.

# Just to double check, do we have any days with multiple shifts?
shift_ids |> 
  mutate(Day = lubridate::day(start_time),
         Month = lubridate::month(start_time)) |> 
  mutate(unique_date = paste0(Month,'-',Day)) |> 
  filter(!duplicated(unique_date)) |> 
  filter(duplicated(observer_workflow_id))
# Nope, none.

# So, we can conclude that there were 232 days with someone active on the IMDP patrol.

# Number of rows with apparently 0 boats inspected?
dat_all |> 
  filter(Year == 2022) |> 
  mutate(across(contains("Counter"), as.numeric)) |> 
  filter(Non_Motorized_Counter == 0, 
         Complex_Counter == 0, 
         Very_Complex_Counter == 0, 
         Simple_Counter == 0) |> 
  dplyr::select(Start_Time, contains("Counter")) |> 
  mutate(Start_Time = openxlsx::convertToDateTime(Start_Time)) |> 
  mutate(Day = lubridate::day(Start_Time),
         Month = lubridate::month(Start_Time)) |> 
  dplyr::select(Month,Day) |> 
  distinct()
# Looks like there's 156 rows (i.e. unique days) with apparently 0 boats inspected.

# How about where at least one boat is inspected?
dat_all |> 
  filter(Year == 2022) |> 
  dplyr::select(Start_Time, contains("Counter")) |> 
  mutate(across(contains("Counter"), as.numeric)) |> 
  filter(Non_Motorized_Counter > 0 | 
         Complex_Counter > 0 |
         Very_Complex_Counter > 0 | 
         Simple_Counter > 0) |> 
  mutate(Start_Time = openxlsx::convertToDateTime(Start_Time)) |> 
  mutate(Day = lubridate::day(Start_Time),
         Month = lubridate::month(Start_Time)) |> 
  dplyr::select(Month,Day) |> 
  distinct()
# 231 rows (i.e. unique days) with at least 1 boat inspected.

# Are any of these days overlapping?
dat_all |> 
  filter(Year == 2022) |> 
  dplyr::select(Start_Time, contains("Counter")) |> 
  mutate(across(contains("Counter"), as.numeric)) |> 
  filter(Non_Motorized_Counter > 0 | 
           Complex_Counter > 0 |
           Very_Complex_Counter > 0 | 
           Simple_Counter > 0) |> 
  mutate(Start_Time = openxlsx::convertToDateTime(Start_Time)) |> 
  mutate(Day = lubridate::day(Start_Time),
         Month = lubridate::month(Start_Time)) |> 
  dplyr::select(Month,Day) |> 
  mutate(at_least_one_boat = T) |> 
  distinct() |> 
  full_join(
    dat_all |> 
      filter(Year == 2022) |> 
      mutate(across(contains("Counter"), as.numeric)) |> 
      filter(Non_Motorized_Counter == 0, 
             Complex_Counter == 0, 
             Very_Complex_Counter == 0, 
             Simple_Counter == 0) |> 
      dplyr::select(Start_Time, contains("Counter")) |> 
      mutate(Start_Time = openxlsx::convertToDateTime(Start_Time)) |> 
      mutate(Day = lubridate::day(Start_Time),
             Month = lubridate::month(Start_Time)) |> 
      dplyr::select(Month,Day) |> 
      mutate(no_boats_inspected = T)
  ) |> 
  mutate(no_boats_inspected = replace_na(no_boats_inspected, FALSE)) |> 
  mutate(unique_date = paste0(Month,'-',Day)) |> 
  filter(!duplicated(unique_date)) |> 
  filter(no_boats_inspected == T) |> 
  count(at_least_one_boat, sort = T)
# All days with records where no boat were reported also had rows with boats reported.
# I.e. No need to add extra days to sum of unique days with at least 1 boat inspected.
