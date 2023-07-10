# Libraries

library(tidyverse)

# Read in a year of metabase data.

dat = read_csv('04_Extra_Figures_and_Scripts/data/metabase_2023.csv')

# Make some plots!

# Missing information for destination waterbody name / closest city.
dest_dat = dat |> 
  mutate(`Destination Major City` = replace(`Destination Major City`,`Destination Major City` == 'None', NA)) |> 
  mutate(destination_any_info = coalesce(`Destination Waterbody 1 Name`,`Destination Waterbody 1 Closest City`,`Destination Major City`)) |> 
  count(destination_any_info, sort = T) |> 
  mutate(something_known = !is.na(destination_any_info)) |> 
  group_by(something_known) |> 
  mutate(total_n = sum(n)) |> 
  dplyr::select(something_known, total_n) |> 
  distinct()

ggplot(dest_dat) + 
  geom_col(aes(x = something_known, y = total_n)) + 
  labs(y = 'Number of Records', x = 'Data for Destination WB name / closest city / "destination closest city"')

# Source information
# Missing information for destination waterbody name / closest city.
prev_dat = dat |> 
  mutate(`Previous Major City` = replace(`Previous Major City`,`Previous Major City` == 'None', NA)) |> 
  mutate(Previous_any_info = coalesce(`Previous Waterbody 1 Name`,`Previous Waterbody 1 Closest City`,`Previous Major City`)) |> 
  count(Previous_any_info, sort = T) |> 
  mutate(something_known = !is.na(Previous_any_info)) |> 
  group_by(something_known) |> 
  mutate(total_n = sum(n)) |> 
  dplyr::select(something_known, total_n) |> 
  distinct()

ggplot(prev_dat) + 
  geom_col(aes(x = something_known, y = total_n)) + 
  labs(y = 'Number of Records', x = 'Data for either Previous WB name / closest city / "previous major city"')
