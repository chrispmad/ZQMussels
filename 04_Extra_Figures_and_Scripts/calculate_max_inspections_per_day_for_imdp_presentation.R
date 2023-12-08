library(tidyverse)
library(openxlsx)
library(knitr)
library(sf)
library(plotly)

knitr::opts_chunk$set(echo = F,warning = F, message = F)

# Read in options file.
my_opts = read_csv('C:/Users/CMADSEN/Downloads/LocalR/long_term_projects/ZQMussels/Options.csv')
all_dat = openxlsx::read.xlsx(paste0(my_opts$zqm_figure_local_folder,'data/figure_dat_all.xlsx'))

# Find number of unique years of data.
unique_years = unique(all_dat$Year)

# Find most recent year.
most_rec_year = max(as.numeric(all_dat$Year),na.rm=T)

# Stations Labels
station_labels = tibble(name = c('Olsen','Osoyoos','Pacific'),
                        label = c('Olsen (Hwy 3)','Osoyoos*','Pacific Border'))

# Replace original station names with labels.
all_dat = all_dat |> 
  rowwise() |> 
  mutate(Station = ifelse(Station %in% station_labels$name,
                          station_labels[station_labels$name == Station,]$label,
                          Station))

# Station filter selection - only applied to certain figures!
stations_to_include = c("Golden","Yahk","Olsen (Hwy 3)","Radium","Mt. Robson","Osoyoos*","Pacific Border","Sumas Border","Dawson Creek","Penticton Roving","Fraser Valley Roving","Keremeos (Hwy 3)","Penticton 97C","Fraser Valley Cultus")

# Pull out Highway 97C from Penticton Roving. Hopefully this is worth it.
pent = all_dat |> 
  filter(Station == 'Penticton Roving',
         Year == 2023)

pent = pent |> 
  mutate(Station = case_when(
    str_detect(Shift_Start_Comment, '(hwy|Hwy|97)') ~ 'Penticton 97C',
    !str_detect(Shift_Start_Comment, '(hwy|Hwy|97)') ~ 'Penticton Roving',
    T ~ Station
  ))

all_dat = all_dat |> 
  filter(!(Station == 'Penticton Roving' & 
             Year == 2023)) |> 
  bind_rows(pent)

# Same for Fraser Valley Roving - Cultus Lake.
cult = all_dat |> 
  filter(Year == 2023) |> 
  filter(Station == 'Fraser Valley Roving') |> 
  filter(str_detect(Shift_Start_Comment, 'Cultus')) |> 
  mutate(Station = 'Fraser Valley Cultus')

all_dat = all_dat |> 
  filter(!(Year == 2023 & Station == 'Fraser Valley Roving' & str_detect(Shift_Start_Comment, 'Cultus'))) |> 
  bind_rows(cult)

max_per_day = all_dat |>
  filter(Station %in% stations_to_include) |>
  filter(Year %in% c(2020:most_rec_year))

max_per_day = max_per_day |> 
  mutate(TimeOfInspection = openxlsx::convertToDateTime(TimeOfInspection))

max_per_day

max_per_day[c(1:100),] |> 
  select(Station, Year, TimeOfInspection) |> 
  mutate(Day = lubridate::yday(TimeOfInspection)) |> 
  group_by(Station, Year, Day) |> 
  reframe(n = n(),
          Day) |> 
  count(Station, Year, Day) |>
  group_by(Station, Year) |>
  slice_max(n) |>
  mutate(Year = as.character(Year)) |>
  mutate(Year = factor(Year, levels = c(2020:most_rec_year))) |>
  group_by(Station) |>
  mutate(station_total = sum(n)) |>
  ungroup() |>
  arrange(desc(station_total)) |>
  mutate(Station = as.factor(Station)) |>
  mutate(Station = forcats::fct_inorder(Station))

readr::write_csv(max_per_day, 'C:/Users/CMADSEN/Downloads/LocalR/long_term_projects/ZQMussels/04_Extra_Figures_and_Scripts/data/max_records_per_day.csv')
