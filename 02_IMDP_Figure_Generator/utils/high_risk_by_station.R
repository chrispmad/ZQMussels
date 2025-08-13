# Join number of inspections to stations.
st = stations |> 
  left_join(dat |> 
              filter(High_Risk_AIS_Ind == TRUE) |> 
              count(Station, name = 'num_insp') |> 
              dplyr::rename(station_name = Station)) |> 
  filter(!is.na(num_insp))

# Get 5 levels of natural breaks ('jenks'); station circle size on map varies with this variable.
st = st |> 
  mutate(jenks = list(BAMMtools::getJenksBreaks(num_insp, k = 6))) |> 
  unnest_wider(jenks, names_sep = '_') |> 
  mutate(num_insp_b = case_when(
    num_insp <= jenks_2 ~ 1,
    num_insp <= jenks_3 ~ 2,
    num_insp <= jenks_4 ~ 3,
    num_insp <= jenks_5 ~ 4,
    num_insp <= jenks_6 ~ 5,
  )) |>
  mutate(bin_label = case_when(
    num_insp_b == 1 ~ paste0(jenks_1, ' - ', jenks_2),
    num_insp_b == 2 ~ paste0(jenks_2+1, ' - ', jenks_3),
    num_insp_b == 3 ~ paste0(jenks_3+1, ' - ', jenks_4),
    num_insp_b == 4 ~ paste0(jenks_4+1, ' - ', jenks_5),
    num_insp_b == 5 ~ paste0(jenks_5+1, ' - ', jenks_6)
  )) |> 
  # mutate(num_insp_b = factor(num_insp_b, levels = c(1:5))) |> 
  arrange(num_insp_b) |> 
  mutate(my_text_size = ifelse(str_detect(map_label,"(Roving|Border)+$"), 'small', 'medium'))

stations_sf = st_as_sf(st, coords = c("lon","lat"), crs = 4326)