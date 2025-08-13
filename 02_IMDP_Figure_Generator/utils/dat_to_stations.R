# Join number of inspections to stations.
st = stations_for_maps |> 
  dplyr::filter(!station_name %in% rovers_to_drop) |>
  # dplyr::filter(station_name %in% permanent.stations) |> 
  left_join(dat |> 
              count(Station, name = 'num_insp') |> 
              dplyr::rename(station_name = Station)) |> 
  filter(!is.na(num_insp))

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
  arrange(num_insp_b) |> 
  mutate(my_text_size = ifelse(str_detect(map_label,"(Roving|Border)+$"), 'small', 'medium'))

 stations_sf = st_as_sf(st, coords = c("lon","lat"), crs = 4326)

 
 # Join number of inspections to stations.
 st_ps = st |> 
   dplyr::filter(station_type != 'Roving Inspection Station') |> 
   dplyr::filter(station_name %in% permanent.stations) |> 
   dplyr::select(!contains('jenks'))
 
 # Get 5 levels of natural breaks ('jenks'); station circle size on map varies with this variable.
 st_ps = st_ps |> 
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
   arrange(num_insp_b) |> 
   mutate(my_text_size = ifelse(str_detect(map_label,"(Roving|Border)+$"), 'small', 'medium'))
 
 first_level = as.character(st_ps$jenks_1[1])
 second_level = as.character(st_ps$jenks_1[2])
 
 if(first_level == second_level){
   st_ps = st_ps |> 
     mutate(bin_label = str_replace_all(bin_label,
                                        paste0(first_level, ' - ',second_level),
                                        first_level))
 }
 
 stations_ps_sf = st_as_sf(st_ps, coords = c("lon","lat"), crs = 4326)
 
 station_labels_sf = station_labels |>
   dplyr::filter(station_name %in% stations_ps_sf$station_name) |>
   st_as_sf(coords = c("lon", "lat"), crs = 4326)
 
 
 #roving stations - join on the data
 st_rs = stations_roving |> 
   left_join(dat |> 
               count(Station, name = 'num_insp') |> 
               dplyr::rename(station_name = Station)) |> 
   filter(!is.na(num_insp))
 
 
 # Get 5 levels of natural breaks ('jenks'); station circle size on map varies with this variable.
 st_rs = st_rs |> 
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
   arrange(num_insp_b) |> 
   mutate(my_text_size = ifelse(str_detect(map_label,"(Roving|Border)+$"), 'small', 'medium'))
 
 first_level = as.character(st_rs$jenks_1[1])
 second_level = as.character(st_rs$jenks_1[2])
 
 stations_rs_sf = st_as_sf(st_rs, coords = c("lon","lat"), crs = 4326)
 
 
 station_labels_roving= station_labels_roving |> 
   filter(station_name %in% stations_rs_sf$station_name)
 
 station_labels_sf = station_labels |>
   dplyr::filter(station_name %in% stations_rs_sf$station_name) |>
   st_as_sf(coords = c("lon", "lat"), crs = 4326)