boatlaunch_dat = dat |> 
  filter(Station %in% c("Penticton","Lower Mainland")) |> 
  filter(str_detect(Shift_Start_Comment,'(lake|Lake|boat l|Boat L)')) |> 
  mutate(likely_lake = str_extract(Shift_Start_Comment,"[a-zA-Z]+(?= ((l|L)ake|(b|B)ay))")) |> 
  # dplyr::select(Station,TimeOfInspection,High_Risk_AIS_Ind,Shift_Start_Comment,likely_lake) |> 
  mutate(likely_lake = str_to_title(likely_lake)) |> 
  mutate(likely_lake = case_when(
    str_detect(Shift_Start_Comment,"summer land") ~ "Summerland",
    str_detect(Shift_Start_Comment,"Island 22") ~ "Island 22",
    str_detect(likely_lake,"(Kekuli|Kakuli)") ~ "Kekuli Bay",
    T ~ likely_lake
  )) 

boatlaunch_dat_count = boatlaunch_dat |> 
  dplyr::select(Station,TimeOfInspection,High_Risk_AIS_Ind,Shift_Start_Comment,likely_lake) |> 
  count(Station,High_Risk_AIS_Ind,likely_lake) |> 
  mutate(likely_lake = replace_na(likely_lake, 'Unknown Location')) |> 
  arrange(desc(n)) |> 
  mutate(likely_lake = as.factor(likely_lake)) |> 
  mutate(likely_lake = forcats::fct_inorder(likely_lake))
