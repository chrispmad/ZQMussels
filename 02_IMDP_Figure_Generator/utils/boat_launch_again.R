
# Stations Labels
station_labels = tibble(name = c('Olsen','Osoyoos','Pacific'),
                        label = c('Olsen (Hwy 3)','Osoyoos*','Pacific Border'))

# Replace original station names with labels.
# dat_all = dat_all |> 
#   rowwise() |> 
#   mutate(Station = ifelse(Station %in% station_labels$name,
#                           station_labels[station_labels$name == Station,]$label,
#                           Station)) |> 
#   dplyr::ungroup()

# Pull out different boat launches, scheduled inspections, etc. from,
# mostly, Penticton Roving and Fraser Valley Roving.
# Apply the detailed Station recoding logic
boatlaunch_dat <- dat %>%
  dplyr::mutate(Station = dplyr::case_when(
    # 1. Scheduled Inspections / Decons
    Station %in% c("Fraser Valley Roving","Penticton Roving") & str_detect(Shift_Start_Comment, '([iI]nspection|[dD]econ|[dD]eacon|Deco|[sS]cheduled)') ~ 'Scheduled Inspection/Decontamination',
    Station == 'Scheduled Inspection' & str_detect(Shift_Start_Comment,"([r,R]ichmond|[d,D]ockside)") ~ 'Lower Mainland Scheduled Inspection',
    Station == 'Scheduled Inspection' & str_detect(Shift_Start_Comment,"([l,L]ake [c,C]ountry|[l,L]akehouse|Martin|martin|MARTIN|[p,P]enticton|[k,K]elowna)") ~ 'Penticton Scheduled Inspection',
    
    # 2. Multiple boat launches visited
    str_detect(Shift_Start_Comment,"([kK]ekuli|[kK]akuli).*([gG]elatley|[gG]ellatley)") ~ 'Multiple',
    str_detect(Shift_Start_Comment,"([gG]elatley|[gG]ellatley).*([kK]ekuli|[kK]akuli)") ~ 'Multiple',
    str_detect(Shift_Start_Comment,"Skaha") & str_detect(Shift_Start_Comment,"summerland")~ "Boat Launch - Multiple",
    
    # 3. Single boat launch visited
    str_detect(Shift_Start_Comment,".*kabuki bay.*Eldorado") ~ 'Boat Launch - Okanagan Lake',
    str_detect(Shift_Start_Comment,"([kK]ekuli|[kK]akuli)") ~ 'Boat Launch - Kekuli Bay',
    str_detect(Shift_Start_Comment,"([gG]ellatly|[gG]ellatley)") ~ 'Boat Launch - Gellatly Bay',
    str_detect(Shift_Start_Comment,"([cC]ultus)") ~ 'Boat Launch - Cultus Lake',
    str_detect(Shift_Start_Comment,"([aA]l(l)?ouette)") ~ 'Boat Launch - Alouette Lake',
    str_detect(Shift_Start_Comment,"([sS]tave)") ~ 'Boat Launch - Stave Lake',
    str_detect(Shift_Start_Comment,"([kK]awkawa)") ~ 'Boat Launch - Kawkawa Lake',
    str_detect(Shift_Start_Comment,"[cC]hristina") ~ "Boat Launch - Christina Lake",
    Station == 'Christina Lake' ~ 'Boat Launch - Christina Lake',
    str_detect(Shift_Start_Comment,"[sS]kaha") ~ "Boat Launch - North Skaha",
    str_detect(Shift_Start_Comment,"[pP]each") ~ "Boat Launch - Peachland Yacht Club",
    str_detect(Shift_Start_Comment,"(Summerland|summer( )?land|Summer( )?[lL]and)") ~ "Boat Launch - Summerland",
    
    # Generic boat launch mention
    str_detect(Shift_Start_Comment,"[bB]oat( )?[lL]aunch") ~ 'Boat Launch',
    
    # Other specific stations
    str_detect(Shift_Start_Comment, '(hwy|Hwy)?( )?97( )?[cC]') ~ 'Penticton 97C',
    str_detect(Shift_Start_Comment, '(hwy|Hwy)( )?3') ~ 'Hwy 3',
    str_detect(Shift_Start_Comment, '[kK]eremeos') ~ 'Keremeos (Hwy 3)',
    
    TRUE ~ Station
  ))

boatlaunch_dat <- boatlaunch_dat %>%
  mutate(likely_lake = case_when(
    str_detect(Station, "Kekuli") ~ "Kekuli Bay",
    str_detect(Station, "Gellatly") ~ "Gellatly Bay",
    str_detect(Station, "Cultus") ~ "Cultus Lake",
    str_detect(Station, "Alouette") ~ "Alouette Lake",
    str_detect(Station, "Stave") ~ "Stave Lake",
    str_detect(Station, "Kawkawa") ~ "Kawkawa Lake",
    str_detect(Station, "Christina") ~ "Christina Lake",
    str_detect(Station, "Skaha") ~ "North Skaha",
    str_detect(Station, "Peach") ~ "Peachland Yacht Club",
    str_detect(Station, "Summerland") ~ "Summerland",
    TRUE ~ NA_character_
  ))

boatlaunch_dat_count <- boatlaunch_dat %>%
  count(Station, High_Risk_AIS_Ind, likely_lake) %>%
  mutate(likely_lake = replace_na(likely_lake, 'Unknown Location')) %>%
  arrange(desc(n)) %>%
  mutate(likely_lake = as.factor(likely_lake)) %>%
  mutate(likely_lake = forcats::fct_inorder(likely_lake))

boatlaunch_dat_count <- boatlaunch_dat_count |> 
  filter(likely_lake != "Unknown Location")
