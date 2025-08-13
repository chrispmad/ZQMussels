# Pull out different boat launches, scheduled inspections, etc. from,
# mostly, Penticton Roving and Fraser Valley Roving.
dat_all = dat_all |> 
  dplyr::mutate(Station = dplyr::case_when(
    # Pull out boat launches.
    # 1. Scheduled Inspections / Decons
    Station %in% c("Fraser Valley Roving","Penticton Roving") & str_detect(Shift_Start_Comment, '([iI]nspection|[dD]econ|[dD]eacon|Deco|[sS]cheduled)') ~ 'Scheduled Inspection/Decontamination',
    Station == 'Scheduled Inspection' & str_detect(Shift_Start_Comment,"([r,R]ichmond|[d,D]ockside)") ~ 'Lower Mainland Scheduled Inspection',
    Station == 'Scheduled Inspection' & str_detect(Shift_Start_Comment,"([l,L]ake [c,C]ountry|[l,L]akehouse|Martin|martin|MARTIN|[p,P]enticton|[k,K]elowna)") ~ 'Penticton Scheduled Inspection',
    # 2. Multiple boat launches visited...
    str_detect(Shift_Start_Comment,"([kK]ekuli|[kK]akuli).*([gG]elatley|[gG]ellatley)") ~ 'Multiple',
    str_detect(Shift_Start_Comment,"([gG]elatley|[gG]ellatley).*([kK]ekuli|[kK]akuli)") ~ 'Multiple',
    str_detect(Shift_Start_Comment,"Skaha") & str_detect(Shift_Start_Comment,"summerland")~ "Boat Launch - Multiple",
    # 3. Single boat launch visited.
    # Okanagan Boat Launch mentions Kelowna and then kabuki bay, Eldorado boat launch, etc.
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
    # No specific boat launch mentioned, but the words "boat launch" mentioned.
    stringr::str_detect(Shift_Start_Comment,"[bB]oat( )?[lL]aunch") ~ 'Boat Launch',
    # Pull out specific stations
    str_detect(Shift_Start_Comment, '(hwy|Hwy)?( )?97( )?[cC]') ~ 'Penticton 97C',
    str_detect(Shift_Start_Comment, '(hwy|Hwy)( )?3') ~ 'Hwy 3',
    str_detect(Shift_Start_Comment, '[kK]eremeos') ~ 'Keremeos (Hwy 3)',
    T ~ Station
  ))