
dat_orig = dat

dat <- dat |> 
  dplyr::mutate(Station = dplyr::case_when(
    str_detect(Station,"Lower Mainland Roving") ~ "Lower Mainland",
    str_detect(Station,"Penticton Roving") ~ "Penticton",
    str_detect(Station,"Sumas Border") ~ "Sumas",
    T ~ Station
  ))

stations.to.include <- dplyr::case_when(
  grepl("Lower Mainland Roving", stations.to.include) ~ "Lower Mainland",
  grepl("Penticton Roving", stations.to.include) ~ "Penticton",
  grepl("Sumas Border", stations.to.include) ~ "Sumas",
  TRUE ~ stations.to.include
)

old__station_types = station_types
  
station_types = station_types |> 
  dplyr::mutate(Station = dplyr::case_when(
    str_detect(Station,"Lower Mainland Roving") ~ "Lower Mainland",
    str_detect(Station,"Penticton Roving") ~ "Penticton",
    str_detect(Station,"Sumas Border") ~ "Sumas",
    T ~ Station
  ))

fig3_data = dat |> 
  dplyr::filter(Station %in% stations.to.include) |> 
  filter(!Station %in% rovers_to_drop) |> 
  mutate(shift_start_time = openxlsx::convertToDateTime(Start_Time),
         shift_end_time = openxlsx::convertToDateTime(End_Time)) |> 
  group_by(Station,Shift_ID) |> 
  reframe(NumberInsp = n(),
          shift_hours = shift_end_time - shift_start_time) |>
  # If we get some negative values for shift hours, just make those 0...
  dplyr::mutate(shift_hours = ifelse(shift_hours < 0, 0, shift_hours)) |> 
  distinct() |> 
  group_by(Station) |> 
  reframe(NumberInsp = sum(NumberInsp),
          effort_hours = sum(shift_hours)) |> 
  mutate(effort_hours = as.numeric(effort_hours/3600)) |> 
  mutate(encounter_freq = NumberInsp / effort_hours) |> 
  left_join(dat_hr |> 
              filter(Year == my.year) |> 
              group_by(Station) |> 
              summarise(NumberHighRiskInsp = n())
  ) |> 
  #For any stations with 0 HR inspections, replace NA with 0.
  mutate(NumberHighRiskInsp = replace_na(NumberHighRiskInsp, 0)) |> 
  mutate(PercentHR = 100*NumberHighRiskInsp/NumberInsp) |> 
  arrange(desc(NumberInsp)) |> 
  mutate(Station = factor(Station)) |> 
  mutate(Station = fct_inorder(Station)) |> 
  left_join(station_types) |> 
  mutate(StationLabel = ifelse(StationType == "Roving", paste0(Station,"*"),Station)) |> 
  mutate(StationLabel = fct_inorder(StationLabel)) |> 
  mutate(col.width = as.numeric(cut(log10(NumberInsp),3))) |> 
  mutate(col.width = replace(col.width, col.width == 3, 4))

fig3b_data = dat |> 
  dplyr::filter(Station %in% permanent.stations) |> 
  mutate(shift_start_time = openxlsx::convertToDateTime(Start_Time),
         shift_end_time = openxlsx::convertToDateTime(End_Time)) |> 
  group_by(Station,Shift_ID) |> 
  reframe(NumberInsp = n(),
          shift_hours = shift_end_time - shift_start_time) |>
  # If we get some negative values for shift hours, just make those 0...
  dplyr::mutate(shift_hours = ifelse(shift_hours < 0, 0, shift_hours)) |> 
  distinct() |> 
  group_by(Station) |> 
  reframe(NumberInsp = sum(NumberInsp),
          effort_hours = sum(shift_hours)) |> 
  mutate(effort_hours = as.numeric(effort_hours/3600)) |> 
  mutate(encounter_freq = NumberInsp / effort_hours) |> 
  left_join(dat_hr |> 
              filter(Year == my.year) |> 
              group_by(Station) |> 
              summarise(NumberHighRiskInsp = n())
  ) |> 
  #For any stations with 0 HR inspections, replace NA with 0.
  mutate(NumberHighRiskInsp = replace_na(NumberHighRiskInsp, 0)) |> 
  mutate(PercentHR = 100*NumberHighRiskInsp/NumberInsp) |> 
  arrange(desc(NumberInsp)) |> 
  mutate(Station = factor(Station)) |> 
  mutate(Station = fct_inorder(Station)) |> 
  left_join(station_types) |> 
  mutate(StationLabel = ifelse(StationType == "Roving", paste0(Station,"*"),Station)) |> 
  mutate(StationLabel = fct_inorder(StationLabel)) |> 
  mutate(col.width = as.numeric(cut(log10(NumberInsp),3))) |> 
  mutate(col.width = replace(col.width, col.width == 3, 4))

fig3c_data = dat |> 
  dplyr::filter(Station %in% stations.to.include) |> 
  dplyr::filter(!Station %in% permanent.stations) |> 
  dplyr::filter(!Station %in% rovers_to_drop) |> 
  mutate(shift_start_time = openxlsx::convertToDateTime(Start_Time),
         shift_end_time = openxlsx::convertToDateTime(End_Time)) |> 
  group_by(Station,Shift_ID) |> 
  reframe(NumberInsp = n(),
          shift_hours = shift_end_time - shift_start_time) |>
  # If we get some negative values for shift hours, just make those 0...
  dplyr::mutate(shift_hours = ifelse(shift_hours < 0, 0, shift_hours)) |> 
  distinct() |> 
  group_by(Station) |> 
  reframe(NumberInsp = sum(NumberInsp),
          effort_hours = sum(shift_hours)) |> 
  mutate(effort_hours = as.numeric(effort_hours/3600)) |> 
  mutate(encounter_freq = NumberInsp / effort_hours) |> 
  left_join(dat_hr |> 
              filter(Year == my.year) |> 
              group_by(Station) |> 
              summarise(NumberHighRiskInsp = n())
  ) |> 
  #For any stations with 0 HR inspections, replace NA with 0.
  mutate(NumberHighRiskInsp = replace_na(NumberHighRiskInsp, 0)) |> 
  mutate(PercentHR = 100*NumberHighRiskInsp/NumberInsp) |> 
  arrange(desc(NumberInsp)) |> 
  mutate(Station = factor(Station)) |> 
  mutate(Station = fct_inorder(Station)) |> 
  left_join(station_types) |> 
  mutate(StationLabel = ifelse(StationType == "Roving", paste0(Station,"*"),Station)) |> 
  mutate(StationLabel = fct_inorder(StationLabel)) |> 
  mutate(col.width = as.numeric(cut(log10(NumberInsp),3))) |> 
  mutate(col.width = replace(col.width, col.width == 3, 4))

p3.x = dat_all |> 
  filter(Year %in% c((my.year-2):my.year)) |> 
  # filter(Station %in% c('Golden','Olsen',
  #                       'Yahk','Radium',
  #                       'Mt. Robson','Osoyoos',
  #                       'Dawson Creek','Pacific')) |>
  filter(Station %in% stations.to.include) |> 
  group_by(Year, Station) |> 
  summarise(NumberInsp = n()) |> 
  group_by(Station) |> 
  mutate(TotalInsp = sum(NumberInsp)) |> 
  ungroup() |> 
  arrange(desc(TotalInsp),desc(NumberInsp)) |> 
  mutate(Station = fct_inorder(Station)) |> 
  dplyr::mutate(Year = as.character(Year)) |> 
  ggplot() + 
  geom_col(aes(x=Station,y=NumberInsp,fill=Year), width = 0.40,position=position_dodge(preserve = "single")) +
  scale_y_continuous(name = "Number of Inspections",
                     breaks = c(0,1000,2500,5000,7500,10000,15000)) +
  theme_classic() +
  labs(x = "") +
  theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1)) + 
  scale_fill_brewer(palette = "Dark2")

p3.x_b = dat_all |> 
  filter(Year %in% c((my.year-2):my.year)) |> 
  filter(Station %in% permanent.stations) |> 
  group_by(Year, Station) |> 
  summarise(NumberInsp = n()) |> 
  group_by(Station) |> 
  mutate(TotalInsp = sum(NumberInsp)) |> 
  ungroup() |> 
  arrange(desc(TotalInsp),desc(NumberInsp)) |> 
  mutate(Station = fct_inorder(Station)) |> 
  dplyr::mutate(Year = as.character(Year)) |> 
  ggplot() + 
  geom_col(aes(x=Station,y=NumberInsp,fill=Year), width = 0.40,position=position_dodge(preserve = "single")) +
  scale_y_continuous(name = "Number of Inspections",
                     breaks = c(0,1000,2500,5000,7500,10000,15000)) +
  theme_classic() +
  labs(x = "") +
  theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1)) + 
  scale_fill_brewer(palette = "Dark2")

p3.x_c = dat_all |> 
  filter(Year %in% c((my.year-2):my.year)) |> 
  dplyr::filter(Station %in% roving.stations) |> 
  group_by(Year, Station) |> 
  summarise(NumberInsp = n()) |> 
  group_by(Station) |> 
  mutate(TotalInsp = sum(NumberInsp)) |> 
  ungroup() |> 
  arrange(desc(TotalInsp),desc(NumberInsp)) |> 
  mutate(Station = fct_inorder(Station)) |> 
  dplyr::mutate(Year = as.character(Year)) |> 
  ggplot() + 
  geom_col(aes(x=Station,y=NumberInsp,fill=Year), width = 0.40,position=position_dodge(preserve = "single")) +
  scale_y_continuous(name = "Number of Inspections",
                     breaks = c(0,1000,2500,5000,7500,10000,15000)) +
  theme_classic() +
  labs(x = "") +
  theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1)) + 
  scale_fill_brewer(palette = "Dark2")

p4 = dat |>
  dplyr::filter(Station %in% stations.to.include) |> 
  filter(!Station %in% rovers_to_drop) |> 
  group_by(Station,Previous_Waterbody_1_Province_Or_State) |> 
  summarise(Different_Sources = n()) |> 
  summarise(TotalSources = n()) |> 
  arrange(desc(TotalSources)) |> 
  left_join(station_types) |> 
  mutate(Station = as.factor(Station)) |> 
  mutate(StationLabel = paste0(Station,
                               ifelse(StationType == "Roving", "*", ""))) |> 
  mutate(StationLabel = fct_reorder(StationLabel, -TotalSources)) |> 
  ggplot() + 
  geom_col(aes(x=StationLabel,y=TotalSources), fill = my.grey, width = 0.40) +
  geom_text(aes(x=StationLabel,y=TotalSources+0.05*max(TotalSources),label=TotalSources)) +
  labs(x = '', y = "Number of Origin Jurisdictions") + 
  theme_classic() + 
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1))

p5 = ggarrange(
  dat |> 
    mutate(the.month = month(TimeOfInspection,label=T,abbr=F)) |> 
    dplyr::filter(!(the.month %in% c("March","November") & Year == 2024)) |> 
    #Calculate the number of inspections for each month
    group_by(the.month) |> 
    summarise(Number_Insp = n()) |> 
    ggplot() + 
    geom_col(aes(x=the.month,y=Number_Insp),fill = my.grey,width = 0.40) +
    geom_text(aes(x=the.month,y=Number_Insp+max(Number_Insp)*0.05,
                  label=round(Number_Insp,0))) +
    theme_classic() +
    labs(x = "", y = "Watercraft Encounters (# of inspections)") +
    theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1)),
  
  dat |> 
    filter(!Station %in% rovers_to_drop) |> 
    mutate(the.month = month(TimeOfInspection,label=T,abbr=F)) |> 
    dplyr::filter(!(the.month %in% c("March","November") & Year == 2024)) |> 
    group_by(the.month) |> 
    select(the.month,Shift_ID,Shift_hours) |> 
    distinct() |> 
    #Some shifts report negative # of hours... remove those.
    filter(Shift_hours > 0) |> 
    summarise(TotalEffort = sum(Shift_hours)) |> 
    ggplot() + 
    geom_col(aes(x=the.month,y=TotalEffort),fill = my.grey,width = 0.40) +
    geom_text(aes(x=the.month,y=TotalEffort+max(TotalEffort)*0.05,
                  label=round(TotalEffort,0))) +
    theme_classic() +
    labs(x = "", y = "Total Effort (hours)") +
    theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1)),
  ncol = 2, nrow = 1)

p6 = dat |> 
  mutate(the.month = month(TimeOfInspection,label=T,abbr=F)) |> 
  mutate(the.day = day(TimeOfInspection)) |> 
  filter(!the.month %in% c("January","February","December")) |> 
  #Calculate the number of inspections for each month
  group_by(the.month,the.day) |> 
  summarise(Number_Insp = n()) |> 
  dplyr::mutate(total_per_month = sum(Number_Insp)) |> 
  dplyr::filter(total_per_month > 10) |> 
  dplyr::select(-total_per_month) |> 
  left_join(
    dat |> 
      mutate(the.month = month(TimeOfInspection,label=T,abbr=F)) |> 
      mutate(the.day = day(TimeOfInspection)) |> 
      group_by(the.month,the.day) |> 
      select(the.month,the.day,Shift_ID,Shift_hours) |> 
      distinct() |> 
      #Some shifts report negative # of hours... remove those.
      filter(Shift_hours > 0) |> 
      summarise(TotalEffort = sum(Shift_hours))
  ) |> 
  mutate(EncounterFreq = Number_Insp/TotalEffort) |>
  mutate(StandardError = plotrix::std.error(EncounterFreq)) |> 
  #Summarise for each month
  group_by(the.month,StandardError) |> 
  summarise(EncounterFreq = mean(EncounterFreq)) |> 
  #If the bottom half of the errorbar would be below 0, change to 0.
  mutate(StandardError = case_when(
    EncounterFreq - StandardError <= 0 ~ EncounterFreq,
    T ~ StandardError
  )) |> 
  ggplot() + 
  geom_col(aes(x=the.month,y=EncounterFreq),fill = my.grey,width = 0.40) +
  geom_errorbar(aes(x=the.month,
                    ymin = EncounterFreq-StandardError,
                    ymax = EncounterFreq+StandardError),width = 0.15) +
  
  geom_text(aes(x=the.month,y=EncounterFreq,
                label=round(EncounterFreq,1)),nudge_x = 0.4) +
  theme_classic() +
  labs(x = "", y = "Encounter Frequency") +
  scale_y_continuous(breaks = seq(0,6.5,0.5))

p7 = ggarrange(dat |> 
                 mutate(the.day = wday(TimeOfInspection,label=T,abbr=F,week_start = getOption("lubridate.week.start", 1))) |> 
                 #Calculate the number of inspections for each month
                 group_by(the.day) |> 
                 summarise(Number_Insp = n()) |> 
                 ggplot() + 
                 geom_col(aes(x=the.day,y=Number_Insp),fill = my.grey,width = 0.40) +
                 geom_text(aes(x=the.day,y=Number_Insp+max(Number_Insp)*0.05,
                               label=round(Number_Insp,0))) +
                 theme_classic() +
                 labs(x = "", y = "Watercraft Encounters (# of inspections)") +
                 theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1)),
               dat |> 
                 mutate(the.day = wday(TimeOfInspection,label=T,abbr=F,week_start = getOption("lubridate.week.start", 1))) |> 
                 group_by(the.day) |> 
                 select(the.day,Shift_ID,Shift_hours) |> 
                 distinct() |> 
                 #Some shifts report negative # of hours... remove those.
                 filter(Shift_hours > 0) |> 
                 summarise(TotalEffort = sum(Shift_hours)) |> 
                 ggplot() + 
                 geom_col(aes(x=the.day,y=TotalEffort),fill = my.grey,width = 0.40) +
                 geom_text(aes(x=the.day,y=TotalEffort+max(TotalEffort)*0.05,
                               label=round(TotalEffort,0))) +
                 theme_classic() +
                 labs(x = "", y = "Total Effort (hours)") +
                 theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1)),
               ncol = 2, nrow = 1)

p8 = dat |> 
  # Drop scheduled inspections?
  filter(!str_detect(Station,"Scheduled")) |> 
  # Replace anything less than 1 hour for shift duration with a 1
  mutate(Shift_hours = ifelse(Shift_hours < 1, 1, Shift_hours)) |> 
  mutate(the.day = wday(TimeOfInspection,
                        label=T,abbr=F,
                        week_start = getOption("lubridate.week.start", 1)),
         the.week = week(TimeOfInspection),
         the.month = month(TimeOfInspection)
  ) |>
  # Drop inspections in January and February
  dplyr::filter(the.month > 2) |> 
  # Only keep rows where shift hours was more than 1 hour.
  # filter(Shift_hours > 1) |> 
  #Calculate the number of inspections for each month
  group_by(the.day,
           Shift_ID) |> 
  reframe(Number_Insp = n(),
          Shift_hours
  ) |> 
  dplyr::distinct() |> 
  mutate(enc_freq = Number_Insp/Shift_hours) |> 
  group_by(the.day) |> 
  reframe(EncounterFreq = mean(enc_freq),
          StandardError = plotrix::std.error(enc_freq)) |> 
  ggplot() + 
  geom_col(aes(x=the.day,y=EncounterFreq),fill = my.grey,width = 0.40) +
  geom_errorbar(aes(x=the.day,
                    ymin = EncounterFreq-StandardError,
                    ymax = EncounterFreq+StandardError),width = 0.15) +
  geom_text(aes(x=the.day,y=EncounterFreq,
                label=round(EncounterFreq,1)),nudge_x = 0.4) +
  theme_classic() +
  labs(x = "", y = "Encounter Frequency")

p9 = dat |> 
  mutate(the.hour = hour(TimeOfInspection)) |>
  group_by(the.hour) |> 
  summarise(Number_Insp = n()) |> 
  ggplot() + 
  geom_col(aes(x=the.hour,y=Number_Insp),fill = my.grey,
           width = 0.40) +
  # geom_text(
  #   size = 3.5,
  #   aes(x=the.hour,y=Number_Insp+max(Number_Insp)*0.05,
  #       label=round(Number_Insp,0))#,
  #   # min.segment.length = 0.1,
  #   # force = 0.1,
  #   # force_pull = 100
  # ) +
  geom_text_repel(
    size = 3.5,
    aes(x=the.hour,y=Number_Insp,#+max(Number_Insp)*0.05,
        label=round(Number_Insp,0)),
    min.segment.length = 0.1,
    force = .01,
    force_pull = 100,
    nudge_y = 100
  ) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0,23,1)) +
  labs(x = "", y = "Watercraft Encounters (# of inspections)")

p9.2 = dat |> 
  filter(High_Risk_AIS_Ind == T) |> 
  mutate(the.hour = hour(TimeOfInspection)) |>
  group_by(the.hour) |> 
  summarise(Number_Insp = n()) |> 
  ggplot() + 
  geom_col(aes(x=the.hour,y=Number_Insp),fill = my.grey,
           width = 0.40) +
  geom_text(aes(x=the.hour,y=Number_Insp+max(Number_Insp)*0.05,
                label=round(Number_Insp,0))) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0,23,1)) +
  labs(x = "", y = "Watercraft Encounters (# of inspections)")

p9.3 = dat |> 
  filter(Station == "Golden") |> 
  mutate(the.hour = hour(TimeOfInspection)) |>
  group_by(the.hour) |> 
  summarise(Number_Insp = n()) |> 
  ggplot() + 
  geom_col(aes(x=the.hour,y=Number_Insp),fill = my.grey,
           width = 0.40) +
  geom_text(aes(x=the.hour,y=Number_Insp+max(Number_Insp)*0.05,
                label=round(Number_Insp,0))) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0,23,1)) +
  labs(x = "", y = "Watercraft Encounters (# of inspections)")

p9.4 = dat |> 
  filter(High_Risk_AIS_Ind == T,
         Station == "Golden") |> 
  mutate(the.hour = hour(TimeOfInspection)) |>
  group_by(the.hour) |> 
  summarise(Number_Insp = n()) |> 
  ggplot() + 
  geom_col(aes(x=the.hour,y=Number_Insp),fill = my.grey,
           width = 0.40) +
  geom_text(aes(x=the.hour,y=Number_Insp+max(Number_Insp)*0.05,
                label=round(Number_Insp,0))) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0,23,1)) +
  labs(x = "", y = "Watercraft Encounters (# of inspections)")

p11 = dat |> 
  filter(!is.na(Destination_Waterbody_1_Name)) |> 
  group_by(Destination_Waterbody_1_Name) |> 
  summarise(NumberInsp = n()) |> 
  arrange(desc(NumberInsp)) |> 
  mutate(TotalInsp = sum(NumberInsp)) |> 
  mutate(Percent = 100*NumberInsp/TotalInsp) |> 
  slice(1:15) |> 
  mutate(Destination_Waterbody_1_Name = as.factor(Destination_Waterbody_1_Name)) |>
  ggplot() + 
  geom_col(aes(x=reorder(Destination_Waterbody_1_Name,-Percent),
               y=Percent),fill = my.grey, width = 0.40) +
  geom_text(aes(x=Destination_Waterbody_1_Name,
                y=Percent+max(Percent)*0.05,
                label=paste0(round(Percent,1),"%"))) +
  theme_classic() +
  labs(x = "",y = "Percent of Total Inspections") + 
  theme(axis.text.x = element_text(angle = 45,hjust=1,vjust=1))
