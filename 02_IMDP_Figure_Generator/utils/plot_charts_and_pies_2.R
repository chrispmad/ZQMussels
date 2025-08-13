
p13 = dat_w_blowbys |> 
  dplyr::rename(m = Motorized_Blow_Bys_Counter,
                nonm = Non_Motorized_Blow_Bys_Counter) |> 
  mutate(Total_Blowbys = nonm + m) |> 
  group_by(Station) |> 
  reframe(`Non-Motorized` = 100*nonm/Total_Blowbys,
          Motorized = 100*m/Total_Blowbys) |> 
  arrange(desc(`Non-Motorized`)) |> 
  dplyr::filter(!is.nan(Motorized)) |> 
  #mutate(Station = factor(Station, levels = .$Station)) |> 
  pivot_longer(cols = -Station) |>
  arrange(desc(value),Station) |> 
  left_join(station_types) |> 
  mutate(StationLabel = paste0(Station,
                               ifelse(StationType == "Roving", "*", ""))) |> 
  mutate(StationLabel = fct_inorder(StationLabel)) |>
  ggplot() + 
  geom_col(aes(x=StationLabel,
               y=value,
               fill = name),
           col = "black",
           width = 0.40,
           position = "stack") +
  theme_classic() +
  labs(x = "",y = "Percent Blow-bys", fill = "") + 
  scale_fill_manual(values = c("Non-Motorized" = my.grey,
                               "Motorized" = "grey")) +
  scale_y_continuous(breaks = seq(0,100,10), labels = paste0(seq(0,100,10),"%")) +
  theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1))

p14_dat = dat |> 
  dplyr::filter(Station %in% stations.to.include) |>
  # filter(!Station %in% c("Scheduled Inspection",
  #                        "Okanagan",
  #                        "Other", "Penticton Roving")) |> 
  group_by(Station) |> 
  summarise(TotalInsp = n()) |> 
  left_join(dat |> 
              filter(!Station %in% c("Scheduled Inspection",
                                     "Okanagan",
                                     "Other")) |> 
              filter(Previous_Inspection_Ind == T) |> 
              group_by(Station) |> 
              summarise(PrevInspected = n())
  ) |> 
  # Drop rows where we have fewer than 5 inspections
  dplyr::filter(TotalInsp >= 5) |> 
  # dplyr::filter(Station != "Lower Mainland Roving") |> 
  mutate(PercPrevInsp = 100*PrevInspected/TotalInsp) |> 
  arrange(desc(PercPrevInsp)) |> 
  left_join(station_types) |> 
  mutate(StationLabel = ifelse(StationType == "Roving",paste0(Station,"*"),Station)) |> 
  mutate(StationLabel = fct_inorder(StationLabel)) |> 
  dplyr::filter(!is.na(PercPrevInsp))

max_perc_plus_buffer = 10*ceiling(max(p14_dat$PercPrevInsp)*1.3 / 10)

p14 = p14_dat |> 
  ggplot() + 
  geom_col(aes(x=StationLabel,
               y=PercPrevInsp), fill = my.grey, width = 0.40) +
  geom_text(aes(x=StationLabel,
                y=PercPrevInsp+max(PercPrevInsp)*0.1,
                label=paste0(round(PercPrevInsp,0),"%"))) +
  theme_classic() +
  labs(x = "",y = "Percent Previously Inspected") + 
  scale_y_continuous(breaks = seq(0,max_perc_plus_buffer,10), 
                     limits = c(0,max_perc_plus_buffer),
                     labels = paste0(seq(0,max_perc_plus_buffer,10),"%")) +  
  theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1)) + 
  scale_fill_brewer(palette = "Dark2")

#Remove Penticton Roving
figure_15 = dat |> 
  filter(!Station %in% rovers_to_drop) |> 
  dplyr::filter(Station %in% stations.to.include) |> 
  filter(Previous_Inspection_Ind == T) |> 
  filter(!is.na(Previous_Inspection_Days_Count)) |> 
  mutate(previnsp = case_when(
    Previous_Inspection_Days_Count %in% c('0','Same day') ~ 'Same Day',
    Previous_Inspection_Days_Count %in% c('30','60','180') ~ '> 30 Days',
    Previous_Inspection_Days_Count %in% c('< 30 days','<30 days') ~ '< 30 Days',
    Previous_Inspection_Days_Count == '> 30 days' ~ '> 30 Days',
    Previous_Inspection_Days_Count == '> 1 year' ~ '> 1 Year',
    T ~ NA
  )) |> 
  select(Station, previnsp) |> 
  count(Station, previnsp, name = 'NumberInsp') |>
  group_by(Station) |> 
  mutate(TotalInsp = sum(NumberInsp)) |> 
  mutate(PercInsp = 100*NumberInsp/TotalInsp) |> 
  mutate(previnsp = factor(previnsp, 
                           levels = c("Same Day",
                                      "< 30 Days",
                                      "> 30 Days",
                                      "> 1 Year"))) |> 
  arrange(Station,previnsp) |> 
  select(Station,previnsp,PercInsp) |> 
  left_join(station_types) |> 
  mutate(StationLabel = ifelse(StationType == "Roving",paste0(Station,"*"),Station)) |> 
  mutate(StationLabel = fct_inorder(StationLabel))

# station_order = figure_15 |> 
#   # filter(previnsp == '< 30 Days') |> 
#   # bind_rows(tibble(
#   #   Station = 'Keremeos',
#   #   StationLabel = 'Keremeos*', 
#   #   previnsp = '< 30 Days', PercInsp = 0)
#   #   )|> 
#   arrange(desc(PercInsp)) |> 
#   dplyr::filter()
#   dplyr::pull(StationLabel)

p15 = figure_15 |>
  # mutate(StationLabel = factor(StationLabel, levels = c(station_order))) |> 
  ggplot() +
  geom_col(aes(x=StationLabel,
               y=PercInsp,
               fill = previnsp),
           width = 0.40,
           position = "stack") +
  theme_classic() +
  labs(x = "",y = "Percent Previously Inspected", fill = "") + 
  scale_fill_manual(values = c("Same Day" = "#ab52c5",
                               "< 30 Days" = "#8b9f4a",
                               "> 30 Days" = "#cc0000",
                               "> 1 Year" = "#2986cc")) +
  scale_y_continuous(breaks = seq(0,100,10), labels = paste0(seq(0,100,10),"%")) + 
  theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1))

dat_w_plugs = dat |> 
  dplyr::filter(Watercraft_Has_Drainplugs_Ind)

pl_of_plugs = dat_w_plugs |> 
  dplyr::count(Drainplug_Removed_at_Inspection_Ind) |> 
  dplyr::mutate(prop = 100*(n / sum(n))) |> 
  dplyr::arrange(-Drainplug_Removed_at_Inspection_Ind) |> 
  dplyr::mutate(Drainplug_Removed_at_Inspection_Ind = ifelse(Drainplug_Removed_at_Inspection_Ind,"Drainplug Removed","Drainplug not Removed")) |> 
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) |> 
  ggplot(aes(x = "", y = prop, 
             fill = Drainplug_Removed_at_Inspection_Ind)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text_repel(aes(y = lab.ypos, x = 1.00, label = paste0(Drainplug_Removed_at_Inspection_Ind,"\n",round(prop,1),"%")),
                  color = "black", nudge_x = 0.7) + 
  theme_void() + 
  # scale_fill_brewer(palette = "Dark2") + 
  labs(fill = "Source") + 
  ggtitle("Pull the Plug Compliance") +
  theme(legend.position = "none")

p16_dat = dat_hr |> 
  #Filter for our target year and the 2 years prior
  filter(Year %in% c(my.year-2, my.year-1, my.year)) |> 
  mutate(the.month = month(TimeOfInspection,label=T,abbr=T)) |> 
  group_by(Year,the.month) |> 
  summarise(Number_HR = n()) |> 
  ungroup() |> 
  #Add in a row for year-month combos with no records: 2020 April.
  add_row(Year = 2020,the.month = month(4,abbr=T,label=T),Number_HR = 0) |> 
  group_by(Year) |> 
  filter(!is.na(the.month)) |> 
  dplyr::filter(Year %in% (my.year-2):my.year)

p16 = ggplot() + 
  geom_col(
    data = p16_dat,
    aes(x=the.month,
        y=Number_HR,
        fill = as.character(Year)), 
    width = 0.40,
    col = "black",
    position = "dodge") +
  geom_text(
    data = p16_dat |> filter(Year == my.year),
    aes(x=the.month,
        y=Number_HR+max(Number_HR)*0.2,
        label=round(Number_HR,0)),
    nudge_x = 0.20) +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2') +
  # scale_fill_manual(values = c("darkgrey","lightgrey","black")) +
  labs(x = "",y = "Number of High-Risk Inspections", fill = 'Year')

#Remove Penticton Roving
fig18_dat = dat_hr |> 
  filter(Year == my.year) |> 
  group_by(Previous_Waterbody_1_Province_Or_State) |> 
  summarise(BoatsFromPlace = n()) |> 
  mutate(TotalBoats = sum(BoatsFromPlace)) |> 
  mutate(PercBoatsFromPlace = 100*BoatsFromPlace/TotalBoats) |> 
  arrange(desc(PercBoatsFromPlace)) |> 
  mutate(Grouper = case_when(
    PercBoatsFromPlace >= 2 ~ "MainFig",
    PercBoatsFromPlace < 2 ~ "SecondFig")
  )

fig18_main_dat = fig18_dat |> 
  filter(Grouper == "MainFig") |> 
  #Add a row for "Other" (for which we have to add together the others)
  bind_rows(fig18_dat |> 
              filter(Grouper == "SecondFig") |> 
              group_by(TotalBoats) |> 
              summarise(BoatsFromPlace = sum(BoatsFromPlace)) |> 
              mutate(PercBoatsFromPlace = 100*BoatsFromPlace/TotalBoats,
                     Previous_Waterbody_1_Province_Or_State = "Other",
                     Grouper = "MainFig")) |> 
  rename(prop = PercBoatsFromPlace) |>
  arrange(desc(Previous_Waterbody_1_Province_Or_State)) |> 
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) |> 
  select(-BoatsFromPlace,-TotalBoats)

fig18_second_dat = fig18_dat |> 
  filter(Grouper != "MainFig") |> 
  rename(prop = PercBoatsFromPlace) |>
  arrange(desc(Previous_Waterbody_1_Province_Or_State)) |> 
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) |> 
  select(-BoatsFromPlace,-TotalBoats)

p18 = ggarrange(
  ggplot(fig18_main_dat,
         aes(x = "", y = prop, 
             fill = Previous_Waterbody_1_Province_Or_State)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text_repel(aes(y = lab.ypos, x = 1.00, label = paste0(Previous_Waterbody_1_Province_Or_State,"\n",round(prop,1),"%")),
                    color = "black", nudge_x = 0.7) + 
    theme_void() + 
    # scale_fill_brewer(palette = "Dark2") + 
    labs(fill = "Source") + 
    ggtitle("Main Sources") +
    theme(legend.position = "none"),
  ggplot(fig18_second_dat,
         aes(x = "", y = prop, 
             fill = Previous_Waterbody_1_Province_Or_State)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text_repel(aes(y = lab.ypos, x = 1.00, label = paste0(Previous_Waterbody_1_Province_Or_State,"\n",round(prop,1),"%")), color = "black",nudge_x = 0.7) +
    theme_void() + 
    labs(fill = "Source") + 
    ggtitle("Breakdown of `Other`") +
    theme(legend.position = "none"),
  ncol = 2, nrow = 1)


fig19_dat = dat_hr |> 
  filter(Year == my.year) |> 
  #Clean up destination regions.
  mutate(DestRegion = case_when(
    str_detect(DestRegion, "Ocean") ~ "Pacific Ocean",
    str_detect(General_Comment, "going to Vancouver") ~ "Lower Mainland",
    str_detect(General_Comment, "to Pacific Ocean") ~ "Pacific Ocean",
    str_detect(General_Comment, "Kelowna") ~ "Okanagan",
    str_detect(General_Comment, "Alaska") ~ "Outside BC",
    str_detect(General_Comment, "Vancouver Island") ~ "Vancouver Island",
    str_detect(General_Comment, "risk of entering BC waters") ~ "Outside BC",
    T ~ DestRegion
  )) |> 
  group_by(DestRegion) |> 
  summarise(BoatsFromPlace = n()) |> 
  mutate(TotalBoats = sum(BoatsFromPlace)) |> 
  mutate(PercBoatsFromPlace = 100*BoatsFromPlace/TotalBoats) |> 
  #Add the FLNRO regions...to do this, for each water body name in the FLNRO lookup table, I sort the FLNRO regions based on their
  #number, low to high (e.g. 1, 2, 3 -> 7), remove the O and P from regions 7,
  #and keep only the first match between a named water body and a region's water body.
  left_join(flnro_lookup |> 
              rename(DestRegion = GNIS_NAME_) |> 
              mutate(REGION_G = str_remove(REGION_G,"[A-Z]{1}")) |> 
              arrange(DestRegion,REGION_G) |> 
              group_by(DestRegion) |> 
              slice(1)) |> 
  #Any holes in this lookup table (e.g. cities) should be filled with DestRegion values, unless
  # those values have the word 'Lake' or 'River' in them, which indicates an unsuccessful spatial
  # match to BC waterbodies (indicating a destination region outside of BC!).
  mutate(REGION_N = case_when(
    is.na(REGION_N) & !str_detect(DestRegion, "(Lake|River)") ~ DestRegion,
    is.na(REGION_N) & str_detect(DestRegion, '(Lake|River)') ~ 'Outside BC',
    T ~ REGION_N)) |> 
  group_by(REGION_N) |> 
  summarise(prop = sum(PercBoatsFromPlace)) |> 
  dplyr::mutate(REGION_N = ifelse(REGION_N == 'No Match', 'Unknown', REGION_N)) |> 
  dplyr::group_by(REGION_N) |> 
  dplyr::summarise(prop = sum(prop))

# Quick test - how many regions fall into the 'Other' category?
# If it's 2 or less, just keep a single figure.
count_for_other_pie = nrow(fig19_dat |> 
                             filter(prop <= 2))

if(count_for_other_pie > 2){
  fig19_main_dat = fig19_dat |> 
    filter(prop > 2) |> 
    bind_rows(fig19_dat |> 
                filter(prop <= 2) |> 
                summarise(prop = sum(prop)) |> 
                mutate(REGION_N = "Other")) |> 
    arrange(desc(REGION_N)) |> 
    mutate(lab.ypos = cumsum(prop) - 0.5*prop)
} else {
  fig19_main_dat = fig19_dat |> 
    arrange(desc(REGION_N)) |> 
    mutate(lab.ypos = cumsum(prop) - 0.5*prop)
}


#### pie chart code 


dark_pal_9 = brewer.pal(9, name = "Dark2") |> str_replace("#666666","#d63735")

dark_pal_9 = c(
  `a` = "#1B9E77",
  `b` = "#D95F02",
  `c` = "#7570B3",
  `d` = "#E7298A",
  `e` = "#66A61E",
  `f` = "#E6AB02",
  `g` = "#A6761D",
  `h` = "#d63735",
  `i` = "#2999d2")

darkpal_9_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (dark_pal_9)
  dark_pal_9[cols]
}

darkpal_9_palette = list("main" = darkpal_9_cols("a","b","c","d","e","f","g","h","i"))

darkpal_9_function <- function(palette = "main", reverse = FALSE, ...) {
  pal <- darkpal_9_palette[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}

scale_fill_darkpal9 <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- darkpal_9_function(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("fill", paste0("darkpal_9_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

if(count_for_other_pie > 2){
  p19 = ggarrange(
    ggplot(fig19_main_dat,
           aes(x = "", y = prop, 
               fill = REGION_N)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text_repel(aes(y = lab.ypos, x = 1, label = paste0(REGION_N,"\n",round(prop,1),"%")), color = "black", nudge_x= 1)+
      theme_void() + 
      theme(legend.position = "none") +
      ggtitle("Main Destination Regions") +
      scale_fill_darkpal9() + 
      labs(fill = "Destination Region"),
    
    ggplot(fig19_dat |> 
             filter(prop <= 2) |> 
             arrange(desc(REGION_N)) |> 
             mutate(lab.ypos = cumsum(prop) - 0.5*prop),
           aes(x = "", y = prop, 
               fill = REGION_N)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text_repel(aes(y = lab.ypos, x = 1.49, label = paste0(REGION_N,"\n",round(prop,1),"%")), color = "black",nudge_x=0.7)+
      theme_void() + 
      theme(legend.position = "none") +
      ggtitle("Breakdown of `Other`") +
      scale_fill_brewer(palette = "Dark2") + 
      labs(fill = "Destination Region"),
    nrow = 1, ncol = 2)
} else {
  p19 = ggplot(fig19_main_dat,
               aes(x = "", y = prop, 
                   fill = REGION_N)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text_repel(aes(y = lab.ypos, x = 1, label = paste0(REGION_N,"\n",round(prop,1),"%")), color = "black",nudge_x=1,force=1)+
    theme_void() + 
    theme(legend.position = "none") +
    ggtitle("Destination Regions") +
    scale_fill_darkpal9() + 
    labs(fill = "Destination Region")
}

#####

