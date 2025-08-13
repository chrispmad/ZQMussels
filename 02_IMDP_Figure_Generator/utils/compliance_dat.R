# Read in blowbys
blowb = vroom::vroom(paste0(zqm.operations.folder,"Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years/metabase_blowby_table_2024_onwards.csv")) |> 
  purrr::set_names(snakecase::to_snake_case) |> 
  dplyr::rename(Workflow_ID = observer_workflow_id) |> 
  dplyr::mutate(Workflow_ID = as.character(Workflow_ID))

blowb = blowb |> 
  left_join(
    dat |> 
      dplyr::select(Workflow_ID,
                    Station) |> 
      dplyr::distinct()
  ) |> 
  dplyr::filter(!is.na(Station))

dat_w_blowbys = dat |> 
  filter(!Station %in% rovers_to_drop) |> 
  filter(!Station %in% c("Lower Mainland Roving","Penticton Roving")) |> 
  dplyr::filter(Station %in% stations.to.include) |> 
  group_by(Station,Shift_ID,Workflow_ID) |> 
  summarise(NumberInsp = n()) |> 
  #Add number of non-motorized blow-bys per unique station / shift combo.
  left_join(
    blowb |> 
      dplyr::filter(watercraft_complexity == 'Non-motorized') |> 
      dplyr::count(Workflow_ID,sort = T, name = "Non_Motorized_Blow_Bys_Counter")
  ) |> 
  #Add number of motorized blow-bys per unique station / shift combo.
  left_join(
    blowb |> 
      dplyr::filter(watercraft_complexity != 'Non-motorized') |> 
      dplyr::count(Workflow_ID,sort = T, name = "Motorized_Blow_Bys_Counter")
  ) |> 
  dplyr::mutate(dplyr::across(c(Non_Motorized_Blow_Bys_Counter,Motorized_Blow_Bys_Counter), \(x) tidyr::replace_na(x,0))) |> 
  group_by(Station) |> 
  summarise(NumberInsp = sum(NumberInsp),
            Non_Motorized_Blow_Bys_Counter = sum(Non_Motorized_Blow_Bys_Counter),
            Motorized_Blow_Bys_Counter = sum(Motorized_Blow_Bys_Counter)) 

# complicance plot
p12 = dat_w_blowbys |> 
  group_by(Station) |> 
  summarise(PercentCompliance = 100*NumberInsp/(NumberInsp + Non_Motorized_Blow_Bys_Counter + Motorized_Blow_Bys_Counter)) |> 
  arrange(desc(PercentCompliance)) |> 
  #Add station type...
  left_join(station_types) |> 
  mutate(StationLabel = paste0(Station,
                               ifelse(StationType == "Roving", "*", ""))) |> 
  mutate(StationLabel = fct_reorder(StationLabel, -PercentCompliance)) |> 
  ggplot() + 
  geom_col(aes(x=StationLabel,
               y=PercentCompliance), fill = my.grey, width = 0.40) +
  geom_text(aes(x=StationLabel,
                y=PercentCompliance+max(PercentCompliance)*0.05,
                label=paste0(round(PercentCompliance,0),"%"))) +
  theme_classic() +
  labs(x = "",y = "Percent Compliance") + 
  scale_y_continuous(breaks = seq(0,100,10), labels = paste0(seq(0,100,10),"%")) + 
  theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1))