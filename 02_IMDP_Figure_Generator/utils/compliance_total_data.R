if(my_opts$year < 2024){
  dat |> 
    filter(!Station %in% rovers_to_drop) |> 
    group_by(Shift_ID) |> 
    summarise(NumberInsp = n()) |> 
    #Add number of non-motorized blow-bys per unique station / shift combo.
    left_join(dat |> 
                group_by(Shift_ID) |> 
                mutate(Non_Motorized_Blow_Bys_Counter = as.numeric(Non_Motorized_Blow_Bys_Counter)) |> 
                select(Shift_ID,Non_Motorized_Blow_Bys_Counter) |> 
                distinct()) |> 
    #Add number of motorized blow-bys per unique station / shift combo.
    left_join(dat |> 
                group_by(Shift_ID) |> 
                mutate(Motorized_Blow_Bys_Counter = as.numeric(Motorized_Blow_Bys_Counter)) |> 
                select(Shift_ID,Motorized_Blow_Bys_Counter) |> 
                distinct()) |> 
    # group_by(Station) |> 
    summarise(NumberInsp = sum(NumberInsp),
              Non_Motorized_Blow_Bys_Counter = sum(Non_Motorized_Blow_Bys_Counter),
              Motorized_Blow_Bys_Counter = sum(Motorized_Blow_Bys_Counter)) |> 
    # group_by(Station) |> 
    summarise(PercentCompliance = 100*NumberInsp/(NumberInsp + Non_Motorized_Blow_Bys_Counter + Motorized_Blow_Bys_Counter)) |> 
    arrange(desc(PercentCompliance)) |> 
    knitr::kable()
  
  print(paste0((my.year-1),"'s Compliance for comparison..."))
  
  dat_all |>
    filter(Year == my.year-1) |> 
    filter(!Station %in% rovers_to_drop) |> 
    group_by(Shift_ID) |> 
    summarise(NumberInsp = n()) |> 
    dplyr::ungroup() |> 
    dplyr::filter(!duplicated(Shift_ID)) |> 
    #Add number of non-motorized blow-bys per unique station / shift combo.
    left_join(dat_all |>
                filter(Year ==  my.year-1) |> 
                group_by(Shift_ID) |> 
                mutate(Non_Motorized_Blow_Bys_Counter = as.numeric(Non_Motorized_Blow_Bys_Counter)) |> 
                dplyr::ungroup() |> 
                select(Shift_ID,Non_Motorized_Blow_Bys_Counter) |> 
                distinct() |> 
                dplyr::filter(!duplicated(Shift_ID))
    ) |> 
    #Add number of motorized blow-bys per unique station / shift combo.
    left_join(dat_all |>
                filter(Year ==  my.year-1) |> 
                group_by(Shift_ID) |> 
                mutate(Motorized_Blow_Bys_Counter = as.numeric(Motorized_Blow_Bys_Counter)) |> 
                dplyr::ungroup() |> 
                select(Shift_ID,Motorized_Blow_Bys_Counter) |> 
                distinct() |> 
                dplyr::filter(!duplicated(Shift_ID))) |> 
    # group_by(Station) |> 
    summarise(NumberInsp = sum(NumberInsp),
              Non_Motorized_Blow_Bys_Counter = sum(Non_Motorized_Blow_Bys_Counter),
              Motorized_Blow_Bys_Counter = sum(Motorized_Blow_Bys_Counter)) |> 
    # group_by(Station) |> 
    summarise(PercentCompliance = 100*NumberInsp/(NumberInsp + Non_Motorized_Blow_Bys_Counter + Motorized_Blow_Bys_Counter))
  
  print('Of the non-compliant boats, the following percentage were non-motorized:')
  
  non_motorized_number = dat |> 
    group_by(Shift_ID) |> 
    mutate(Non_Motorized_Blow_Bys_Counter = as.numeric(Non_Motorized_Blow_Bys_Counter)) |> 
    dplyr::ungroup() |> 
    select(Shift_ID,Non_Motorized_Blow_Bys_Counter) |> 
    distinct() |> 
    dplyr::summarise(total = sum(Non_Motorized_Blow_Bys_Counter))
  
  number_motorized = dat |> 
    group_by(Shift_ID) |> 
    mutate(Motorized_Blow_Bys_Counter = as.numeric(Motorized_Blow_Bys_Counter)) |> 
    dplyr::ungroup() |> 
    select(Shift_ID,Motorized_Blow_Bys_Counter) |> 
    distinct() |> 
    dplyr::summarise(total = sum(Motorized_Blow_Bys_Counter))
  
  paste0(100*round(non_motorized_number / (non_motorized_number + number_motorized),4), "%")
} else {
  # From 2024 onwards, blowbys are now recorded in a table that is separate from the inspections.
  dat |> 
    filter(!Station %in% rovers_to_drop) |> 
    # filter(!Station %in% c("Lower Mainland Roving","Penticton Roving")) |> 
    dplyr::filter(Station %in% stations.to.include) |> 
    group_by(Station,Shift_ID,Workflow_ID) |> 
    summarise(NumberInsp = n()) |> 
    dplyr::ungroup() |> 
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
    dplyr::reframe(NumberInsp = sum(NumberInsp),
                   Non_Motorized_Blow_Bys_Counter = sum(Non_Motorized_Blow_Bys_Counter),
                   Motorized_Blow_Bys_Counter = sum(Motorized_Blow_Bys_Counter)) |> 
    dplyr::reframe(PercentCompliance = 100*NumberInsp/(NumberInsp + Non_Motorized_Blow_Bys_Counter + Motorized_Blow_Bys_Counter))
  
  # What percentage were non-motorized?
  number_blowb_nonmot = blowb |> 
    dplyr::filter(watercraft_complexity == 'Non-motorized') |> 
    dplyr::reframe(number_non_mot = n()) 
  
  number_blowb_total = blowb |> 
    dplyr::count(watercraft_complexity) |> 
    dplyr::reframe(total_blowbys = sum(n))
  
  print(paste0(round(100*number_blowb_nonmot / number_blowb_total,1), "% of blowbys were non-motorized"))
}