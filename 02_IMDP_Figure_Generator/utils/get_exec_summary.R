min_date = paste(lubridate::month(min(dat$TimeOfInspection),label = TRUE, abbr = F),
                 lubridate::day(min(dat$TimeOfInspection)))

max_date = paste(lubridate::month(max(dat$TimeOfInspection),label = TRUE, abbr = F),
                 lubridate::day(max(dat$TimeOfInspection)))

print(paste0("Stations active in ",my.year," from ",min_date," to ",max_date))

print(paste0('Inspections by Station in ',my.year))

dat |> 
  dplyr::count(Station, sort = T) |> 
  knitr::kable()

print(paste0('Inspections by Month in ',my.year))

dat |> 
  dplyr::mutate(the_month = lubridate::month(TimeOfInspection, label = T, abbr = F)) |> 
  dplyr::count(the_month) |> 
  knitr::kable()

# Number of stations, split by type.
print('Stations and Station Type')

station_types |> 
  dplyr::filter(!Station %in% rovers_to_drop) |> 
  dplyr::add_count(StationType) |> 
  knitr::kable()

print('Approximate number of inspections and person interactions')

data.frame(
  total_inspections = nrow(dat),
  total_interactions = sum(dat$Number_Of_People_In_Party,na.rm=T)
) |> 
  knitr::kable()

print("High Risk:")

#Turn to Metabase - just in case some filtering steps have dropped some of these
data.frame(
  number_highrisk = nrow(dat_hr |> dplyr::filter(Year == my.year)),
  decons_issued = sum(dat$Decontamination_order_issued_Ind,na.rm=T),
  deconts_performed = sum(dat$Decontamination_Performed_Ind,na.rm=T),
  quarantine_periods = sum(dat$Quarantine_Period_Issued_Ind,na.rm=T)
) |> 
  knitr::kable()

state_longnames = data.frame(abbr = state.abb, long = state.name)
prov_names = data.frame(abbr = c("AB","BC","MB","ON","QC","YT","SK","NS"),
                        long = c("Alberta","British Columbia",
                                 "Manitoba","Ontario","Quebec","Yukon","Saskatchewan","Nova Scotia"))
state_longnames = dplyr::bind_rows(
  state_longnames,
  prov_names
)

tracked_mf = read.xlsx(MusselFouledTracker, sheet = MF_tracker_sheet)

print('Mussel Fouled: Source Prov/State')

if(my.year == 2023){
  tracked_mf |> 
    dplyr::count(BIG.SOURCE.SUMMARY) |> 
    dplyr::rename(name = BIG.SOURCE.SUMMARY) |> 
    arrange(desc(n)) |> 
    knitr::kable()
}

print('Mussel Fouled: Destination Region')
if(my.year == 2023){
  data.frame(
    destination_region = c("Thompson-Okanagan","Lower Mainland","Northeast","Kootenay-Boundary"),
    number = c(8,4,1,1)
  ) |> 
    knitr::kable()
}


print("Previous Warning for MF Boat - not currently calculated in this code...")

print("For Compliance stats, please see below...")

print("Lake sampling numbers")

# ld = openxlsx::read.xlsx(paste0(my_opts$base_dir,"04_Extra_Figures_and_Scripts/CRB/output/2023_Provincial_PlanktonTow_LabResults_CRB_Format.xlsx"))
# 
# # Fill in data gaps for waterbody name with info in the sampling location column.
# # Note that these values do not always name the lake...
# ld = ld |> 
#   dplyr::mutate(wb_name_fill_na = ifelse(is.na(Water.Body.Name), Sampling.Location.Description, Water.Body.Name))
#   
# # Number of samples, unique wb names or sampling locatoins, and unique wb names
# data.frame(
#   number_of_samples = nrow(ld),
#   number_of_unique_wb_names_or_sampling_locations = length(unique(ld$wb_name_fill_na)),
#   number_of_unique_wb_names = length(unique(ld$Water.System.Name))
# ) |> 
#   knitr::kable()