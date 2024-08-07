# Martina asked for a table of mussel-fouled boats, counted by year
# and complexity.

# I ran the top of the '02_IMDP_FinalReport_Figures.Rmd' file to read in the data from 
# all years ("dat_all").

dat_2024 = readxl::read_excel('C:/Users/CMADSEN/Downloads/mussel_fouled_summary_2024_07_18.xlsx')

# Temporary: make sure 2 MF inspections, which were just follow-up 
# inspections, are not toggled as MF!
dat_2024[dat_2024$`Watercraft Risk Assessment ID` %in% c(111332,114041),]$`Adult Dreissenidae Mussel Found Ind` = FALSE
dat_2024[dat_2024$`Watercraft Risk Assessment ID` %in% c(111332,114041),]$`Adult Dressenidae Found Ind` = FALSE

# Maybe just filter out those two inspections?
dat_2024 = dat_2024 |> 
  dplyr::filter(!`Watercraft Risk Assessment ID` %in% c(111332,114041))

names(dat_2024) <- stringr::str_to_title(stringr::str_replace_all(names(dat_2024)," ","_"))
  
dat_2024 = dat_2024 |>
  dplyr::mutate(Year = 2024) |> 
  dplyr::select(Year,
                Simple_Counter = Simple_counter,
                Complex_Counter = Complex_counter,
                Very_Complex_Counter = Very_complex_counter,
                Non_Motorized_Counter = Non_motorized_counter) |> 
  dplyr::mutate(MusselsFound_Ind = TRUE)

d = dat_all |> 
  dplyr::bind_rows(dat_2024)

result = d |> 
  dplyr::filter(MusselsFound_Ind) |> 
  dplyr::select(Year,ends_with("_Counter")) |> 
  dplyr::select(-c(Non_Motorized_Blow_Bys_Counter,Motorized_Blow_Bys_Counter)) |> 
  # Make all numbers 1; see if totals add up properly after that.
  dplyr::mutate(across(-Year, \(x) ifelse(x > 1 & Year > 2015, 1, x))) |> 
  pivot_longer(-Year) |> 
  # Ensure we don't have more MF boats in each year than we have on the website
  # dplyr::group_by(Year) |> 
  dplyr::count(Year, name, wt = value) |> 
  # dplyr::group_by(Year) |> 
  # dplyr::mutate(total = sum(n)) |> 
  # dplyr::group_by(Year,name)
  # dplyr::summarise(value = sum(value)) |> 
  pivot_wider(names_from = name, values_from = n) |>
  dplyr::mutate(Total = Simple_Counter + Complex_Counter + Very_Complex_Counter + Non_Motorized_Counter) |>
  dplyr::select(Year,Simple_Counter,Complex_Counter,Very_Complex_Counter,Non_Motorized_Counter,Total) |> 
  dplyr::ungroup()

result

openxlsx::write.xlsx(result, "04_Extra_Figures_and_Scripts/output/MF_boats_by_year_and_complexity.xlsx")
