# Load in libraries

library(tidyverse)
library(openxlsx)
library(sf)


## STEPS ## 
# 1. Clean up waterbody names in sampling sheet.
# 2. Look for name match. If one-to-one match, great, grab polygon (and area)!
# 3. If one-to-many match, if we have sample coordinates, measure distance between point and polygons, choose closest.
# 3-continued. If no sample coordinates (i.e. just waterbody name), choose the polygon that is largest.


# Load in lab data.
dat = openxlsx::read.xlsx('J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2023/Lab results/BC Veliger Sampling Inventory 2023_09_03.xlsx') |> 
  as_tibble() |> 
  set_names(snakecase::to_snake_case) |> 
  mutate(date = openxlsx::convertToDateTime(date_collected_yyyy_mm_dd))


clean_coordinates = function(dat){
  # Look at the first row of the data. Which row LIKELY has lat / long coordinates?
  likely_lat = names(which(lapply(dat[1,], \(x) str_detect(x, '[0-9]{2}\\.[0-9]+')) == TRUE))[1]
  likely_lon = names(which(lapply(dat[1,], \(x) str_detect(x, '[0-9]{3}\\.[0-9]+')) == TRUE))[1]
  
  dat |> 
    rename(lat := !!sym(likely_lat),
           lon := !!sym(likely_lon)) |> 
    mutate(lat = stringr::str_remove_all(lat, '^(\\.)?'),
           lon = stringr::str_remove_all(lon, '^(\\.)?')) |> 
    mutate(lat = stringr::str_remove_all(lat, '[A-Za-z]+')) |> 
    mutate(lon = stringr::str_remove_all(lon, '[A-Za-z]+')) |> 
    mutate(lat = as.numeric(lat),
           lon = as.numeric(lon)) |> 
    mutate(lat = ifelse(lat < 0, lat * -1, lat),
           lon = ifelse(lon > 0, lon * -1, lon))
}

dat = clean_coordinates(dat)




clean_waterbody_names = function(dat, waterbody_column){
# Some corrections to waterbody names.
  dat = dat |> 
    dplyr::rename(waterbody := !!sym(waterbody_column)) |> 
    dplyr::mutate(waterbody = stringr::str_squish(waterbody)) |>
    dplyr::mutate(waterbody = stringr::str_to_title(waterbody)) |> 
    dplyr::mutate(waterbody = dplyr::case_when(
      waterbody == 'Lac La Hache' ~ 'Lac la Hache',
      waterbody == 'Okangan Lake' ~ 'Okanagan Lake',
      waterbody == 'Arrow Lake, Upper' ~ 'Upper Arrow Lake',
      waterbody == 'Arrow Lake, Lower' ~ 'Lower Arrow Lake',
      waterbody == 'Kootenay River (Nelson)' ~ 'Kootenay River',
      waterbody == 'Lower Fraser River' ~ 'Fraser River',
      waterbody == 'Upper Kettle River' ~ 'Kettle River',
      waterbody == 'Lower Kettle River' ~ 'Kettle River',
      waterbody == 'Alouette' ~ 'Alouette Lake',
      waterbody == 'Wahleach' ~ 'Wahleach Lake',
      waterbody == 'Koocanusa Lake' ~ 'Lake Koocanusa',
      waterbody == 'St Mary Lake' ~ 'St. Mary Lake',
      waterbody == 'Norbury Lake' ~ 'Norbury Lakes',
      waterbody == 'Lake Winderemere' ~ 'Windermere Lake',
      waterbody == 'Jimsmith Lake' ~ 'Jim Smith Lake',
      T ~ waterbody
    ))
  # Did we change the waterbody column name? If so, rename to original column name.
  if(waterbody_column != 'waterbody'){
    dat = dat |> 
      dplyr::mutate(!!sym(waterbody_column) := waterbody) |> 
      dplyr::select(-waterbody)
  }
  dat
}

dat = clean_waterbody_names(dat, 'waterbody')

# Zoom in on just July records.
dat_j = dat |> 
  filter(lubridate::month(date, label = T, abbr = F) == 'July')

# Remove rows where 'secchi_depth' was either NA or 0
dat_j = dat_j |> 
  filter(!secchi_depth_m %in% c('0','Na','na','N/A'),
         !is.na(secchi_depth_m))

# # Add in a new column, just in case, that acts as unique_id
# dat_j = dat_j |> 
#   mutate(row_index = dplyr::row_number())
# Match lake names to polygons of lakes from the BCG Warehouse.

if(!exists('wbs_s')) wbs_s = read_sf('W:/CMadsen/shared_data_sets/summarized_bc_waterbodies.shp')

# Measure area of lakes that match names with our lab data.
if(!exists('matched_wbs')){
  matched_wbs = wbs_s |> filter(GNIS_NAME_ %in% dat_j$waterbody) |> 
    dplyr::select(waterbody = GNIS_NAME_,
                  area_sq_m = FEATURE_A) |> 
    sf::st_transform(crs = 4326)
}

add_waterbody_area = function(dat, waterbody_polygons, units = c("sq_meters","sq_km","acres"), digits = 2){
  
  if(length(units) > 1) stop("Please choose one unit for area from options 'sq_meters','sq_km', or 'acres'")
  # Check there's a column named 'row_index' in dat.
  if(!'row_index' %in% names(dat)){
    dat = dat |> mutate(row_index = dplyr::row_number())
  }
  
  unique_wb_names = unique(dat$waterbody)
  
  dat$wb_area = 0
  
  for(row in unique_wb_names){
    print(row)
    
    dat_by_name = dat |> filter(waterbody == row)
    
    # Do name match with polygons.
    matched_polys = waterbody_polygons |> filter(waterbody == row)
    
    if(nrow(matched_polys) == 1){
      # Single name match. Nice! Grab the lake area, set to proper units, and add
      # to data.
      if(units == 'sq_meters') dat[dat$waterbody == row,]$wb_area <- round(matched_polys$area_sq_m, digits)
      if(units == 'sq_kilometers') dat[dat$waterbody == row,]$wb_area <- round(matched_polys$area_sq_m/1000000, digits)
      if(units == 'acres') dat[dat$waterbody == row,]$wb_area <- round(matched_polys$area_sq_m * 0.000247105, digits)

    } else {
      
      # Spatialize the lab data with this loop's waterbody name.
      dat_by_name = sf::st_as_sf(dat_by_name, coords = c('lon','lat'), crs = 4326)
      
      for(i in 1:nrow(dat_by_name)){
        print(paste0('lab samples ',i,' for ', row))
        
        row_index_for_ith_loop = dat_by_name[i,]$row_index
        
        area_to_add = matched_polys |> 
          dplyr::mutate(distance_to_sample = as.numeric(sf::st_distance(dat_by_name[i,]))) |> 
          dplyr::arrange(distance_to_sample) |> 
          dplyr::slice(1) |> 
          dplyr::pull(area_sq_m)
        
        dat[dat$row_index == dat_by_name[i,]$row_index,]$wb_area <- area_to_add
        
        if(units == 'sq_meters') dat[dat$row_index == dat_by_name[i,]$row_index,]$wb_area <- round(area_to_add, digits)
        if(units == 'sq_kilometers') dat[dat$row_index == dat_by_name[i,]$row_index,]$wb_area <- round(area_to_add/1000000, digits)
        if(units == 'acres') dat[dat$row_index == dat_by_name[i,]$row_index,]$wb_area <- round(area_to_add * 0.000247105, digits)
      }
    }
  }
  dat
}

# Add waterbody area from BG data catalogue's lake and river polygon layers.
dat_j = add_waterbody_area(dat_j, matched_wbs, units = 'acres', digits = 2)

# Remove text from secchi depth field
dat_j = dat_j |> mutate(secchi_depth_m = case_when(
  stringr::str_detect(secchi_depth_m, '[a-zA-Z\\+]+') & stringr::str_detect(secchi_depth_m, '[0-9]+') ~ str_remove_all(secchi_depth_m, '[A-Za-z\\(\\)]*(\\+)*'),
  stringr::str_detect(secchi_depth_m, '[a-zA-Z\\+]+') & !stringr::str_detect(secchi_depth_m, '[0-9]+') ~ NA,
  T ~ secchi_depth_m
)) |> 
  mutate(secchi_depth_m = stringr::str_squish(secchi_depth_m)) |> 
  mutate(secchi_depth_m = as.numeric(secchi_depth_m))

dat_for_output = dat_j |> 
  dplyr::reframe(`Monitoring Location ID` = waterbody,
                 `Region Name` = location_site_name,
                 `Waterbody size(acres)` = wb_area,
                 `Date of Reading` = openxlsx::convertToDate(date_collected_yyyy_mm_dd),
                 Time = stringr::str_extract(date_collected_yyyy_mm_dd, '\\..*'),
                 Latitude = lat,
                 Longitude = lon,
                 `Secchi Depth (m)` = secchi_depth_m,
                 `Water temp` = water_temperature_c,
                 pH = round(as.numeric(p_h_in_water_column),3)) |> 
  mutate(Time = as_datetime(openxlsx::convertToDateTime(Time))) |>
  mutate(Time = lubridate::hms(stringr::str_extract(Time,'(?<= ).*')))

# Load in format for data to fit into.
output_format = openxlsx::read.xlsx('C:/Users/CMADSEN/Downloads/Copy of AWQMS BCLSS 2023 Secchi Dip-In Data.xlsx', sep.names = ' ') |> 
  mutate(`Date of Reading` = openxlsx::convertToDate(`Date of Reading`)) |> 
  mutate(Time = as_datetime(openxlsx::convertToDateTime(Time))) |> 
  mutate(Time = lubridate::hms(stringr::str_extract(Time,'(?<= ).*'))) |> 
  mutate(pH = 0) |> 
  as_tibble()

output = output_format[0,] |> 
  bind_rows(dat_for_output) |> 
  dplyr::select(all_of(names(output_format)))

output

openxlsx::write.xlsx(output, 'C:/Users/CMADSEN/Downloads/Secchi Sampling Records in AWQMS BCLSS 2023 Secchi format.xlsx')
