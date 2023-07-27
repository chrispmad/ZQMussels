# Load libraries
library(readxl)
library(sf)
library(dplyr)
library(stringr)
library(purrr)

# Load options
my_opts = readr::read_csv(paste0(str_extract(getwd(),".*ZQMussels[/]?"),"/Options.csv"))

my.year = my_opts$year

# Load data

## Our lab data from 2022
lab_dat = read_excel(paste0(my_opts$zqm_operations_data_folder,'Lake Monitoring/',my.year,'/Lab Analysis/Final report and data/BC Veliger Sampling Inventory 2022_FinalReport.xlsx'))

## HCTF sampling list
hctf = read_excel('04_Extra_Figures_and_Scripts/data/Appendix 1 for MoE report_2022 Draft.xlsx')

hctf = hctf |> 
  dplyr::select(-...1) |> 
  # Remove the final row, which is a total. Otherwise, throws off our numbers. 
  filter(Waterbody != 'Total Samples') |> 
  # Change some lake names to match the naming style of our own lab data.
  mutate(Waterbody = case_when(
    Waterbody == 'Lake Revelstoke' ~ 'Revelstoke Reservoir',
    Waterbody == 'Upper Arrow Lake' ~ 'Arrow Lake, Upper',
    Waterbody == 'Kootenay River' ~ 'Kootenay River (Nelson)',
    Waterbody == 'Lake Enid' ~ 'Enid Lake',
    Waterbody == 'St. Mary Lake' ~ "St Mary's",
    T ~ Waterbody
  ))

# There are a couple of waterbodies with multiple rows in the hctf table.
# Add together the plankton samples and substrate samples for such rows
# so that we have unique waterbody names for each row.
hctf = hctf |> 
  group_by(Waterbody) |> 
  summarise(across(everything(), \(x) sum(x,na.rm=T)))

# Polygons for Fraser and Columbia priority watershed areas
# (made from the subwatershed groups polygon)
fraser = read_sf(paste0(my_opts$remote_spatial_data,'shared_data_sets/fraser_watershed_priority_area.gpkg'))

columbia = read_sf(paste0(my_opts$remote_spatial_data,'shared_data_sets/columbia_watershed_priority_area.gpkg'))

# Simplify the watershed priority areas into monolithic shapes.
fraser = fraser |> 
  summarise(priority_area = 'Fraser')

columbia = columbia |> 
  summarise(priority_area = 'Columbia')

# Make our lab data into a spatial table
lab_sf = lab_dat |> 
  dplyr::rename(
    lat = `Lat (Decimal degrees)`,
    lng = `Long (Decimal degrees)`
  ) |>
  mutate(lat = as.numeric(lat),
         lng = as.numeric(lng)) |> 
  st_as_sf(coords = c("lng","lat"),
           crs = 4326)

# Spatial match between our lab samples and the two priority watershed areas.
lab_matched = lab_sf |> 
  st_join(fraser, st_intersects) |> 
  st_join(columbia, st_intersects) |> 
  mutate(watershed_priority_area = coalesce(priority_area.x,
                                              priority_area.y)) |> 
  dplyr::select(-priority_area.x,-priority_area.y)

# Now we can just drop the lakes that did not match to either
# of the priority areas.
lab_matched = lab_matched |> 
  filter(!is.na(watershed_priority_area))

# At this point, if we join this table up with our list of HCTF
# sampled lakes, we can see how many lakes were sampled by HCTF in priority areas.
lab_matched |> 
  dplyr::select(Waterbody) |>
  left_join(hctf |> 
              dplyr::select(Waterbody) |> 
              distinct() |> 
              mutate(hctf = T)) |> 
  filter(!is.na(hctf))
# Looks like 757 samples. This might be incorrect; it may be 
# correct to simply sum the number of plankton tows directly in the 
# HCTF table.
sum(hctf$`# Plankton Samples`)
# 759 if we just sum the number of plankon samples in the HCTF sheet.

length(unique(hctf$Waterbody))
# 64 unique waterbodies sampled by HCTF.

sum(hctf$`# Substrate samplers`)
# 52 substrate sites.

nrow(hctf[hctf$`# Substrate samplers` > 0,])
# 28 waterbodies with subtrate samples.

# Leaflet test
library(leaflet)
leaflet() |> 
  addTiles() |> 
  addCircleMarkers(
    label = ~Waterbody,
    data = lab_matched |> 
      st_join(bcmaps::bc_cities() |> st_transform(crs = 4326) |> 
                dplyr::select(city_name = NAME),
              st_nearest_feature) |> 
      mutate(lat = unlist(map(lab_matched$geometry,2)),
             lng = unlist(map(lab_matched$geometry,1))) |> 
      mutate(coords = paste0(round(lat,3),", ",round(lng,3))) |> 
      dplyr::select(Waterbody,
                    coords,
                    city_name)
  )
    
# And match to the nearest city.
lab_matched = lab_matched |> 
  st_join(bcmaps::bc_cities() |> st_transform(crs = 4326) |> 
            dplyr::select(city_name = NAME),
          st_nearest_feature) |> 
  mutate(lat = unlist(map(lab_matched$geometry,2)),
         lng = unlist(map(lab_matched$geometry,1))) |> 
  mutate(coords = paste0(round(lat,3),", ",round(lng,3))) |> 
  dplyr::select(Waterbody,
                coords,
                city_name) |> 
  st_drop_geometry() |> 
  group_by(Waterbody,city_name) |> 
  add_count() |> 
  group_by(Waterbody) |> 
  arrange(Waterbody,desc(n)) |> 
  dplyr::select(-n) |> 
  slice(1) |> 
  ungroup()

# Make sure the lake names are not duplicated.
lab_matched = lab_matched |> 
  mutate(Waterbody = str_squish(Waterbody)) |> 
  distinct()

# Quick comparison with the HCTF list.
hctf |> 
  filter(Waterbody %in% lab_matched$Waterbody)
# 65 lakes in common. Which are not in common?

hctf |> 
  filter(!Waterbody %in% lab_matched$Waterbody)
# Lake Revelstoke, Upper Arrow Lake, Kootenay River, Lake Enid, St. Mary Lake

# # Correct the name mismatches identified above.
# hctf = hctf |> 
#   mutate(Waterbody = case_when(
#     Waterbody == 'Lake Revelstoke' ~ 'Revelstoke Reservoir',
#     Waterbody == 'Upper Arrow Lake' ~ 'Arrow Lake, Upper',
#     Waterbody == 'Kootenay River' ~ 'Kootenay River (Nelson)',
#     Waterbody == 'Lake Enid' ~ 'Enid Lake',
#     Waterbody == 'St. Mary Lake' ~ "St Mary's",
#     T ~ Waterbody
#   ))

# For HCTF list, any waterbodies that were ONLY subtrate?
hctf |> 
  filter(`# Substrate samplers` > 0 & `# Plankton Samples` == 0)
#Nope!

# How many waterbodies were sampled for substrate?
hctf |> 
  filter(`# Substrate samplers` != 0,
         Waterbody != 'Total Samples') |> 
  summarise(number_waterbodies = n(),
         total_samplers = sum(`# Substrate samplers`)
  )

# It looks like we have a few more samples in the lab dat than we do
# in the HCTF datasheet. Which lakes are present in only the former dataset?
lab_dat |> 
  filter(Waterbody %in% lab_matched$Waterbody) |> 
  count(Waterbody) |> 
  left_join(hctf |> 
              count(Waterbody, name = 'hctf')) |> 
  filter(is.na(hctf)) |> 
  mutate(total = sum(n))
# 9 lakes!


unique(
  lab_matched |>
  filter(Waterbody %in% (lab_matched |> 
                           filter(!Waterbody %in% hctf$Waterbody) |> 
                           pull(Waterbody))) |> 
  dplyr::pull(Waterbody)
)

# Number of plankton samples.
sum(hctf$`# Plankton Samples`)

# Number of substrate samples.
sum(hctf$`# Substrate samplers`)

# Number of waterbodies for substrate samples.
length(unique(hctf[hctf$`# Substrate samplers` > 0,]$Waterbody))

# Are these lakes present in some capacity in our list, but perhaps with
# a different spelling of the name?
# Yes for Enid Lake, St Mary's, Kootenay River (Nelson), Arrow Lake, Upper, and Revelstoke Reservoir
# Write out to excel.

# Which lakes are in our sampling, but not in theirs?
lab_matched |> 
  filter(!Waterbody %in% hctf$Waterbody)
# Alouette, Burns, Clucluz, Cultus, Francois, Fraser, Nicola,
# Stuart and Wahleach Lakes, plus Pend d'Oreille River.

# How many samples were taken inside those 2 priority regions
lab_dat |> 
  filter(Waterbody %in% lab_matched$Waterbody)
#804 samples taken in those 2 priority regions.

openxlsx::write.xlsx(lab_matched, '02_IMDP_Figure_Generator/output/sampled lakes in fraser or columbia priority areas.xlsx')

