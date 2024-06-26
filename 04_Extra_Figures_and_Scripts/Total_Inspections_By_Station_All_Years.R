library(openxlsx)
library(tidyverse)
library(bcdata)

all_l = bcdc_list()
all_l[str_detect(all_l, 'boundaries')]

# Read in options file.
my_opts = read_csv("C:/Users/CMADSEN/Downloads/LocalR/long_term_projects/ZQMussels/Options.csv")

my.year = 2024

#Data folders
my.data.folder = paste0(my_opts$zqm_figure_local_folder,"data/")
my.external.data.folder = paste0(my_opts$remote_spatial_data,"Projects/ZQMussels/data/")

#Which folder should we put specific output files in?
my.output.folder = paste0(my_opts$zqm_figure_local_folder,"output/")
my.external.output.folder = paste0(my_opts$zqm_figure_output_remote_folder,my.year)
zqm.operations.folder = my_opts$zqm_operations_data_folder
this.years.report.folder = paste0(my_opts$remote_spatial_data,"Projects/ZQMussels/",my.year," IMDP Final Report/")

# # Read in all-years data; 
# if(!exists('dat_all')){
#   dat_all = read.xlsx(paste0(my.data.folder,'figure_dat_all.xlsx')) |>
#     mutate(TimeOfInspection = openxlsx::convertToDateTime(TimeOfInspection)) |> 
#     as_tibble()
# }
# Read in 'Forward Selection Areas', which are derived from Postal codes.
# Hopefully just gives us postal codes?
pc = sf::read_sf('C:/Users/CMADSEN/Downloads/LocalR/long_term_projects/ZQMussels/04_Extra_Figures_and_Scripts/data/lfsa000b21a_e.shp')

# Remove stuff outside of BC.
pc = pc |> dplyr::filter(str_detect(PRNAME,'British Columbia'))

# Reproject postal code spatial file into WGS 84.
pc = sf::st_transform(pc, 4326)

# Maybe try simplifying too?
pc = rmapshaper::ms_simplify(pc)

ggplot() + geom_sf(data = pc)

# Try to make postal code spatial file valid...
pc = sf::st_make_valid(pc)

# See if I can find my own postal code.
my_coords = tibble(lat = 48.422824263765676, lng = -123.35681570128305) |> 
  st_as_sf(coords = c("lng","lat"), crs = 4326)

my_coords |> 
  sf::st_join(pc)
# Pretty good, but how about the BC Geocoder?

# library(httr)
# library(jsonlite)
# 
# url <- sprintf("https://geocoder.api.gov.bc.ca/addresses.json?point=%f,%f&maxResults=1", -123.35681570128305, 48.422824263765676)
# url <- "https://geocoder.api.gov.bc.ca/sites/nearest.json?point=-122.377%2C50.121&outputSRS=4326&locationDescriptor=any&setBack=0&brief=false&excludeUnits=false&onlyCivic=false"
# url <- sprintf("https://geocoder.api.gov.bc.ca/sites/nearest.json?point=%f,%f&maxResults=1", -123.35681570128305, 48.422824263765676)
# response = httr::GET(url)
# data = fromJSON(content(response, as = "text"), flatten = TRUE)
# full_address <- data$features$properties.fullAddress

# Read in destination waterbody file for all years combined.
wbs = sf::read_sf(paste0(my_opts$remote_spatial_data,"Projects/ZQMussels/data/Waterbodies_with_Inspection_Data_Summaries_all_years.gpkg"))

wbs = wbs |> 
  dplyr::arrange(dplyr::desc(TotalInspections))

# Add on ZIP codes.
wbs_w_zip = sf::st_join(
  wbs,
  pc |> dplyr::select(zipcode = CFSAUID) |> sf::st_transform(3005)
)

wbs_w_zip = wbs_w_zip |> 
  dplyr::filter(!GNIS_NA %in% c("Dry Storage","Pacific Ocean")) |> 
  sf::st_transform(4326) |> 
  dplyr::mutate(Centroid_Lat = st_coordinates(st_centroid(geom))[,2],
                Centroid_Lng = st_coordinates(st_centroid(geom))[,1]) |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(Watershed_ID = WATERSH, Waterbody_Name = GNIS_NA, Total_Inspections = TotalInspections,
                  Centroid_Lat, Centroid_Lng) |> 
  dplyr::summarise(Zipcode = paste0(zipcode, collapse = ', ')) |> 
  dplyr::ungroup()

wbs_w_zip = wbs_w_zip |> 
  dplyr::arrange(dplyr::desc(Total_Inspections))

openxlsx::write.xlsx(wbs_w_zip, paste0(my_opts$base_dir,'04_Extra_Figures_and_Scripts/output/Destination_Waterbodies_Total_Inspections_with_Postal_Codes.xlsx'))
