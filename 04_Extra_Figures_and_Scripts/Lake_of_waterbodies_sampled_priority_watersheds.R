# Map with points on waterbodies, colour of which is binary, either just sampled 
# for plankton tows, or both kinds of samples.

library(tidyverse)
library(sf)
library(readxl)
library(tmap)
library(leaflet)

# Load in lake sampling data.
my_opts = read_csv(paste0(str_extract(getwd(),".*ZQMussels[/]?"),"/Options.csv"))

# samples = read_excel(paste0(my_opts$base_dir,'02_IMDP_Figure_Generator/data/lake_monitoring_for_report_appendix.xlsx'))
lab_dat = read_excel(paste0(my_opts$zqm_operations_data_folder,'Lake Monitoring/',my.year,'/Lab Analysis/Final report and data/BC Veliger Sampling Inventory 2022_FinalReport.xlsx'))

wb = read_sf('W:/CMadsen/Projects/ZQMussels/2022 IMDP Final Report/data/spatial/Waterbodies_with_binned_and_original_values.shp') %>% 
  st_transform(crs = 4326)

dat = read_excel(paste0(my_opts$base_dir,'02_IMDP_Figure_Generator/output/sampled lakes in fraser or columbia priority areas.xlsx')) |> 
  mutate(lng = str_remove_all(coords, '.*, '),
         lat = str_remove_all(coords, ',.*'))

## HCTF list
hctf = read_excel('04_Extra_Figures_and_Scripts/data/Appendix 1 for MoE report_2022 Draft.xlsx')

# 2 Priority watersheds
fraser = read_sf(paste0(my_opts$remote_spatial_data,'shared_data_sets/fraser_watershed_priority_area.gpkg'))

columbia = read_sf(paste0(my_opts$remote_spatial_data,'shared_data_sets/columbia_watershed_priority_area.gpkg'))

# Simplify the watershed priority areas into monolithic shapes.
fraser = fraser |> 
  summarise(priority_area = 'Fraser')

columbia = columbia |> 
  summarise(priority_area = 'Columbia')

priority_wb = bind_rows(fraser, columbia)

hctf = hctf |> 
  filter(!duplicated(Waterbody),
         `# Substrate samplers` > 0) |> 
  mutate(substrate = T) |> 
  dplyr::select(Waterbody, substrate)

# Join dat and HCTF list.
dat = dat |> 
  left_join(hctf) |> 
  mutate(substrate = replace_na(substrate, FALSE))

dat_sf = st_as_sf(dat, coords = c('lng','lat'),
                  crs = 4326)

dat_sf = dat_sf |> 
  mutate(substrate = ifelse(substrate == T, 'Plankton and Substrate', 'Plankton Only'))

# subwatersheds = read_sf("W:/CMadsen/shared_data_sets/WatershedGroups.shp") |> 
#   st_transform(crs = 4326)

# Map
boundingbox = st_bbox(st_buffer(st_as_sfc(st_bbox(dat_sf)),10000))

background_layer = maptiles::get_tiles(x = boundingbox, 
                                       # provider = 'CartoDB.Positron', 
                                       provider = 'CartoDB.Voyager', 
                                       zoom = 7, crop = T)

my_pal = colorFactor(palette = 'Set1',
                     domain = dat_sf$substrate)

my_priority_wb_pal = colorFactor(
  palette = 'BrBG',
  domain = priority_wb$priority_area
)

l = leaflet() |>
  # addProviderTiles(providers$OpenTopoMap)
  # addProviderTiles(providers$Stamen.TerrainBackground)
  # addProviderTiles(providers$Esri.WorldTopoMap) #Very nice
  # addProviderTiles(providers$Stamen.Terrain)
  addProviderTiles(providers$CartoDB.Voyager) |>
  addPolygons(
    color = ~my_priority_wb_pal(priority_area),
    weight = 4,
    opacity = 0.8,
    fillOpacity = 0.00,
    fillColor = ~my_priority_wb_pal(priority_area),
    data = priority_wb
  ) |> 
  addCircleMarkers(
    col = ~my_pal(substrate),
    fillOpacity = 0.50,
    opacity = 0.50,
    # weight = 10,
    radius = 5,
    data = dat_sf
  ) |> 
  addLegend(title = 'Samples Taken',
            pal = my_pal,
            values = dat_sf$substrate,
            opacity = 0.5) |> 
  addLegend(title = 'Priority Watersheds',
            pal = my_priority_wb_pal,
            values = priority_wb$priority_area,
            opacity = 0.8) |> 
  leaflet::addScaleBar('bottomleft')

library(htmlwidgets)
library(webshot)

saveWidget(l, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "02_IMDP_Figure_Generator/output/Lake_sampling_map.jpg",
        cliprect = "viewport",
        zoom = 2,
        vwidth = 750,
        vheight = 600
)

# # Set plotting mode for tmap.
# tmap_mode('plot')
# 
# tm_shape(background_layer)+ #, bbox = boundingbox) +
#   tm_rgb() +
# tm_shape(dat_sf) +
#   tm_symbols(col = "substrate", title.col = '') +
# tm_add_legend(type = 'symbol',
#               border.col = 'grey40',
#               title = 'Samples Taken')
