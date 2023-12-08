library(readxl)
library(tidyverse)
library(sf)
library(leaflet)
library(tmap)

my_opts = read_csv('./Options.csv')

my.year = 2023

lab_dat = read_excel(paste0(my_opts$zqm_operations_data_folder,'Lake Monitoring/',my.year,'/Lab results/Copy of BC Veliger Sampling Inventory 2023Final Report.xlsx'))

# Clean up the lat / long columns.
lab_sf = lab_dat |> 
  filter(!is.na(...1)) |> 
  filter(!is.na(`Lat (Decimal degrees)`)) |> 
  mutate(lat = `Lat (Decimal degrees)`,
         lng = `Long (Decimal degrees)`) |> 
  mutate(
    lng = str_replace_all(lng, '^\\.', '')
  ) |>
  mutate(lat = as.numeric(lat),
         lng = as.numeric(lng)) |> 
  st_as_sf(
    coords = c("lng","lat"),
    crs = 4326
  ) |> 
  mutate(`sampling group/agency` = case_when(
    `sampling group/agency` == 'MLWRS' ~ 'WLRS',
    `sampling group/agency` == 'MOECCS PG' ~ 'ENV',
    T ~ `sampling group/agency`
  ))


sampling_pal = leaflet::colorFactor(
  palette = 'Set3',
  domain = lab_sf$`sampling group/agency`
)

lab_sf = lab_sf %>% 
  mutate(my_colors = sampling_pal(`sampling group/agency`))

bc_bound = bcmaps::bc_bound()

# Make a custom-sized viewing box around stations in BC.
bc_station_view = st_bbox(tibble(lon = c(-127.58, -112),
                                 lat = c(48, 56)) %>% 
                            st_as_sf(coords = c("lon","lat"), crs = 4326))

# Download maptiles for BC station map
bc_stations_basemap = maptiles::get_tiles(x = bc_station_view, provider = 'CartoDB.Positron', zoom = 6, crop = F)


map_9_tmap = tm_shape(bc_stations_basemap, bbox = bc_station_view) +
  tm_rgb() + 
  tm_add_legend(title = '2023 Invasive \nMussel Lake Monitoring',
                type = 'symbol',
                labels = unique(lab_sf$`sampling group/agency`)[order(unique(lab_sf$`sampling group/agency`))],
                col = sampling_pal(unique(lab_sf$`sampling group/agency`))) + 
  tm_shape(bc_bound) + 
  tm_borders(col = 'grey', lwd = 2) + 
  tm_shape(lab_sf) + 
  tm_symbols(col = "my_colors",
             title.col = '') +
  tm_scale_bar() + 
  tm_layout(legend.frame = 'black', 
            legend.position = c('right','top'),
            scale = 1.25)

map_9_tmap

tmap_save(tm = map_9_tmap, filename = './04_Extra_Figures_and_Scripts/output/2023_lake_monitoring_map.jpg', width = 7.6, height = 6.4, dpi = 300)

