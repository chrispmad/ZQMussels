library(tmap)
library(sf)
library(tidyverse)

# Options
my_opts = read_csv(paste0(str_extract(getwd(),".*ZQMussels[/]?"),"/Options.csv"))

# Download station spatial file
stations = read_sf(paste0(my_opts$remote_spatial_data,
                          'Projects/ZQMussels/data/inspection_stations.gpkg'))

stats23 = stations %>% 
  filter(station_name %in% c("Dawson Creek","Mt. Robson","Golden","Olsen",
                             "Yahk","Penticton Roving","Osoyoos","Lower Mainland Roving","Fraser Valley Roving")) %>% 
  mutate(station_type = ifelse(station_type == 'Unknown', 'Roving Inspection Crew', 'Permanent Inspection Station')) %>% 
  mutate(station_name = replace(station_name, station_name == 'Fraser Valley Roving', 'Lower Mainland Roving'),
         map_label = replace(station_name, station_name == 'Fraser Valley Roving', 'Lower Mainland Roving'))


# Make a custom-sized viewing box around stations in BC.
bc_station_view = st_bbox(tibble(lon = c(-127.58, -112),
                                 lat = c(48, 56)) %>% 
                            st_as_sf(coords = c("lon","lat"), crs = 4326))

bc_bound = bcmaps::bc_bound()

# Download maptiles for BC station map
bc_stations_basemap = maptiles::get_tiles(x = bc_station_view, provider = 'CartoDB.Positron', zoom = 6, crop = F)

# Set plotting mode for tmap.
tmap_mode('plot')

map_1_tmap = tm_shape(bc_stations_basemap, bbox = bc_station_view) +
  tm_rgb() + 
  tm_add_legend(title = '2023 Watercraft \nInspection Stations') + 
  tm_shape(bc_bound) + 
  tm_borders(col = 'grey', lwd = 2) + 
  tm_shape(stats23) + 
  tm_symbols(col = "station_type", title.col = '') +
  tm_text("map_label", 
          ymod = -0.5,
          size = 0.65) +
  tm_scale_bar() + 
  tm_layout(legend.frame = 'black', 
            legend.position = c('right','top'),
            scale = 1.25)

map_1_tmap

tmap_save(tm = map_1_tmap, filename = paste0(my_opts$base_dir,'04_Extra_Figures_and_Scripts/output/Inspection_Stations_2023.png'), dpi = 300)

          