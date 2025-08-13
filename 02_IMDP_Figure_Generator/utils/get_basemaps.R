# Make a custom-sized viewing box around stations in BC.
bc_station_view = st_bbox(tibble(lon = c(-127.58, -112),
                                 lat = c(48, 56)) |> 
                            st_as_sf(coords = c("lon","lat"), crs = 4326))

bc_stations_basemap_colour<- basemaps::basemap_terra(ext = bc_station_view, map_service = 'esri', map_type = 'world_street_map')
#basemaps::get_maptypes()
bc_stations_basemap_white<- basemaps::basemap_terra(ext = bc_station_view, map_service = 'carto', map_type = 'light')

#roving position
bc_station_view_roving = st_bbox(tibble(lon = c(-126, -114.5),
                                        lat = c(48, 52)) |> 
                                   st_as_sf(coords = c("lon","lat"), crs = 4326))
bc_stations_basemap_green_roving = maptiles::get_tiles(x = bc_station_view_roving, provider = 'OpenStreetMap', zoom = 7, crop = T)

northamerica_view = tibble(lon = c(-174,-50),
                           lat = c(73,11.2)) |> 
  st_as_sf(coords = c("lon","lat"), crs = 4326)

# Download maptiles North America
# northamerica_basemap = maptiles::get_tiles(x = northamerica_view, provider = 'CartoDB.Positron', zoom = 4, crop = F)
north_america_stations_basemap_colour<- basemaps::basemap_terra(ext = northamerica_view, map_service = 'esri', map_type = 'world_street_map')
