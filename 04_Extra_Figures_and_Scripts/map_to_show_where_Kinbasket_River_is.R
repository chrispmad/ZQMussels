library(sf)

wbs = read_sf(paste0("//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/Projects/ZQMussels/2025 IMDP Final Report/data/spatial/waterbodies_zqm_shortlist_risk_estimates.gpkg"))

kb = wbs |> 
  dplyr::filter(GNIS_NA == "Kinbasket River")

library(basemaps)
library(ggplot2)
library(terra)
library(tidyterra)
library(bcdata)

kb_b = st_buffer(kb, 20000)

my_bm = basemaps::basemap_terra(ext = kb_b, map_service = 'esri', map_type = "world_imagery")
map_frame = terra::ext(terra::project(my_bm,"EPSG:3005")) |> 
  terra::as.polygons(map_frame) |> 
  sf::st_as_sf()
map_frame = sf::st_set_crs(map_frame, sf::st_crs(kb_b))

all_c = bcmaps::bc_cities()
the_c = all_c |> sf::st_filter(kb_b)
near_lakes = bcdata::bcdc_query_geodata("freshwater-atlas-lakes") |> 
  filter(INTERSECTS(kb_b)) |> 
  filter(AREA_HA > 150) |> 
  collect() |> 
  sf::st_intersection(map_frame)

kb_centr_wgs = kb |> 
  sf::st_transform(4326) |> 
  st_centroid() |> 
  dplyr::mutate(lat = st_coordinates(geom)[,2],
                lng = st_coordinates(geom)[,1]) |> 
  dplyr::select(GNIS_NA, lat, lng) |> 
  mutate(lat = round(lat, 3), lng = round(lng, 3)) |> 
  mutate(label = paste0(lat, ", ",lng)) |> 
  st_transform(crs = 32611)
  
ggplot() + 
  tidyterra::geom_spatraster_rgb(data = my_bm) +
  # geom_sf(data = kb_b, fill = 'transparent') + 
  geom_sf(data = kb, aes(col = GNIS_NA), fill = 'transparent') + 
  geom_sf(data = near_lakes, aes(col = GNIS_NAME_1), fill = 'transparent') +
  geom_sf_label(data = kb_centr_wgs, aes(label = label), nudge_x = 20000) +
  labs(col = "Location") +
  ggthemes::theme_map() + 
  theme(legend.position = 'right') +
  ggspatial::annotation_scale() + 
  ggspatial::annotation_north_arrow(pad_x = unit(7.5, "cm"), pad_y = unit(8.8, "cm"))
