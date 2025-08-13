sources_centroid = read_sf(paste0(my_opts$remote_spatial_data,'Projects/ZQMussels/',my.year,' IMDP Final Report/data/spatial/Inspections_by_source_centroid.gpkg'))

# Add some labelling logic:
# 1. If a given state/province/estado has no inspections, its name should be grey.
#    and it should have no dot.
sources_centroid = sources_centroid |> 
  mutate(map_label_colour = ifelse(is.na(TotalInsp), 'transparent', 'black'))

sources_centroid = sources_centroid |> 
  mutate(jenks = list(BAMMtools::getJenksBreaks(TotalInsp, k = 6))) |> 
  unnest_wider(jenks, names_sep = '_') |> 
  mutate(TotalInsp_b = case_when(
    TotalInsp <= jenks_2 ~ 1,
    TotalInsp <= jenks_3 ~ 2,
    TotalInsp <= jenks_4 ~ 3,
    TotalInsp <= jenks_5 ~ 4,
    TotalInsp <= jenks_6 ~ 5,
  )) |>
  mutate(bin_label = case_when(
    TotalInsp_b == 1 ~ paste0(jenks_1, ' - ', jenks_2),
    TotalInsp_b == 2 ~ paste0(jenks_2+1, ' - ', jenks_3),
    TotalInsp_b == 3 ~ paste0(jenks_3+1, ' - ', jenks_4),
    TotalInsp_b == 4 ~ paste0(jenks_4+1, ' - ', jenks_5),
    TotalInsp_b == 5 ~ paste0(jenks_5+1, ' - ', jenks_6)
  )) |> 
  arrange(TotalInsp)

sources_centroid = st_set_geometry(sources_centroid, sources_centroid$geom)

# Remove rows for Mexican estados without inspections.
sources_centroid = sources_centroid |> 
  filter(!(NAME_0 == 'Mexico' & is.na(TotalInsp)))

source_pal = leaflet::colorFactor(
  palette = 'Spectral',
  domain = unique(sources_centroid$TotalInsp_b),
  reverse = T
)



# Split the centroid spatial object into 2: one with high risk inspections,
# and one without.
centr_with_dat = sources_centroid |> filter(!is.na(TotalInsp))
centr_without_dat = sources_centroid |> filter(is.na(TotalInsp))

# Add in colours as new column.
centr_with_dat = centr_with_dat |>
  mutate(my_colors = source_pal(TotalInsp_b))

centr_offset = centr_with_dat |> 
  dplyr::mutate(lat = sf::st_coordinates(geom)[,2],
                lng = sf::st_coordinates(geom)[,1]) |> 
  dplyr::mutate(lat = lat - 2,
                lng = lng - 2) |> 
  sf::st_drop_geometry() |> 
  sf::st_as_sf(coords = c("lng","lat"), crs = 4326)