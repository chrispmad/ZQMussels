offsets <- tibble::tribble(
  ~map_label,                ~lat_offset, ~lon_offset,
  "Lower Mainland Roving",    0.10,        -1.00,
  "Penticton Roving",        -0.10,         1.50,
  "Cutts (Hwy 93)",          -0.10,         1.25,
  "Sumas",                   -0.50,         0.50,
  "Lower Mainland",           0.10,        -0.50,
  "Penticton",               -0.10,         1.00,
  "Yahk",                    -0.50,         NA,
  "Douglas \nCrossing",      -0.50,        -0.80,
  "Sumas Border",            -0.50,         0.50,
  
)

adjust_station_labels <- function(data, offsets, base_lat_offset = 0.25) {
  data |>
    sf::st_drop_geometry() |>
    dplyr::left_join(offsets, by = "map_label") |>
    dplyr::mutate(
      lat = lat + base_lat_offset + dplyr::coalesce(lat_offset, 0),
      lon = lon + dplyr::coalesce(lon_offset, 0)
    ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
}