offsets <- tibble::tribble(
  ~map_label,                ~lat_offset, ~lon_offset,
  "Lower Mainland",    0.10,        -1.00,
  "Penticton",        -0.10,         1.00,
  "Cutts (Hwy 93)",          -0.10,         1.25,
  "Yahk",                    -0.50,         NA,
  "Douglas",      -0.50,        -0.80,
  "Sumas",            -0.50,         0.50,
  "Keremeos",                NA,            -0.5,
  "Osoyoos",                 -0.5,            -0.5,
  
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