# Making map of monitored lakes from Mike Sokal's 2023 email.

# First, get the list (and coordinates) of lakes that Mike says were sampled last year.

library(tidyverse)
library(rvest)

# Reading in table of sampled lakes from URL that Mike Sokal provided.
mikelist = rvest::read_html(x = 'https://www2.gov.bc.ca/gov/content/environment/research-monitoring-reporting/monitoring/lake-monitoring/bc-lake-monitoring-network/lake-monitoring-network-sites')

# Make a spatial table from Mike's lake sampling data.
ml_sf = mikelist |> 
  # Get tables from the web page.
  rvest::html_table() |> 
  # Put tables (vertically) together.
  bind_rows() |> 
  # Simplify column names.
  set_names(snakecase::to_snake_case) |> 
  # Select subset of columns.
  dplyr::select(lake_name_ems_site_name, region, site_information) |> 
  # Keep only the first two words in the lake_name column (drop anything in brackets)
  mutate(lake_name_ems_site_name = str_extract(lake_name_ems_site_name, '[a-zA-Z]+ [a-zA-Z]+')) |> 
  # Pull the latitude and longitude info out of the site information column.
  mutate(lat = str_extract(site_information, '[0-9]+\\.[0-9]+'),
         lon = str_extract(site_information, '[0-9]{3}\\.[0-9]{4}')) |> 
  # Convert lat and long data to numeric type.
  mutate(across(c('lat','lon'), as.numeric)) |> 
  # Ensure that longitude is negative.
  mutate(lon = -1*lon) |> 
  # Convert table to spatial table.
  st_as_sf(coords = c("lon","lat"), crs = 4326) |> 
  # Simplify lake_name column name.
  dplyr::rename(lake_name = lake_name_ems_site_name) |> 
  # Retain only lake_name, region and geometry columns.
  dplyr::select(lake_name, region)

# Drop duplicated lake names (multiple samples from same lake)
ml_sf = ml_sf |> 
  filter(!duplicated(lake_name))

# Bring in priority waterbodies
wbs = sf::read_sf('//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/Projects/ZQMussels/2022 IMDP Final Report/data/spatial/Waterbodies_with_binned_and_original_values.shp')

hr = wbs |> 
  filter(Risk_bn >= 2)

# Which of our high-rist waterbodies were sampled in 2022?
hr = hr |> 
  st_join(
    ml_sf |> 
      dplyr::select(sokal_lake_name = lake_name) |> 
      mutate(sampled_2022 = TRUE) |> 
      st_transform(crs = 3005) |> 
      # Add a 100-meter buffer to sample points, in case lat-long was off by a bit.
      st_buffer(dist = 100),
    st_intersects
  )

hr = hr |> 
  mutate(sampled_2022 = ifelse(!is.na(sampled_2022), 'Sampled', 'Not Sampled'))

# Tranform the high-risk waterbody spatial file to WGS 84.
hr = hr |> 
  st_transform(crs = 4326)

hr |> 
  st_drop_geometry() |> 
  count(sampled_2022)
# Only 33 of our high-risk lakes were sampled in 2022?!

# Also add a column to our spatial table from Mike Sokal, indicating
# which samples came from high-priority waterbodies.
ml_sf = ml_sf |> 
  mutate(high_risk_wb = lake_name %in% unique(hr$sokal_lake_name))

ml_sf |> 
  st_drop_geometry() |> 
  count(high_risk_wb)
# Also 33 sampled here match up with our high-risk waterbody list.

# The map should show the priority waterbodies and the samples. If the 
# samples that match up with priority waterbodies, maybe make those points green.
# Samples that don't match a priority waterbody, make those purple.
# Waterbodies could be coloured based on their priority: orange for mid, red for high.

hr = hr |> 
  mutate(map_col = ifelse(Risk_bn == 2, 'orange', 'red'))

ml_sf = ml_sf |> 
  mutate(map_col = ifelse(high_risk_wb == TRUE, 'green','purple'))

# # Decide on colour palette for map.
# my_pal = color(palette = 'viridis',
#                      domain = hr$sampled_2022)
# 
# my_priority_wb_pal = colorFactor(
#   palette = 'Spectral',
#   domain = hr$Risk_bn
# )

l = leaflet() |>
  addProviderTiles(providers$CartoDB.Voyager) |>
  addPolygons(
    color = ~map_col,
    fillColor = ~map_col,
    label = ~GNIS_NA,
    group = 'PriorityWB',
    data = hr
  ) |> 
  addCircleMarkers(
    color = "black",
    weight = 4,
    opacity = 0.8,
    label = ~lake_name,
    # labelOptions = labelOptions(noHide = TRUE),
    fillOpacity = 0.5,
    fillColor = ~map_col,
    group = 'Mike',
    data = ml_sf
  ) |>
  leaflet::addScaleBar('bottomleft') |> 
  leaflet.extras::addSearchFeatures(
    targetGroups = c('PriorityWB'),
    options = searchFeaturesOptions(zoom = 5, openPopup=TRUE))

l

library(data.table)
data.table::data.table(
  hr |> 
    sf::st_drop_geometry()
  )

library(htmlwidgets)
library(webshot)

saveWidget(l, "temp.html", selfcontained = FALSE)

webshot("temp.html", file = "04_Extra_Figures_and_Scripts/output/Mike_Sokal_Lake_Monitoring_2022_Map.jpg",
        cliprect = "viewport",
        zoom = 2,
        vwidth = 900,
        vheight = 800
)

# If we use tmap, could we avoid overlapping labels?
library(tmap)

bc_bound = bcmaps::bc_bound()

# Calculate the bounding box that includes out spatial table,
# plus a buffer of 10,000 meters.
boundingbox = st_bbox(st_buffer(st_as_sfc(st_bbox(ml_sf)),10000))

big_bbox = st_bbox(st_buffer(st_as_sfc(st_bbox(ml_sf)),50000))

# Download background layer for map
background_layer = maptiles::get_tiles(x = sf::st_bbox(bc_bound), 
                                       provider = 'CartoDB.Voyager', 
                                       zoom = 6, crop = T)

full_prov_tm = tm_shape(background_layer, 
                        bbox = big_bbox,
                        raster.downsample = F) +
  tm_rgb() + 
  tm_add_legend(title = '2022 Lake Monitoring',
                type = 'symbol'#,
                # labels = unique(stations_sf$bin_label),
                # col = my_pal(unique(stations_sf$num_insp_b)),
                # size = log(unique(stations_sf$num_insp_b)+1)
                ) + 
  tm_shape(bc_bound) +
  tm_borders(col = 'grey', lwd = 2) +
  tm_shape(ml_sf) + 
  tm_symbols(scale = 1.5,
             col = 'purple',
             alpha = 0.75,
             title.col = '') +
  tm_text("lake_name", auto.placement = 1, 
          size = 0.75) +
  tm_scale_bar(position = c('left','bottom')) + 
  tm_layout(legend.frame = 'black', 
            legend.position = c('right','top'),
            scale = 1.25
            )

full_prov_tm

tmap::tmap_save(full_prov_tm, 
                "04_Extra_Figures_and_Scripts/output/Mike_Sokal_Lake_Monitoring_2022_Map_full_province_tmap.jpg",
                dpi = 150,
                scale = 0.95)
