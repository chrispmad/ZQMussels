library(sf)
library(tidyverse)
library(bcdata)
library(magick)

my_opts = read_csv("Options.csv")

dat = read_rds('03_PrioritizationModel/data/data_for_making_shortlist.rds')

wbs = dat[[2]]
rm(dat)

waterbodies_for_stream_search = wbs |> 
  dplyr::filter(!GNIS_NA %in% c("Dry Storage","Pacific Ocean")) |> 
  dplyr::group_by(WATERSH, GNIS_NA) |> 
  summarise() |> 
  dplyr::arrange(WATERSH,GNIS_NA)

number_stream_and_road_table = waterbodies_for_stream_search |> 
  sf::st_drop_geometry() |> 
  dplyr::filter(!GNIS_NA %in% c("Dry Storage","Pacific Ocean")) |> 
  dplyr::arrange(WATERSH,GNIS_NA)

number_stream_and_road_table$number_streams = NA
number_stream_and_road_table$number_roads = NA

for(this_row in 1:nrow(number_stream_and_road_table)){
  print(this_row)
  
  if(is.na(number_stream_and_road_table[this_row,]$number_streams))
  
  # STREAMS
  # Search BC Data Catalogue for streams within 5 meters of this waterbody.
  stream_res = tryCatch(
    bcdc_query_geodata('freshwater-atlas-stream-network') |> 
    filter(bcdata::DWITHIN(waterbodies_for_stream_search[this_row,]$geom, distance = 5, units = 'meters')) |> 
    collect() |> 
    dplyr::filter(FEATURE_SOURCE != 'lake-def skelet',
                  FEATURE_SOURCE != 'OP') |> 
      dplyr::filter(!duplicated(BLUE_LINE_KEY)),
   
    # If no stream available, return a simple table with number_streams of 0.
    error = function(e) data.frame(number_streams = 0)
  )
  
  # If the query returns a blank table (e.g. there were only lake-def skelet pieces),
  # nrow should now be 0; convert 'res' to equivalent to an error (i.e. value of 0)
  if(nrow(stream_res) == 0){
    stream_res = data.frame(number_streams = 0)
  }
  
  # ROADS 
  # Search BC Data Catalogue for roads within 15 meters of this waterbody.
  road_res = tryCatch(
    bcdc_query_geodata('digital-road-atlas-dra-master-partially-attributed-roads') |> 
      filter(bcdata::DWITHIN(waterbodies_for_stream_search[this_row,]$geom, distance = 15, units = 'meters')) |> 
      collect(),
    
    # If no stream available, return a simple table with number_streams of 0.
    error = function(e) data.frame(number_roads = 0)
  )
  
  # If the query returns a blank table (e.g. there were only lake-def skelet pieces),
  # nrow should now be 0; convert 'res' to equivalent to an error (i.e. value of 0)
  if(nrow(road_res) == 0){
    road_res = data.frame(number_roads = 0)
  }
  
  # Make a plot if we're in here ourselves.
  if(interactive()){
    ggplot() + 
      geom_sf(data = waterbodies_for_stream_search[this_row,]) + 
      geom_sf(data = stream_res, col = 'blue') + 
      geom_sf(data = road_res, col = 'red') + 
      labs(title = 'Red are roads, blue are streams')
  }
  
  # If 'stream_res' or 'road_res' are not empty, the first column will be 'id'; double check that
  # matched streams are within 5 meters. This is necessary only in the case of 
  # VERY large waterbodies because they end up searching BC data catalogue with
  # a bounding box rather than a polygon.
  if(names(stream_res)[1] == 'id'){
    stream_res = stream_res |> 
      filter(st_intersects(geometry, st_buffer(waterbodies_for_stream_search[this_row,],5), sparse = F)) |> 
      summarise(number_streams = n()) |> 
      st_drop_geometry()
  }
  if(names(road_res)[1] == 'id'){
    road_res = road_res |> 
      filter(st_intersects(geometry, st_buffer(waterbodies_for_stream_search[this_row,],15), sparse = F)) |> 
      summarise(number_roads = n()) |> 
      st_drop_geometry()
  }
  
  # Add the number of streams within 5 m of waterbody to our results table.
  number_stream_and_road_table[this_row,]$number_streams = stream_res$number_streams
  number_stream_and_road_table[this_row,]$number_roads = road_res$number_roads
}

write_csv(number_stream_and_road_table,'03_PrioritizationModel/data/number_stream_and_road_table.csv')


## Make figure for the report file.

shus = waterbodies_for_stream_search |> 
  dplyr::filter(GNIS_NA == 'Shuswap Lake')

shus_b = st_buffer(shus, 5)

shus_streams = bcdc_query_geodata('freshwater-atlas-stream-network') |> 
  filter(bcdata::DWITHIN(shus$geom, distance = 5, units = 'meters')) |> 
  collect() |> 
  dplyr::filter(FEATURE_SOURCE != 'lake-def skelet',
                FEATURE_SOURCE != 'OP') |> 
  dplyr::filter(!duplicated(BLUE_LINE_KEY))

shus_roads = bcdc_query_geodata('digital-road-atlas-dra-master-partially-attributed-roads') |> 
  filter(bcdata::DWITHIN(shus$geom, distance = 15, units = 'meters')) |> 
  collect()

zoom_in_view = st_as_sfc(
  st_bbox(
    data.frame(
      x = c(1468024.5,1476890.1),
      y = c(653000,664000)) |> 
      st_as_sf(coords = c('x','y'),
               crs = 3005)))

panel_full = ggplot() + 
  geom_sf(data = shus, fill = 'lightblue') + 
  geom_sf(data = shus_streams, col = 'red',lwd = 1) + 
  geom_sf(data = shus_roads, col = 'orange',lwd = 1) + 
  geom_text(aes(x = 1465024.5, y = 710000),
            label = paste0('Number of \nabutting streams: ',nrow(shus_streams))) + 
  geom_text(aes(x = 1465024.5, y = 700000),
            label = paste0('Number of \nnearby roads: ',nrow(shus_roads))) +
  ggthemes::theme_map() +
  geom_sf(data = zoom_in_view,
          col = 'purple',
          fill = 'transparent')

panel_zoomed = ggplot() + 
  geom_sf(data = shus, fill = 'lightblue') + 
  geom_sf(data = shus_streams, col = 'red', lwd = 1) +  
  geom_sf(data = shus_roads, col = 'orange', lwd = 1) + 
  ggthemes::theme_map() + 
  coord_sf(xlim = c(st_coordinates(zoom_in_view)[c(1,2),1]),
           ylim = c(st_coordinates(zoom_in_view)[c(2,3),2])) + 
  theme(panel.border = element_rect(colour = 'purple', fill = 'transparent'))

combo_fig = ggpubr::ggarrange(panel_full, panel_zoomed, ncol = 2)

ggsave('03_PrioritizationModel/data/abutting_stream_example.png',
       width = 6, height = 4)

# image_read('03_PrioritizationModel/data/abutting_stream_example.png') |> 
