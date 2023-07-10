library(ggplot2)
library(sf)
library(leaflet)
# Load in data.

watersheds = sf::read_sf('W:/CMadsen/shared_data_sets/WatershedGroups_lowres.shp') |> 
  st_transform(crs = 4326)

watershed_names = watersheds |> 
  st_drop_geometry() |> 
  dplyr::rename(watershed_name = WATERSHE_1) |> 
  dplyr::arrange(watershed_name) |> 
  dplyr::pull(watershed_name)

fraser = watersheds |> 
  dplyr::filter(WATERSHE_1 %in% c("Lower Fraser","Harrison River",
                                  "Chilliwack River","Fraser Canyon",
                                  "Lower Nicola River","Nicola River",
                                  "Guichon Creek","Lillooet","Seton Lake",
                                  "Big Bar Creek","Dog Creek","Big Creek",
                                  "Taseko River","Chilko River",
                                  "Lower Chilcotin River","Upper Fraser River",
                                  "Morkill River","Upper North Thompson River",
                                  "Murtle Lake","Clearwater River","Mahood Lake",
                                  "Upper Shuswap","Shuswap Lake","Adams River",
                                  "Lower North Thompson River","Bridge Creek",
                                  "Green Lake","Deadman River","Bonaparte River",
                                  "Thompson River","South Thompson River",
                                  "Middle Fraser","Twan Creek","Quesnel River",
                                  "Cottonwood River","Blackwater River",
                                  "San Jose River","Horsefly River","Narcosli Creek",
                                  "Upper Chilcotin River","Lower Chilako River",
                                  "Tabor River","Willow River","Herrick Creek",
                                  "McGregor River","Bowron","Cariboo River",
                                  "Nazko River","Euchiniko River","Euchiniko Lake",
                                  "Chilako River","Lower Eutsuk Lake","Upper Nechako Reservoir",
                                  "Lower Nechako Reservoir","Upper Eutsuk Lake",
                                  "Francois Lake","Nechako River","Cheslatta River",
                                  "Stuart River","Stuart Lake","Lower Salmon River",
                                  "Salmon River","Lower Trembleur Lake","Middle River",
                                  "Upper Trembleur Lake","Takla Lake","Driftwood River")) |> 
  dplyr::filter(WATERSHED1 != 'SALM')

ggplot() + 
  geom_sf(data = fraser, fill = 'lightblue')

watersheds_l = watersheds |> 
  dplyr::mutate(already_selected = WATERSHE_1 %in% unique(fraser$WATERSHE_1))

leaflet() |> 
  addPolygons(
    color = 'grey',
    weight = 1,
    data = watersheds_l
  ) |> 
  addPolygons(
    label = ~paste0(WATERSHE_1,", ",WATERSHED1),
    fillColor = 'lightblue',
    fillOpacity = 0.8,
    data = fraser
  )

columbia = watersheds |> 
  dplyr::filter(WATERSHE_1 %in% c("Elk River","Bull River",
                                  "Kootenay Lake","St. Mary River",
                                  "Kootenay River","Columbia River",
                                  "Kicking Horse River","Columbia Reach",
                                  "Revelstoke Lake","Upper Arrow Lake",
                                  "Lower Arrow Lake","Kettle River",
                                  "Okanagan River","Similkameen River",
                                  "Duncan Lake","Slocan River",
                                  "Canoe Reach"))

leaflet() |> 
  addPolygons(
    color = 'grey',
    label = ~paste0(WATERSHE_1,", ",WATERSHED1),
    weight = 1,
    data = watersheds_l
  ) |> 
  addPolygons(
    color = 'black',
    fillColor = 'lightblue',
    fillOpacity = 0.8,
    label = ~paste0(WATERSHE_1,", ",WATERSHED1),
    data = columbia
  )

write_sf(fraser, "W:/CMadsen/shared_data_sets/fraser_watershed_priority_area.gpkg")
write_sf(columbia, "W:/CMadsen/shared_data_sets/columbia_watershed_priority_area.gpkg")
