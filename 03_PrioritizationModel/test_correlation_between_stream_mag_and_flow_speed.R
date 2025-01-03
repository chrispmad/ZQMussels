# Test to see if flow speeds map well to stream magnitude.

library(readxl)
library(bcdata)

readxl::excel_sheets("03_PrioritizationModel/data/HydroMaster.xlsx")

ron = read_excel("03_PrioritizationModel/data/HydroMaster.xlsx", skip = 2)

# Is it possible to correlate stream order and MAD (mean annual discharge)?

cor.test(ron_f$Order, ron_f$`mad (L/s)`)
# Only 0.49 correlation, is sig., but not great.

# How about if we grab the stream magnitude instead from the BC data warehouse?
set.seed(1234)

example_names = dplyr::sample_n(ron_f,size = 10) |> dplyr::pull(Stream)

streams_raw = bcdc_query_geodata('freshwater-atlas-stream-network') |> 
  filter(GNIS_NAME %in% c("Keremeos Creek","Whipsaw Creek","Luxor Creek","Lost Horse Creek","Selous Creek",
                            "Trapping Creek","Naver Creek","Kettle River")) |> 
  collect() |> 
  sf::st_drop_geometry()

streams_sum = streams_raw |> 
  dplyr::group_by(BLUE_LINE_KEY,GNIS_NAME,STREAM_MAGNITUDE) |> 
  dplyr::summarise() |> 
  dplyr::ungroup()

streams_sum |> sf::st_drop_geometry() |> dplyr::count(GNIS_NAME,BLUE_LINE_KEY, sort = T)

ron_f = ron |> 
  dplyr::mutate(Stream = stringr::str_to_title(Stream)) |> 
  dplyr::filter(str_detect(Stream, paste0('(',paste0(unique(streams_raw$GNIS_NAME),collapse='|'),')')))
