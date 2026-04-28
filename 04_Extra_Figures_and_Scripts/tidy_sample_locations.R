library(tidyverse)
library(openxlsx)
library(dplyr)
library(lubridate)

parse_mixed_excel_dates <- function(x) {
  # Initialize output as NA Date
  parsed <- as.Date(rep(NA, length(x)))
  
  # Numeric Excel dates
  x_num <- suppressWarnings(as.numeric(x))
  is_num <- !is.na(x_num)
  parsed[is_num] <- as.Date(floor(x_num[is_num]), origin = "1899-12-30")
  
  # Character dates
  x_char <- as.character(x)
  is_char <- !is_num & !is.na(x_char)
  parsed[is_char] <- suppressWarnings(as.Date(x_char[is_char], format = "%Y-%m-%d"))
  
  parsed
}



sample_locations = read.xlsx("./data/2025_sample_locations.xlsx")

sample_locations <- sample_locations |>
  janitor::clean_names() |> 
  mutate(across(everything(), as.character))

sample_locations <- sample_locations %>%
  mutate(date_collected = parse_mixed_excel_dates(date_collected))

sample_locations <- sample_locations %>%
  mutate(long_decimal_degrees = ifelse(
    !is.na(long_decimal_degrees),
    -abs(as.numeric(long_decimal_degrees)),
    NA_real_
  ))


sample_locations <- sample_locations %>%
  mutate(
    type_of_plankton_tow_vertical_or_horizontal = case_when(
      type_of_plankton_tow_vertical_or_horizontal == "Verticle" ~ "Vertical",
      type_of_plankton_tow_vertical_or_horizontal == "vertical" ~ "Vertical",
      type_of_plankton_tow_vertical_or_horizontal == "Horzontal, 2x vertical" ~ "1 Horizontal, 2 Vertical",
      type_of_plankton_tow_vertical_or_horizontal == "Horzontal, 2x vertical" ~ "1 Horizontal, 2 Vertical",
      type_of_plankton_tow_vertical_or_horizontal == "2x horizontal , vertical" ~ "2 Horizontal, 1 Vertical",
      type_of_plankton_tow_vertical_or_horizontal == "1 Horizontal, 2 Vertical" ~ "1 Horizontal, 2 Vertical",
      type_of_plankton_tow_vertical_or_horizontal == "horizontal (shoreline toss)" ~ "Horizontal",
      type_of_plankton_tow_vertical_or_horizontal == "H" ~ "Horizontal",
      type_of_plankton_tow_vertical_or_horizontal == "horizontal" ~ "Horizontal",
      type_of_plankton_tow_vertical_or_horizontal == "-" ~ "NA",
      type_of_plankton_tow_vertical_or_horizontal == "V" ~ "Vertical",
      type_of_plankton_tow_vertical_or_horizontal == "Vertica/Horizontal" ~ "Vertical/Horizontal",
      type_of_plankton_tow_vertical_or_horizontal == "horizontal and vertical" ~ "Vertical, Horizontal",
      type_of_plankton_tow_vertical_or_horizontal == "H/V" ~ "Vertical, Horizontal",
      type_of_plankton_tow_vertical_or_horizontal == "Vertical, Horizontal" ~ "Vertical, Horizontal",
      type_of_plankton_tow_vertical_or_horizontal == "Vertical/Horizontal" ~ "Vertical, Horizontal",
      type_of_plankton_tow_vertical_or_horizontal == "Verical/Horizontal" ~ "Vertical, Horizontal",
      type_of_plankton_tow_vertical_or_horizontal == "NA" ~ NA,
      TRUE ~ type_of_plankton_tow_vertical_or_horizontal  # keep everything else unchanged
    )
  )

write.csv(sample_locations, "./data/2025_sample_locations_final.csv")
