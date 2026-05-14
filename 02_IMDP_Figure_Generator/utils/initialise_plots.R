# =====================================================
#                PACKAGE SETUP
# =====================================================

pacman::p_load(
  readxl, ggpubr, ggrepel, plotly, ggExtra, ggtext,
  tidyverse, leaflet, sf, RColorBrewer, openxlsx,
  scales, lubridate, plotrix, basemaps, ggplot2
)

# =====================================================
#                  OPTIONS
# =====================================================

my_opts <- read_csv(here::here("Options.csv"))

my.year   <- my_opts$year
update.gis <- FALSE

# -----------------------------------------------------
# Station name corrections
# -----------------------------------------------------

stations.to.change <- tibble(
  Station  = c("Fraser Valley Roving", "Penticton Roving",
               "Sumas Border", "Keremeos (Hwy 3)"),
  new_name = c("Lower Mainland", "Penticton",
               "Sumas (Huntington)", "Keremeos")
)

# -----------------------------------------------------
# Year-specific station configuration
# -----------------------------------------------------

year_config <- list(
  
  `2022` = list(
    include = c("Golden","Dawson Creek","Mt. Robson","Osoyoos",
                "Olsen","Penticton Roving","Radium","Pacific",
                "Fraser Valley Roving","Keremeos"),
    aside = c("Scheduled Inspection","Boat Launch - Okanagan",
              "Okanagan","Penticton Roving - Hwy 33",
              "Penticton Roving - Inspection Event"),
    leaflet = c("Golden","Radium","Olsen","Dawson Creek",
                "Mt. Robson","Penticton Roving",
                "Osoyoos","Fraser Valley Roving")
  ),
  
  `2023` = list(
    include = c("Golden","Radium","Olsen","Yahk","Pacific",
                "Osoyoos","Hwy 97c","Mt. Robson","Keremeos",
                "Dawson Creek","Lower Mainland Roving",
                "Sumas Border","Cutts (Hwy 93)",
                "Penticton Roving"),
    aside = c("Scheduled Inspection","Boat Launch - Okanagan",
              "Okanagan","Penticton Roving - Hwy 33",
              "Penticton Roving - Inspection Event"),
    leaflet = c("Golden","Olsen","Dawson Creek","Mt. Robson",
                "Penticton Roving","Osoyoos",
                "Lower Mainland Roving","Yahk")
  ),
  
  `2024` = list(
    include = c("Golden","Radium","Olsen","Yahk","Pacific",
                "Osoyoos","Hwy 97c","Mt. Robson","Keremeos",
                "Dawson Creek","Lower Mainland","Sumas Border",
                "Cutts (Hwy 93)","Penticton","Douglas Crossing"),
    aside = c("Scheduled Inspection","Boat Launch - Okanagan",
              "Okanagan","Penticton Roving - Hwy 33",
              "Penticton Roving - Inspection Event"),
    leaflet = c("Golden","Olsen","Dawson Creek","Mt. Robson",
                "Radium","Sumas","Penticton","Osoyoos",
                "Lower Mainland","Yahk","Pacific","Keremeos",
                "Hwy 97c","Cutts (Hwy 93)",
                "Douglas Crossing")
  ),
  
  `2025` = list(
    include = c("Golden","Radium","Olsen","Yahk","Pacific",
                "Osoyoos","Hwy 97c","Mt. Robson","Keremeos",
                "Dawson Creek","Lower Mainland","Sumas Border",
                "Cutts (Hwy 93)","Penticton","Douglas Crossing"),
    aside = c("Scheduled Inspection","Boat Launch - Okanagan",
              "Okanagan","Penticton Roving - Hwy 33",
              "Penticton Roving - Inspection Event"),
    leaflet = c("Golden","Olsen","Dawson Creek","Mt. Robson",
                "Radium","Sumas (Huntington)","Penticton","Osoyoos",
                "Lower Mainland","Yahk","Pacific","Keremeos",
                "Hwy 97c","Cutts (Hwy 93)",
                "Douglas Crossing")
  )
)

cfg <- year_config[[as.character(my.year)]]

stations.to.include        <- cfg$include
stations.to.put.aside      <- cfg$aside
leaflet.stations.to.include <- cfg$leaflet

# -----------------------------------------------------
# Station groupings
# -----------------------------------------------------

permanent.stations <- c("Dawson Creek" ,"Mt. Robson",
                        "Golden","Olsen","Yahk",
                        "Osoyoos")

roving.stations <- c("Penticton","Lower Mainland",
                     "Pacific","Cutts (Hwy 93)",
                     "Douglas Crossing")

part.time.stations <- c("Sumas (Huntington)","Pacific","Cutts (Hwy 93)",
                        "Radium","Douglas Crossing",
                        "Hwy 97c","Keremeos")

# -----------------------------------------------------
# Paths
# -----------------------------------------------------

my.data.folder <- file.path(my_opts$zqm_figure_local_folder, "data")

my.external.data.folder <- "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/Projects/ZQMussels/data/"

my.output.folder <- file.path(my_opts$zqm_figure_local_folder, "output")

my.external.output.folder <- file.path(
  my_opts$zqm_figure_output_remote_folder,
  my.year
)

zqm.operations.folder <- my_opts$zqm_operations_data_folder

this.years.report.folder <- file.path(
  my_opts$remote_spatial_data,
  "Projects/ZQMussels",
  paste0(my.year, " IMDP Final Report")
)

# -----------------------------------------------------
# Mussel fouled tracker
# -----------------------------------------------------

MusselFouledTracker <- switch(
  as.character(my.year),
  "2022" = file.path(zqm.operations.folder,
                     "Watercraft Inspection Data/2022 data",
                     "2022 mussel fouled boats tracking sheet.xlsx"),
  "2023" = file.path(zqm.operations.folder,
                     "Watercraft Inspection Data/2023 data",
                     "2023_mussel_fouled_details.xlsx"),
  "2024" = file.path(zqm.operations.folder,
                     "Watercraft Inspection Data/2024 data",
                     "mussel_fouled_summary.xlsx"),
  "2025" = file.path(zqm.operations.folder,
                     "Watercraft Inspection Data/2025 data",
                     "mussel_fouled_summary.xlsx")
)

# -----------------------------------------------------
# CBSA notifications
# -----------------------------------------------------

cbsa_file <- file.path(
  my.external.output.folder,
  "GIS Maps and Excel Figures",
  paste0(my.year, " COS inbox notifications.xlsx")
)

if (file.exists(cbsa_file)) {
  cbsa_dat <- read_excel(cbsa_file, sheet = "Filtered list")
}

# -----------------------------------------------------
# Knitr + plotting defaults
# -----------------------------------------------------

knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.width = 8,
  fig.height = 4
)

knitr::opts_knit$set(
  root.dir = file.path(
    my.external.output.folder,
    "GIS Maps and Excel Figures",
    "ExcelFigures"
  )
)

my.grey <- "#5f6a6f"

# -----------------------------------------------------
# Ensure output folder exists
# -----------------------------------------------------

excel_fig_dir <- knitr::opts_knit$get("root.dir")

if (!dir.exists(excel_fig_dir)) {
  dir.create(excel_fig_dir, recursive = TRUE)
}

