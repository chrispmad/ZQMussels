library(sf)
library(tidyverse)
library(readxl)
library(tmap)
library(maptools)
library(geosphere)
library(leaflet)
library(leaflet.extras2)

# Plotting functions

#Create function to draw Brezier curve
bezier.curve <- function(p1, p2, p3) {
  n <- seq(0,1,length.out=50)
  bx <- (1-n)^2 * p1[[1]] +
    (1-n) * n * 2 * p3[[1]] +
    n^2 * p2[[1]]
  by <- (1-n)^2 * p1[[2]] +
    (1-n) * n * 2 * p3[[2]] +
    n^2 * p2[[2]]
  data.frame(lon=bx, lat=by)
}

bezier.arc <- function(p1, p2) {
  intercept.long <- (p1[[1]] + p2[[1]]) / 2
  intercept.lat  <- 85
  p3 <- c(intercept.long, intercept.lat)
  bezier.curve(p1, p2, p3)
}


bezier.uv.arc <- function(p1, p2) {
  # Get unit vector from P1 to P2
  u <- p2 - p1
  u <- u / sqrt(sum(u*u))
  d <- sqrt(sum((p1-p2)^2))
  # Calculate third point for spline
  m <- d / 2
  h <- floor(d * .2)
  # Create new points in rotated space 
  pp1 <- c(0,0)
  pp2 <- c(d,0)
  pp3 <- c(m, h)
  mx <- as.matrix(bezier.curve(pp1, pp2, pp3))
  # Now translate back to original coordinate space
  theta <- acos(sum(u * c(1,0))) * sign(u[2])
  ct <- cos(theta)
  st <- sin(theta)
  tr <- matrix(c(ct,  -1 * st, st, ct),ncol=2)
  tt <- matrix(rep(p1,nrow(mx)),ncol=2,byrow=TRUE)
  points <- tt + (mx %*% tr)
  tmp.df <- data.frame(points)
  colnames(tmp.df) <- c("lon","lat")
  tmp.df
}

bezier.uv.merc.arc <- function(p1, p2) {
  pp1 <- p1
  pp2 <- p2
  pp1[2] <- asinh(tan(p1[2]/180 * pi))/pi * 180
  pp2[2] <- asinh(tan(p2[2]/180 * pi))/pi * 180
  
  arc <- bezier.uv.arc(pp1,pp2)
  arc$lat <-  atan(sinh(arc$lat/180 * pi))/pi * 180
  arc
}

# Read in my options file.
my_opts = read_csv(paste0(str_extract(getwd(),".*ZQMussels[/]?"),"/Options.csv"))


# Read in excel data
dat = read_excel('C:/Users/CMADSEN/Downloads/LocalR/ZQMussels/02_IMDP_Figure_Generator/data/figure_dat_all.xlsx')

### OPTION 1  ###
# separate static figures for each year # 

# Read in source centroids for map.
sources_centroid = read_sf(paste0(my_opts$remote_spatial_data,'Projects/ZQMussels/2022 IMDP Final Report/data/spatial/Inspections_by_source_centroid.gpkg'))

# Excel summary sheet for Martina
source_excel = sources_centroid |> 
  dplyr::select(NAME_0, ABBR) |> 
  left_join(dat |> 
              group_by(Year) |> 
              rename(ABBR = Previous_Waterbody_1_Province_Or_State) |> 
              count(ABBR, name = "Total Inspections") |> 
              mutate(`Total percent` = paste0(round(100*`Total Inspections`/ sum(`Total Inspections`),2),"%"))) |>
  left_join(dat |> 
              group_by(Year) |> 
              filter(High_Risk_AIS_Ind == T) |> 
              rename(ABBR = Previous_Waterbody_1_Province_Or_State) |> 
              count(ABBR, name = "HR Inspections")) |> 
  filter(!is.na(NAME_0)) |> 
  mutate(`HR Inspections` = replace_na(`HR Inspections`, 0)) |> 
  mutate(`HR percent` = paste0(round(100*`HR Inspections`/ sum(`HR Inspections`),2),"%")) |> 
  rename(Country = NAME_0,
         Abbreviation = ABBR) |> 
  st_drop_geometry() |> 
  arrange(desc(`Total Inspections`))

# Destination summary excel sheet.
destination_excel = dat |> 
  rename(Destination = Destination_Waterbody_1_Province_Or_State) |> 
  count(Destination, name = 'Total Inspections', sort = T) |> 
  mutate(`Total percent` = paste0(round(100*`Total Inspections`/ sum(`Total Inspections`),2),"%")) |> 
  mutate(Destination = replace_na(Destination, 'Unknown')) |> 
  left_join(
    dat |> 
      filter(High_Risk_AIS_Ind == T) |> 
      rename(Destination = Destination_Waterbody_1_Province_Or_State) |> 
      count(Destination, name = 'HR Inspections', sort = T) |> 
      mutate(`HR percent` = paste0(round(100*`HR Inspections`/ sum(`HR Inspections`),2),"%")) |> 
      mutate(Destination = replace_na(Destination, 'Unknown'))  
  ) |> 
  arrange(desc(`Total Inspections`)) |> 
  left_join(
    sources_centroid |> 
      dplyr::select(Country = NAME_0, Destination = ABBR) |> 
      st_drop_geometry()) |> 
  dplyr::select(Country,everything())

openxlsx::write.xlsx(source_excel,
                     'J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Communications/Presentations/2023 season/watercraft_source_prov_state_summary.xlsx')

openxlsx::write.xlsx(destination_excel,
                     'J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Communications/Presentations/2023 season/watercraft_destination_prov_state_summary.xlsx')

# Drop most columns; join inspections to spatial object.
sources_centroid = sources_centroid |> 
  dplyr::select(NAME_0, ABBR) |> 
  left_join(dat |> 
              group_by(Year) |> 
              rename(ABBR = Previous_Waterbody_1_Province_Or_State) |> 
              count(ABBR) |> 
              bind_rows(
                dat |> 
                  mutate(Year = '1') |> 
                  group_by(Year) |> 
                  rename(ABBR = Previous_Waterbody_1_Province_Or_State) |> 
                  count(ABBR))) |> 
  filter(!is.na(NAME_0))

# Add some labelling logic:
# 1. If a given state/province/estado has no inspections, its name should be grey.
#    and it should have no dot.
sources_centroid = sources_centroid %>% 
  mutate(map_label_colour = ifelse(is.na(n), 'transparent', 'black'))

northamerica_view = tibble(lon = c(-174,-50),
                           lat = c(73,11.2)) %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326)

# Download maptiles North America
northamerica_basemap = maptiles::get_tiles(x = northamerica_view, provider = 'CartoDB.Positron', zoom = 4, crop = F)


for(this.year in c(unique(sources_centroid$Year),'1')){

  this_year_dat = sources_centroid %>% 
    filter(!is.na(n)) |> 
    filter(Year %in% this.year)
  
  this_year_dat = this_year_dat %>% 
    mutate(jenks = list(BAMMtools::getJenksBreaks(n, k = 6))) %>% 
    unnest_wider(jenks, names_sep = '_') %>% 
    mutate(n_b = case_when(
      n <= jenks_2 ~ 1,
      n <= jenks_3 ~ 2,
      n <= jenks_4 ~ 3,
      n <= jenks_5 ~ 4,
      n <= jenks_6 ~ 5,
    )) %>%
    mutate(bin_label = case_when(
      n_b == 1 ~ paste0(jenks_1, ' - ', jenks_2),
      n_b == 2 ~ paste0(jenks_2+1, ' - ', jenks_3),
      n_b == 3 ~ paste0(jenks_3+1, ' - ', jenks_4),
      n_b == 4 ~ paste0(jenks_4+1, ' - ', jenks_5),
      n_b == 5 ~ paste0(jenks_5+1, ' - ', jenks_6)
    )) %>% 
    arrange(n)
  
  this_year_dat = st_set_geometry(this_year_dat, this_year_dat$geom)
  
  source_pal = leaflet::colorFactor(
    palette = 'Spectral',
    domain = unique(this_year_dat$n_b),
    reverse = T
  )
  
  # Add in colours as new column.
  this_year_dat = this_year_dat %>%
  mutate(my_colors = source_pal(n_b))

  # Calculate source lines, and give them colours
  from_p = sources_centroid |> 
    filter(Year %in% this.year) |> 
    filter(!is.na(NAME_0)) |> 
    filter(ABBR != 'BC')
  
  to_p = sources_centroid |> 
    filter(ABBR == 'BC') |> 
    slice(1)
  
  sources_lines = tibble(row = 1:nrow(from_p),
                         line_shape = list(NULL))
  
  for(i in 1:nrow(from_p)){
    p1 = c(unlist(map(from_p[i,]$geom,1)),
           unlist(map(from_p[i,]$geom,2)))
    
    p2 = c(unlist(map(to_p$geom,1)),
           unlist(map(to_p$geom,2)))
    
    arc3 <- bezier.arc(p1,p2)
    
    arc4 <- bezier.uv.arc(p1,p2)
    
    arc5 <- bezier.uv.merc.arc(p1, p2)
    
    sources_lines[i,]$line_shape = list(arc5)
  }
  
  sources_lines = bind_rows(sources_lines$line_shape, .id = 'ID')
  
  sources_lines = as_tibble(sources_lines) |> 
    mutate(ID = ID)
  
  sources_lines = st_as_sf(sources_lines, 
                           coords = c('lon','lat'), crs = 4326) |> 
    group_by(ID) |> 
    summarise(do_union = F) |> 
    st_cast("LINESTRING")
  
  sources_lines = sources_lines |> 
    left_join(from_p |>
                st_drop_geometry() |> 
                dplyr::select(ABBR) |> 
                mutate(ID = as.character(row_number())))
  
  sources_lines = sources_lines |> 
    left_join(this_year_dat |> 
                dplyr::select(ABBR,my_colors) |> 
                st_drop_geometry()) |> 
    filter(!is.na(my_colors))
    # mutate(my_colors = replace(my_colors, is.na(my_colors), 'transparent'))
  
  sources_lines = sources_lines |> 
    mutate(my_linewidth = case_when(
      my_colors == '#2B83BA' ~ 1,
      my_colors == '#ABDDA4' ~ 2,
      my_colors == '#FFFFBF' ~ 3,
      my_colors == '#FDAE61' ~ 4,
      my_colors == 'transparent' ~ 0,
      T ~ 5
    ))
  
  sources_lines = sources_lines |> 
    arrange(my_linewidth)
  
  # sources_lines = st_set_geometry(sources_lines, sources_lines$geometry)
  
  
option_1_tmap = tm_shape(northamerica_basemap, 
                      bbox = northamerica_view) +
  tm_rgb() + 
  tm_add_legend(title = paste0('Source of Watercraft (',ifelse(this.year == "1","All Years",this.year),')'),
                type = 'symbol',
                labels = unique(this_year_dat$bin_label),
                col = unique(this_year_dat$my_colors),
                size = log(unique(this_year_dat$n_b)+1)) + 
  tm_shape(sources_lines) + 
    tm_lines(alpha = 0.75,
             lwd = 'my_linewidth',
             col = 'my_colors',
             scale = 5,
             lty = 1,
             legend.lwd.show = F) +
  tm_shape(this_year_dat) + 
  tm_symbols(col = 'my_colors', 
             palette = '-Spectral',
             size = 'n_b',
             legend.col.show = FALSE,
             legend.size.show = FALSE,
             title.col = '') +
  tm_text("ABBR", auto.placement = 1, 
          size = 0.60) +
  tm_scale_bar() + 
  tm_layout(legend.frame = 'black', 
            legend.position = c('left','bottom'),
            scale = 1.25)

option_1_tmap

tmap::tmap_save(option_1_tmap, filename = paste0('04_Extra_Figures_and_Scripts/output/option_1_static_tmap_',ifelse(this.year == "1","All Years",this.year),'.png'),
                dpi = 300)
}
