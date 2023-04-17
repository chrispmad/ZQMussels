#Load in packages

library(tidyverse)
library(readxl)
library(tidytext)
library(ggrepel)
library(patchwork)
library(ggpubr)
library(plotly)

#Read in files from 2019 and 2020.
rm(list = ls())
d2019 = read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Communications/Inspection data reporting/Final report/2019/Graphs_Data files/2019 COS notifications.xlsx",
                   sheet = "Master list (Dups & NA removed)") %>% 
  select(From, Subject, Received, Category)

d2020 = read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Communications/Inspection data reporting/Final report/2020/GIS Maps and Excel Figures/ExcelFigureData/Figure 32_2020 COS inbox notifications.xlsx",
                   sheet = "Master_clean")

#Read in 2021 excel data.
dat = read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Communications/Inspection data reporting/Final report/2021/GIS Maps and Excel Figures/2021 COS inbox notifications.xlsx",
                 sheet = "Filtered list")


### Custom functions ###
donut_plot = function(x, hsize = hsize, grouper.col, subset, unique.col){
  
  x = x %>% 
    filter(!!sym(grouper.col) == subset)

  unique.col.levels = unique(x %>% 
                               pull(unique.col))
  
  #If there are more than 6 unique levels, the plot gets too complicated. Group all levels beyond fifth into an "Other" group.
  if(length(unique.col.levels) > 6){
    unique.col.levels.excess.levels = unique.col.levels[6:length(unique.col.levels)]
    unique.col.levels = unique.col.levels[1:5]
    x = x %>% 
      mutate(row.number = row_number()) %>% 
      mutate(!!sym(unique.col) := replace(!!sym(unique.col), !!sym(unique.col) %in% all_of(unique.col.levels) == F, "Other")) %>% 
      mutate(row.number = replace(row.number, Subtype == "Other", 6)) %>% 
      group_by(!!sym(grouper.col), !!sym(unique.col), row.number) %>% 
      summarise(n = sum(n, na.rm=T), .groups = "drop") %>% 
      mutate(!!sym(unique.col) := fct_inorder(!!sym(unique.col))) %>% 
      arrange(row.number) %>% 
      select(-row.number)
    unique.col.levels = c(unique.col.levels, "Other")
  }
  
  plotlist = list()
  
  for(i in 1:length(unique.col.levels)){
    my.plot = x %>% 
      mutate(perc = n / sum(n)) %>% 
      filter(!!sym(unique.col) == unique.col.levels[i]) %>% 
      ggplot() +
      geom_col(data = data.frame(x = hsize, y = 1), 
               aes(x = x + 0.5, y = y), fill = "#e5e2ee") +
      geom_col(aes(x = hsize, y = perc, fill = unique.col.levels[i])) +
      coord_polar(theta = "y") +
      scale_y_continuous(limits = c(0,1)) +
      theme_void() + 
      labs(fill = grouper.col) +
      scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique.col.levels), 
                                                          name = "Paired")[i]) + 
      theme(text = element_text(size = 16)) +
      theme(legend.position = "none") +
      geom_label(aes(x = hsize*1.5, y = perc, label = scales::percent(perc,0.1)),
                 position = position_stack(vjust = 0.5),
                 size = 5,
                 show.legend = F) + 
      geom_text(aes(0,0,label = !!sym(unique.col)), size = 6)
    
    plotlist[[i]]  = my.plot
  }
  complete_plots = patchwork::wrap_plots(plotlist) +
    patchwork::plot_annotation(title = paste0(subset, " Breakdown")) & theme(plot.title = element_text(hjust = 0.5))
  
  return(complete_plots)
}
## Chat with Martina to get an idea of keywords to look for / emails to disregard.



# What are the categories that Martina has applied to past years?
unique(d2019$Category)
unique(d2020$Categories)
unique(dat$Categories)

d2019 = d2019 %>% 
  rename(Categories = Category) %>% 
  mutate(Categories = str_to_upper(Categories)) %>% 
  mutate(Categories = replace(Categories, Categories == "US STATES", "USA")) %>% 
  mutate(Categories = str_remove_all(Categories, "\\(|\\)")) %>% 
  mutate(Subtype = str_extract(Categories, "(?<=CBSA ).*")) %>% 
  mutate(Categories = str_remove_all(Categories, "(?<=CBSA).*")) %>% 
  mutate(Year = 2019)

d2020 = d2020 %>% 
  rename(Subtype = CBSA_Type) %>%
  mutate(Categories = str_to_upper(Categories)) %>% 
  mutate(Categories = replace(Categories, Categories == "US", "USA")) %>% 
  mutate(Categories = str_remove_all(Categories, "\\(|\\)")) %>% 
  mutate(Subtype = str_to_upper(Subtype)) %>% 
  mutate(Year = 2020)

dat = dat %>% 
  filter(!is.na(Categories)) %>% 
  mutate(Categories = str_to_upper(Categories)) %>% 
  #mutate(Categories = replace(Categories, Categories == "US", "USA")) %>% 
  mutate(Categories = str_remove_all(Categories, " -")) %>% 
  mutate(Subtype = str_extract(Categories, "(?<=CBSA ).*")) %>% 
  mutate(Categories = str_remove_all(Categories, "(?<=CBSA).*")) %>% 
  mutate(Year = 2021)
  
dat %>% 
  mutate(Categories = case_when(
  Categories %in% c("NEW BOATS","NEW BOAT SHIPMENT") ~ "NEW BOATS",
  str_detect(Categories, "INVASIVE SPECIES") ~ "ISCBC",
  T ~ Categories
)) %>% 
  mutate(Subtype = coalesce(Subtype, Categories)) %>% 
  mutate(Categories = case_when(
    Categories %in% c("USA","US","US STATE","ID","MT") ~ "USA",
    Categories %in% c("AB","SK","MB") ~ "PROVINCES",
    T ~ Categories
  )) %>% 
  count(Categories, Subtype, sort = T) %>% 
  openxlsx::write.xlsx(., "C:/Users/CMADSEN/Downloads/LocalRWork/output/2021_CBSA_numbers.xlsx",
                       overwrite = T)

dat_all = dat %>% 
  mutate(Year = 2021) %>% 
  bind_rows(d2019 %>% mutate(Year = 2019)) %>% 
  bind_rows(d2020 %>% mutate(Year = 2020)) %>% 
  select(-Size)

dat_all = dat_all %>%
  mutate(Categories = case_when(
    Categories %in% c("NEW BOATS","NEW BOAT SHIPMENT") ~ "NEW BOATS",
    str_detect(Categories, "INVASIVE SPECIES") ~ "ISCBC",
    T ~ Categories
  )) %>% 
  mutate(Subtype = coalesce(Subtype, Categories)) %>% 
  mutate(Categories = case_when(
    Categories %in% c("USA","US","US STATE","ID","MT") ~ "USA",
    Categories %in% c("AB","SK","MB") ~ "PROVINCES",
    T ~ Categories
  )) %>% 
  count(Categories, Subtype, sort = T)

province_donuts = donut_plot(x = dat_all, hsize = 3, grouper.col = "Categories", subset = "PROVINCES", unique.col = "Subtype")
USA_donuts = donut_plot(x = dat_all, hsize = 3, grouper.col = "Categories", subset = "USA", unique.col = "Subtype")
CBSA_donuts = donut_plot(x = dat_all, hsize = 3, grouper.col = "Categories", subset = "CBSA", unique.col = "Subtype")


subtype_labels = dat_all %>% 
  filter(Categories != Subtype) %>% 
  group_by(Categories) %>% 
  mutate(group_n = sum(n)) %>% 
  #Lump!
  mutate(Subtype = fct_lump(Subtype, 2, w = n, other_level = "OTHER")) %>% 
  group_by(Categories, Subtype) %>% 
  summarise(n = sum(n), group_n = group_n) %>% 
  distinct() %>% 
  arrange(desc(group_n), desc(n)) %>% 
  group_by(Categories) %>% 
  mutate(label_height = cumsum(n)-(0.5*n)) %>% 
  arrange(desc(group_n),n) %>% 
  ungroup() %>% 
  mutate(Categories = fct_inorder(Categories),
         Subtype = fct_inorder(Subtype))


horizontal_barchart

cbsa_plot = horizontal_barchart + wrap_elements(province_donuts) + wrap_elements(USA_donuts) + wrap_elements(CBSA_donuts)

cbsa_plot

ggsave("output/CBSA_Distribution.png", cbsa_plot,
       width = 16, height = 8, dpi = 300)

