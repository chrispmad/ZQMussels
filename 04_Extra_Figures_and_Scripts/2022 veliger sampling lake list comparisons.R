#libraries
library(tidyverse)
library(readxl)
library(rvest)

#This script compares three lists of lakes:
# 1. Our priority list of lakes from the ZQM risk model work.
# 2. A list of lakes sent to us by Mike Sokal: https://www2.gov.bc.ca/gov/content/environment/research-monitoring-reporting/monitoring/lake-monitoring/bc-lake-monitoring-network/list-of-lake-monitoring-network-sites
# 3. A list of waterbodies approved for sampling this year by HCTF

rm(list = ls())

#List 1.
zqm = read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2022/Prioritization model/Final waterbody list based on risk estimates.xlsx")

#List 2.
mike = read_html('https://www2.gov.bc.ca/gov/content/environment/research-monitoring-reporting/monitoring/lake-monitoring/bc-lake-monitoring-network/list-of-lake-monitoring-network-sites')

mike = mike %>% 
  html_table() %>% 
  .[[40]] %>% 
  setNames(c("Waterbody","Region","Site","Data"))

#List 3.
hctf = read_excel("C:/Users/CMADSEN/Downloads/LocalRWork/data/List of approved Waterbodies 2022.xlsx")

hctf_comp = hctf %>% 
  rename(Waterbody = `APPROVED Waterbody name`) %>% 
  mutate(HCTF = "on list")

mike_comp = mike %>% 
  mutate(Waterbody = str_extract(Waterbody, "^[a-zA-Z]* [a-zA-Z]*")) %>% 
  mutate(Waterbody = case_when(
    Waterbody == "Francois Lake" ~ "François Lake",
    T ~ Waterbody
  )) %>% 
  select(Region, Waterbody) %>% 
  mutate(Mike = "on list")

zqm_comp = zqm %>% 
  select(Region, Waterbody) %>% 
  #Split commas into separate rows.
  mutate(Region = strsplit(Region, ", ")) %>% 
  unnest(Region) %>% 
  #Replace 'Thompson-Nicola' into just 'Thompson' to match formatting of Mike's list.
  mutate(Region = replace(Region, Region == "Thompson-Nicola", "Thompson")) %>% 
  mutate(ZQM = "on list")

lakes_comparison = zqm_comp %>% 
  full_join(mike_comp) %>% 
  full_join(hctf_comp) %>% 
  arrange(Waterbody,Region) %>% 
  distinct()

lakes_comparison

openxlsx::write.xlsx(lakes_comparison, "output/2022_veliger_lake_sampling_comparison.xlsx",
                     overwrite = T)
#Not yet compared with HCTF lake list.
