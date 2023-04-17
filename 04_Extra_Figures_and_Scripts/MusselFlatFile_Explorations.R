## This script is for exploring the mussel summary flat file.

## The flat file changes over time, so this script may be
## a bit of a hodge-podge of different questions.

## Q1. Which records have very strange dates that need to be
## corrected?

library(readxl)
library(tidyverse)

#2020 data.
dat = read_csv("data/2020_MusselSummary.csv")

#Number of unique source jurisdictions for Pacific station.
dat %>%
  filter(str_detect(Station, "Pacific")) %>% 
  select(`Watercraft Risk Assessment ID`,
         `Province Code`,
         SourceFromManyFields,
         contains("Previous Waterbody 1"),
         -`Previous Waterbody 1 Days Out Of Water`,
         `General Comment`) %>% 
  mutate(ExtractedProvState = str_extract(`General Comment`, " [A-Z]{2} ")) %>%
  select(SourceFromManyFields) %>% distinct()

dat %>%
  filter(is.na(`Clean Drain Dry After Inspection Ind`)==F) %>% 
  select(`Watercraft Risk Assessment ID`, 
         raw_timestamp, 
         `Clean Drain Dry After Inspection Ind`) %>%
  distinct()

dat %>%
  select(`Aquatic Plants Found Ind`,
         `Other Inspection Findings`) %>%
  group_by(`Other Inspection Findings`) %>% summarise(n())

dat %>%
  filter(`High Risk AIS Ind` == T) %>%
  select(`Previous Inspection Ind`) %>% 
  group_by(`Previous Inspection Ind`) %>% summarise(n())
  filter(`Previous Inspection Ind` == T)

dat %>%
  filter(`High Risk AIS Ind` == T) %>%
  select(`Watercraft Risk Assessment ID`, contains("Counter")) %>%
  pivot_longer(-`Watercraft Risk Assessment ID`) %>%
  group_by(name) %>% summarise(sum(value))

dat %>%
  filter(`High Risk AIS Ind` == T) %>%
  group_by(`Commercially Hauled Ind`) %>%
  summarise(n())

dat %>% 
  filter(`Commercially Hauled Ind` == TRUE) %>% 
  select(`Province Code`, SourceFromManyFields, `General Comment`) %>%
  filter(`General Comment` != "None") %>% View()
  group_by(SourceFromManyFields) %>%
  summarise(n())
unique(dat$Station)
# dat <- read_excel("C:/Users/CMADSEN/Downloads/LocalRWork/data/MusselSummary_flatfile.xlsx", 
#                                      col_types = c("numeric", "text", "text", 
#                                                    "date", "date", "date", "date", 
#                                                    "date", "date", "text", "text", 
#                                                    "numeric", "numeric", "numeric", 
#                                                    "numeric", "numeric", "numeric", 
#                                                    "text", "numeric", "date", "text", 
#                                                    "text", "text", "text", "numeric", 
#                                                    "numeric", "numeric", "numeric", 
#                                                    "numeric", "numeric", "numeric", 
#                                                    "numeric", "numeric", "text", "numeric", 
#                                                    "numeric", "numeric", "numeric", 
#                                                    "text", "text", "text", "numeric", 
#                                                    "text", "text", "text", "text", "numeric", 
#                                                    "numeric", "text", "text", "text", 
#                                                    "text", "numeric", "numeric", "numeric", 
#                                                    "numeric", "numeric", "numeric", 
#                                                    "numeric", "numeric", "numeric", 
#                                                    "numeric", "numeric", "text", "text", 
#                                                    "text", "numeric", "text", "text", 
#                                                    "text", "numeric", "numeric", "numeric", 
#                                                    "numeric", "numeric", "numeric", 
#                                                    "numeric", "numeric", "numeric", 
#                                                    "numeric", "numeric", "text", "numeric", 
#                                                    "numeric", "numeric", "numeric", 
#                                                    "text", "numeric", "numeric", "text", 
#                                                    "numeric", "numeric", "text", "numeric", 
#                                                    "text", "text", "numeric", "numeric", 
#                                                    "numeric", "numeric", "text", "numeric", 
#                                                    "text"))
# 
# dat = dat[,c(1:11,20)]

dat %>%
  filter(str_detect(`Shift Start Comment`,'[0-9]+'))
  
nrow(dat)
