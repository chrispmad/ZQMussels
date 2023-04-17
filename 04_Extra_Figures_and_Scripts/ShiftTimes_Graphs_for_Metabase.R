library(readxl)
library(tidyverse)
library(lubridate)

dat = read_excel("C:/Users/CMADSEN/Downloads/LocalRWork/data/MusselSummary_flatfile.xlsx")

dat = dat %>% 
  rename(StartTime = `Start Time`, 
         EndTime = `End Time`) %>%
  select(StartTime, EndTime) %>% 
  gather(key = "Var", value = "Time", c(StartTime, EndTime)) %>% 
  mutate(Hour = hour(Time))

dat %>%
  ggplot() + 
  geom_bar(aes(hour(Time))) + 
  facet_wrap(~ Var, ncol = 1) + 
  labs(x = "Hour (24 hour clock)",
       y = "Number of Records",
       title = "2021 Inspection Data - Shift Start and End Times") + 
  scale_x_discrete(limits = 0:22) 

