# Making R figures for Martina 2021-10-26

library(tidyverse)
library(lubridate)

rm(list=ls())

# load in data
dat = readxl::read_xlsx("C:/Users/CMADSEN/Downloads/LocalRWork/data/WatercraftInspectionData_AllYears_PreCleaning.xlsx",
                        col_types = "text")

# Just keep 2020 and 2021 data.
dat = dat %>% filter(str_detect(Watercraft_Risk_Assessment_ID, "2020-") | str_detect(Watercraft_Risk_Assessment_ID, "2021-"))

#Add a year column.
dat$Year = str_extract(dat$Watercraft_Risk_Assessment_ID, "^202.{1}")

#Filter for Sep 1 to Oct 31.
dat = dat %>% 
  filter(month(Start_Time) %in% c(9,10))
gc()

#Remove year from Inspection_Date, flip order of month and day.
dat = dat %>% 
  mutate(my.date = str_remove_all(dat$Inspection_Date, "-202.{1}$")) %>% 
  mutate(my.date = paste0(str_extract(my.date, "(?<=-).*$"),
                          "-",
                          str_extract(my.date, ".*(?=-)"))) %>% 
  mutate(my.date = forcats::fct_inorder(factor(my.date)))

#Make day-of-week field.
dat = dat %>% 
  mutate(my.day.of.week = factor(lubridate::wday(raw_timestamp,label=T)))

#Clean up inspection hour field.
dat = dat %>% 
  mutate(Inspection_Hour = case_when(
    Year == "2021" ~ coalesce(StartTimeComments,StartTimeAuto),
    Year == "2020" ~ Start_Time)) %>%
  mutate(Inspection_Hour = str_extract(Inspection_Hour, "(?<=T)[0-9]{2}")) %>% 
  mutate(Inspection_Hour = case_when(
    Year == "2020" ~ as.character(as.numeric(Inspection_Hour)+6),
    Year == "2021" ~ Inspection_Hour
  )) %>% 
  mutate(Inspection_Hour = str_remove(Inspection_Hour,"^0{1}")) %>% 
  mutate(Inspection_Hour = factor(Inspection_Hour, levels = c(1:24)))

#Save all dat to an object.
dat_all = dat

#Just keep data for Mt. Robson.
dat = dat %>% 
  filter(Station == "Mt. Robson")

# Fig 1. Low-risk Inspections Daily for Year -filtered just for Sep 1st to 
# end of October and if possible compare 2020 and 2021 data
# Use my.date

f1_part1 = dat %>% 
  filter(Year == "2020") %>% 
  filter(High_Risk_AIS_Ind == "false") %>% 
  group_by(Year,my.date) %>% 
  summarise(Number = n()) %>% 
  ggplot() + 
  geom_col(aes(x = my.date, y = Number, fill = Year)) +
  geom_text(aes(x = my.date, y = Number+3, label = Number)) +
  labs(y = "Inspections", x = "Inspection Date") + 
  # scale_x_discrete(breaks = levels(dat$my.date)[floor(seq(1,
  #                                                    nlevels(dat$my.date),
  #                                                    length.out = 10))]) +
  scale_x_discrete(drop = FALSE) +
  guides(fill = "none") +
  theme(panel.grid = element_line(linetype = "dashed", colour = "grey"),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette = "Dark2") + 
  ggtitle("Mt. Robson Low-risk Inspections Daily",
  subtitle = "September - October, 2020")

f1_part2 = dat %>% 
  filter(Year == "2021") %>% 
  filter(High_Risk_AIS_Ind == "false") %>% 
  group_by(Year,my.date) %>% 
  summarise(Number = n()) %>% 
  ggplot() + 
  geom_col(aes(x = my.date, y = Number, fill = Year)) +
  geom_text(aes(x = my.date, y = Number+3, 
                label = Number), size = 4) +
  labs(y = "Inspections", x = "Inspection Date") + 
  # scale_x_discrete(breaks = levels(dat$my.date)[floor(seq(1,
  #                                                    nlevels(dat$my.date),
  #                                                    length.out = 10))]) +
  scale_x_discrete(drop = FALSE) +
  guides(fill = "none") +
  theme(panel.grid = element_line(linetype = "dashed", colour = "grey"),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette = "Dark2") + 
  ggtitle("Mt. Robson Low-risk Inspections Daily",
          subtitle = "September - October, 2021")

# Fig 2. High-risk Inspections Daily for Year 
# Filtered just for Sep 1st to 
# end of October and if possible compare 2020 and 2021 data
# Use my.date

f2_part1 = dat %>% 
  filter(Year == "2020") %>% 
  filter(High_Risk_AIS_Ind == "true") %>% 
  mutate(Number = 0) %>% 
  bind_rows(data.frame(Year = "2020", 
                       High_Risk_AIS_Ind = "true", 
                       Number = 0,
                       my.date = dat[1,]$my.date)) %>% 
  ggplot() + 
  geom_col(aes(x = my.date, y = Number)) +
  labs(y = "Inspections", x = "Inspection Date") + 
  # scale_x_discrete(breaks = levels(dat$my.date)[floor(seq(1,
  #                                                    nlevels(dat$my.date),
  #                                                    length.out = 10))]) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(limits = c(0, 5)) +
  theme(panel.grid = element_line(linetype = "dashed", colour = "grey"),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette = "Dark2") + 
  ggtitle("Mt. Robson High-risk Inspections Daily",
          subtitle = "September - October, 2020")

f2_part2 = dat %>% 
  filter(Year == "2021") %>% 
  filter(High_Risk_AIS_Ind == "true") %>% 
  group_by(my.date) %>% 
  summarise(Number = n()) %>% 
  ggplot() + 
  geom_col(aes(x = my.date, y = Number, fill = "none")) +
  geom_text(aes(x = my.date, y = Number*1.3, 
                label = Number), size = 4) +
  labs(y = "Inspections", x = "Inspection Date") + 
  # scale_x_discrete(breaks = levels(dat$my.date)[floor(seq(1,
  #                                                    nlevels(dat$my.date),
  #                                                    length.out = 10))]) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(limits = c(0, 5)) +
  guides(fill = "none") +
  theme(panel.grid = element_line(linetype = "dashed", colour = "grey"),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette = "Dark2") + 
  ggtitle("Mt. Robson High-risk Inspections Daily",
          subtitle = "September - October, 2021")

# Fig 3. 	Low-risk per day (summarised).
f3 = dat %>% 
  filter(High_Risk_AIS_Ind == "false") %>% 
  group_by(Year, my.day.of.week) %>% 
  summarise(Number_of_Inspections = n()) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_col(aes(x = my.day.of.week, y = Number_of_Inspections, 
               fill = Year)) +
  geom_text(aes(x = my.day.of.week, y = Number_of_Inspections+10, 
                label = Number_of_Inspections), size = 4) +
  labs(y = "Inspections", x = "Day of Week",
       fill = "Day of Week") + 
  guides(fill = "none") +
  facet_wrap( ~ Year) +
  scale_x_discrete(drop = F) +
  theme(panel.grid = element_line(linetype = "dashed", colour = "grey"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette = "Dark2") + 
  ggtitle("Mt. Robson Daily Low-Risk Inspections",
          subtitle = "September - October, 2020 and 2021")

# Fig 4. 	High-risk per day (summarised).
f4 = dat %>% 
  filter(High_Risk_AIS_Ind == "true") %>% 
  group_by(Year, my.day.of.week) %>% 
  summarise(Number_of_Inspections = n()) %>% 
  ungroup() %>% 
  bind_rows(data.frame(Year = "2020")) %>% 
  mutate(my.day.of.week = case_when(
    is.na(my.day.of.week) ~ lag(my.day.of.week),
    T ~ my.day.of.week
  )) %>% 
  ggplot() + 
  geom_col(aes(x = my.day.of.week, y = Number_of_Inspections, 
               fill = Year)) +
  geom_text(aes(x = my.day.of.week, y = Number_of_Inspections*1.3, 
                label = Number_of_Inspections), size = 4) +
  labs(y = "Inspections", x = "Day of Week",
       fill = "Day of Week") + 
  guides(fill = "none") +
  facet_wrap( ~ Year) +
  scale_y_continuous(limits = c(0,5),breaks = c(0,1,3,5)) +
  scale_x_discrete(drop = F) +
  theme(panel.grid = element_line(linetype = "dashed", colour = "grey"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette = "Dark2") + 
  ggtitle("Mt. Robson Daily High-Risk Inspections",
          subtitle = "September - October, 2020 and 2021")

# Fig 5. Low-risk inspections by hour, Sep 1 to Oct 31
f5 = dat %>% 
  filter(High_Risk_AIS_Ind == "false") %>% 
  group_by(Year, Inspection_Hour) %>% 
  summarise(Number_of_Inspections = n()) %>% 
  ggplot() + 
  geom_col(aes(x = Inspection_Hour, y = Number_of_Inspections, 
               fill = Year)) +
  geom_text(aes(x = Inspection_Hour, y = jitter(Number_of_Inspections+20, 5),
                 label = Number_of_Inspections), size = 4) +
  guides(fill = "none") +
  labs(y = "Inspections", x = "Hour") + 
  lims(x = as.character(c(1:24))) +
  facet_wrap( ~ Year) +
  theme(panel.grid = element_line(linetype = "dashed", colour = "grey"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette = "Dark2") + 
  ggtitle("Mt. Robson Low-Risk Inspections by Hour",
          subtitle = "September - October, 2020 and 2021")

# Fig 6. High-risk inspections by hour, Sep 1 to Oct 31
f6 = dat %>% 
  filter(High_Risk_AIS_Ind == "true") %>% 
  group_by(Year, Inspection_Hour) %>% 
  summarise(Number_of_Inspections = n()) %>% 
  bind_rows(data.frame(Year = "2020")) %>% 
  ggplot() + 
  geom_col(aes(x = Inspection_Hour, y = Number_of_Inspections, 
               fill = Year)) +
  geom_text(aes(x = Inspection_Hour, y = Number_of_Inspections+0.2,
                label = Number_of_Inspections), size = 4) +
  guides(fill = "none") +
  scale_y_continuous(limits = c(0,3), breaks = c(0,1,2,3)) +
  labs(y = "Inspections", x = "Hour") + 
  lims(x = as.character(c(1:24))) +
  facet_wrap( ~ Year) +
  theme(panel.grid = element_line(linetype = "dashed", colour = "grey"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette = "Dark2") + 
  ggtitle("Mt. Robson High-Risk Inspections by Hour",
          subtitle = "September - October, 2020 and 2021")

setwd("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/2021 data/Figures_for_Martina")

# Martina has asked for f2_part2 to be recreated for five other stations.

# - Radium -
f2_rad = dat_all %>% 
  filter(Station == "Radium") %>% 
  filter(Year == "2021") %>% 
  filter(High_Risk_AIS_Ind == "true") %>% 
  group_by(my.date) %>% 
  summarise(Number = n()) %>% 
  ggplot() + 
  geom_col(aes(x = my.date, y = Number, fill = "none")) +
  geom_text(aes(x = my.date, y = Number*1.3, 
                label = Number), size = 4) +
  labs(y = "Inspections", x = "Inspection Date") + 
  # scale_x_discrete(breaks = levels(dat$my.date)[floor(seq(1,
  #                                                    nlevels(dat$my.date),
  #                                                    length.out = 10))]) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(limits = c(0, 5)) +
  guides(fill = "none") +
  theme(panel.grid = element_line(linetype = "dashed", colour = "grey"),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette = "Dark2") + 
  ggtitle("Radium High-risk Inspections Daily",
          subtitle = "September - October, 2021")

# - Yahk - 
f2_yahk = dat_all %>% 
  filter(Station == "Yahk") %>% 
  filter(Year == "2021") %>% 
  filter(High_Risk_AIS_Ind == "true") %>% 
  group_by(my.date) %>% 
  summarise(Number = n()) %>% 
  ggplot() + 
  geom_col(aes(x = my.date, y = Number, fill = "none")) +
  geom_text(aes(x = my.date, y = Number*1.3, 
                label = Number), size = 4) +
  labs(y = "Inspections", x = "Inspection Date") + 
  # scale_x_discrete(breaks = levels(dat$my.date)[floor(seq(1,
  #                                                    nlevels(dat$my.date),
  #                                                    length.out = 10))]) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(limits = c(0, 5)) +
  guides(fill = "none") +
  theme(panel.grid = element_line(linetype = "dashed", colour = "grey"),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette = "Dark2") + 
  ggtitle("Yahk High-risk Inspections Daily",
          subtitle = "September - October, 2021")

# - Yahk - 
f2_olsen = dat_all %>% 
  filter(str_detect(Station, "Olsen")) %>% 
  filter(Year == "2021") %>% 
  filter(High_Risk_AIS_Ind == "true") %>% 
  group_by(my.date) %>% 
  summarise(Number = n()) %>% 
  ggplot() + 
  geom_col(aes(x = my.date, y = Number, fill = "none")) +
  geom_text(aes(x = my.date, y = Number*1.3, 
                label = Number), size = 4) +
  labs(y = "Inspections", x = "Inspection Date") + 
  # scale_x_discrete(breaks = levels(dat$my.date)[floor(seq(1,
  #                                                    nlevels(dat$my.date),
  #                                                    length.out = 10))]) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(limits = c(0, 5)) +
  guides(fill = "none") +
  theme(panel.grid = element_line(linetype = "dashed", colour = "grey"),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette = "Dark2") + 
  ggtitle("Olsen (Hwy 3) High-risk Inspections Daily",
          subtitle = "September - October, 2021")

# - Pacific - 
f2_pac = dat_all %>% 
  filter(str_detect(Station, "Pacific")) %>% 
  filter(Year == "2021") %>% 
  filter(High_Risk_AIS_Ind == "true") %>% 
  group_by(my.date) %>% 
  summarise(Number = n()) %>% 
  bind_rows(data.frame(#Number = 0, 
            my.date = dat[1,]$my.date)) %>% 
  ggplot() + 
  geom_col(aes(x = my.date, y = Number, fill = "none")) +
  geom_text(aes(x = my.date, y = Number*1.3, 
                label = Number), size = 4) +
  labs(y = "Inspections", x = "Inspection Date") + 
  # scale_x_discrete(breaks = levels(dat$my.date)[floor(seq(1,
  #                                                    nlevels(dat$my.date),
  #                                                    length.out = 10))]) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(limits = c(0, 5)) +
  guides(fill = "none") +
  theme(panel.grid = element_line(linetype = "dashed", colour = "grey"),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette = "Dark2") + 
  ggtitle("Pacific High-risk Inspections Daily",
          subtitle = "September - October, 2021")

# - Golden - 
f2_gold = dat_all %>% 
  filter(str_detect(Station, "Golden")) %>% 
  filter(Year == "2021") %>% 
  filter(High_Risk_AIS_Ind == "true") %>% 
  group_by(my.date) %>% 
  summarise(Number = n()) %>% 
  ggplot() + 
  geom_col(aes(x = my.date, y = Number, fill = "none")) +
  geom_text(aes(x = my.date, y = Number*1.3, 
                label = Number), size = 4) +
  labs(y = "Inspections", x = "Inspection Date") + 
  # scale_x_discrete(breaks = levels(dat$my.date)[floor(seq(1,
  #                                                    nlevels(dat$my.date),
  #                                                    length.out = 10))]) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(limits = c(0, 5)) +
  guides(fill = "none") +
  theme(panel.grid = element_line(linetype = "dashed", colour = "grey"),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette = "Dark2") + 
  ggtitle("Golden High-risk Inspections Daily",
          subtitle = "September - October, 2021")

my.width = 8
my.height = 4

ggsave("2020_LowRisk_Inspections_DailyForYear.jpg",f1_part1,width = my.width, height = my.height, dpi = 150)
ggsave("2021_LowRisk_Inspections_DailyForYear.jpg",f1_part2,width = my.width, height = my.height, dpi = 150)
ggsave("2020_HighRisk_Inspections_DailyForYear.jpg",f2_part1,width = my.width, height = my.height, dpi = 150)
ggsave("2021_HighRisk_Inspections_DailyForYear.jpg",f2_part2,width = my.width, height = my.height, dpi = 150)
ggsave("Mt.Robson_LowRiskByDay.jpg",f3,width = my.width, height = my.height, dpi = 150)
ggsave("Mt.Robson_HighRiskByDay.jpg",f4,width = my.width, height = my.height, dpi = 150)
ggsave("Mt.Robson_LowRiskByHour.jpg",f5,width = my.width, height = my.height, dpi = 150)
ggsave("Mt.Robson_HighRiskByHour.jpg",f6,width = my.width, height = my.height, dpi = 150)
ggsave("Radium_HighRiskByDay.jpg",f2_rad,width = my.width, height = my.height, dpi = 150)
ggsave("Yahk_HighRiskByDay.jpg",f2_yahk,width = my.width, height = my.height, dpi = 150)
ggsave("Olsen_HighRiskByDay.jpg",f2_olsen,width = my.width, height = my.height, dpi = 150)
ggsave("Pacific_HighRiskByDay.jpg",f2_pac,width = my.width, height = my.height, dpi = 150)
ggsave("Golden_HighRiskByDay.jpg",f2_gold,width = my.width, height = my.height, dpi = 150)
