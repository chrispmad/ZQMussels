#Combine CBSA files from various years into one file.

#Also, make some graphs :)

library(tidyverse)
library(readxl)
library(openxlsx)

rm(list = ls())

#Set our working directory to the final report folder on the I: drive.
setwd('I:/SPECIES/Zebra_Quagga_Mussel/Communications/Inspection data reporting/')

#Load in excel sheets
d17 = read_excel('Monthly reports/2017/COS email analysis.xlsx', sheet = 'CBSA')

d18 = read_excel('Monthly reports/2018/2018_COS notifications.xlsx',
              sheet = 'Master_list')

d19 = read_excel('Final report/2019/Graphs_Data files/2019 COS notifications.xlsx',
              sheet = 'Master list (Dups & NA removed)')

d20 = read_excel('Final report/2020/GIS Maps and Excel Figures/ExcelFigureData/Figure 32_2020 COS inbox notifications.xlsx',
              sheet = 'Master_clean')

#Combine the sheets into one large sheet
d17 = d17 %>% mutate(Year = 2017)
d18 = d18 %>% mutate(Year = 2018)
d19 = d19 %>% mutate(Year = 2019)
d20 = d20 %>% mutate(Year = 2020)

d17 = d17 %>% 
  select(Year, From, Subject, Received = `Rec'd date`, Category = `Crossing (in BC unless noted otherwise)`) %>%
  filter(!is.na(From)) %>% 
  #Some entries for the Category field as either "?" or blank - set these all to Unknown.
  mutate(Category = replace(Category, Category == "?" | is.na(Category), "Unknown")) %>% 
  #Some date entries in the sheet just say the time of day and day of week... after reading some,
  #it looks like they are on the same date as whatever row above them lists the actual year/month/date.
  #So, I fill those in with the date above.
  mutate(Received = replace(Received,  str_detect(Received, "[0-9]:[0-9]"), NA)) %>% 
  fill(Received, .direction = "down") %>% 
  #Next, replace the day of the week with '2018', so that we get a nice, clean date entry.
  mutate(Received = str_replace(Received, "[a-zA-Z]+[ ,-]?", "2018-")) %>%
  #Parse date column - it can either be in excel format (e.g. 49238), or in nice text.
  mutate(Received = case_when(
    str_detect(Received, "^4[0-9]*") ~ convertToDate(as.numeric(Received)),
    str_detect(Received, "^2018") ~ as_date(Received)
  ))

d18 = d18 %>% 
  select(Year, From, Subject, Received, Category = Categories, Size) %>% 
  #Some date entries in the sheet just say the time of day and day of week... after reading some,
  #it looks like they are on the same date as whatever row above them lists the actual year/month/date.
  #So, I fill those in with the date above.
  mutate(Received = replace(Received,  str_detect(Received, "[0-9]:[0-9]"), NA)) %>% 
  fill(Received, .direction = "down") %>% 
  #Next, replace the day of the week with '2018', so that we get a nice, clean date entry.
  mutate(Received = str_replace(Received, "[a-zA-Z]+[ ,-]?", "2018-")) %>%
  #Parse date column - it can either be in excel format (e.g. 49238), or in nice text.
  mutate(Received = case_when(
    str_detect(Received, "^4[0-9]*") ~ convertToDate(as.numeric(Received)),
    str_detect(Received, "^2018") ~ as_date(Received)
  ))


d19 = d19 %>% 
  select(Year, From, Subject, Received, Category)

cbsa_dat = d20 %>% 
  rename(Category = Categories) %>% 
  select(Year, From, Subject, Received, Category, CBSA_Type, Size) %>% 
  bind_rows(d19) %>% 
  bind_rows(d18) %>% 
  bind_rows(d17)

openxlsx::write.xlsx(cbsa_dat,
                     "I:/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/Multiyear data/CBSA_All_Years.xlsx",
                     overwrite = T)

#Let's make some figures!

cdat = cbsa_dat %>% 
  mutate(Category = str_to_upper(Category)) %>% 
  mutate(Category = case_when(
    str_detect(Category, "US") ~ "USA",
    is.na(Category) ~ "Unknown",
    str_detect(Category, "NEW") ~ "NEW BOAT(S)",
    str_detect(Category, "ROOS") ~ "ROOSEVILLE",
    T ~ Category
  ))

#Horizontal bar chart that shows the top Senders overall
cdat %>% 
  mutate(Category = fct_lump(Category, n = 15)) %>% 
  count(Category, sort = T) %>% 
  mutate(Category = fct_reorder(Category,n)) %>% 
  ggplot(aes(Category,n)) + 
  geom_col(position = position_dodge()) + 
  coord_flip() + 
  labs(x = "Sender", y = "Number of Emails Received")

#Horizontal bar chart that shows the top Senders by year
cdat %>% 
  mutate(Year = as.character(Year)) %>% 
  count(Year, Category) %>% 
  group_by(Category) %>% 
  mutate(CrossYearTotal = sum(n)) %>% 
  ungroup() %>% 
  arrange(desc(CrossYearTotal),Year) %>% 
  mutate(Category = fct_reorder(Category,CrossYearTotal)) %>% 
  mutate(Category = fct_lump(Category, 6)) %>% 
  ggplot(aes(Category,n, group = Year, fill = Year)) + 
  geom_col(position = position_dodge()) + 
  coord_flip() + 
  labs(x = "Sender", y = "Number of Emails Received")
