### Moss Ball analysis for Martina

# Here is where the master retailer tracking sheet is stored and I’m hoping you can help me analyze 
# the data for the following information:
# 1. Filter for the total number of contacts that are yes for the field called “Confirm if 
#    you do or do not have moss/algal balls for sale”
#    i. Complete list of the individual contacts that answered yes to the above question 
#       and sort by the field “If you do have moss/algal balls for sale provide the COS 
#       with the supplier information” and if possible tease out from the comments field 
#       how many moss balls each contact has in stock.
#    
#    ii. Also a summary of total contacts by supplier that are yes for having moss balls.
#
#    iii. Complete list of those contacts that said yes for having moss balls and yes 
#         for having confirmed ZQM found on initial inspection 

#Load in libraries
library(tidyverse)
library(readxl)

#Load in file
dat = MASTER_pet_store_tracking_sheet_March_18_Updated_copy <- read_excel("I:/Ecosystems/Conservation Science/Invasive Species/SPECIES/Zebra_Quagga_Mussel/Operations/Moss balls 2021/retailer tracking sheets/MASTER pet store tracking sheet_March 18 Updated copy.xlsm", 
                                                                          col_types = c("text", "text", "text", 
                                                                                        "text", "text", "date", "text", "text", 
                                                                                        "text", "text", "text", "text", "text", 
                                                                                        "text", "text", "text", "text", "text", 
                                                                                        "text", "text", "text"))

#i. 
dat_yes = dat %>%
  rename(mossballs = `Confirm if you do or do not have moss/algal balls for sale.`) %>%
  mutate(mossballs = str_to_lower(mossballs)) %>%
  filter(!str_detect(mossballs, pattern = "no"),
         !str_detect(mossballs, pattern = "pending"),
         !str_detect(mossballs, pattern = "\\?"),
         !str_detect(mossballs, "permanently"),
         !str_detect(mossballs, "^n$")) %>%
  arrange(`If you do have moss/algal balls for sale provide the COS with the supplier information;`) %>%
  rename(`Confirm if you do or do not have moss/algal balls for sale.` = mossballs)

write.csv(dat_yes, "I:/Ecosystems/Conservation Science/Invasive Species/SPECIES/Zebra_Quagga_Mussel/Operations/Moss balls 2021/retailer tracking sheets/Retailers with moss balls.csv", row.names = F)
#write.csv(dat_yes, "output/Retailers with moss balls.csv", row.names = F)

#i. Extra - trying to pull out number of moss balls from commments. So far, no success...
dat_yes %>%
  mutate(NumMB = case_when(
    str_detect(Comments, "tock") ~ str_trim(str_extract(Comments, "[0-9]{2} "), "right"),
    str_detect(Comments, "alls") ~ str_trim(str_extract(Comments, "[0-9]{1,2} "), "right")
  )) %>% select(Comments, NumMB) %>% View(.)

#ii.
dat_yes_contact_sum = dat_yes %>%
  mutate(`Supplier confirmed to have contaminated product?` = replace(`Supplier confirmed to have contaminated product?`,
                                                                      `Supplier confirmed to have contaminated product?` == "NA",
                                                                      NA)) %>%
  mutate(`If you do have moss/algal balls for sale provide the COS with the supplier information;` = replace(`If you do have moss/algal balls for sale provide the COS with the supplier information;`,
                                                                                                             `If you do have moss/algal balls for sale provide the COS with the supplier information;` == "NA",
                                                                                                             NA)) %>%
  group_by(`If you do have moss/algal balls for sale provide the COS with the supplier information;`,
           `Supplier confirmed to have contaminated product?`,
           `Confirmed by`) %>% 
  summarise(NumberContactsBySupplier = n())

write.csv(dat_yes_contact_sum, "I:/Ecosystems/Conservation Science/Invasive Species/SPECIES/Zebra_Quagga_Mussel/Operations/Moss balls 2021/retailer tracking sheets/Number of Contacts by Supplier.csv", row.names = F)
#write.csv(dat_yes_contact_sum, "output/Number of Contacts by Supplier.csv", row.names = F)

# #iii. Yes for moss balls and yes for ZQM confirmed on initial inspection.
# dat_yes_and_yes = dat_yes %>%
#   filter(`Have mussels been found in the moss balls` == "Yes")

# # # # UPDATE # # # # 

#Martina has homogenized supplier names. Needs to resummarise.

dat_yes_contact_sum_update = read_csv("I:/Ecosystems/Conservation Science/Invasive Species/SPECIES/Zebra_Quagga_Mussel/Operations/Moss balls 2021/retailer tracking sheets/Number of Contacts by Supplier.csv") %>%
  group_by(`If you do have moss/algal balls for sale provide the COS with the supplier information;`) %>% 
  summarise(NumberContactsBySupplier = sum(NumberContactsBySupplier))

write.csv(dat_yes_contact_sum_update, "I:/Ecosystems/Conservation Science/Invasive Species/SPECIES/Zebra_Quagga_Mussel/Operations/Moss balls 2021/retailer tracking sheets/Number of Contacts by Supplier.csv",
          row.names = F)
