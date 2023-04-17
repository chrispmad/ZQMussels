### This script compares lists of aquarium retailers in BC to a national list given to us by DFO.

library(tidyverse)
library(readxl)

bclist <- read_excel("I:/Ecosystems/Conservation Science/Invasive Species/SPECIES/Zebra_Quagga_Mussel/Operations/Moss balls 2021/tracking sheets/MASTER pet store tracking sheet_March 10.xlsx", 
                                                       col_types = c("text", "text", "text", 
                                                                     "date", "text", "text", "text", "text", 
                                                                     "text", "text"))

dfo <- read_excel("I:/Ecosystems/Conservation Science/Invasive Species/SPECIES/Zebra_Quagga_Mussel/Operations/Moss balls 2021/tracking sheets/DFO data/2 - Science Data - RA -  Retailer List - 4 Months of Data.xlsx")

dfo_filtered = dfo %>%
  filter(Province == "BC") %>%
  filter(sum(Fishes, Invertebrates, Plants, Aquarium, WaterGarden) >= 1)

#How many unique stores do we have in the BC list?
unique(bclist$`Store Name`) #We have 478 unique names.

#How many unique stores do we have in the DFO list, filtered just for BC stores that sell more than just seafood?
unique(dfo_filtered$Store_Company_Name) #We have 110 unique names.

#Which of those 110 unique names from the DFO filtered list are already present in the BC list?
already_included = unique(dfo_filtered[dfo_filtered$Store_Company_Name %in% bclist$`Store Name`,]$Store_Company_Name)
#There are 32 stores already shared between the BC list and the DFO filtered list.

#Which of the DFO filtered list stores are NOT present currently in the BC list?
not_included = unique(dfo_filtered[!dfo_filtered$Store_Company_Name %in% bclist$`Store Name`,]$Store_Company_Name)
#There are 78 stores that are NOT already present in the BC list. Let's verify that this is true. 

#Some non-matches caused by spelling (uppercase vs. lowercase) and apostrophes.
#Let's make a new column in both sheets that does not use upper case nor apostrophes.
bclist = bclist %>%
  mutate(simplename = str_to_lower(`Store Name`),
         simplename = str_remove_all(simplename,"\\."),
         simplename = str_remove_all(simplename,","),
         simplename = str_remove_all(simplename,"'"),
         simplename = str_remove_all(simplename, "’"),
         simplename = str_remove_all(simplename,"\\("),
         simplename = str_remove_all(simplename,"\\)"))

dfo_filtered = dfo_filtered %>%
  mutate(simplename = str_to_lower(Store_Company_Name),
         simplename = str_remove_all(simplename,"\\."),
         simplename = str_remove_all(simplename,","),
         simplename = str_remove_all(simplename,"'"),
         simplename = str_remove_all(simplename, "’"),
         simplename = str_remove_all(simplename,"\\("),
         simplename = str_remove_all(simplename,"\\)"))

already_included = unique(dfo_filtered[dfo_filtered$simplename %in% bclist$simplename,]$simplename)
#Now we have 38 matches between the BC list and the filtered DFO list.

not_included = unique(dfo_filtered[!dfo_filtered$simplename %in% bclist$simplename,]$simplename)

# DFO stores not currently in the bc list:
#   All for pets, Cowichan pet emporium, Ed's World of Critters & Supplies, 
#Happy Tails Pet Supplies, Just 4 Pets, maybe Kootenay Critters & Pet Supplies 
#(though there are 'Kootenay Critters' and 'Kootenay Pet Supplies' in bc list already), 
#Lucky's Pet Supply, Mother Mother Nature Garden Home & Pet, Robins Pet Supplies 
#(BC list has 'Robins Pet Store'), Roger's Aquatics(BC list has 'Roger's Aquarium'), 
#The Purple Seahorse Pet Store, World's Exotic Pets, Anderton Nurseries, Art Knapp 
#Kamloops, Arts Nursery, Okanagan Koi & Water Gardens, The Greenary, 88 Supermarket, 
#A fine Ketter o' Fish, Anchor's Fish Market, Atlantic Canada Seafood, 
#China World Supermarket AKA Rice World Supermarket, Costco, Creekside Market, 
#Dolly's Fish Market, Fanny Bay Oyster Bar & Shellfish Market, 
#Fanny Bay Osters seafood shop, foody world, Fresh Ideas Start Here,
#Fresh Str. Market, H Mart, Henlong Market, Hung win Seafood, Loblaws, 
#Longliner Seafoods, Lucky Supermarket, Nester's Market, No Frills, 
#North American Seafood, Quality Foods, Seaborn, Seafood City, Seven Seas Fish Market, 
#Sobeys, South Harbour Fish & Gift Shop, T & T Supermarket, The Crab Shop, 
#The Daily Catch, The Fishermans Market, The Lobster Man, SaveEasy.


adds_for_bclist = dfo_filtered %>%
  filter(str_detect(simplename, 
  "all for pets|cowichan pet emp|eds world|happy tail|just 4|luckys|mother nature|robins pet|rogers aquatics|the purple|worlds exot|anderton|napp kamloops|koi &|greenary|88|kettle|anchors|atlantic canada|rice world|costco|creekside|dollys|bar &|fanny bay oysters|foody|fresh ideas|fresh st|h mart|henl|hung|loblaws|longliner|lucky super|nesters|no fri|north american seafood|quality|seaborn|seafood city|seven seas|sobeys|& gift sho|t & t|crab shop|daily ca|fishermans mark|lobster man|saveeasy"))

unique(adds_for_bclist$Store_Company_Name)

#Add new field that indicates which of these shops ONLY sell invertebrates (mostly seafood markets)
adds_for_bclist = adds_for_bclist %>%
  mutate(InvertsOnly = N,
         InvertsOnly = replace(InvertsOnly, (Fishes+Plants+Aquarium+WaterGarden) == 0, Y)) %>%
  arrange(InvertsOnly, Store_Company_Name) %>%
  select(-simplename)

write_excel_csv(adds_for_bclist, "additions for BC list.csv")
