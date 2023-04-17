### Moss Ball Follow-up work: We want to find out more about
### the Municipal Waste Water Treatment that is used in various cities through
### out BC. Certain cities are deemed high priority because they received
### ZQMussel-infested moss balls and those retailers likely disposed of these
### moss balls down the drain ... potential invasions! We want to know
### where we should follow up.

library(tidyverse)
library(readxl)
library(openxlsx)

rm(list = ls())

# Read in the datasheet prepared by Martina; this is where we will store results.
dat = read_xlsx("\\\\SFP.IDIR.BCGOV/S140/S40203/Ecosystems/Conservation Science/Invasive Species/SPECIES/Zebra_Quagga_Mussel/Operations/Moss balls 2021/retailer tracking sheets/moss ball city tracking sheet .xlsx",
                skip = 1)

#This is the look-up excel sheet for municipal authorities and their wastewater treatment stuff.
db = read_xlsx("data/all_ams_discharges.xlsx")

# ====================================================

# Martina recommended starting with the high-priority cities.
head(dat)
