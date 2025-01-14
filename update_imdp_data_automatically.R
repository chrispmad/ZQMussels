# Automatic updater of IMDP data. Run once a week.

# Get arguments from the BAT file.
args <- commandArgs(trailingOnly = TRUE)

# Read in options
# my_opts = readr::read_csv("C:/Users/CMADSEN/Downloads/LocalR/long_term_projects/ZQMussels/Options.csv")
my_opts = readr::read_csv(args[1])

# Remove 'old' version of this year's IMDP inspection data.
this_year = stringr::str_extract(Sys.time(),'^[0-9]{4}')

imdp::update_all_imdp_data(year = my_opts$year, options_filepath = args[1])
