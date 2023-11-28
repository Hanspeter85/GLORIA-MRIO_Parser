### Parser for compiling basic IO variables for the GLORIA MRIO from the raw tables in Tvy format

# Load R packages
library(stringr)
library(data.table)
library(reshape2)
library(openxlsx)
library(dplyr)
# library(parallel)

## Set paths where tables in Tvy format are located and where the results should be stored
# In case functions or certain scripts don't work, the reason might be found here!
path <- list("rawMRIO" = "./input/Version 55_March 2022/GLORIA_MRIOs_55_",
             "rawExtension" = "D:/WU/MRIO/GLORIA_version_55/Raw_Tvy_tables/GLORIA_MRIO_Loop055_part_III_satelliteaccounts/GLORIA_SatelliteAccounts_055_",
             "storeMRIOModel" = "D:/WU/MRIO/GLORIA_version_55/Parsed/",
             "storeResults" = "D:/WU/MRIO/GLORIA_version_55/Results/")

filename <- list("pre" = "_120secMother_AllCountries_002_",
                 "mid" = "-Results_",
                 "post" = "_055_Markup001(full).csv",
                 "labels" = "GLORIA_ReadMe.xlsx",
                 "RegConcordance" = "GLORIA_164RegAgg.xlsx")

# Load all labels, codes, concordances and other meta information including the agg function
source("./R/0_create_labels.R")




# Load function for performing basic MRIO footprint analysis
source("./R/2_calculate_footprint_FromTo.R")
# source("./R/2_calculate_all_sector_footprints_for_single_region.R")


## Set years of the time series and perform parsing
years <- 1990:1994
# year <- 1990

for(year in years)
{
  # calculate_all_sector_footprints_for_single_region(year = year, region = "USA")
  calculate_all_footprints_FromTo(year = year)
  # calculate_all_sector_flows_for_selected_regiongroup(year = year, region = "LDC")
}


# Execute script for parsing the extensions (materials, labor, carbon, energy, land)
# source("./R/1_Extension_parser.R")
# Load function for parsing the basic MRIO variables (L, A, S, U, Y, ...)
# source("./R/1_MRIO_parser.R")

# Select year and stressor (biomass, metals, minerals, fossilfuels, energy, GWP100, landuse, employment 
# and caculate from-to flow results. Set region to 1:164 for getting results for all countries
# calculate_footprint_FromTo(stressor = "biomass", year = 2008, region = 1)
# calculate_footprint_FromTo(stressor = "biomass", year = 2008, region = 1:164)


