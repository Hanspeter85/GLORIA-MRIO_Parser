### Parser for compiling basic IO variables for the GLORIA MRIO from the raw tables in Tvy format

# Load R packages
library(stringr)
library(data.table)
library(reshape2)
library(openxlsx)
library(dplyr)
library(parallel)

## Set paths where tables in Tvy format are located and where the results should be stored
# In case functions or certain scripts don't work, the reason might be found here!
path <- list("rawMRIO" = "./input/Version 55_March 2022/GLORIA_MRIOs_55_",
             "rawExtension" = "./input/Version 55_March 2022/GLORIA_MRIO_Loop055_part_III_satelliteaccounts/GLORIA_SatelliteAccounts_055_",
             "storeMRIOModel" = "./output/EEMRIO/",
             "storeResults" = "./output/results/")

filename <- list("PreMRIO" = "20220314_120secMother_AllCountries_002_",
                 "PreExtension" = "20220106_120secMother_AllCountries_002_",
                 "mid" = "-Results_",
                 "post" = "_055_Markup001(full).csv",
                 "labels" = "GLORIA_ReadMe.xlsx" )

# Load all labels, codes and other meta information including the agg function
source("./R/0_create_labels.R")

## Set years of the time series and perform parsing
years <- 1990:2020
year <- 2008

# Execute script for parsing the extensions (materials, labor, carbon, energy, land)
source("./R/1_Extension_parser.R")
# Load function for parsing the basic MRIO variables (L, A, S, U, Y, ...)
source("./R/1_MRIO_parser.R")

# Set number of cores for parallelisation
nr_core <- 10
# Take time and start/stop cluster
start1 <- Sys.time()
cl <- makeCluster(nr_core)
parLapply(cl,year,calculate)
stopCluster(cl)
end1 <- Sys.time()
end1 - start1


          