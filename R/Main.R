### Parser for compiling basic IO variables for the GLORIA MRIO from the raw tables in Tvy format

library(stringr)
library(data.table)
library(openxlsx)
library(dplyr)

## Set paths where tables in Tvy format are located and where the results should be stored
path <- list("root" = "W:/WU/Projekte/GRU/04_Daten/MRIO/GLORIA/",
             "raw" = "Version 54_December 2021/GLORIA_MRIOs_54_" )

filename <- list("pre" = "20211119_97secMother_AllCountries_002_",
                 "mid" = "-Results_",
                 "post" = "_054_Markup001(full).csv",
                 "labels" = "GLORIA_Countries_Sectors.xlsx")

## Read unique lists and create labels
unique <- list("region" = read.xlsx( str_c("./input/", filename$labels), sheet = 1, colNames = FALSE  ),
               "sector" = read.xlsx( str_c("./input/", filename$labels), sheet = 2, colNames = FALSE  ),
               "finaldemand" = read.xlsx( str_c("./input/", filename$labels), sheet = 3, colNames = FALSE  ))

# Set colnames
colnames(unique$region) <- colnames(unique$sector) <- colnames(unique$finaldemand) <- c("index", "name")

# Read dimensions
nreg <- nrow(unique$region)
nsec <- nrow(unique$sector)
nfd <- nrow(unique$finaldemand)

# Create labels for raw and parsed tables
tmp_raw <- data.frame("index" = 1:(nreg * nsec * 2),
                      "region_code" = rep(unique$region$index, each = nsec * 2),
                      "region_name" = rep(unique$region$name, each = nsec * 2),
                      "entity_code" = rep( 1:2, each = nsec ), 
                      "entity_name" = rep( c("Industry", "Product"), each = nsec ),
                      "sector_code" = unique$sector$index,
                      "sector_name" = unique$sector$name)

tmp_parsed <- data.frame("index" = 1:(nreg * nsec),
                         "region_code" = rep(unique$region$index, each = nsec),
                         "region_name" = rep(unique$region$name, each = nsec),
                         "sector_code" = unique$sector$index,
                         "sector_name" = unique$sector$name)

labels <- list("T" = tmp_raw,
               "parsed" = tmp_parsed)

indices <- list("ind" = labels$T %>% filter(entity_code == 1) %>% pull(index),
                "pro" = labels$T %>% filter(entity_code == 2) %>% pull(index) )

remove(tmp_parsed, tmp_raw)

## Set years of the time series and perform transformation
years <- 2008

for(year in years)
{
  # Read transaction matrix
  T <- fread( str_c(path$root, path$raw, year, "/", 
                    filename$pre, "T", filename$mid, year, filename$post) )
  
  # Read final demand matrix
  Y_raw <- fread( str_c(path$root, path$raw, year, "/", 
                        filename$pre, "Y", filename$mid, year, filename$post) )
  
  # Transform to matrix format
  T <- as.matrix(T)
  Y_raw <- as.matrix(Y_raw)
  
  # Subset matrices to get variables
  S <- T[indices$ind,indices$pro]
  U <- T[indices$pro,indices$ind]
  Y <- Y_raw[indices$pro,]
  
  # Gross production of all industries (q) and products (x)
  x <- colSums(S)                 
  q <- rowSums(S)             
  
  # Matrix of all commodity output proportions (industry by product)
  D <- t(S/q)                     
  # Set NaN (due to zero gross output) to zero
  D[is.na(D)] <- 0                
  
  # Commodity by industry coefficient matrix
  B <- t(t(U)/x)                  
  
  # Set NaN (due to zero gross output) to zero
  B[is.na(B)] <- 0                
  
  # Calculate pro-by-pro technology matrix
  A <- B %*% D
  
  # Create identity matrix
  I <- diag( rep( 1,nrow(A) ) )
  
  # Create inverse
  L <- solve(I - A)               
  
  
}


