### Parser for compiling basic IO variables for the GLORIA MRIO from the raw tables in Tvy format

library(stringr)
library(data.table)
library(openxlsx)
library(dplyr)
library(parallel)

## Set paths where tables in Tvy format are located and where the results should be stored
path <- list("root" = "/data/RStudio_Github/GLORIA-MRIO_Parser/input/",
             "raw" = "Version 54_December 2021/GLORIA_MRIOs_54_",
             "store" = "/data/RStudio_Github/GLORIA-MRIO_Parser/output/")

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
year <- 2008

calculate <- function(year)
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
  
  fwrite( S, str_c(path$store,year,"_S.csv") )
  fwrite( U, str_c(path$store,year,"_U.csv") )
  fwrite( Y, str_c(path$store,year,"_Y.csv") )
  
  # Matrix of all commodity output proportions (industry by product)
  D <- t(S/q)                     
  # Set NaN (due to zero gross output) to zero
  D[is.na(D)] <- 0                
  
  # Commodity by industry coefficient matrix
  B <- t(t(U)/x)                  
  
  # Set NaN (due to zero gross output) to zero
  B[is.na(B)] <- 0                
  B[B == Inf] <- 0
  
  # Calculate pro-by-pro technology matrix
  A <- B %*% D
  
  # Set negative and very small values to zero to allow inversion 
  A[A < 0.000001] <- 0
  
  fwrite( A, str_c(path$store,year,"_A.csv") )

  # Create identity matrix
  I <- diag( rep( 1,nrow(A) ) )

  # Create inverse
  L <- solve(I - A)
  fwrite( L, str_c(path$store,year,"_L.csv") )
  
}


start1 <- Sys.time()
calculate(year)
end1 <- Sys.time()
end1 - start1

# nr_core <- 10
# start1 <- Sys.time()
# cl <- makeCluster(nr_core)
# parLapply(cl,year,calculate)
# stopCluster(cl)
# end1 <- Sys.time()

L <- fread(str_c(path$store,year,"_L.csv") )
L <- as.matrix(L)

A <- fread(str_c(path$store,year,"_A.csv") )
A <- as.matrix(A)

