# This script creates all labels, codes and other information that the IO calculation needs
# It also in the end compiles the aggregation function

## Import region concordances
RegConco <- list("world" = read.xlsx( str_c("./input/", filename$RegConcordance), sheet = 3, colNames = TRUE),
                 "income" = read.xlsx( str_c("./input/", filename$RegConcordance), sheet = 4, colNames = TRUE),
                 "development" = read.xlsx( str_c("./input/", filename$RegConcordance), sheet = 5, colNames = TRUE) )

## Read unique lists and create labels
unique <- list("region" = read.xlsx( str_c("./input/", filename$labels), sheet = 1, colNames = TRUE ),
               "sector" = read.xlsx( str_c("./input/", filename$labels), sheet = 2, colNames = TRUE ),
               "finaldemand" = read.xlsx( str_c("./input/", filename$labels), sheet = 3, colNames = TRUE  ),
               "extension" = read.xlsx( str_c("./input/", filename$labels), sheet = 5, colNames = TRUE  ) )

## Read region groupings and add to unique list

tmp <- melt(RegConco$world, id.vars = "region_index") %>% 
  filter(value == 1) %>% 
  select(region_index, variable ) %>% 
  arrange(region_index)

unique$region["World_region"] <- as.character( tmp$variable )


tmp <- melt(RegConco$income, id.vars = "region_index") %>% 
  filter(value == 1) %>% 
  select(region_index, variable ) %>% 
  arrange(region_index)

unique$region["Income_group"] <- as.character( tmp$variable )


tmp <- melt(RegConco$development, id.vars = "region_index") %>% 
  filter(value == 1) %>% 
  select(region_index, variable ) %>% 
  arrange(region_index)

unique$region["Development_group"] <- as.character( tmp$variable )

# Remove region indices in concordances
RegConco$world$region_index <- NULL
RegConco$income$region_index <- NULL
RegConco$development$region_index <- NULL

## Read dimensions
nreg <- nrow(unique$region)
nsec <- nrow(unique$sector)
nfd <- nrow(unique$finaldemand)

## Create labels for raw and parsed tables
tmp_raw <- data.frame("index" = 1:(nreg * nsec * 2),
                      "region_code" = rep(unique$region$Lfd_Nr, each = nsec * 2),
                      "region_name" = rep(unique$region$Region_names, each = nsec * 2),
                      "entity_code" = rep( 1:2, each = nsec ), 
                      "entity_name" = rep( c("Industry", "Product"), each = nsec ),
                      "sector_code" = unique$sector$Lfd_Nr,
                      "sector_name" = unique$sector$Sector_names)

tmp_parsed_Z <- data.frame("index" = 1:(nreg * nsec),
                           "region_code" = rep(unique$region$Lfd_Nr, each = nsec),
                           "region_name" = rep(unique$region$Region_names, each = nsec),
                           "sector_code" = unique$sector$Lfd_Nr,
                           "sector_name" = unique$sector$Sector_names)

tmp_parsed_Y <- data.frame("index" = 1:(nreg * nfd),
                           "region_code" = rep(unique$region$Lfd_Nr, each = nfd),
                           "region_name" = rep(unique$region$Region_names, each = nfd),
                           "sector_code" = unique$finaldemand$Lfd_Nr,
                           "sector_name" = unique$finaldemand$Final_demand_names )

labels <- list("T" = tmp_raw,
               "parsed" = list("Z" = tmp_parsed_Z,
                               "Y" = tmp_parsed_Y ) )

indices <- list("ind" = labels$T %>% filter(entity_code == 1) %>% pull(index),
                "pro" = labels$T %>% filter(entity_code == 2) %>% pull(index) )

# Remove redundant objects 
remove(tmp_parsed_Y, tmp_parsed_Z, tmp_raw, tmp)

# Write labels to folder
fwrite( labels$parsed$Z, str_c(path$storeMRIOModel,"SectorLabels.csv") )
fwrite( labels$parsed$Y, str_c(path$storeMRIOModel,"FinalDemandLabels.csv") )


# Compile aggregation function
Agg <- function(x,aggkey,dim)
{
  if(dim == 1) x <- t(x)
  
  colnames(x) <- aggkey
  
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  
  if(dim == 1) x <- t(x)
  
  return(x)
}

