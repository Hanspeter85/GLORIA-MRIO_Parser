### This script creates all labels, codes and other information that the IO calculation needs
### It also compiles the aggregation function

## Import region concordances
RegConco <- list("world" = read.xlsx( str_c("./input/", filename$RegConcordance), sheet = 3, colNames = TRUE),
                 "income" = read.xlsx( str_c("./input/", filename$RegConcordance), sheet = 4, colNames = TRUE),
                 "development" = read.xlsx( str_c("./input/", filename$RegConcordance), sheet = 5, colNames = TRUE) )

## Import sector concordance
SecConc <- read.xlsx( str_c("./input/", filename$labels), sheet = 8, colNames = TRUE, startRow = 2)

# Clean look-up table for unique sector list and sector aggregation:
tmp <- SecConc %>% 
  pivot_longer(cols = colnames(SecConc)[-1]) %>% 
  filter(value == 1) %>% 
  select(X1, name) %>%
  mutate(name = str_replace_all(name, "[.]", " ")) %>% 
  `colnames<-`(c("Sector_names", "Sector_group")) 

# Clean the aggregation matrix
rownames(SecConc) <- SecConc$X1
SecConc$X1 <- NULL

## Read unique lists and create labels
unique <- list("region" = read.xlsx( str_c("./input/", filename$labels), sheet = 1, colNames = TRUE ),
               "sector" = read.xlsx( str_c("./input/", filename$labels), sheet = 2, colNames = TRUE ),
               "FDVA" = read.xlsx( str_c("./input/", filename$labels), sheet = 3, colNames = TRUE  ),
               "extension" = read.xlsx( str_c("./input/", filename$labels), sheet = 6, colNames = TRUE  ) )

# Add sector grouping to unique list
unique$sector <- left_join(unique$sector, tmp, by = "Sector_names")

# Add region groupings to unique list
unique$region["World_region"] <- RegConco$world %>% 
  pivot_longer(cols = colnames(RegConco$world)[-1]) %>% 
  filter(value == 1) %>% 
  select(region_index, name ) %>% 
  arrange(region_index) %>% 
  mutate(name = str_replace_all(name, "[.]", " ")) %>% 
  pull(name)

unique$region["Income_group"] <- RegConco$income %>% 
  pivot_longer(cols = colnames(RegConco$income)[-1]) %>% 
  filter(value == 1) %>% 
  select(region_index, name ) %>% 
  arrange(region_index) %>% 
  mutate(name = str_replace_all(name, "[.]", " ")) %>%
  pull(name)

unique$region["Development_group"] <- RegConco$development %>% 
  pivot_longer(cols = colnames(RegConco$development)[-1]) %>% 
  filter(value == 1) %>% 
  select(region_index, name ) %>% 
  arrange(region_index) %>% 
  mutate(name = str_replace_all(name, "[.]", " ")) %>%
  pull(name)


# Remove region indices in concordances
RegConco$world$region_index <- NULL
RegConco$income$region_index <- NULL
RegConco$development$region_index <- NULL

## Read dimensions
n <- list("reg" = nrow(unique$region),
          "sec" = nrow(unique$sector),
          "fd" = nrow(unique$FDVA))

## Create labels for raw and parsed tables
tmp_raw <- data.frame("index" = 1:(n$reg * n$sec * 2),
                      "region_code" = rep(unique$region$Lfd_Nr, each = n$sec * 2),
                      "region_name" = rep(unique$region$Region_names, each = n$sec * 2),
                      "entity_code" = rep( 1:2, each = n$sec ), 
                      "entity_name" = rep( c("Industry", "Product"), each = n$sec ),
                      "sector_code" = unique$sector$Lfd_Nr,
                      "sector_name" = unique$sector$Sector_names)

tmp_parsed_Z <- data.frame("index" = 1:(n$reg * n$sec),
                           "region_code" = rep(unique$region$Lfd_Nr, each = n$sec),
                           "region_name" = rep(unique$region$Region_names, each = n$sec),
                           "sector_code" = unique$sector$Lfd_Nr,
                           "sector_name" = unique$sector$Sector_names)

tmp_parsed_Y <- data.frame("index" = 1:(n$reg * n$fd),
                           "region_code" = rep(unique$region$Lfd_Nr, each = n$fd),
                           "region_name" = rep(unique$region$Region_names, each = n$fd),
                           "sector_code" = unique$FDVA$Lfd_Nr,
                           "sector_name" = unique$FDVA$Final_demand_names )

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


# Compile matrix aggregation function
Agg <- function(x,aggkey,dim)
{
  if(dim == 1) x <- t(x)
  
  colnames(x) <- aggkey
  
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  
  if(dim == 1) x <- t(x)
  
  return(x)
}

