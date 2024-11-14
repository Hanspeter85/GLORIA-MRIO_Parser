# This script caculates all material footprint mulitpliers following a given 
# material concordance for specific region (= Austria)

# material concordance to aggregate extension
mat_conc <- read.xlsx("./input/concordance_material_groups_metals_focus.xlsx")

# Set years
TIME <- 2018:2001


# Names of materials
mat_names <- mat_conc$material_group |> unique()

nr <- list("footprints" = (length(mat_names)))

# Loop across years
# year <- 2019
for( year in TIME )
{
  print(year)
  
  ## Load preprocessed MRIO
  source("./R/2_prepare_MRIO_for_calculation.R")
  
  ## Extract non-material extensions (i.e. direct intensities)
  E_other <- Q[, 5:10]
  
  ## Load raw extension, select materials and aggregated according to concordance
  
  # folder for respective year
  folder <- str_c(path$rawExtension,
                  "GLORIA_SatelliteAccounts_059_", year, "/")
  
  # Get names of files in extension folder
  files <- list.files(folder)
  
  # Select file name that belongs to the inter-industry matrix 
  i <- grep('TQ', files)
  
  # Read raw matrix, transform to matrix and select industries
  Q <- fread( str_c(folder, files[i]) )
  
  # Read row indices belonging to materials
  index <- unique$extension %>% filter(Sat_head_indicator == "Material") %>% pull(Lfd_Nr)
  Q_mat <- Q[index,]
  
  # Aggregate material extension
  Q_mat_agg <- Agg(Q_mat, mat_conc$material_group, 1)
  
  # Estimate gross production
  x <- colSums(t(L)*rowSums(Y))
  
  # Calculate direct intensities
  E_mat <- t(Q_mat_agg[,indices$ind])/x

  # Because division by zeros exist...
  E_mat[is.na(E_mat)] <- 0
  E_mat[E_mat == Inf] <- 0
  
  # Combine extensions in one data frame
  E <- cbind(E_mat, E_other)
  
  ## Load raw final demand and aggregate accordingly
  
  # Get names of files in raw folder
  files <- list.files(path$rawMRIO)
  
  # Select file name that belongs to the final demand matrix 
  i <- grep(str_c("_Y-Results_",year,"_059_Markup001"), files)
  
  # Get final demand matrix, transform to matrix and select product flows
  Y_raw <- fread( str_c(path$rawMRIO, files[i]))[indices$pro,] |> as.matrix()
  
  # Create aggregation key for aggregating final demand
  key <- data.frame("fd" = unique$FDVA$Final_demand_names,
                    "reg" = rep(unique$region$Development_group, each = 6)) %>% 
    mutate(fd = case_when(fd == 'Gross fixed capital formation' ~ 'GFCF',
                          .default = 'Other final demand'),
           key = str_c(reg, "_",fd))
  
  Y <- Agg(Y_raw, key$key, 2)
  
  Y_names <- data.frame("key" = colnames(Y)) %>% 
    separate(key, into = c("region", "demand"), sep = "_")
  
  # Create key for aggregating L i.e. Footprint Matrix
  key_L <- data.frame("sec" = unique$sector$Sector_names,
                      "reg" = rep(unique$region$Development_group, each = 120)) %>% 
    mutate(key = str_c(reg, "_",sec))
  
  L <- as.matrix(L)
  Y <- as.matrix(Y)
  
  # Loop over extensions
  # m <- 1
  for(m in 1:ncol(E))
  {
    # Get extension name
    ext_name <- colnames(E)[m]
    
    print(ext_name)
    
    # Loop over final demand
    #f <- 1
    for(f in 1:nrow(Y_names))
    {
      print(Y_names[f,])
      
      MP <- L * E[,m]
      FP <- t(t(MP)*Y[,f])
      FP <- Agg(FP, key_L$reg, 1)
      FP <- Agg(FP, key_L$sec, 2)
      
      # Create empty result object in first iteration
      if(m == 1 & f == 1)
      {
        result <- data.frame("year" = year,
                             "stressor" = rep(colnames(E), each = (nrow(Y_names)*nrow(FP)*ncol(FP))),
                             "source_region" = unique(Y_names$region),
                             "consuming_region" = rep(Y_names$region, each = (nrow(FP)*ncol(FP)) ),
                             "final_demand" = rep(Y_names$demand, each = (nrow(FP)*ncol(FP)) ),
                             "final_product_code" = rep(unique$sector$Lfd_Nr, each = nrow(FP)),
                             "final_product_name" = rep(unique$sector$Sector_names, each = nrow(FP)),
                             "final_product_group" = rep(unique$sector$Sector_group, each = nrow(FP)),
                             "value" = NA)
      }
      result$value[result$stressor == ext_name & result$year == year & result$consuming_region == Y_names$region[f] & result$final_demand == Y_names$demand[f]] <- c(FP)
    }
  }
  write.xlsx( result, str_c(path$storeResults,"GlobalSouthNorthTrade_AllStressors_FullSectorDetail_",year,".xlsx") )
}


### Compute time series
TIME <- 1990:2028

year <- 1990
for(year in TIME)
{
  print(year)
  tmp <- read.xlsx(str_c(path$storeResults,"GlobalSouthNorthTrade_AllStressors_FullSectorDetail_",year,".xlsx"))
  
  if(year == 1990)
  {
    df <- tmp
  }else
  {
    df <- df %>% 
      bind_rows(tmp)
  }
}

df_agg <- df %>% 
  group_by(year, stressor, source_region, consuming_region) %>% 
  summarise(value = sum(value), .groups = 'drop')

write.xlsx( df_agg, str_c(path$storeResults,"GlobalSouthNorthTrade_timeseries_by_stressor_and_regions.xlsx") )

df_agg <- df %>% 
  mutate(final_product_name = case_when(final_product_name != "Electric power generation, transmission and distribution" ~ "Other final products",
                                        .default = final_product_name)) %>% 
  group_by(year, stressor, source_region, consuming_region, final_product_name, final_demand) %>% 
  summarise(value = sum(value), .groups = 'drop')

write.xlsx( df_agg, str_c(path$storeResults,"GlobalSouthNorthTrade_timeseries_by_stressor_regions_finaldemand_finalproduct.xlsx") )
