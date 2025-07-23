# Script for RME-Stat Project
# This script caculates all material footprint multipliers following a given 
# material concordance for specific region (= Austria)

# material concordance to aggregate extension
mat_conc <- read.xlsx("./input/concordance_material_groups_RME_Stat_Tool.xlsx")

# Set years
TIME <- 1990:2028

# Select region
reg <- "Austria"

# Select column indices of region
# index_reg <- labels$parsed$Z$index[labels$parsed$Z$region_name == reg]

# Create aggregation key for Z and Y to have an MRIO containing Austria and RoW aggregated
agg_key <- labels$parsed$Z %>% 
  mutate(agg_reg = case_when(region_name == reg ~ 1,
                             .default = 2),
         key = str_c(agg_reg, "_",sector_code) )

agg_key_Y <- labels$parsed$Y %>% 
  mutate(key = case_when(region_name == reg ~ 1,
                             .default = 2))

# Labels for aggregated two-region MRIO
labels_agg <- data.frame("index" = 1:240,
                         "region_code" = rep(c(1,2),each = 120),
                         "region_name" = rep(c("Austria","ROW"),each = 120),
                         "sector_code" = rep(1:120,2),
                         "sector_name" = rep(agg_key$sector_name[1:120],2))

# Names of materials
mat_names <- mat_conc$material_group_StAT |> unique()

result_footprint <- data.frame("consuming_region" = reg,
                               "material" = rep( mat_names, length(TIME) ),
                               "year" = rep(TIME, each = 19),
                               "value" = NA,
                               stringsAsFactors = FALSE)


result_multiplier <- data.frame("producer_region" = "ROW",
                                "material" = rep( mat_names, each = n$sec),
                                "sector" = labels$parsed$Z$sector_name[1:120],
                                "year" = rep(TIME, each = (120*19)),
                                "MP_Austria" = NA,
                                "MP_ROW" = NA,
                                "gross_production" = NA,
                                "imports_monetary_to_Austria" = NA,
                                stringsAsFactors = FALSE)

result_global_FP <- data.frame("material" = rep( mat_names, length(TIME)),
                               "year" = rep(TIME, each = length(mat_names)),
                               "FP_Austria" = NA,
                               "FP_ROW" = NA,
                               stringsAsFactors = FALSE)

result_finalproduct_import <- data.frame("material" = rep( mat_names, each = n$sec ),
                                         "year" =  rep(TIME, each = (length(mat_names)*n$sec) ),
                                         "final_product" = labels$parsed$Z$sector_name[1:120],
                                         "producer" = "ROW",
                                         "final_consumer" = reg,
                                         "MP_ROW" = NA,
                                         "MP_Austria" = NA,
                                         "monetary_import_final_product" = NA)


# Empty list for storing monetary import block of Austria
IM <- list()

# Loop across years
# year <- 1990
for( year in TIME )
{
  print(year)
  # Load raw extension, select materials and aggregated according to concordance
  
  # folder for respective year
  # folder <- str_c(path$rawExtension,
  #                 "GLORIA_SatelliteAccounts_059_", year, "/")
  
  # Get names of files in extension folder
  files <- list.files(path$rawExtension)
  
  # Select file name that belongs to the inter-industry matrix 
  i <- grep(str_c("TQ-Results_",year), files)
  
  # Read raw matrix, transform to matrix and select industries
  Q <- fread( str_c(path$rawExtension, files[i]) )
  
  # Read row indices belonging to materials
  index <- unique$extension %>% filter(Sat_head_indicator == "Material") %>% pull(Lfd_Nr)
  Q_mat <- as.matrix(Q)[index,indices$ind]
  
  # Aggregate material extension according to materials list and two-region classification
  tmp <- Agg(x = Q_mat, aggkey = mat_conc$material_group_StAT,dim =  1)
  Q_mat_agg <- Agg(x = tmp, aggkey =  agg_key$key, dim = 2)[,c(121:240,1:120)]
  
  # Load MRIO tables
  Z <- fread(str_c(path$storeMRIOModel,year,"_U.csv"))
  Y <- fread(str_c(path$storeMRIOModel,year,"_Y.csv"))
  
  # Aggregate MRIO to two regions
  tmp_1 <- Agg(x = as.matrix(Z), dim = 1, aggkey = agg_key$key )
  tmp_2 <- Agg(x = as.matrix(tmp_1), dim = 2, aggkey = agg_key$key )
  Z_agg <- tmp_2[c(121:240,1:120),c(121:240,1:120)]
  
  tmp_1 <- Agg(x = as.matrix(Y), dim = 1, aggkey = agg_key$key)
  tmp_2 <- Agg(x = tmp_1, dim = 2, aggkey = agg_key_Y$key)
  Y_agg <- tmp_2[c(121:240,1:120),c(2,1)]
  
  sum(Y) - sum(Y_agg)
  sum(Z) - sum(Z_agg)
  
  # Estimate gross production
  x <- rowSums(Z_agg) + rowSums(Y_agg)
  
  # Calculate direct intensities
  E <- t(Q_mat_agg)/x

  # If division by zeros...
  E[is.na(E)] <- 0
  E[E == Inf] <- 0
  
  # Technology matrix
  A <- t(t(Z_agg)/x)
  
  # Identity matrix
  I <- diag(1, nrow = nrow(A))
  
  # Leontief
  L <- solve(I - A)
  sum(L)
  
  print(str_c("sum of L ",sum(L)))
  print(str_c("min of L ",min(L)))
  print(str_c("max of L ",max(L)))
  
  # Estimate output multipliers
  L_star <- t(t(L)/diag(L))
  
  print(str_c("sum of L_star ",sum(L_star)))
  print(str_c("min of L_star ",min(L_star)))
  print(str_c("max of L_star ",max(L_star)))
  
  sum(L %*% Y_agg) - sum(x)
  
  # Write import block into list
  IM[[as.character(year)]] <- Z_agg[121:240,1:120]
  
  # Loop over materials
  # m <- 1
  for(m in 1:ncol(E))
  {
    # Get material name
    mat_name <- colnames(E)[m]
  
    print(str_c("Global extraction of ",mat_name, " ", round( sum(x*E[,m])/10^6, digits = 2 ), " Mt" ) )
    
    # 1. Output multiplier calculations 
    
    # Estimate multipliers
    MP <- L_star * E[,m]
      
    # Calculate domestic content
    MP_dom <- colSums(MP[1:120,121:240])
      
    # Calculate foreign content
    MP_for <- colSums(MP[,121:240]) - MP_dom
  
    # Monetary imports of intermediates to Austria
    intermed <- rowSums(Z_agg[121:240,1:120])
    
    # Write into data frame
    result_multiplier$MP_Austria[result_multiplier$material == mat_name & result_multiplier$year == year] <- MP_dom
    result_multiplier$MP_ROW[result_multiplier$material == mat_name & result_multiplier$year == year] <- MP_for
    result_multiplier$gross_production[result_multiplier$material == mat_name & result_multiplier$year == year] <- x[121:240]
    result_multiplier$imports_monetary_to_Austria[result_multiplier$material == mat_name & result_multiplier$year == year] <- intermed
      
    # 2. Total Final Demand footprint calculation for two regions 
    
    # Estimate multiplier 
    MP <- colSums( L * E[,m] )
  
    # Estimate footprint
    FP <- MP %*% Y_agg
    
    # Write into result data frame
    result_global_FP$FP_Austria[result_global_FP$year == year & result_global_FP$material == mat_name] <- FP[1,1]
    result_global_FP$FP_ROW[result_global_FP$year == year & result_global_FP$material == mat_name] <- FP[1,2]
    
    # 3. Final Demand Multipliers for imports of final products from ROW to Austria
    
    # Estimate multiplier 
    MP <- L * E[,m]
    
    # Calculate domestic content in imports
    MP_dom <- colSums(MP[1:120,121:240])
    
    # Calculate foreign content in imports 
    MP_for <- colSums(MP[,121:240]) - MP_dom
    
    # Monetary imports of final products to Austria
    import_final <- Y_agg[121:240,1]
    
    # Write into result data frame
    result_finalproduct_import$MP_ROW[result_finalproduct_import$year == year & result_finalproduct_import$material == mat_name] <- MP_for
    result_finalproduct_import$MP_Austria[result_finalproduct_import$year == year & result_finalproduct_import$material == mat_name] <- MP_dom
    result_finalproduct_import$monetary_import_final_product[result_finalproduct_import$year == year & result_finalproduct_import$material == mat_name] <- import_final
  }
}


results <- list("output_mulitplier" = result_multiplier,
                "consumption_footprints" = result_global_FP,
                "CBA_mulitpliers_imports" = result_finalproduct_import)

write.xlsx( results, str_c(path$storeResults,"RME_Stat_Tool_GLORIA_output_multipliers_and_consumption_footprints_Austria_1990_2028.xlsx") )
write.xlsx( IM , str_c(path$storeResults,"RME_Stat_Tool_GLORIA_intermediate_imports_from_ROW_to_Austria.xlsx") )





results_FINAL_from_to <- result_from_to %>% 
  filter(value != 0)

# Check footprints of target country
CHECK <- results_FINAL_from_to %>% 
  filter(To_RegionName == reg) %>% 
  group_by(year, stressor) %>% 
  summarise(value_from_to = sum(value), .groups = 'drop') %>% 
  full_join(result_footprint %>% 
              select(-consuming_region),
            by = c("stressor" = "material", "year" = "year"))
  
fwrite(results_FINAL_from_to, "./output/RME_flows_GLORIA_MRIO_v59_FromToFormat_for_StatA_Tool_Project.csv")


results_FINAL_from_to_Austria <- results_FINAL_from_to %>% 
  filter(To_RegionName == reg | From_RegionName == reg)

write.xlsx(results_FINAL_from_to_Austria, "./output/RME_flows_for_Austria_GLORIA_MRIO_v59_for_StatA_Tool_Project.xlsx")

# Remove zero values and write results to file
# result <- result[result$value != 0,]
#   
# 
#   
# remove(result, s)
