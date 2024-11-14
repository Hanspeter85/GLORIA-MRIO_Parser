# This script caculates all material footprint mulitpliers following a given 
# material concordance for specific region (= Austria)

# material concordance to aggregate extension
mat_conc <- read.xlsx("./input/concordance_material_groups_RME_Stat_Tool.xlsx")

# Set years
TIME <- 1990:2028

# Select region
reg <- "Austria"

# Select column indices of region
index_reg <- labels$parsed$Z$index[labels$parsed$Z$region_name == reg]

# Names of materials
mat_names <- mat_conc$material_group_StAT |> unique()

result_footprint <- data.frame("consuming_region" = reg,
                               "material" = rep( mat_names, length(TIME) ),
                               "year" = rep(TIME, each = 19),
                               "value" = NA,
                               stringsAsFactors = FALSE)


result_multiplier <- data.frame("region" = reg,
                                "material" = rep( mat_names, each = n$sec),
                                "sector" = labels$parsed$Z$sector_name[1:120],
                                "year" = rep(TIME, each = (120*19)),
                                "MP_domestic" = NA,
                                "MP_foreign" = NA,
                                "gross_production" = NA,
                                stringsAsFactors = FALSE)

result_from_to <- data.frame("From_RegionCode" = rep( unique$region$Lfd_Nr, n$reg),
                             "From_RegionAcronym" = rep( unique$region$Region_acronyms, n$reg),
                             "From_RegionName" = rep( unique$region$Region_names, n$reg),
                             "From_WorldRegion" = rep( unique$region$World_region, n$reg),
                             "From_IncomeGroup" = rep( unique$region$Income_group, n$reg),
                             "From_DevelopmentGroup" = rep( unique$region$Development_group, n$reg),
                             "To_RegionCode" = rep( unique$region$Lfd_Nr, each = n$reg ),   
                             "To_RegionAcronym" = rep( unique$region$Region_acronyms, each = n$reg ),   
                             "To_RegionName" = rep( unique$region$Region_names, each = n$reg ),   
                             "To_WorldRegion" = rep( unique$region$World_region, each = n$reg),
                             "To_IncomeGroup" = rep( unique$region$Income_group, each = n$reg),
                             "To_DevelopmentGroup" = rep( unique$region$Development_group, each = n$reg),
                             "stressor" = rep(mat_names, each = (n$reg*n$reg) ),
                             "year" = rep(TIME, each = (n$reg * n$reg * length(mat_names))),
                             "value" = 0,
                             stringsAsFactors = FALSE )

# Loop across years
# year <- 1990
for( year in TIME )
{
  print(year)
  # Load raw extension, select materials and aggregated according to concordance
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
  Q_mat_agg <- Agg(Q_mat, mat_conc$material_group_StAT, 1)
  
  # Load MRIO tables
  L <- fread(str_c(path$storeMRIOModel,year,"_L.csv"))
  Y <- fread(str_c(path$storeMRIOModel,year,"_Y.csv"))
  
  # Estimate gross production
  x <- colSums(t(L)*rowSums(Y))
  
  # Calculate direct intensities
  E <- t(Q_mat_agg[,indices$ind])/x

  # Because division by zeros exist...
  E[is.na(E)] <- 0
  E[E == Inf] <- 0
  
  L <- as.matrix(L)
  Y <- as.matrix(Y)
  
  print(str_c("sum of L ",sum(L)))
  print(str_c("min of L ",min(L)))
  print(str_c("max of L ",max(L)))
  
  # Estimate output multipliers
  L_star <- t(t(L)/diag(L))
  
  print(str_c("sum of L_star ",sum(L_star)))
  print(str_c("min of L star ",min(L_star)))
  print(str_c("max of L star ",max(L_star)))
  
  # Extract region from inverse
  L_star_reg <- L_star[,index_reg]
  
  # Extract final demand of region
  Y_reg <- rowSums( Y[ , labels$parsed$Y$index[labels$parsed$Y$region_name == reg] ] )
  
  Y_tot <- Agg(Y, labels$parsed$Y$region_code, 2)
  
  # Loop over materials
  # m <- 1
  for(m in 1:ncol(E))
  {
    # Get material name
    mat_name <- colnames(E)[m]
  
    print(str_c("Global extraction of ",mat_name, " ", round( sum(x*E[,m])/10^6, digits = 2 ), " Mt" ) )
    
    # 1. Output multiplier calculations 
    
    # Estimate multipliers
    MP <- L_star_reg * E[,m]
      
    # Calculate domestic content
    MP_dom <- colSums(MP[index_reg,])
      
    # Calculate foreign content
    MP_for <- colSums(MP) - MP_dom
  
    # Write into data frame
    result_multiplier$MP_domestic[result_multiplier$material == mat_name & result_multiplier$year == year] <- MP_dom
    result_multiplier$MP_foreign[result_multiplier$material == mat_name & result_multiplier$year == year] <- MP_for
    result_multiplier$gross_production[result_multiplier$material == mat_name & result_multiplier$year == year] <- x[index_reg]
      
    # 2. Final Demand footprint calculation for selected region 
    
    # Estimate mulitplier 
    MP <- colSums( L * E[,m] )
  
    # Estimate footprint
    FP <- sum( MP * Y_reg )
      
    # Write into result data frame
    result_footprint$value[result_footprint$year == year & result_footprint$material == mat_name] <- FP
    
    # 3. From-To global flows of raw material equivalents for all regions
    
    # Mulitpliers
    MP <- L * E[,m]
    
    # Estimate footprint
    FP <- Agg( MP %*% Y_tot, labels$parsed$Z$region_code, 1 ) |> c()
    
    # Write into result data frame
    result_from_to$value[result_from_to$year == year & result_from_to$stressor == mat_name] <- FP
  }
}

results <- list("output_mulitplier" = result_multiplier,
                "consumption_footprints" = result_footprint)

write.xlsx( results, str_c(path$storeResults,"RME_Stat_Tool_GLORIA_output_multipliers_and_consumption_footprints_Austria_1990_2028.xlsx") )

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
