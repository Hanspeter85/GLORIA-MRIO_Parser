


  # Load footprints of specific regions and the world 
  FP_reg <- read.xlsx( str_c(path$storeResults,year,"_AllStressors_FromSourceRegions_to_Sectors_in_",region,".xlsx") ) %>% 
    mutate_if(is.factor, as.character)
  
  FP_globe <- read.csv(str_c(path$storeResults,year,"_AllStressors_FromTo.csv")) %>% 
    mutate_if(is.factor, as.character)  
  
  # Define stressors for analysis
  stressor <- data.frame("name" = colnames(Q),
                         stringsAsFactors = FALSE)
  
  
  RME_global <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[1:4]] )
  Energy_global <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[5]] )
  GWP100_global <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[7]] )
  Landuse_global <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[8]] )
  Employment_global <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[9]] )
  Valueadded_global <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[10]] )
  
  RME_trade <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[1:4] & FP_globe$From_RegionCode != FP_globe$To_RegionCode  & FP_globe$From_DevelopmentGroup == "Developed.economies"])
  Energy_trade <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[5] & FP_globe$From_RegionCode != FP_globe$To_RegionCode  & FP_globe$From_DevelopmentGroup == "Developed.economies"] )
  GWP100_trade <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[7] & FP_globe$From_RegionCode != FP_globe$To_RegionCode  & FP_globe$From_DevelopmentGroup == "Developed.economies"] )
  Landuse_trade <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[8] & FP_globe$From_RegionCode != FP_globe$To_RegionCode  & FP_globe$From_DevelopmentGroup == "Developed.economies"] )
  Employment_trade <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[9] & FP_globe$From_RegionCode != FP_globe$To_RegionCode  & FP_globe$From_DevelopmentGroup == "Developed.economies"] )
  Valueadded_trade <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[10] & FP_globe$From_RegionCode != FP_globe$To_RegionCode  & FP_globe$From_DevelopmentGroup == "Developed.economies"] )
  
  # Sum of global production-based accounts
  summary <- data.frame( "stressor" = c("materials", "energy", "GWP100","landuse", "employment", "valueadded"),
                         "unit" = c("tons", "tera joule", "kilo tons CO2eq", "1000 ha", "ppl", "1000 USD"),
                         "global_sum" = c(RME_global, Energy_global, GWP100_global, Landuse_global, Employment_global, Valueadded_global),
                         "trade_sum_north_to_south" = c(RME_trade, Energy_trade, GWP100_trade, Landuse_trade, Employment_trade, Valueadded_trade) )
  
  
  # Compiling the function for computing the sector results
  Compile_FP_Source_Region_By_Sector <- function(stressors)
  {
    # Filter stressors
    tmp_orignal <- FP_reg %>% 
      filter( stressor %in% stressors) %>% 
      select(value, To_SectorCode, From_RegionCode)
    
#      pivot_wider(values_from = "value", names_from = "To_SectorCode")
  
    # Transform from long to wide
    tmp <- as.data.frame( acast(tmp_orignal,From_RegionCode ~ To_SectorCode, sum) )
    
    # which rows and columns are missing:
    row_miss <- setdiff(1:n$reg, as.numeric(rownames(tmp)))
    col_miss <- setdiff(1:n$sec, as.numeric(colnames(tmp)))
    
    # Insert missing elements in case
    if(length(row_miss) > 0)
    {
      tmp[(nrow(tmp) + length(row_miss)), ] <- 0
      rownames(tmp)[(1+n$reg-length(row_miss)):n$reg] <- row_miss
    }
    
    if(length(col_miss) > 0)
    {
      tmp[,(ncol(tmp) + length(col_miss))] <- 0
      colnames(tmp)[(1+n$sec-length(col_miss)):n$sec ] <- col_miss
    }  
    
    # Re-order columns and rows according to index
    tmp <- tmp[order(as.numeric(rownames(tmp))),]
    tmp <- tmp[,order(as.numeric(colnames(tmp)))]
    
    # Remove NA
    tmp[is.na(tmp)] <- 0
    
    print(str_c("Sum of orginal raw data: ", sum(tmp_orignal$value) ))
    print(str_c("Sum of processed data: ", sum(tmp) ))
    
    return(tmp)
  }
  
  # Calculate full detail source regions
  RME_reg <- Compile_FP_Source_Region_By_Sector(c("biomass[t]", "metals[t]", "minerals[t]", "fossilfuels[t]"))
  Energy_reg <- Compile_FP_Source_Region_By_Sector(c("energy[TJ]"))
  GHG_reg <- Compile_FP_Source_Region_By_Sector(c("OECD_GWP100[kt]"))
  Land_reg <- Compile_FP_Source_Region_By_Sector(c("landuse[1000ha]"))
  Employment_reg <- Compile_FP_Source_Region_By_Sector(c("employment[ppl]"))
  VA_reg <- Compile_FP_Source_Region_By_Sector(c("valueadded[1000USD]"))
  
  # Aggregate to development groups
  RME_reg <- Agg(RME_reg, unique$region$Development_group, 1)
  Energy_reg <- Agg(Energy_reg, unique$region$Development_group, 1)
  GHG_reg <- Agg(GHG_reg, unique$region$Development_group, 1)
  Land_reg <- Agg(Land_reg, unique$region$Development_group, 1)
  Employment_reg <- Agg(Employment_reg, unique$region$Development_group, 1)
  VA_reg <- Agg(VA_reg, unique$region$Development_group, 1)
  
  # Aggregate to North and South
  key <- c( "South", "North", "South", "South", "South", "South")
  RME_reg <- Agg(RME_reg, key, 1)
  Energy_reg <- Agg(Energy_reg, key, 1)
  GHG_reg <- Agg(GHG_reg, key, 1)
  Land_reg <- Agg(Land_reg, key, 1)
  Employment_reg <- Agg(Employment_reg, key, 1)
  VA_reg <- Agg(VA_reg, key, 1)
  
  # Merge in one data sheet
  rownames(RME_reg) <- str_c("RME.",rownames(RME_reg))
  rownames(Energy_reg) <- str_c("Energy.",rownames(Energy_reg))
  rownames(GHG_reg) <- str_c("GHG.",rownames(GHG_reg))
  rownames(Land_reg) <- str_c("Land.",rownames(Land_reg))
  rownames(Employment_reg) <- str_c("Employment.",rownames(Employment_reg))
  rownames(VA_reg) <- str_c("VA.",rownames(VA_reg))
  
  dat <- as.data.frame( t( rbind(RME_reg, Energy_reg, GHG_reg, Land_reg, Employment_reg, VA_reg) ) )
  dat["Price.RME.South"] <- dat$VA.South / dat$RME.South
  dat["Price.RME.North"] <- dat$VA.North / dat$RME.North
  dat["Price.Energy.South"] <- dat$VA.South / dat$Energy.South
  dat["Price.Energy.North"] <- dat$VA.North / dat$Energy.North
  dat["Price.GHG.South"] <- dat$VA.South / dat$GHG.South
  dat["Price.GHG.North"] <- dat$VA.North / dat$GHG.North
  dat["Price.Land.South"] <- dat$VA.South / dat$Land.South
  dat["Price.Land.North"] <- dat$VA.North / dat$Land.North
  dat["Price.Employment.South"] <- dat$VA.South / dat$Employment.South
  dat["Price.Employment.North"] <- dat$VA.North / dat$Employment.North
  
  dat["Drain.RME"] <- dat$RME.South * dat$VA.North / dat$RME.North - dat$VA.South
  dat["Drain.Energy"] <- dat$Energy.South * dat$VA.North / dat$Energy.North - dat$VA.South
  dat["Drain.GHG"] <- dat$GHG.South * dat$VA.North / dat$GHG.North - dat$VA.South
  dat["Drain.Land"] <- dat$Land.South * dat$VA.North / dat$Land.North - dat$VA.South
  dat["Drain.Employment"] <- dat$Employment.South * dat$VA.North / dat$Employment.North - dat$VA.South
  
  dat <- cbind(unique$sector, dat)
  
  # Calculate average drain across all stressor variables
  dat[is.na(dat)] <- 0
  dat["Drain.Average"] <- rowMeans(dat[,c("Drain.RME", "Drain.Energy", "Drain.GHG", "Drain.Land", "Drain.Employment")], na.rm=TRUE)
  
  ## Load MRIO variables (final demand, ...) to estimate drain for a unit of final demand
  
  # Y <- fread( str_c(path$storeMRIOModel, year, "_Y.csv" ) )
  # Y <- as.matrix(Y)
  # # Aggregate final demand categories
  # Y <- Agg( Y, labels$parsed$Y$region_name, 2)
  # 
  # L <- fread( str_c(path$storeMRIOModel, year, "_L.csv" ) )
  # L <- as.matrix(L)
  # 
  # # Calculate gross production vector
  # x <- colSums(t(L) * rowSums(Y))
  # 
  # U <- fread( str_c(path$storeMRIOModel, year, "_U.csv" ) )
  # U <- as.matrix(U)
  
  # Create value added extension
  VA <- as.vector( x - colSums(U) )
  VA[VA < 0] <- 0
  
  # Filter and aggregate value added and final demand of target region 
  index_reg <- labels$parsed$Z$index[labels$parsed$Z$region_code == unique$region$Lfd_Nr[unique$region$Region_acronyms == region]]
  
  VA_reg <- VA[index_reg]
  
  Y_reg <- Agg(Y[,unique$region$Lfd_Nr[unique$region$Region_acronyms == region]], labels$parsed$Z$sector_code, 1)
  
  dat_2 <- cbind(unique$sector, "Final.demand" = Y_reg[,1], "Value.added" = VA_reg, "Drain.Average.Based.On.Final.Demand" = dat$Drain.Average)
  dat_2["Final.Demand.Multiplier.For.Drain.Average"] <- dat_2$Drain.Average.Based.On.Final.Demand/dat_2$Final.demand 
  dat_2["Drain.Average.Based.On.Value.Added"] <- dat_2$Final.Demand.Multiplier.For.Drain.Average * dat_2$Value.added
  
  list_of_arrays <- list("final_demand_based" = dat, "value_added_based" = dat_2)
  write.xlsx(list_of_arrays, file = str_c(path$storeResults,"EuE_Sector_",region,"_",year,".xlsx"), overwrite = TRUE )
  
remove(Employment_reg, Energy_reg, FP_globe, FP_reg, GHG_reg, Land_reg, Y_reg,
       RME_reg, dat, dat_2, stressor, summary, Employment_global, Employment_trade,
       Energy_global, Energy_trade, GWP100_global, GWP100_trade, Landuse_global, Landuse_trade,
       RME_global, RME_trade, VA, VA_reg, Valueadded_global, Valueadded_trade, index_reg, key, list_of_arrays)

# years <- 1995:2020
# 
# dat <- cbind(unique$sector, matrix(NA, nrow = 120, ncol = length(years)))
# colnames(dat)[4:ncol(dat)] <- years
# 
# VA_TIME <- Y_TIME <- VA_Drain_TIME <- Y_Drain_TIME <- dat
# 
# for(year in years)
# {
#   print(year)
#   tmp <- Ecological_unequal_exchange(year = year, region = "USA")
#   VA_TIME[as.character(year)] <- tmp$value_added_based$Value.added  
#   Y_TIME[as.character(year)] <- tmp$value_added_based$Final.demand
#   VA_Drain_TIME[as.character(year)] <- tmp$value_added_based$Drain.Average.Based.On.Value.Added
#   Y_Drain_TIME[as.character(year)] <- tmp$value_added_based$Drain.Average.Based.On.Final.Demand
# }
# 
# list_of_arrays <- list("value_added" = VA_TIME,
#                        "final_demand" = Y_TIME,
#                        "value_added_drain" = VA_Drain_TIME,
#                        "final_demand_drain" = Y_Drain_TIME )
# 
# write.xlsx(list_of_arrays, file = str_c(path$storeResults,"EuE_Sector_USA_TIMESERIES_1995-2020.xlsx"), overwrite = TRUE )
