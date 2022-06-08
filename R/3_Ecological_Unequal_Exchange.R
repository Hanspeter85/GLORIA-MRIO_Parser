
# year <- 2018
# region <- "USA"

Ecological_unequal_exchange <- function(year, region)
{
  # Load footprints of specific regions and the world 
  FP_reg <- read.xlsx( str_c(path$storeResults,year,"_AllStressors_FromSourceRegions_to_Sectors_in_",region,".xlsx") ) %>% 
    mutate_if(is.factor, as.character)
  FP_globe <- read.csv(str_c(path$storeResults,year,"_AllStressors_FromTo.csv")) %>% 
    mutate_if(is.factor, as.character)  
  
  # Define stressors for analysis
  stressor <- data.frame("name" = c("biomass", 
                                    "metals", 
                                    "minerals", 
                                    "fossilfuels", 
                                    "energy", 
                                    "GWP100", 
                                    "landuse", 
                                    "employment", 
                                    "valueadded"),
                         "unit" = c("tons", 
                                    "tons", 
                                    "tons", 
                                    "tons", 
                                    "tera joule", 
                                    "kilo tons CO2eq", 
                                    "1000 ha", 
                                    "1000 persons",
                                    "1000 USD"),
                         stringsAsFactors = FALSE)
  
  
  RME_global <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[1:4]] )
  Energy_global <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[9]] )
  GWP100_global <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[6]] )
  Landuse_global <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[7]] )
  Employment_global <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[8]] )
  Valueadded_global <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[9]] )
  
  RME_trade <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[1:4] & FP_globe$From_RegionCode != FP_globe$To_RegionCode  & FP_globe$From_DevelopmentGroup == "Developed.economies"])
  Energy_trade <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[9] & FP_globe$From_RegionCode != FP_globe$To_RegionCode  & FP_globe$From_DevelopmentGroup == "Developed.economies"] )
  GWP100_trade <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[6] & FP_globe$From_RegionCode != FP_globe$To_RegionCode  & FP_globe$From_DevelopmentGroup == "Developed.economies"] )
  Landuse_trade <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[7] & FP_globe$From_RegionCode != FP_globe$To_RegionCode  & FP_globe$From_DevelopmentGroup == "Developed.economies"] )
  Employment_trade <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[8] & FP_globe$From_RegionCode != FP_globe$To_RegionCode  & FP_globe$From_DevelopmentGroup == "Developed.economies"] )
  Valueadded_trade <- sum( FP_globe$value[FP_globe$stressor %in% stressor$name[9] & FP_globe$From_RegionCode != FP_globe$To_RegionCode  & FP_globe$From_DevelopmentGroup == "Developed.economies"] )
  
  # Sum of global production-based accounts
  summary <- data.frame( "stressor" = c("materials", "energy", "GWP100","landuse", "employment", "valueadded"),
                         "unit" = c("tons", "tera joule", "kilo tons CO2eq", "1000 ha", "1000 persons", "1000 USD"),
                         "global_sum" = c(RME_global, Energy_global, GWP100_global, Landuse_global, Employment_global, Valueadded_global),
                         "trade_sum_north_to_south" = c(RME_trade, Energy_trade, GWP100_trade, Landuse_trade, Employment_trade, Valueadded_trade) )
  
  
  # Compiling the function for computing the sector results
  Compile_FP_Source_Region_By_Sector <- function(stressors)
  {
    # Filter stressors
    tmp_orignal <- FP_reg %>% 
      filter( stressor %in% stressors) %>% 
      select(value, To_SectorCode, From_RegionCode)
  
    # Transform from long to wide
    tmp <- as.data.frame( acast(tmp_orignal,From_RegionCode ~ To_SectorCode, sum) )
    
    # which rows and columns are missing:
    row_miss <- setdiff(1:nreg, as.numeric(rownames(tmp)))
    col_miss <- setdiff(1:nsec, as.numeric(colnames(tmp)))
    
    # Insert missing elements in case
    if(length(row_miss) > 0)
    {
      tmp[(nrow(tmp) + length(row_miss)), ] <- 0
      rownames(tmp)[(1+nreg-length(row_miss)):nreg] <- row_miss
    }
    
    if(length(col_miss) > 0)
    {
      tmp[,(ncol(tmp) + length(col_miss))] <- 0
      colnames(tmp)[(1+nsec-length(col_miss)):nsec ] <- col_miss
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
  RME_reg <- Compile_FP_Source_Region_By_Sector(c("biomass", "metals", "minerals", "fossilfuels"))
  Energy_reg <- Compile_FP_Source_Region_By_Sector(c("energy"))
  GHG_reg <- Compile_FP_Source_Region_By_Sector(c("GWP100"))
  Land_reg <- Compile_FP_Source_Region_By_Sector(c("landuse"))
  Employment_reg <- Compile_FP_Source_Region_By_Sector(c("employment"))
  VA_reg <- Compile_FP_Source_Region_By_Sector(c("valueadded"))
  
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
  
  write.xlsx(dat, file = str_c(path$storeResults,"EuE_Sector_",region,"_",year,".xlsx") )
  
  Sector_price_of_RME <- VA_reg/RME_reg
  
  rownames(RME_reg) <- str_c("RME.from.",rownames(RME_reg))
  rownames(VA_reg) <- str_c("VA.from.",rownames(VA_reg))
  rownames(Sector_price_of_RME) <- str_c("Prices.of.",rownames(Sector_price_of_RME))
  
  result <- as.data.frame( t( rbind(RME_reg, VA_reg, Sector_price_of_RME) ) )
  
  # Global average prices
  Global_price_of_RME <-  summary$trade_sum_north_to_south[ summary$stressor == "valueadded"] / summary$trade_sum_north_to_south[ summary$stressor == "materials"]
  
  # Estimate drain from South to North
  result["T"] <- result$RME.from.Global.South * Global_price_of_RME - result$VA.from.Global.South
  
  result[is.na(result)] <- 0
  result[result == Inf] <- 0
  
  colSums(result)
  
  
  
  
  
  
  rownames(test) <- unique$region$Region_names
  sum(tmp)
  
  str(RME_reg)
  nrow(RME_reg)
  
  
  
  test <- as.numeric(rownames(RME_reg))
  reshape2::
  test <- pivot_wider(RME_reg, names_from = To_SectorCode, values_from = value)
  
  ?pivot_wider
  test <- reshape(RME_reg, idvar = "From_RegionCode", timevar = "To_SectorCode", v.names =  "value", direction = "wide")
  
  
  
  ?reshape
  sum(RME_reg$value)
  sum(test[,2:120])
  
  pivot_wider()
  
  tmp <- matrix(0, nrow = nreg, ncol = nsec)
  
  tmp[cbind(RME_reg$From_RegionCode, RME_reg$To_SectorCode)] <- RME_reg$value
  
  sum(tmp)
  &, 
  test <- cast(test, From_RegionCode + To_SectorCode, sum)
  
  reshape(dat1, idvar = "name", timevar = "numbers", direction = "wide")
  
  ?cast
    

}