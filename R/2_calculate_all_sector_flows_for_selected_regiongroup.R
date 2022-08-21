# This script caculates all footprint results including bilateral trade

# year <- 2018
# region <- "LDC"
calculate_all_sector_flows_for_selected_regiongroup <- function(year, region)
{
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
  

  ## Load MRIO model                                          
  Y <- fread( str_c(path$storeMRIOModel, year, "_Y.csv" ) )
  Y <- as.matrix(Y)
  # Aggregate final demand categories
  Y <- Agg( Y, labels$parsed$Y$region_name, 2)
  
  # Read region indices of region group
  reg_sel <- unique$region$Lfd_Nr[unique$region$Development_group == region]
  
  # Load inverse  
  L <- fread( str_c(path$storeMRIOModel, year, "_L.csv" ) )
  L <- as.matrix(L)
  
  # Calculate gross production vector
  x <- colSums(t(L) * rowSums(Y))
  
  U <- fread( str_c(path$storeMRIOModel, year, "_U.csv" ) )
  U <- as.matrix(U)
  
  Q <- fread( str_c(path$storeMRIOModel, year, "_Q.csv" ) )
  Q <- as.matrix(Q)
  
  # Create value added extension
  tmp <- as.vector( x - colSums(U) )
  tmp[tmp < 0] <- 0
  Q <- cbind(Q, "valueadded" = tmp)
  
  # Calculate direct intensities
  Q <- Q/x
  Q[is.na(Q)] <- 0  
  Q[is.infinite(Q)] <- 0
  
  # Loop across stressors and calculate embodied flows
  for( s in 1:nrow(stressor) )
  {
    ## 1. Calculate flows associated with consumption (footprint)
    print(str_c("Calculating embodied flows of ",stressor$name[s], " (",s,"/",nrow(stressor),") serving consumption in ", region) )
    
    # Multipliers
    MP <- L * Q[,s]
    
    # Aggregate source sectors of regions to speed up matrix multiplication
    MP <- Agg(MP, labels$parsed$Z$region_code, 1)
    
    # Footprints and embodied flows for stressor s of selected region  
    FP <- t( t(MP) * rowSums(Y[, reg_sel]) )
    
    # Aggregate across sectors
    FP <- Agg(FP, labels$parsed$Z$sector_name, 2)
    
    # Melt data frame from wide to long and extend with additional information (labels, ...)
    tmp <- cbind(unique$region, FP)
    tmp <- melt(tmp, id.vars = colnames(unique$region))
    colnames(tmp)[7] <- "To_Sector_names" 
    tmp <- left_join(tmp, unique$sector[,2:3], by = c("To_Sector_names" = "Sector_names"))
    tmp["stressor"] <- stressor$name[s]
    tmp["unit"] <- stressor$unit[s]
    tmp["year"] <- year
    tmp["To_Region_Group"] <- region
    colnames(tmp)[1:6] <- str_c("From_",colnames(tmp)[1:6]) 
    tmp <- tmp[,c(1:7,9,13,10:12,8)]
    colnames(tmp)[8] <- "To_Sector_group"
    
    if(s == 1) result_consumption <- tmp
    if(s > 1) result_consumption <- rbind(result_consumption, tmp)
    
    
    ## 1. Calculate flows associated with production-side
    print(str_c("Calculating embodied flows of ",stressor$name[s], " (",s,"/",nrow(stressor),") originating in ", region) )
    
    # Multipliers
    MP <- L * Q[,s]
    
    # Select and aggregate source sectors of target regions to speed up matrix multiplication
    sec_sel <- labels$parsed$Z$index[ labels$parsed$Z$region_code %in% reg_sel]
    MP <- MP[ sec_sel,]
    MP <- Agg(MP, labels$parsed$Z$sector_name[sec_sel], 1)
    
    # Footprints and embodied flows originating from selected region  
    FP <- MP %*% Y
    
    # Melt data frame from wide to long and extend with additional information (labels, ...)
    tmp <- cbind(unique$region, t(FP) )
    tmp <- melt(tmp, id.vars = colnames(unique$region))
    
    colnames(tmp)[7] <- "From_Sector_names" 
    tmp <- left_join(tmp, unique$sector[,2:3], by = c("From_Sector_names" = "Sector_names"))
    tmp["stressor"] <- stressor$name[s]
    tmp["unit"] <- stressor$unit[s]
    tmp["year"] <- year
    tmp["From_Region_Group"] <- region
    colnames(tmp)[1:6] <- str_c("To_",colnames(tmp)[1:6]) 
    tmp <- tmp[,c(1:7,9,13,10:12,8)]
    colnames(tmp)[8] <- "From_Sector_group"
    
    if(s == 1) result_production <- tmp
    if(s > 1) result_production <- rbind(result_production, tmp)
    
  }
  
  # Remove zero values and write results to file
  result_production <- result_production[result_production$value != 0,]
  result_consumption <- result_consumption[result_consumption$value != 0,]
  
  list_of_arrays <- list("production-side" = result_production, "consumption-side" = result_consumption)
  
  write.xlsx( list_of_arrays, str_c(path$storeResults,year,"_AllStressors_of_Sectors_in_",region,".xlsx") )
  
}
