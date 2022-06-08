# This script caculates all footprint results including bilateral trade

# year <- 2015
# region <- "USA"
calculate_all_sector_footprints_for_single_region <- function(year, region)
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
  
  ## Create empty data frame for results
  result <- data.frame("From_RegionCode" = rep( unique$region$Lfd_Nr, nsec),
                       "From_RegionAcronym" = rep( unique$region$Region_acronyms, nsec),
                       "From_RegionName" = rep( unique$region$Region_names, nsec),
                       "From_WorldRegion" = rep( unique$region$World_region, nsec),
                       "From_IncomeGroup" = rep( unique$region$Income_group, nsec),
                       "From_DevelopmentGroup" = rep( unique$region$Development_group, nsec),
                       "To_RegionCode" = unique$region$Lfd_Nr[unique$region$Region_acronyms == region],   
                       "To_RegionAcronym" =  region,   
                       "To_RegionName" = unique$region$Region_names[unique$region$Region_acronyms == region],   
                       "To_SectorCode" = rep( unique$sector$Lfd_Nr, each = nreg ),
                       "To_SectorName" = rep( unique$sector$Sector_names, each = nreg ),  
                       "stressor" = rep(stressor$name, each = (nreg*nsec) ),
                       "unit" = rep(stressor$unit, each = (nreg*nsec) ),
                       "year" = year,
                       "value" = 0,
                       stringsAsFactors = FALSE )
  
  ## Load MRIO model                                          
  Y <- fread( str_c(path$storeMRIOModel, year, "_Y.csv" ) )
  Y <- as.matrix(Y)
  # Aggregate final demand categories
  Y <- Agg( Y, labels$parsed$Y$region_name, 2)
    
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
    print(str_c("Calculating embodied flows of ",stressor$name[s], " (",s,"/",nrow(stressor),") serving consumption in ", region) )
    
    # Multipliers
    MP <- L * Q[,s]
    
    # Aggregate source sectors of regions to speed up matrix multiplication
    MP <- Agg(MP, labels$parsed$Z$region_code, 1)
    
    # Footprints and embodied flows for stressor s of selected region  
    FP <- t( t(MP) * Y[, unique$region$Lfd_Nr[unique$region$Region_acronyms == region] ] )
    
    # Aggregate across sectors
    FP <- Agg(FP, labels$parsed$Z$sector_code, 2)
    
    # Write into results data frame
    result$value[result$stressor == stressor$name[s]] <- c(FP)
  }
  
  # Remove zero values and write results to file
  result <- result[result$value != 0,]
  
  write.xlsx( result, str_c(path$storeResults,year,"_AllStressors_FromSourceRegions_to_Sectors_in_",region,".xlsx") )
  
}
