# This script caculates all footprint results including bilateral trade

# year <- 2015

calculate_all_footprints_FromTo <- function(year)
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
  result <- data.frame("From_RegionCode" = rep( unique$region$Lfd_Nr, nreg),
                       "From_RegionAcronym" = rep( unique$region$Region_acronyms, nreg),
                       "From_RegionName" = rep( unique$region$Region_names, nreg),
                       "From_WorldRegion" = rep( unique$region$World_region, nreg),
                       "From_IncomeGroup" = rep( unique$region$Income_group, nreg),
                       "From_DevelopmentGroup" = rep( unique$region$Development_group, nreg),
                       "To_RegionCode" = rep( unique$region$Lfd_Nr, each = nreg ),   
                       "To_RegionAcronym" = rep( unique$region$Region_acronyms, each = nreg ),   
                       "To_RegionName" = rep( unique$region$Region_names, each = nreg ),   
                       "To_WorldRegion" = rep( unique$region$World_region, each = nreg),
                       "To_IncomeGroup" = rep( unique$region$Income_group, each = nreg),
                       "To_DevelopmentGroup" = rep( unique$region$Development_group, each = nreg),
                       "stressor" = rep(stressor$name, each = (nreg*nreg) ),
                       "unit" = rep(stressor$unit, each = (nreg*nreg) ),
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
    print(str_c("Calculating embodied flows of ",stressor$name[s], " (",s,"/",nrow(stressor),")") )
    
    # Multipliers
    MP <- L * Q[,s]
    
    # Aggregate source sectors of regions to speed up matrix multiplication
    MP <- Agg(MP, labels$parsed$Z$region_code, 1)
    
    # Footprints and region-by-region embodied flows for stressor s 
    FP <- MP %*% Y
    
    # Write into results data frame
    result$value[result$stressor == stressor$name[s]] <- c(FP)
  }
  
  # Remove zero values and write results to file
  result <- result[result$value != 0,]
  
  fwrite( result, str_c(path$storeResults,year,"_AllStressors_FromTo.csv") )

}
