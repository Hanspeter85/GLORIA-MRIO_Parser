# This script caculates all footprint results including bilateral trade

# As input argument the script/function needs a stressor from the list:
# "biomass"     "metals"      "minerals"    "fossilfuels" "energy"      "GWP100"      "landuse"    
# "employment" 

calculate_footprint_FromTo <- function(stressor, year, region)
{
  print(year)
  
  # Create empty data frame for results
  
  result <- data.frame("FromRegionCode" = rep( unique$region$Lfd_Nr, nreg),
                       "FromRegionAcronym" = rep( unique$region$Region_acronyms, nreg),
                       "FromRegionName" = rep( unique$region$Region_names, nreg),
                       "ToRegionCode" = rep( unique$region$Lfd_Nr, each = nreg),
                       "ToRegionAcronym" = rep( unique$region$Region_acronyms, each = nreg),
                       "ToRegionName" = rep( unique$region$Region_names, each = nreg),
                       "FinalConsumerCode" = rep( unique$region$Lfd_Nr, each = (nreg*nreg) ),   
                       "FinalConsumerAcronym" = rep( unique$region$Region_acronyms, each = (nreg*nreg) ),   
                       "FinalConsumerName" = rep( unique$region$Region_names, each = (nreg*nreg) ),   
                       "stressor" = stressor,
                       "year" = year,
                       "value" = 0,
                       stringsAsFactors = FALSE )
  
                                            
  Y <- fread( str_c(path$storeMRIOModel, year, "_Y.csv" ) )
  Y <- as.matrix(Y)
  Y <- Agg( Y, labels$parsed$Y$region_name, 2)
    
  L <- fread( str_c(path$storeMRIOModel, year, "_L.csv" ) )
  L <- as.matrix(L)
    
  Q <- fread( str_c(path$storeMRIOModel, year, "_Q.csv" ) )
  Q <- as.matrix(Q)
  
  x <- colSums(t(L) * rowSums(Y))
  Q <- Q/x
  Q[is.na(Q)] <- 0  
  Q[is.infinite(Q)] <- 0
  Q <- Q[,colnames(Q) == stressor]
  sum(Q)
  MP <- L * Q
  

  for(r in region)
  {
      print(str_c("Region ",r, " footprint"))
      FP <- t( t(MP) * Y[,r] )
      FP <- Agg(FP, labels$parsed$Z$region_code, 1)
      FP <- Agg(FP, labels$parsed$Z$region_code, 2)
      result$value[result$FinalConsumerCode == r] <- FP
  }
  
  result <- result[result$value != 0,]
  fwrite( result, str_c(path$storeResults,year,"_",stressor,"_FromTo.csv") )

}