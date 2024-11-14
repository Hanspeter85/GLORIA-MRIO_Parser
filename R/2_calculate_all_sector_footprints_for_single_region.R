# This script caculates all footprint results including bilateral trade

print( str_c("Calculating sector footprints of ",region) )

## Create empty data frame for results
result <- data.frame("From_RegionCode" = rep( unique$region$Lfd_Nr, n$sec),
                     "From_RegionAcronym" = rep( unique$region$Region_acronyms, n$sec),
                     "From_RegionName" = rep( unique$region$Region_names, n$sec),
                     "From_WorldRegion" = rep( unique$region$World_region, n$sec),
                     "From_IncomeGroup" = rep( unique$region$Income_group, n$sec),
                     "From_DevelopmentGroup" = rep( unique$region$Development_group, n$sec),
                     "To_RegionCode" = unique$region$Lfd_Nr[unique$region$Region_acronyms == region],   
                     "To_RegionAcronym" =  region,   
                     "To_RegionName" = unique$region$Region_names[unique$region$Region_acronyms == region],   
                     "To_SectorCode" = rep( unique$sector$Lfd_Nr, each = n$reg ),
                     "To_SectorName" = rep( unique$sector$Sector_names, each = n$reg ),  
                     "stressor" = rep(colnames(Q), each = (n$reg*n$sec) ),
                     "year" = year,
                     "value" = 0,
                     stringsAsFactors = FALSE )
  

# Loop across stressors and calculate embodied flows
# s <- 1
for( s in 1:ncol(Q) )
{
  print(str_c("Calculating embodied flows of ",colnames(Q)[s], " (",s,"/",ncol(Q),") serving consumption in ", region) )
    
  # Multipliers
  MP <- L * Q[,s]
    
  # Aggregate source sectors of regions to speed up matrix multiplication
  MP <- Agg(MP, labels$parsed$Z$region_code, 1)
    
  # Footprints and embodied flows for stressor s of selected region  
  FP <- t( t(MP) * Y[, unique$region$Lfd_Nr[unique$region$Region_acronyms == region] ] )
    
  # Aggregate across sectors
  FP <- Agg(FP, labels$parsed$Z$sector_code, 2)
    
  # Write into results data frame
  result$value[result$stressor == colnames(Q)[s]] <- c(FP)
}
  
# Remove zero values and write results to file
result <- result[result$value != 0,]
  
write.xlsx( result, str_c(path$storeResults,year,"_AllStressors_FromSourceRegions_to_Sectors_in_",region,".xlsx") )
  
remove(result, s)
