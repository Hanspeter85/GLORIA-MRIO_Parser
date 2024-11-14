# This script caculates all footprint results including bilateral trade

print("Calculating all footprints FromTo")

## Create empty data frame for results
result <- data.frame("From_RegionCode" = rep( unique$region$Lfd_Nr, n$reg),
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
                     "stressor" = rep(colnames(Q), each = (n$reg*n$reg) ),
                     "year" = year,
                     "value" = 0,
                     stringsAsFactors = FALSE )
  
  
  
  
# Loop across stressors and calculate embodied flows
# s = 1
for( s in 1:ncol(Q) )
{
  print(str_c("Calculating embodied flows of ",colnames(Q)[s], " (",s,"/",ncol(Q),")") )
    
  # Multipliers
  MP <- L * Q[,s]
    
  # Aggregate source sectors of regions to speed up matrix multiplication
  MP <- Agg(MP, labels$parsed$Z$region_code, 1)
    
  # Footprints and region-by-region embodied flows for stressor s 
  FP <- MP %*% Y
    
  # Write into results data frame
  result$value[result$stressor == colnames(Q)[s]] <- c(FP)
}
  
# Remove zero values and write results to file
result <- result[result$value != 0,]
  
fwrite( result, str_c(path$storeResults,year,"_AllStressors_FromTo.csv") )


remove(result, s)
