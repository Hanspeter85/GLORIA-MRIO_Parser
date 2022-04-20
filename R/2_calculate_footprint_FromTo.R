# This script caculates all footprint results including bilateral trade

length(years)

result <- data.frame("FromRegionCode" = rep( unique$region$Lfd_Nr, nreg),
                     "FromRegionAcronym" = rep( unique$region$Region_acronyms, nreg),
                     "FromRegionName" = rep( unique$region$Region_names, nreg),
                     "ToRegionCode" = rep( unique$region$Lfd_Nr, each = nreg),
                     "ToRegionAcronym" = rep( unique$region$Region_acronyms, each = nreg),
                     "ToRegionName" = rep( unique$region$Region_names, each = nreg), 
                     "stressor" = rep( colnames(Q), each =  )

                                          

for(year in years)
{
  Y <- fread( str_c(path$storeMRIOModel, year, "_Y.csv" ) )
  Y <- as.matrix(Y)
  Y <- Agg( Y, labels$parsed$Y$region_name, 2)
  
  L <- fread( str_c(path$storeMRIOModel, year, "_L.csv" ) )
  L <- as.matrix(L)
  
  Q <- fread( str_c(path$storeMRIOModel, year, "_Q.csv" ) )
  Q <- as.matrix(Q)
  
  for(r in 1:nreg)
  {
    
  }
  
}