# This script parses the extensions for the time series and computes summary files
# Makes sure to unzip the folders beforehand.

for(year in years)
{
  print( str_c("Computing extension for ",year) )
  
  # Read processing date of files of specific year
  date <- substr( list.files( str_c(path$rawExtension, year, "/") )[1], 1, 8)
  
  # Read raw matrix, transform to matrix and select industries
  Q <- fread( str_c(path$rawExtension, 
                    year, "/", date, 
                    filename$pre, "TQ", 
                    filename$mid, year, filename$post) )
  
  Q <- as.matrix(Q)
  Q <- Q[,indices$ind]
  
  # Compile GWP100 vector (kilo tons CO2eq)
  tmp_CO2 <- colSums(Q * unique$extension$CO2_eq)
  
  # Read material indices and aggregate to material groups (kilo tons)
  i <- unique$extension$Lfd_Nr[ !is.na(unique$extension$materials) ]
  tmp_mat <- Agg(x = Q[i,], aggkey = unique$extension$materials[i], dim = 1)
  
  # Read employment data (1000 persons)
  i <- unique$extension$Lfd_Nr[ unique$extension$Sat_head_indicator == "Employment" ]
  tmp_empl <- colSums(Q[i,])
  
  # Read land use data (1000 ha)
  i <- unique$extension$Lfd_Nr[ unique$extension$Sat_head_indicator == "Land use" ]
  tmp_land <- colSums(Q[i,])
  
  # Read energy data (Tera Joule)
  i <- unique$extension$Lfd_Nr[ unique$extension$Sat_head_indicator == "Energy" ]
  tmp_energy <- colSums(Q[i,])
  
  # Create single matrix and remove tmp files
  Q <- t( rbind(tmp_mat, tmp_energy, tmp_CO2, tmp_land, tmp_empl) )
  colnames(Q) <- c("biomass", "metals", "minerals", "fossilfuels", "energy", "GWP100", "landuse", "employment")
  remove(tmp_mat, tmp_energy, tmp_CO2, tmp_land, tmp_empl)
  
  # Create overview/summary
  tmp <- Agg(x = Q, aggkey = labels$parsed$Z$region_name, dim = 1)
  tmp <- cbind( reshape2::melt(tmp), "year" = year)
  
  if(year == 1990) overview <- tmp
  if(year > 1990) overview <- rbind(overview, tmp)
  
  # Write extension matrix to MRIO mopdel folder
  fwrite( Q, str_c(path$storeMRIOModel,year,"_Q.csv") )
}

# Write extension matrix to MRIO mopdel folder
fwrite( overview, str_c(path$storeResults, "Extension_check_timeseries_1990to2020.csv") )

# Clean summary and write to folder
colnames(overview) <- c("region", "stressor", "value", "year")

units <- data.frame("stressor" = c("biomass", "metals", "minerals", "fossilfuels", "energy", "GWP100", "landuse", "employment"),
                    "unit" = c("kilo tons", "kilo tons", "kilo tons", "kilo tons", "tera joule", "kilo tons CO2eq", "1000 ha", "1000 persons") )

overview <- left_join(overview, units, by = "stressor")



remove(overview, tmp)
