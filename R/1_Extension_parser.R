# This script parses the extensions for the time series and computes summary files
# Makes sure to have unzipped the folders

# The first loop computes the stressors of the inter-industry matrix. The second loop further down
# reads the direct emissions of households 

# Get concordance for material groups
conco_mat <- read.xlsx("./input/concordance_material_groups.xlsx")

TIME <- 1990:2028

# year <- 1990
for(year in TIME)
{
  print( str_c("Computing extension for ",year) )
 
  # folder for respective year
  folder <- str_c(path$rawMRIO,
                  "GLORIA_MRIO_Loop059_part_III_satelliteaccounts/",
                  "GLORIA_SatelliteAccounts_059_", year, "/")
  
  # Get names of files in extension folder
  files <- list.files(folder)
  
  # Select file name that belongs to the inter-industry matrix 
  i <- grep('TQ', files)
  
  # Read raw matrix, transform to matrix and select industries
  Q <- fread( str_c(folder, files[i]) )
  
  Q <- as.matrix(Q)
  Q <- Q[,indices$ind]
  
  # Select GWP100 vector (kilo tonnes CO2eq) from EDGAR database
  j <- unique$extension %>% 
    filter(Sat_head_indicator == "Emissions (EDGAR)", 
           Sat_unit == "kilotonnes CO2-equivalent") %>% 
    pull(Lfd_Nr)
  
  tmp_CO2_EDGAR <- Q[j,]
  
  # Select GWP100 vector (kilo tonnes CO2eq) from OECD database
  j <- unique$extension %>% 
    filter(Sat_head_indicator == "Emissions (OECD)", 
           Sat_unit == "kilotonnes CO2-equivalent") %>% 
    pull(Lfd_Nr)
  
  tmp_CO2_OECD <- Q[j,]
  
  # Read material indices and aggregate to material groups (kilo tons)
  tmp_mat <- Agg(x = Q[conco_mat$Lfd_Nr,], aggkey = conco_mat$material_group, dim = 1)
  
  # Read employment data (ppl)
  j <- unique$extension$Lfd_Nr[ unique$extension$Sat_head_indicator == "Employment" ]
  tmp_empl <- colSums(Q[j,])
  
  # Read land use data (1000 ha)
  j <- unique$extension$Lfd_Nr[ unique$extension$Sat_head_indicator == "Land use" ]
  tmp_land <- colSums(Q[j,])
  
  # Read energy data (Tera Joule)
  j <- unique$extension$Lfd_Nr[ unique$extension$Sat_head_indicator == "Energy" ]
  tmp_energy <- colSums(Q[j,])
  
  # Create single matrix and remove tmp files
  Q <- t( rbind(tmp_mat, tmp_energy, tmp_CO2_EDGAR, tmp_CO2_OECD, tmp_land, tmp_empl) )
  
  colnames_NEW <- c("biomass[t]", "metals[t]", "minerals[t]", "fossilfuels[t]", 
                    "energy[TJ]", "EDGAR_GWP100[kt]", "OECD_GWP100[kt]", "landuse[1000ha]", "employment[ppl]")
  
  colnames(Q) <- colnames_NEW
  
  remove(tmp_mat, tmp_energy, tmp_CO2_EDGAR, tmp_CO2_OECD, tmp_land, tmp_empl)
  
  # Create overview/summary
  
  tmp <- as.data.table( Agg(Q, labels$parsed$Z$region_code, dim = 1) ) %>%
    add_column(year = year,
               region = unique$region$Region_names) %>%
    pivot_longer(names_to = "stressor", col = all_of(colnames_NEW))
     
    
  if(year == 1990) overview <- tmp
  if(year > 1990) overview <- rbind(overview, tmp)
  
  # Write extension matrix to MRIO mopdel folder
  fwrite( Q, str_c(path$storeMRIOModel,year,"_Q.csv") )
}

# Write extension matrix to MRIO mopdel folder
fwrite( overview, str_c(path$storeResults, "Z_extension_timeseries_1990-2028_CHECK.csv") )

remove(overview, tmp, conco_mat, i, j, colnames_NEW, Q, files, folder)



# Second loop for reading direct emissions of households 
for(year in TIME)
{
  print( str_c("Computing household GHG extension for ",year) )
  
  # folder for respective year
  folder <- str_c(path$rawMRIO,
                  "GLORIA_MRIO_Loop059_part_III_satelliteaccounts/",
                  "GLORIA_SatelliteAccounts_059_", year, "/")
  
  # Get names of files in extension folder
  files <- list.files(folder)
  
  # Select file name that belongs to the inter-industry matrix 
  i <- grep('YQ', files)
  
  # Read raw matrix, transform to matrix and select industries
  Q <- fread( str_c(folder, files[i]) )
  Q <- as.matrix(Q)
  
  # Select GWP100 vector (kilo tonnes CO2eq) from EDGAR database
  j <- unique$extension %>% 
    filter(Sat_head_indicator == "Emissions (EDGAR)", 
           Sat_unit == "kilotonnes CO2-equivalent") %>% 
    pull(Lfd_Nr)
  
  tmp_CO2_EDGAR <- Q[j,]
  
  # Select GWP100 vector (kilo tonnes CO2eq) from OECD database
  j <- unique$extension %>% 
    filter(Sat_head_indicator == "Emissions (OECD)", 
           Sat_unit == "kilotonnes CO2-equivalent") %>% 
    pull(Lfd_Nr)
  
  tmp_CO2_OECD <- Q[j,]
  
  tmp <- data.frame('EDGAR_in_kt' = tmp_CO2_EDGAR,
                    'OECD__in_kt' = tmp_CO2_OECD)
  
  # Write extension matrix to MRIO mopdel folder
  fwrite( tmp, str_c(path$storeMRIOModel,year,"_Q_HH_GWP100.csv") )
}

remove(Q, tmp, TIME, files, folder, i, j, tmp_CO2_EDGAR, tmp_CO2_OECD, year)


