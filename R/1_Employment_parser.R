# This script parses the extensions for the time series and computes summary files
# Makes sure to have unzipped the folders

# The first loop computes the stressors of the inter-industry matrix. The second loop further down
# reads the direct emissions of households 


TIME <- 1990:2028

# year <- 1990
for(year in TIME)
{
  print( str_c("Employment extension for ",year) )
 
  # folder for respective year
  folder <- str_c(path$rawExtension,
                  "GLORIA_SatelliteAccounts_059_", year, "/")
  
  # Get names of files in extension folder
  files <- list.files(folder)
  
  # Select file name that belongs to the inter-industry matrix 
  i <- grep('TQ', files)
  
  # Read raw matrix, transform to matrix and select industries
  Q <- fread( str_c(folder, files[i]) )
  
  Q <- as.matrix(Q)
  Q <- Q[,indices$ind]
  
  # Read employment data (ppl)
  j <- unique$extension$Lfd_Nr[ unique$extension$Sat_head_indicator == "Employment" ]
  tmp_empl <- t(Q[j,]) |> as.data.frame()
  colnames(tmp_empl) <- unique$extension$Sat_indicator[ unique$extension$Sat_head_indicator == "Employment" ]
  
  # Read employment data (ppl)
  j <- unique$extension$Lfd_Nr[ unique$extension$Sat_head_indicator == "Employment skill" ]
  tmp_empl_skill <- t(Q[j,]) |> as.data.frame()
  colnames(tmp_empl_skill) <- c("Skill_high","Skill_middle","Skill_low")
  
  clean <- labels$parsed$Z %>% 
    mutate(year = year) %>% 
    bind_cols(tmp_empl, tmp_empl_skill) %>% 
    pivot_longer(cols = c("Female","Male","Skill_high","Skill_middle","Skill_low"),
                 names_to = "employment category")
  
  sum(clean$value)
  if(year == 1990) overview <- clean
  if(year > 1990) overview <- rbind(overview, clean)
  
  # Write extension matrix to MRIO mopdel folder
  fwrite( clean, str_c(path$storeMRIOModel,year,"_Employment.csv") )
}

sum(overview$value)
unique(overview$year)

overview_agg <- overview %>% 
  filter(`employment category` %in% c("Male","Female")) %>% 
  group_by(year, region_code, region_name, sector_code, sector_name) %>% 
  summarise(value = sum(value))

# Write extension matrix to MRIO mopdel folder
fwrite( overview_agg, str_c(path$storeMRIOModel, "GLORIA_employment_female_male_aggregated_1990_2028.csv") )




# Second loop for reading direct emissions of households 
for(year in TIME)
{
  print( str_c("Computing household GHG extension for ",year) )
  
  # folder for respective year
  folder <- str_c(path$rawExtension,
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

remove(Q, tmp, TIME, files, folder, i, j, tmp_CO2_EDGAR, tmp_CO2_OECD, year, conco_mat, overview, colnames_NEW)


