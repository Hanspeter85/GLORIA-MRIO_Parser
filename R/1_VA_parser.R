# This function loads the raw tables and creates the MRIO variables for the calculation
# It stores the data as csv in the output folder. To run the function it needs the labels. 
# Parsing tables for one year can take a lot of time. Makes sure to unzip the folders beforehand.


TIME <- 1990:2028

# year <- 1990

for(year in TIME)
{
  print( str_c("Computing VA block for ",year," at ",Sys.time() ) )

    # Get filenames for respective year
    files <- list.files(path = path$rawMRIO,
                        pattern = str_c("Results_",year),
                        ignore.case = T)
  
    # Get index and filename of transaction matrix in basic prices
    i <- grep(str_c("V-Results_",year,"_059_Markup001"), files)
    
    # Load transaction matrix in basic prices
    raw <- fread( str_c(path$rawMRIO, files[i]) )
    
    # Indices of industries
    index <- labels$T %>% filter(entity_code == 1) %>% pull(index)
    
    # Aggregate rows into 6 value added categories
    raw_agg <- Agg( as.matrix( raw )[,index], rep(1:6, 164),1)
    
    key <- str_c(labels$parsed$Z$region_code, "_", labels$parsed$Z$sector_code)
    
    colnames(raw_agg) <- key
    
    clean <- as.data.frame(raw_agg) %>% 
      mutate(ValueAdded_name = unique$FDVA$Value_added_names,
             ValueAdded_code = unique$FDVA$Lfd_Nr) %>% 
      pivot_longer(cols = all_of(key),
                   names_to = "col_key") %>% 
      separate(col = col_key,
               into = c("region_code","sector_code"),
               sep = "_") %>% 
      mutate(region_code = as.numeric(region_code),
             sector_code = as.numeric(sector_code)) %>% 
      left_join(unique$region, by = c("region_code" = "Lfd_Nr")) %>% 
      left_join(unique$sector, by = c("sector_code" = "Lfd_Nr")) %>% 
      select(ValueAdded_code, ValueAdded_name, region_code, Region_names,Region_acronyms, 
             World_region, Income_group, Development_group, sector_code, Sector_names, Sector_group, value)
    
    fwrite( clean, str_c(path$storeMRIOModel,year,"_VA.csv") )
    
    print("Sum of value added block")
    print(sum(clean$value))
}

# Compile aggregated VA data set
# year <- 1990
for(year in TIME)
{
  print(year)
  
  tmp <- fread(str_c(path$storeMRIOModel,year,"_VA.csv")) %>% 
    mutate(year = year) %>% 
    group_by(region_code,Region_names, sector_code, Sector_names, year) %>% 
    summarise(value = sum(value), .groups = 'drop')
    
  if(year == 1990)
  {
    df <- tmp
  }else
  {
    df <- df %>% 
      bind_rows(tmp)
  }
}

fwrite(df, str_c(path$storeMRIOModel, "GLORIA_value_added_aggregated_1990_2028.csv"))

remove(TIME, year)