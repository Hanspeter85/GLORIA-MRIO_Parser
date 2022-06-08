# This function takes the pre-calculated from-to results for various stressors 
# and produces aggregated time series for selected country groups

# For test purpose
# TIMESERIES <- 1995:2020
# year <- 1995
analysis_timeseries_aggregate <- function(TIMESERIES)
{

  # Loop over time series and import data for further aggregation
  for(year in TIMESERIES)
  {
    tmp <- fread( str_c(path$storeResults,year,"_AllStressors_FromTo.csv") ) %>%
      select(From_DevelopmentGroup, To_DevelopmentGroup, stressor, year, value, unit) %>% 
      group_by(From_DevelopmentGroup, To_DevelopmentGroup, stressor, year, unit) %>% 
      summarise(value = sum(value))
    
    if(year == TIMESERIES[1]) dat_agg <- tmp
    if(year != TIMESERIES[1]) dat_agg <- rbind(dat_agg, tmp)
  }
  
    
  # Write aggregated results to folder
  fwrite( dat_agg, str_c(path$storeResults,TIMESERIES[1],"-",TIMESERIES[length(TIMESERIES)],"_AllStressors_FromTo_DevelopmentGroups.csv") )
    
}
