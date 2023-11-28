library(openxlsx)
library(dplyr)

# Set path to raw results:
path <- list("GLORIA" = "D:/WU/MRIO/GLORIA_version_55/Results/GLORIA_AllStressors_FromTo_1990-2020/GLORIA_AllStressors_FromTo_1990-2020/",
             "result"= "C:/Users/hpwie/OneDrive/Dokumente/Projects/Juan_Dabo_Paper/",
             "storeMRIOModel" = "D:/WU/MRIO/GLORIA_version_55/Parsed/",
             "rawExtension" = "D:/WU/MRIO/GLORIA_version_55/Raw_Tvy_tables/GLORIA_MRIO_Loop055_part_III_satelliteaccounts/GLORIA_SatelliteAccounts_055_")

# year <- 1990

# The first loop reads the emissions that are embodied in final consumption
for(year in 1990:2020)
{
  print(year)
  
  # Reading embodied emissions from industrial activities
  tmp <- read.csv( paste0( path$GLORIA, year, "_AllStressors_FromTo.csv") )
  tmp <- tmp[tmp$unit == "kilo tons CO2eq",]
  
  tmp <- tmp %>% select(To_RegionCode, To_RegionAcronym, To_RegionName, stressor, unit, year, value) %>% 
    group_by(To_RegionCode, To_RegionAcronym, To_RegionName,stressor, unit, year) %>% summarise(value = sum(value))
  
  tmp <- cbind(tmp, "entity" = "Industry")
  tmp <- tmp[,c(1:5,8,6,7)]
  
  # Reading direct emissions from household activities
  tmp_2 <- read.csv( paste0( path$storeMRIOModel,year,"_Q_HH_GWP100.csv") )
  
  tmp <- rbind(tmp, tmp_2)
  
  if(year == 1990) result <- tmp
  if(year != 1990) result <- rbind(result, tmp)
  sum(tmp$value)
}

write.csv( result, paste0( path$result, "GLORIA_Carbon_Footprints_of_Nations_1990-2020.csv"), row.names = FALSE )





}