library(openxlsx)
library(dplyr)

# Set path to raw results:
path <- list("GLORIA" = "D:/WU/GLORIA_AllStressors_FromTo_1990-2020/GLORIA_AllStressors_FromTo_1990-2020/",
             "result"= "C:/Users/hpwie/OneDrive/Dokumente/Projects/Juan_Dabo_Paper/" )

# year <- 1990

for(year in 1990:2020)
{
  print(year)
  tmp <- read.csv( paste0( path$GLORIA, year, "_AllStressors_FromTo.csv") )
  tmp <- tmp[tmp$unit == "tons",]
  
  sum(tmp$value)
  
  tmp <- tmp %>% select(To_RegionCode, To_RegionAcronym, To_RegionName, stressor, unit, year, value) %>% 
    group_by(To_RegionCode, To_RegionAcronym, To_RegionName,stressor, unit, year) %>% summarise(value = sum(value))
  
  if(year == 1990) result <- tmp
  if(year != 1990) result <- rbind(result, tmp)
  sum(tmp$value)
}

write.csv( result, paste0( path$result, "GLORIA_Material_Footprints_of_Nations_1990-2020.csv"), row.names = FALSE )
