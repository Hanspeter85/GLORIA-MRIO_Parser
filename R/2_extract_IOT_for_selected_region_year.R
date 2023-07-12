## Extract domestic table of selected country and year

# Select region and year
year <- 2015
region <- 157 

## Load MRIO model                                          
Y <- fread( str_c(path$storeMRIOModel, year, "_Y.csv" ) )
Y <- as.matrix(Y)

U <- fread( str_c(path$storeMRIOModel, year, "_U.csv" ) )
U <- as.matrix(U)

i <- labels$parsed$Z %>% filter(region_code == region) %>% pull(index)
j <- ( 1 + (region-1) * 6 ):( region * 6  )

Z_US <- U[i,i]
Z_US <- round(Z_US/1000, 2)
colnames(Z_US) <- rownames(Z_US) <- unique$sector$Sector_names

Y_US <- Y[i,j]
colnames(Y_US) <- unique$finaldemand$Final_demand_names
Y_US <- round(Y_US/1000, 2)

sum(Y_US)
sum(Z_US)

IO_US <- cbind( Z_US, Y_US)

# Next, aggregate the tables to one world
Z_globe <- Agg(U, labels$parsed$Z$sector_code, 1)
Z_globe <- Agg(Z_globe, labels$parsed$Z$sector_code, 2)
Z_globe <- round(Z_globe/1000, 2)
colnames(Z_globe) <- rownames(Z_globe) <- unique$sector$Sector_names

Y_globe <- Agg(Y, labels$parsed$Z$sector_code, 1)
Y_globe <- Agg(Y_globe, labels$parsed$Y$sector_code, 2)
colnames(Y_globe) <- unique$finaldemand$Final_demand_names
Y_globe <- round(Y_globe/1000, 2)

sum(Z_globe)
sum(Y_globe)

IO_globe <- cbind(Z_globe, Y_globe)

table_list <- list("US_2015" = IO_US,"Global_2015" = IO_globe)
write.xlsx( table_list, file = paste0( "./output/IOTs_",year,"_US_&_Global.xlsx"), rowNames = TRUE )

?write.xlsx

sum(IO_globe)