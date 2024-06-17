# This function loads the raw tables and creates the MRIO variables for the calculation
# It stores the data as csv in the output folder. To run the function it needs the labels. 
# Parsing tables for one year can take a lot of time. Makes sure to unzip the folders beforehand.


TIME <- 1990:2028

# year <- 1990
for(year in TIME)
{
  print( str_c("Computing MRIO for ",year," at ",Sys.time() ) )

  # Get filenames for respective year
  files <- list.files(path = path$rawMRIO,
                      pattern = str_c("Results_",year),
                      ignore.case = T)

  # Get index and filename of transaction matrix in basic prices
  i <- grep(str_c("T-Results_",year,"_059_Markup001"), files)
  
  # Load transaction matrix in basic prices
  T <- fread( str_c(path$rawMRIO, files[i]) )
  
  # Get index and filename of final demand matrix in basic prices
  i <- grep(str_c("Y-Results_",year,"_059_Markup001"), files)
  
  # Load transaction matrix in basic prices
  Y_raw <- fread( str_c(path$rawMRIO, files[i]) )
  
  
  # Transform to matrix format
  T <- as.matrix(T)
  Y_raw <- as.matrix(Y_raw)
  
  # Subset matrices to get variables
  # S <- T[indices$ind,indices$pro]
  U <- T[indices$pro,indices$ind]
  Y <- Y_raw[indices$pro,]
  
  
  # Set negatives due to stock change sto zero
  # Y[Y < 0] <- 0
  
  # Gross production of all industries (x) and products (q)
  # q <- rowSums(U) + rowSums(Y)
  # q[q == 0] <- 10^-7
  
  # D <- t( t(S) / colSums(S) ) # Commodity proportions i.e. market share matrix (ixp)
  # D[is.na(D)] <- 0            # Set NaN (due to zero gross output) to zero
  
  x <- colSums(U)
  # x <- colSums( t(D) * q )  # If x is calculated directly from S, this results in negative values in L
  # x[x == 0] <- 10^-7
  
  # Commodity by industry coefficient matrix
  A <- t(t(U)/x)                  
  
  # Set NaN (due to zero gross output) to zero
  A[is.na(A)] <- 0                
  A[A == Inf] <- 0
  
  # Calculate pro-by-pro technology matrix
  # A <- B %*% D
  
  # Set negative and very small values to zero to allow inversion 
  # A[A < 0] <- 0
  
  fwrite( A, str_c(path$storeMRIOModel,year,"_A.csv") )
  # fwrite( S, str_c(path$storeMRIOModel,year,"_S.csv") )
  fwrite( U, str_c(path$storeMRIOModel,year,"_U.csv") )
  fwrite( Y, str_c(path$storeMRIOModel,year,"_Y.csv") )
  
  # Create identity matrix
  # I <- diag( rep( 1,nrow(A) ) )
  
  # Set diagonal values that are zero to small number 
  # diag(A)[diag(A) == 0] <- 10^-7
  
  # Create inverse
  # L <- solve(I - A)
  # fwrite( L, str_c(path$storeMRIOModel,year,"_L.csv") )
  
  # print("Minimum value in L")
  # print(min(L))
  # print("Sum of L")
  # print(sum(L))
  # print("Sum of orginal production")
  # print( sum(x) )
  # print("Sum of production when using L")
  # print( sum(t(L) * rowSums(Y) ) )
  
}


remove(TIME, year)