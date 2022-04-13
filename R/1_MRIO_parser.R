# This function loads the raw tables and creates the MRIO variables for the calculation
# It stores the data as csv in the output folder. To run the function it needs the labels. 
# Parsing tables for one year can take a lot of time. Makes sure to unzip the folders beforehand.

MRIO_parser <- function(year)
{
  # Read transaction matrix
  T <- fread( str_c(path$rawMRIO, year, "/", 
                    filename$PreMRIO, "T", filename$mid, year, filename$post) )
  
  # Read final demand matrix
  Y_raw <- fread( str_c(path$rawMRIO, year, "/", 
                        filename$PreMRIO, "Y", filename$mid, year, filename$post) )
  
  # Transform to matrix format
  T <- as.matrix(T)
  Y_raw <- as.matrix(Y_raw)
  
  # Subset matrices to get variables
  S <- T[indices$ind,indices$pro]
  U <- T[indices$pro,indices$ind]
  Y <- Y_raw[indices$pro,]
  
  
  # Set negatives due to stock change sto zero
  Y[Y < 0] <- 0
  
  # Gross production of all industries (x) and products (q)
  q <- rowSums(U) + rowSums(Y)
  q[q == 0] <- 10^-7
  # q <- colSums(S)
  x_old <- rowSums(S)             
  
  D <- t( t(S) / colSums(S) )  # Commodity proportions i.e. market share matrix (ixp)
  
  # Set NaN (due to zero gross output) to zero
  D[is.na(D)] <- 0                
  
  sum(D)
  sum(x)
  sum(x_old)
  
  x <- colSums( t(D) * q )
  x[x == 0] <- 10^-7
  
  # Commodity by industry coefficient matrix
  B <- t(t(U)/x)                  
  
  # Set NaN (due to zero gross output) to zero
  B[is.na(B)] <- 0                
  B[B == Inf] <- 0
  
  # Calculate pro-by-pro technology matrix
  A <- B %*% D
  
  # Set negative and very small values to zero to allow inversion 
  # A[A < 0] <- 0
  
  fwrite( A, str_c(path$storeMRIOModel,year,"_A.csv") )
  fwrite( S, str_c(path$storeMRIOModel,year,"_S.csv") )
  fwrite( U, str_c(path$storeMRIOModel,year,"_U.csv") )
  fwrite( Y, str_c(path$storeMRIOModel,year,"_Y.csv") )
  
  # Create identity matrix
  I <- diag( rep( 1,nrow(A) ) )
  
  # Create inverse
  L <- solve(I - A)
  fwrite( L, str_c(path$storeMRIOModel,year,"_L.csv") )
  
}
