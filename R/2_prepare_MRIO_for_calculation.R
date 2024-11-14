

## Load MRIO model                                          
Y <- fread( str_c(path$storeMRIOModel, year, "_Y.csv" ) )
Y <- as.matrix(Y)

# Aggregate final demand categories
Y <- Agg( Y, labels$parsed$Y$region_name, 2)

L <- fread( str_c(path$storeMRIOModel, year, "_L.csv" ) )
L <- as.matrix(L)

# Calculate gross production vector
x <- colSums(t(L) * rowSums(Y))

A <- fread( str_c(path$storeMRIOModel, year, "_A.csv" ) )
A <- as.matrix(A)

U <- t(t(A)*x)

Q <- fread( str_c(path$storeMRIOModel, year, "_Q.csv" ) )
Q <- as.matrix(Q)

# Create value added extension
tmp <- as.vector( x - colSums(U) )
tmp[tmp < 0] <- 0
Q <- cbind(Q, "valueadded[1000USD]" = tmp)

# Calculate direct intensities
Q <- Q/x
Q[is.na(Q)] <- 0  
Q[is.infinite(Q)] <- 0

remove(tmp)