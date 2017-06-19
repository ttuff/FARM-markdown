#==================================================================
# Build the world based on real coordinates and real conditions.
# coords: a matrix of longitude and latitude in this order.
# conditions: a vector indicating how good the conditions are for agriculture

BuildWorld <- function(coords, conditions) {
  n <- nrow(coords)
  myWorld <- matrix(ncol = 8, nrow = n)
  myWorld[, 2] <- coords[, 1]
  myWorld[, 3] <- coords[, 2]
  myWorld[, 8] <- 1:n
  myWorld[, 1] <- 1:n
  myWorld[, 7] <- conditions
  colnames(myWorld) <- c('cellID', 'Longitude', 'Latitude', 
                         "Parent", "BirthT", "Trait", 
                         "Environment", "TipLabel")
  return(myWorld)
}