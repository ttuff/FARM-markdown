#'Create a phylogeny
#'@param size The size of the phylogeny (number of tips).
#'@param start The tip label (number) used for the start.
#'@export
# The begging
TheOriginOfSpecies <- function(size, start) {
  edge <- matrix(ncol = 4, nrow = (2 * size) - 1)
  edge[1, ] <- c(1, 2, start, 1)
  return(edge)
}

