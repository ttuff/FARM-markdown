# Functions to make the code cleaner and less repetitive


#==================================================================
# Function to make the parameters matrix automatic
parameters <- function(prob1, prob2, prob3, prob4, colname1, colname2,
                       rowname1, rowname2) {
  result <- matrix(c(prob1, prob2, prob3, prob4), 2, 2, byrow = TRUE)
  colnames(result) <- c(colname1, colname2)
  rownames(result) <- c(rowname1, rowname2)
  return(result)
}
# Example
# parameters(0.05, 0, 0, 0.05, "test1", "test2", "test3", "test4")

#==================================================================
# Function to select targets for difusion and takeover
getTargets <- function(cellID, myWorld, empty, nbs, traits = FALSE) {
  # empty if TRUE will keep targets with no trait,
  #       if false will keep only targets with traits
  # nbs a neihbor class object
  AllTargets <-  nbs[cellID, ]
  AllTargets <-  AllTargets[!is.na(AllTargets)]
  # Figure out which of the neighboring cells are good options for this context
  if (empty) {
    PosTargets <- AllTargets[is.na(myWorld[AllTargets, 6])]
  }
  if (!empty) {
    PosTargets <- AllTargets[!is.na(myWorld[AllTargets, 6])]
  }
  if (length(PosTargets) == 0) {
    PosTargets <- NULL
  }
  if (traits) {
    PosTargets <- myWorld[PosTargets, 6]
  }
  return(PosTargets)
}

#==================================================================
# Function to remove the species from the world and the
# phylogenetic tree
extinct <- function(mytree, remove, myWorld) {
  mytree <- DropTip(mytree, remove)
  myWorld[remove, 4:6] <- NA
  return(list("mytree" = mytree, "myWorld" = myWorld))
}
