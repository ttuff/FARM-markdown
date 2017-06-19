# Phylogenetic signal
Dsig <- function(mytree, myWorld) {
  traits <- data.frame("trait" = myWorld[, 6],
                       "tips" = myWorld[, 8])
  compdata <- comparative.data(mytree, traits, 'tips')
  # Phylogenetic signal for binary traits (D of Fritz and Purvis 2010)
  trait <- NULL
  phylo.d(compdata, binvar = trait, permut = 1)$DEstimate
}

