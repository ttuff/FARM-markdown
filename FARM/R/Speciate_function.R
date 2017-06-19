# Specitaion function
speciate <- function(myT, Parent, PosTargets, myWorld,
                     mytree, BL) {
  # Create descendant lineage
  if (length(PosTargets) > 1) {
    PosTargets <- sample(PosTargets, 1)
  }
  
  # Add a bifurcation to the node that used to be the parent
  mytree <- NewTip(mytree, Parent, PosTargets, BL)  

  # keep track of this for confirmation
  myWorld[PosTargets, 4] <- Parent
  myWorld[PosTargets, 5] <- myT
  myWorld[PosTargets, 6] <- myWorld[Parent, 6]
  
  
  return(list("myWorld" = myWorld, "mytree" = mytree))
}