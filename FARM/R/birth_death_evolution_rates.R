# BirthDeath function
bd <- function(tree) {
  tree$edge.length <- tree$edge.length / max(tree$edge.length) 
  x <- birthdeath(tree)  
  b <- x$para[2] / (1 - x$para[1])
  d <- b - x$para[2]
  c(setNames(c(b, d), c("b", "d")), x$para)
}

