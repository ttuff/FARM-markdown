evol.distinct2 <- function(tree, type = c("equal.splits", "fair.proportion"), 
                          scale = FALSE, use.branch.lengths = TRUE) 
{
  type <- match.arg(type)
  if (is.rooted(tree) == FALSE) 
    warning("A rooted phylogeny is required for meaningful output of this function", 
            call. = FALSE)
  if (scale == TRUE) {
    if (is.ultrametric(tree) == TRUE) 
      tree$edge.length <- tree$edge.length/(as.numeric(branching.times(tree)[1]))
    else tree$edge.length <- tree$edge.length/sum(tree$edge.length)
  }
  if (use.branch.lengths == FALSE) 
    tree$edge.length <- rep(1, length(tree$edge.length))
  for (i in 1:length(tree$tip.label)) {
    spp <- tree$tip.label[i]
    nodes <- .get.nodes(tree, spp)
    nodes <- nodes[1:(length(nodes) - 1)]
    internal.brlen <- tree$edge.length[which(tree$edge[, 
                                                       2] %in% nodes)]
    if (length(internal.brlen) != 0) {
      internal.brlen <- internal.brlen * switch(type, equal.splits = sort(rep(0.5, 
                                                                              length(internal.brlen))^c(1:length(internal.brlen))), 
                                                fair.proportion = {
                                                  for (j in 1:length(nodes)) {
                                                    sons <- .node.desc(tree, nodes[j])
                                                    n.descendents <- length(sons$tips)
                                                    if (j == 1) portion <- n.descendents else portion <- c(n.descendents, 
                                                                                                           portion)
                                                  }
                                                  1/portion
                                                })
    }
    ED <- sum(internal.brlen, tree$edge.length[which.edge(tree, 
                                                          spp)])
    if (i == 1) 
      w <- ED
    else w <- c(w, ED)
  }
  return(w)
}
