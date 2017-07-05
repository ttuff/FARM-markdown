#'Transform into a matrix
#'@param edge A matrix generated from the other functions
#'@export
makePhy <- function(edge) {
  # target0 <- which(edge[, 1] == edge[1, 2])
  # edge[target0, 4] <- edge[target0, 4] + edge[1, 4]
  edge <- edge[-1, ]

  edge2 <- edge[!is.na(edge[, 1]), ]
  Edge <- edge2[, 1:2]
  Edge2 <- Edge
  tips <- !Edge[, 2] %in% Edge[, 1]
  Ntips <- sum(tips)
  Nnodes <- Ntips - 1
  Edge2[tips, 2] <- 1:Ntips
  nodes <- unique(Edge[, 1]) # including the root
  newnodes <- (Ntips + 1):(Ntips + Nnodes)
  for (i in 1:(Nnodes + 1)) {
    Edge2[Edge == nodes[i]] <- newnodes[i]
  }

  labels <- paste0("t", na.omit(edge2[, 3]))
  mytree <- list("edge" = Edge2, "tip.label" = labels,
                 "edge.length" = edge2[, 4], "Nnode" =  Nnodes)
  attributes(mytree) <- list(names = c("edge", "tip.label", "edge.length",
                                       "Nnode"),
                             class = "phylo")
  mytree <- reorder.phylo(mytree, order = "cladewise")
  return(mytree)
  return(edge)
}
