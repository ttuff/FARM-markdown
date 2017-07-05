## Function controlling the random arisal of a new trait somewhere in space

## Last updated: 5 July 2016
## input is a standardized data hub containing information about the current state of the world and the probabilities of different events taking place.

Arisal <- function(input) {
  P.speciation <- input[[1]]
  P.Arisal <- input[[2]]
  P.diffusion <- input[[3]]
  P.extinction <- input[[4]]
  P.TakeOver <- input[[5]]
  myWorld <- input[[6]]
  mytree <- input[[7]]
  myT <- input[[8]]
  multiplier <- input[[9]]
  nbs <- input[[10]]
  independent <- input[[11]]
  fullworld <- table(myWorld[, 6], exclude = FALSE)
  # Arisal only starts to occur if the world has 80% of occupancy
  if((fullworld[1] / sum(fullworld)) > 0.8 |
     any(myWorld[, 6] == 2, na.rm = TRUE)) {
    trait.nonNA <- !is.na(myWorld[, 6])
    trait.length <- sum(trait.nonNA)
    prob.ar <- numeric(trait.length)
    index.tips <- which(trait.nonNA)
    D <- myWorld[trait.nonNA, 6] == 2
    P.Arisal2 <- P.Arisal[trait.nonNA, , drop = FALSE]
    prob.ar[D] <- P.Arisal2[D, 1] # Prob of
    prob.ar[!D] <- P.Arisal2[!D, 2] # Prob of

    arisal <- runif(trait.length) < prob.ar

    if (any(arisal)) {
      myWorld[index.tips[arisal], 6] <- ifelse(myWorld[index.tips[arisal], 6] == 1, 2, 1)
    }
  }
  output <- list(P.speciation, P.Arisal, P.diffusion, P.extinction, P.TakeOver,
                 myWorld, mytree, myT, multiplier, nbs, independent)

  return(output)
}


