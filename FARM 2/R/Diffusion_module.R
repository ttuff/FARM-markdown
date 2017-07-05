## Function controlling the diffusion of a trait through space

## Last updated: 5 July 2016
## input and output are a standardized data hub containing information about the current state of the world and the probabilities of different events taking place.
## RULE SET: The act of diffusion is when one trait, in one cell, switches the trait of an occupied adjoining cell to match its own but does not alter the phylogenetic tree when it does so.


Diffusion  <- function(input) {
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

  if (sum(P.diffusion, na.rm = TRUE) != 0) {
    trait.nonNA <- !is.na(myWorld[, 6])
    trait.length <- sum(trait.nonNA)
    index.tips <- which(trait.nonNA)

    if (trait.length > 1) { # Only occurs if there is more than 1 societies
      index.tips <- sample(index.tips)
      for (i in index.tips) {
        myHex <- myWorld[i, 1]
        PosTargets <- getTargets(myHex, myWorld, nbs, empty = FALSE)
        PosTargets <- PosTargets[myWorld[PosTargets, 6] != myWorld[i, 6]]
        l.targets <- length(PosTargets)
        if (l.targets > 0) {
          # PosTargets different from me
          if (l.targets > 1) {
            PosTargets <- sample(PosTargets, 1)
          }
          source.dom <- myWorld[i, 6] == 2
          prob.dif <- numeric(1)
          prob.dif[!source.dom] <- P.diffusion[1, 2] # Prob of
          prob.dif[source.dom] <- P.diffusion[2, 1] # Prob of
          # HERE DIVIDE THE PROBABILITY BY THE ENV MATCH (NOW SET TO 0)
          prob.dif <- ifelse(myWorld[PosTargets, 6] == myWorld[PosTargets, 7] &
                               myWorld[PosTargets, 6] == 2,
                             0, prob.dif)
          prob.dif <- ifelse(myWorld[i, 6] != myWorld[PosTargets, 7] &
                               myWorld[i, 6] == 2,
                             0, prob.dif)

          if (prob.dif > runif(1)) {
            myWorld[PosTargets, 6] <- myWorld[i, 6]
          }
        }
      }
    }
  }
  output <- list(P.speciation, P.Arisal, P.diffusion, P.extinction, P.TakeOver,
                 myWorld, mytree, myT, multiplier, nbs, independent)
  return(output)
}
