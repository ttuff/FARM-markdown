## Function controlling the extinction of cells

## Last updated: 5 July 2016
## input and output are a standardized data hub containing information about the current state of the world and the probabilities of different events taking place.
## RULE SET:


# Extinction function
Extinction <- function(input) {
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

  if (sum(P.extinction) != 0) {
    trait.nonNA <- !is.na(myWorld[, 6])
    trait.length <- sum(trait.nonNA)
    if (trait.length > 2) { # Only occurs if there is more than 2 societies
      prob.ext <- numeric(trait.length)
      index.tips <- which(trait.nonNA)
      env.match <- myWorld[trait.nonNA, 7] == myWorld[trait.nonNA, 6]
      domesticator <- myWorld[trait.nonNA, 6] == 2
      prob.ext[env.match & !domesticator] <- P.extinction[1, 1] # Prob of
      prob.ext[env.match & domesticator] <- P.extinction[2, 2] # Prob of
      prob.ext[!env.match & domesticator] <- P.extinction[2, 1] # Prob of
      prob.ext[!env.match & !domesticator] <- P.extinction[1, 2]# Prob of

      extinction <- runif(trait.length) < prob.ext
      survivors <- (trait.length - sum(extinction))
      if(survivors <= 1) {
        stop("One or less survivors, World extinction!!!")
      }
      if (any(extinction)) {
        temp <- extinct(mytree, index.tips[extinction], myWorld)
        mytree <- temp$mytree
        myWorld <- temp$myWorld
      }
    }
  }
  output <- list(P.speciation, P.Arisal, P.diffusion, P.extinction, P.TakeOver,
                 myWorld, mytree, myT, multiplier, nbs, independent)
  return(output)
}

