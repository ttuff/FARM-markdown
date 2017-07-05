# Speciation function!
SpeciationTakeOver.push <- function(input) {
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

  trait.nonNA <- !is.na(myWorld[, 6])
  trait.length <- sum(trait.nonNA)
  index.tips <- which(trait.nonNA)
  extinct.list <- c()
  if (trait.length > 1) { # Only shuffles if there is more than 1 societies
    index.tips <- sample(index.tips)
  }
  spec <- FALSE
  # Probabililty of speciation
  BL <- (1 / trait.length)
  for (i in index.tips) {
    myT <-  BL + myT # Change time

    if (!i %in% extinct.list) {
      myHex <- myWorld[i, 1]
      PosTargets <- getTargets(myHex, myWorld, nbs, empty = TRUE)
      emptyORtakeover <- is.null(PosTargets)

      # If not null than Speciate going to an empty cell
      if (!emptyORtakeover) {
        temp <- Speciation(myWorld, i, P.speciation,
                           myT, PosTargets, mytree, BL)
        mytree <- temp$mytree
        myWorld <- temp$myWorld
        spec <- sum(spec, temp$spec)
      }

      # If yes go to take over
      if (emptyORtakeover & sum(P.TakeOver) != 0) {
        temp <- TakeOver.push(myWorld, mytree, P.TakeOver,
                              myT, multiplier = multiplier,
                              i, BL, independent, nbs)
        mytree <- temp$mytree
        myWorld <- temp$myWorld
        extinct.list <- c(extinct.list, temp$extinct.list)
        spec <- sum(spec, temp$spec)
      }
    }
  }

  tips <- !is.na(mytree[, 3])
  mytree[tips, 4] <- mytree[tips, 4] + (1 - (BL * spec))
  output <- list(P.speciation, P.Arisal, P.diffusion, P.extinction, P.TakeOver,
                 myWorld, mytree, myT, multiplier, nbs, independent)
  return(output)
}




