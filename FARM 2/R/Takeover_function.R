# Takeover function
TakeOver <- function(myWorld, mytree, P.TakeOver,
                     myT, multiplier = 1.3,
                     i, BL, independent, nbs) {

  ind <- independent < runif(1)
  extinct.list <- NULL
  myHex <- myWorld[i, 1]
  PosTargets <- getTargets(myHex, myWorld, nbs, empty = FALSE)
  source.trait.dom <- myWorld[i, 6] == 2
  target.trait.dom <- myWorld[PosTargets, 6] == 2
  P.TakeOver2 <- P.TakeOver
  # If I am at the rigth place give me more chances to attack!
  # if (myWorld[i, 6] != myWorld[i, 7] | !source.trait.dom &
  #     ind) {
  #   P.TakeOver2 <- P.TakeOver2 / multiplier
  # }

  # How easy is to takeover
  prob.to <- numeric(length(PosTargets))

  prob.to[!source.trait.dom & !target.trait.dom] <- P.TakeOver2[1, 1]
  prob.to[!source.trait.dom & target.trait.dom] <- P.TakeOver2[1, 2]
  prob.to[source.trait.dom & !target.trait.dom] <- P.TakeOver2[2, 1]
  prob.to[source.trait.dom & target.trait.dom] <- P.TakeOver2[2, 2]

  # How good is the env for me
  good <- myWorld[i, 6] == myWorld[PosTargets, 7] & source.trait.dom
  test <- FALSE
  if (any(good)) {
    PosTargets <- PosTargets[good]
    prob.to <- prob.to[good]
    target.trait.dom <- target.trait.dom[good]


    l.pos <- length(PosTargets)
    if (l.pos > 1) {
      choice <- sample(1:l.pos, 1)
      PosTargets <- PosTargets[choice]
      prob.to <- prob.to[choice]
      target.trait.dom <- target.trait.dom[choice]
    }

    # If I am at the rigth place give me more chances to defend!
    # if (myWorld[PosTargets, 6] == myWorld[PosTargets, 7] & target.trait.dom &
    #     ind) {
    #   prob.to <- prob.to / multiplier
    # }
    test <- prob.to > runif(1)
    if (test) {
      extinct.list <- PosTargets
      temp <- sub.TakeOver(mytree, index.tips = PosTargets,
                           myWorld, myT, i, BL)
      mytree <- temp$mytree
      myWorld <- temp$myWorld
    }
  }
  return(list("mytree" = mytree, "myWorld" = myWorld, "extinct.list" = extinct.list,
              "spec" = test))
}



#==================================================================
sub.TakeOver <- function(mytree, index.tips, myWorld, myT,
                         i, BL) { #index.tips = PosTargets
  # eliminate any record of the society that used to occupy the chosen spot
  temp <- extinct(mytree, index.tips, myWorld)
  mytree <- temp$mytree
  myWorld <- temp$myWorld

  # and now occupy this spot with a descendant of the domest society
  Temp <- speciate(myT = myT, Parent = i, index.tips,
                   myWorld = myWorld, mytree = mytree,
                   BL)
  return(Temp)
}
