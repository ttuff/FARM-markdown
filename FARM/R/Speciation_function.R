# Speciation function used inside the speciation takeover
Speciation <- function(myWorld, i, P.speciation,
                       myT, PosTargets, mytree, BL) {
  env.match <- myWorld[i, 7] == myWorld[i, 6]
  domesticator <- myWorld[i, 6] == 2
  prob.sp <- numeric(1)
  prob.sp[env.match & !domesticator] <- P.speciation[1, 1] # Prob of
  prob.sp[env.match & domesticator] <- P.speciation[2, 2] # Prob of 
  prob.sp[!env.match & !domesticator] <- P.speciation[1, 2] # Prob of
  prob.sp[!env.match & domesticator] <- P.speciation[2, 1] # Prob of
  test <- runif(1) < prob.sp
  if (test) {
    # speciate (i.e., send diaspora to an adjacent empty cell)
    Temp <- speciate(myT = myT, Parent = i, PosTargets = PosTargets, 
                     myWorld = myWorld, mytree = mytree, BL)
    
    myWorld <- Temp$myWorld
    mytree <- Temp$mytree
  }
  return(list("mytree" = mytree, "myWorld" = myWorld, "spec" = test))
}