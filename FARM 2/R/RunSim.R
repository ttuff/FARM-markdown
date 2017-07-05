#==================================================================
# SimulationFunctions.R
#
# Contains a function for simulation of cultural evolution in space and time
# Allows for (1) Vertical Transmission (phylogenetic inheritance); (2) Horizontal
# Transmission (cultural diffusion); (3) Ecological selection (Both speciation and
# extinction are determined by the match between the state of a binary trait and the
# environment a societuy occupies).
#
# 7 Jun 2016
# Carlos A. Botero, Bruno Vilela & Ty Tuff
# Washington University in Saint Louis
#==================================================================
RunSim <- function(myWorld, P.extinction, P.speciation,
                   P.diffusion, P.Arisal, P.TakeOver, nbs, independent,
                   N.steps, multiplier, start) {
  # myWorld = The hexagonal world created with the function BuildWorld
  # P.extinction = Probability matrix of extinction
  # P.speciation = Probability matrix of speciation
  # P.diffusion = Probability matrix of diffusion
  # P.Arisal = Probability matrix of arisal
  # P.TakeOver = Probability matrix of takeover
  # N.steps = Number of steps in the model
  # multiplier = The number that will multiply the probabilities according
  # to environmetal fitness.
  # start = the point ID in 'myWorld' that will give risen to humans.
  # (humans origin will be in one of the existing positions)

  world.size <- nrow(myWorld)
  # Initialize parameters we will use later to build the phylogeny
  rootnode <-  world.size + 1 # standard convention for root node number

  # set the seed for simulation
  if (is.null(start)) {
  start <- sample(1:world.size, 1)
  }

  myWorld[start, 4:6] <- c(0, 0, 1) # Setting root(0), time(0), ancestral(1, forager)

  mytree <- TheOriginOfSpecies(world.size, start) # Empty tree
  myT <- 0 # Time starts at zero

  # Common input and output for all the internal modules
  input <- list(P.speciation, P.Arisal, P.diffusion, P.extinction, P.TakeOver,
                myWorld, mytree, myT, multiplier, nbs, independent)

  # Functions order to be randomized
  rand_order_func_run <- list("Extinction", "Diffusion", "SpeciationTakeOver", "Arisal")

  cat("0% [") # Time count

  for (steps in 1:N.steps) { # Starts the loop with 'n' steps

    if (steps %% round((N.steps / 10)) == 0) { # Time count
      cat('-') # Time count
    }# Time count
    if (steps == N.steps) { # Time count
      cat("] 100 %\n")# Time count
    }# Time count

    # Randomize functions order
    rand_order <- sample(rand_order_func_run)
    # Run the functions
    input <- do.call(rand_order[[1]], list(input = input))
    input <- do.call(rand_order[[2]], list(input = input))
    input <- do.call(rand_order[[3]], list(input = input))
    input <- do.call(rand_order[[4]], list(input = input))

  }
  # Trunsform the input/output into the final result and return it
  myWorld <- as.data.frame(input[[6]])
  myWorld[, 8] <- paste0("t", myWorld[, 8])
  mytree <- makePhy(input[[7]])
  mytree$edge.length <- mytree$edge.length / N.steps
  return(list('mytree' = mytree, 'myWorld' = myWorld))
}
