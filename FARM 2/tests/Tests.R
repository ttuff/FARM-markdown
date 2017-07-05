#
#
# #####################################################################
#
# #####################################################################
# ## need to document which functions we use from each of these libraries.
# library(ape)
# library(spdep)
# library(Rcpp)
# library(msm)
# library(FARM)
#
#
# sim_run_cluster <- function(replicate_cycle, myWorld, number_of_time_steps, nbs,
#                             number_of_tips) {
#   # Calls the full simulation script
#   #
#   # Purpose: Need to wrap the entire simulation script into a function so it can be called in parallel from a cluster call
#   #
#   # Args:
#   #    replicate_cycle: An integer indicating the replicate number of a simulation. This variable is used in this function to label
#   #			the saved output file and control the number of replicates run by the cluster.
#   #
#   #    combo_number: An interger between 1 and 31 indicating the combinations of S, E, A, D, and T modules to be included
#   #			in the simulation. The full list of these combinations can be printed using the function combo_of_choice(28, TRUE).
#   # 		We are currently using combinations 25,28,29,and 31 as our four competing models for the spread of agriculture.
#   #
#   #    myWorld: Matrix that defines the scope of the available world and acts as a data hub for organizing and reporting
#   #			results from the different elements of the simulation.
#   #
#   #    number_of_time_steps: An integer indicating how many iterations the simulation will calculated before writing the data
#   #			file.
#   #
#   #    nbs: A list of the available neighbors for each spatial point. This is passed to the function for calculating the interaction
#   #			of neighbors through time.
#   #
#   #    number_of_tips: An interger indicating the number of tree tips the simulation should be truncated to. The default is to
#   #			include all the available tips (e.g. 1254 for human languages).
#   #
#   # Returns:
#   #    myOut: A list object containing a 'phylo' tree object called mytree in the first position and the myWorld matrix of
#   #      	spatial and tree data in the second position
#   #
#
#   x1 <- 4 #Number of runs per core
#   if (replicate_cycle != 1) {
#     replicate_cycle <- ((replicate_cycle - 1) * x1) + 1
#   }
#   count <- 0
#   myOut <- list()
#   for (i in replicate_cycle:(replicate_cycle + (x1 - 1))) {
#     independent <- 0
#     count <- count + 1
#
#     # Probability of Arisal
#     prob_choose_a <- rexp(4, rate = 9)
#     P.Arisal0  <- parameters(prob_choose_a[1], prob_choose_a[2],
#                              prob_choose_a[3], prob_choose_a[4],
#                              "Env_NonD", "Env_D",
#                              "Evol_to_F", "Evol_to_D")
#     # P.Arisal0 is the one you should change the parameters
#     P.Arisal <- matrix(NA, ncol = 2, nrow = nrow(myWorld)) # probability per cell
#     colnames(P.Arisal) <- c("Evolve_to_F", "Evolve_to_D")
#     Env.Dom <- myWorld[, 7] == 2
#     P.Arisal[Env.Dom, 1] <- P.Arisal0[1, 2]
#     P.Arisal[!Env.Dom, 1] <- P.Arisal0[1, 1]
#     P.Arisal[Env.Dom, 2] <- P.Arisal0[2, 2]
#     P.Arisal[!Env.Dom, 2] <- P.Arisal0[2, 1]
#
#     colnames(P.Arisal) <- c("Prob_of_Foraging", "Porb_of_Domestication")
#     #####
#     prob_choose <- runif(12, 0.1, 1)
#     sub <- (prob_choose[1] - 0.1)
#     sub <- ifelse(sub < .1, .1, sub)
#     prob_choose[c(4)] <- runif(1, 0.1, sub)
#     prob_choose[c(5)] <- runif(1, .5, 1) # High extinction
#     prob_choose[c(6)] <- runif(1, 0, (prob_choose[3] - 0.1))
#
#     prob_choose[c(9, 10, 12)] <- runif(3, 0.1, prob_choose[11])
#     if (count == 1) {
#       prob_choose[7:12] <- 0
#     }
#     if (count == 2) {
#       prob_choose[9:12] <- 0
#     }
#     if (count == 3) {
#       prob_choose[7:8] <- 0
#       independent <- 0
#     }
#     if (count == 4) {
#       independent <- 0
#     }
#     P.speciation <- parameters(prob_choose[1], prob_choose[1],
#                                prob_choose[2], prob_choose[3],
#                                "Env_NonD", "Env_D", "For", "Dom")
#
#     P.extinction  <- parameters(prob_choose[4], prob_choose[4],
#                                 prob_choose[5], prob_choose[6],
#                                 "Env_NonD", "Env_D", "For", "Dom")
#
#
#     P.diffusion <- parameters(0, prob_choose[7],
#                               prob_choose[8], 0,
#                               "Target_For", "Target_Dom",
#                               "Source_For", "Source_Dom")
#
#     P.TakeOver <- parameters(prob_choose[9], prob_choose[10],
#                              prob_choose[11], prob_choose[12],
#                              "Target_For", "Target_Dom",
#                              "Source_For", "Source_Dom")
#     multiplier <- 1 # always 1 now.
#
#     myOut <- RunSimUltimate(myWorld, P.extinction, P.speciation,
#                              P.diffusion, P.Arisal, P.TakeOver, nbs, independent,
#                              N.steps = number_of_time_steps, silent = F,
#                              multiplier = multiplier)
#    replicate_cycle <- replicate_cycle + 1
#   }
#   return(myOut)
#
# }
#
#
#
#
# #####################################################################
# coords <- apply(language_centroids[, 3:4], 2, as.numeric)
# conds <- suitability2
# conds <- ifelse(conds <= 21, 1, 2)
# conds[is.na(conds)] <- sample(c(1, 2), sum(is.na(conds)), replace = TRUE)
#
#
# ##### Specify simulation parameters #################################
#
# number_of_tips <- 300
# number_of_time_steps_a <- 300
# #replicate_cycle <- c(1)  #number of replicates
# #####################################################################
#
# sub <- sample(1:number_of_tips, number_of_tips) # subsample (remove when running for all)
# system.time(
#   myWorld <- BuildWorld(coords[sub, ], conds[sub, ])
# )
# nbs <- knn2nb(knearneigh(coords[sub, ], k = 7, longlat = TRUE),
#               sym = TRUE) # 7 symmetric neighbors
# n.obs <- sapply(nbs, length)
# seq.max <- seq_len(max(n.obs))
# nbs <- t(sapply(nbs, "[", i = seq.max))
#
#
#
# NAI <- 1000
#
# a <- sim_run_cluster(replicate_cycle = NAI,
#                 myWorld, number_of_time_steps = number_of_time_steps_a,
#                 nbs, number_of_tips = nrow(myWorld))
#
