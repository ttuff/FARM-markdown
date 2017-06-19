## This module analyzes the results from module 1 and returns a list based on how many values each stat returns
## Ty Tuff and Bruno Vilela
## 24 August 2016

###### Specify function ##############################

Module_2 <- function(Module_1_output) {
  cat("\nAnalyzing: 0% [")
  if (any(is.na(Module_1_output))) {
    cat("----------]")
    return(NA)
  } else {

    this_tree <- Module_1_output$mytree
    this_world <- Module_1_output$myWorld


    ##### (0) Pull necessary variables from simulated trees and organize into a single object for all the tests below to pull from.

    #str(all_trees)
    #str(this_tree)


    ## 0a) Branch lengths
    Branch_Lengths <- this_tree$edge.length
    number_of_branches <- length(Branch_Lengths)

    # Anchor test = PD (Faith's phylogenetic diversity)
    Pylo_diversity_is_sum_of_BL <- sum(Branch_Lengths)

    # avPD -- Average phylogenetic diversity
    average_phylogenetic_diversity_is_mean_of_BL <- mean(Branch_Lengths)

    variance_Pylo_diversity_is_variance_of_BL <- var(Branch_Lengths)
    cat("-")
    ## 0b) Pairwise distance between tips
    Pairwise_dist <- cophenetic.phylo(this_tree)
    cat("-")
    # 2b) Pairwise distance -- Sum of pairwise distances

    # F -- Extensive quadratic entropy
    F_quadratic_entropy_is_sum_of_PD <- sum(Pairwise_dist)

    #Mean inter-species distances

    # Anchor test = MPD (mean pairwise distance)

    Mean_pairwise_distance <- mean(Pairwise_dist)

    cat("-")
    #Pairwise distance/all distances -- Variance of pairwise distances

    # Anchor test = VPD (variation of pairwise distance)

    variance_pairwise_distance <- var(as.vector(Pairwise_dist))




    ## 0c) Phylogenetic isolation

    # Using equal.splits method, faster computation
    Evolutionary_distinctiveness <- evol.distinct2(this_tree, type = "fair.proportion")
    cat("-")
    # ED - Summed evolutionary distinctiveness

    Evolutionary_distinctiveness_sum <- sum(Evolutionary_distinctiveness)

    ## 3d) Phylogenetic isolation -- Mean of species evolutionary distinctiveness

    # mean(ED)

    mean_Phylogenetic_isolation <- mean(Evolutionary_distinctiveness)

    ## 4d) Phylogenetic isolation -- Variance of species isolation metrics

    #var(ED)

    variance_Phylogenetic_isolation <- var(Evolutionary_distinctiveness)
    cat("-")

    ## Tree topology

    #Gamma index

    ltts <- ltt(this_tree, gamma = TRUE, plot = FALSE)
    lineages_through_time <- as.numeric(ltts[[1]])
    time_steps <- as.numeric(ltts[[2]])
    gamma <- ltts[[3]]
    gamma_p_value <- ltts[[4]]
    cat("-")

    colless_stat <- colless(as.treeshape(this_tree))
    sackin_index <- sackin(as.treeshape(this_tree))
    tree_shape_stat <- shape.statistic(as.treeshape(this_tree))

    ##### (5) Tree metric -- Macroevolutionary - Rate and rate changes ###############
    ##################################################

    ## Speciation vs extinction rates and Net diversification
    bds <- bd(this_tree)
    speciation_rate <- bds[1]
    extinction_rate <- bds[2]
    extinction_per_speciation <- bds[3]
    speciation_minus_extinction <- bds[4]
    cat("-")


    ## Speciation vs extinction rates and Net diversification dependent on trait
    N.for.dom <- table(this_world[, 6])
    if(length(N.for.dom) == 2) {
      par.div.dep <- DivDep( mytree = this_tree, myWorld = this_world)
      trait_1_speciation <- par.div.dep[1]
      trait_2_speciation <- par.div.dep[2]
      trait_1_extinction <- par.div.dep[3]
      trait_2_extinction <- par.div.dep[4]
      transition_from_trait_1_to_2 <- par.div.dep[5]
      transition_from_trait_2_to_1 <- par.div.dep[6]
      transition_rate_ratio_1to2_over_2to1 <- transition_from_trait_1_to_2/transition_from_trait_2_to_1
      cat("-")

      ## Phylogenetic signal (D)
      Phylogenetic_signal <- Dsig(mytree = this_tree, myWorld = this_world)
      cat("-")

      ## Spatial Analysis
      nbs0 <- knearneigh(as.matrix(this_world[, 2:3]), k = 7, longlat = TRUE)
      nbs <- knn2nb(nbs0, sym = TRUE) # 7 symmetric neighbors
      nbs.listw <- nb2listw(nbs)
      factors.nbs <- as.factor(ifelse(is.na(this_world[, 6]), 3, this_world[, 6]))
      spatial.tests <- joincount.test(fx = factors.nbs, listw = nbs.listw)
      spatial.tests.fora <- spatial.tests[[1]]$statistic
      spatial.tests.dom <- spatial.tests[[2]]$statistic
      prevalence <- (N.for.dom[1] - N.for.dom[2]) / sum(N.for.dom)
      cat("-")
    } else {
      trait_1_speciation <- NA
      trait_2_speciation <- NA
      trait_1_extinction <- NA
      trait_2_extinction <- NA
      transition_from_trait_1_to_2 <- NA
      transition_from_trait_2_to_1 <- NA
      transition_rate_ratio_1to2_over_2to1 <- NA
      Phylogenetic_signal <- NA
      spatial.tests.fora <- NA
      spatial.tests.dom <- NA
      prevalence <- ifelse(names(table(this_world[, 6])[1]) == "1", 1,
                           -1)
      cat("---")

    }




    results_summary_matrix_1 <- cbind(

      number_of_branches,
      Pylo_diversity_is_sum_of_BL,
      average_phylogenetic_diversity_is_mean_of_BL,
      variance_Pylo_diversity_is_variance_of_BL,

      F_quadratic_entropy_is_sum_of_PD,
      Mean_pairwise_distance,
      variance_pairwise_distance,

      colless_stat ,
      sackin_index ,
      tree_shape_stat,

      Evolutionary_distinctiveness_sum,
      mean_Phylogenetic_isolation,
      variance_Phylogenetic_isolation,

      gamma,
      gamma_p_value,
      speciation_rate,
      extinction_rate,
      extinction_per_speciation,
      speciation_minus_extinction,
      trait_1_speciation,
      trait_2_speciation ,
      trait_1_extinction ,
      trait_2_extinction ,
      transition_from_trait_1_to_2 ,
      transition_from_trait_2_to_1 ,
      transition_rate_ratio_1to2_over_2to1 ,
      Phylogenetic_signal,
      spatial.tests.fora,
      spatial.tests.dom,
      prevalence
    )
    rownames(results_summary_matrix_1) <- 1

    results_summary_matrix_2 <- cbind(
      c(Evolutionary_distinctiveness,NA),
      lineages_through_time,
      time_steps
    )
    colnames(results_summary_matrix_2) <- c("Evolutionary_distinctiveness",
                                            "lineages_through_time", "time_steps")
    head(results_summary_matrix_2)

    ### Returns from function in list form
    returns <- list(
      #Branch_Lengths,
      #Pairwise_dist,
      results_summary_matrix_1,
      results_summary_matrix_2

    )

    names(returns) <- c(
      #"Branch_Lengths",
      #"Pairwise_distance",
      "results_summary_of_single_value_outputs",
      "results_summary_matrix_of_multi_value_outputs"
    )
    cat("] 100%")

    return(returns)

  }
}


#Module_2(myOut)
