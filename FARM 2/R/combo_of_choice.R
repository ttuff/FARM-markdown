## Possible combinations of movement functions
## there are currently 5  possible modes of movement through the environment. We want to used a nested comparison design to compare all the combinations of these modes. The order with which these are applied later in the algorythm is later randomized so we need to run a lot of simulations to overcome the variation in that randomization.

# 17 June 2016



combo_of_choice <- function(combo_number, list.combos){
	
mode_Extinct <- "Extinct" 
mode_Diffusion <- "Diffusion" 
mode_Takeover <- "Takeover"
mode_Speciate <- "Speciate" 
mode_Appear <- "Random_new_origin"
	
	possible_combos <- list(
	
	Extinction_only = mode_Extinct, 
	Diffusion_only = mode_Diffusion, 
	Takeover_only = mode_Takeover, 
	Speciate_only = mode_Speciate, 
	Random_appear_only = mode_Appear,
	
	pairwise_combo_1 = c(mode_Extinct, mode_Diffusion),
	pairwise_combo_2 = c(mode_Extinct, mode_Takeover),
	pairwise_combo_3 = c(mode_Extinct, mode_Speciate),
	pairwise_combo_4 = c(mode_Extinct, mode_Appear),
	pairwise_combo_5 = c(mode_Diffusion, mode_Takeover),
	pairwise_combo_6 = c(mode_Diffusion, mode_Speciate),
	pairwise_combo_7 = c(mode_Diffusion, mode_Appear),
	pairwise_combo_8 = c(mode_Takeover, mode_Speciate),
	pairwise_combo_9 = c(mode_Takeover, mode_Appear),
	pairwise_combo_10 = c(mode_Speciate, mode_Appear),
	
	triple_combo_1 = c(mode_Extinct, mode_Diffusion, mode_Takeover),
	triple_combo_2 = c(mode_Extinct, mode_Diffusion, mode_Speciate),
	triple_combo_3 = c(mode_Extinct, mode_Diffusion, mode_Appear),
	triple_combo_4 = c(mode_Extinct, mode_Takeover, mode_Speciate),
	triple_combo_5 = c(mode_Extinct, mode_Takeover, mode_Appear),
	triple_combo_6 = c(mode_Diffusion, mode_Takeover, mode_Speciate),
	triple_combo_7 = c(mode_Diffusion, mode_Takeover, mode_Appear),
	triple_combo_8 = c(mode_Diffusion, mode_Speciate, mode_Appear),
	triple_combo_9 = c(mode_Takeover, mode_Speciate, mode_Appear),
	triple_combo_10 = c(mode_Extinct, mode_Speciate, mode_Appear),
	
	quad_combo_1 = c(mode_Extinct, mode_Diffusion, mode_Takeover, mode_Speciate),
	quad_combo_2 = c(mode_Extinct, mode_Diffusion, mode_Takeover, mode_Appear),
	quad_combo_3 = c(mode_Extinct, mode_Diffusion, mode_Speciate, mode_Appear),
	quad_combo_4 = c(mode_Extinct, mode_Takeover, mode_Speciate, mode_Appear),
	quad_combo_5 = c(mode_Diffusion, mode_Takeover, mode_Speciate, mode_Appear),
	
	all_modes = c(mode_Extinct, mode_Diffusion, mode_Takeover, mode_Speciate, mode_Appear)
	
	)
	
	if(list.combos){print(possible_combos)}
	return(list(paste("YOU HAVE CHOSEN '", names(possible_combos)[combo_number],
	                  "', which includes the following functions:", sep=""),
	possible_combos[[combo_number]]))
}



#chosen_combo <- combo_of_choice(28, FALSE)
#do.call(chosen_combo[[2]], args = list())















