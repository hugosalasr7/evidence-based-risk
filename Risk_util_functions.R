#############################################
# Functions to simulate a Risk attack and 
# estimate quantities of interest.

# Author: Hugo Salas
#############################################

simulate_turn <- function(dice_a, dice_d){
  
  # Simulates one turn of risk by rolling dice for both the attacker and the defender
  # and determining the number of armies that each will lose.
  
  # Inputs:
  #     dice_a (int): # of dice that the attacker will roll (1,2,3)
  #     dice_d (int): # of dice that the defender will roll (1,2)
  # Output:
  #     Matrix with two columns. Each column represents the number of 
  #     armies lost by either the attacker and defender
  
  if ((dice_a>3) | (dice_a<1) | (dice_d>2) | (dice_d<1)){
    stop("One of these conditions is not being fulfilled: 0 > dice_a > 3, 0 > dice_d > 2")
  }
  
  min_dice = min(dice_a, dice_d)
  lost_armies = matrix(data = 0, nrow = 1, ncol = 2)
  colnames(lost_armies) = c("Attacker", "Defender")
  
  attacker = sample(1:6, dice_a, replace = TRUE) ##Attacker throws dice 
  attacker = sort(attacker, decreasing = TRUE) 
  defender = sample(1:6, dice_d, replace = TRUE) #Defender throws dice
  defender = sort(defender, decreasing = TRUE) 
  
  for (die in 1:min_dice){
    if (attacker[die] > defender[die]){
      lost_armies[2] = lost_armies[2] + 1
    } else {
      lost_armies[1] = lost_armies[1] + 1
    }
    
  }
  
  return(lost_armies)
}


montecarlo_sim_turn <- function(dice_a, dice_d, seed = 12345, reps = 10000){
  
  # Performs Monte-Carlo simulations (reps) times, of the simulate_turn function
  # based on the (seed) set as input.
  
  # Inputs:
  #     dice_a (int): # of dice that the attacker will roll (1,2,3)
  #     dice_d (int): # of dice that the defender will roll (1,2)
  #     seed: Random seed to be used during the simulations
  #     reps: # of repetitions/simulations that will be executed
  
  # Output:
  #     List with 5 elements:
  #         1. Expected losses for attacker 
  #         2. Expected losses for defender
  #         3. Probability that attacker loses 0 armies
  #         4. Probability that attacker loses 1 armies
  #         5. Probability that attacker loses 2 armies

  
  set.seed(seed)
  sim_results = matrix(data = NA, nrow = reps, ncol = 2)
  
  for (i in 1:reps){
    sim_results[i, ] = simulate_turn(dice_a,dice_d)
  }
  
  rmatrix = matrix(data = c(mean(sim_results[,1]), mean(sim_results[,2]),
                       mean(sim_results[,1]==0), mean(sim_results[,1]==1),
                       mean(sim_results[,1]==2), dice_a, dice_d), nrow = 1, ncol = 7)
  colnames(rmatrix) = c("Expected loss: attacker", "Expected loss: defender", 
                        "Prob(attacker loses 0)", "Prob(attacker loses 1)", 
                        "Prob(attacker loses 2)", "# dice: attacker", 
                        "# dice: defender")
  
  return(rmatrix)

}


optim_dice_n <- function(army, attacker = TRUE){
  
  # Chooses the number of dice that a player should chose to throw. 
  # For risk, this is always the maximum number of dice available.
  
  # Inputs:
  #     army (int): # of armies that a player has left (strictly positive)
  #     attacker (boolean): TRUE if we're trying to choose the number of dice
  #                         for an attacker; FALSE for a defender
  
  # Output:
  #     (Int) Number with the number of dice that the player should throw
  
  if (attacker){
    if (army>3){
      dice = 3
    } else if (army==3 | army==2) {
      dice = army - 1
    } 
  } else {
    if (army>=2){
      dice = 2
    } else if (army==1)
      dice = 1
  }
  
  return(dice)
}



roll_until_defeat <- function(army_a, army_d){
  
  # Rolls dice for the attacker and the defender until one of them can no longer 
  # roll a dice. This happens when the attacker has only 1 army left or when the 
  # defender has no more armies. This function calculates the number of turns that 
  # it took for this final state to happen and returns the final state of the armies.
  
  # Inputs:
  #     army_a (int): # of armies owned by the attacker (strictly positive)
  #     army_d (int): # of armies owned by the defender (strictly positive)
  
  # Output:
  #     Matrix with three columns: the final number of armies for the attacker, 
  #     the defender and the number of turns it took for this final state.  
  
  times_rolled = 0
  
  while ((army_a > 1) & (army_d > 0)){ #While each player has enough armies to proceed
    lost = simulate_turn(optim_dice_n(army_a) , optim_dice_n(army_d, FALSE))
    army_a = army_a - lost[1]
    army_d = army_d - lost[2]
    times_rolled = times_rolled + 1
  }
  
  return( matrix(data = c(army_a, army_d, times_rolled), nrow = 1, ncol = 3, 
                 dimnames = list(c(),c("Armies left: Attacker", 
                                       "Armies left: Defender", 
                                       "# Rolls needed"))))
}



montecarlo_until_defeat <- function(army_a, army_d, seed = 12345, reps = 10000){
  
  # Performs Monte-Carlo simulations (reps) times, of the roll_until_defeat function
  # based on the (seed) set as input.
  
  # Inputs:
  #     army_a (int): # of armies owned by the attacker (strictly positive)
  #     army_d (int): # of armies owned by the defender (strictly positive)
  #     seed: Random seed to be used during the simulations
  #     reps: # of repetitions/simulations that will be executed
  
  # Output:
  #     Matrix with (reps) rows and three columns: the final number of armies for the attacker, 
  #     the defender and the number of turns it took for this final state. Each row represents
  #     one simulation
  
  set.seed(seed)
  rv = matrix(data = NA, nrow = reps, ncol = 3)
  
  for (i in 1:reps){
    rv[i, ] = roll_until_defeat(army_a, army_d)
  }
  
  return(rv)
  
}



success_until_defeat <- function(army_a, army_d, seed = 12345, reps = 10000, quant = c(0.0275, 0.975),
                                 verbose = TRUE, output = TRUE){
  
  # Based on Monte-Carlo simulations, computes the likelihood that a given attack
  # until defeat will succeed. i.e. that the defender will end up with 0 armies.
  
  # Inputs:
  #     army_a (int): # of armies owned by the attacker (strictly positive)
  #     army_d (int): # of armies owned by the defender (strictly positive)
  #     seed: Random seed to be used during the simulations
  #     reps: # of repetitions/simulations that will be executed
  #     quant (vector): quantiles
  #     verbose (boolean): when TRUE, the function will print the results of the simulations 
  #     output (boolean): when TRUE, the function will execute a return statement with the results                        
  
  # Output:
  #    List with three elements:
  #           1) Probability of success
  #           2) Expected losses given success
  #           1) Values of quantiles specified of lost armies
  
  
  rv = montecarlo_until_defeat(army_a, army_d, seed = seed, reps = reps)
  prob_succ = mean(rv[,2]==0) #Probability of success
  losses = army_a - rv[rv[,2]==0,1] #Expected armies lost for attacker
  losses_95p = quantile(losses, c(0.0275, 0.975))
  exp_losses = mean(losses)
  
  if (verbose == TRUE){
    print(paste("Probability of success:", round(prob_succ, 2)))
    print(paste("   If succesful, the attacker is expected to lose:", round(exp_losses, 2), "armies"))
    print(paste("   95% of the time, an attack of this nature results in",
                losses_95p[1], "to", losses_95p[2], "army losses for the attacker"))
  }
  
  if (output == TRUE){
    return( list("prob_succ" = prob_succ, "exp_loss"= exp_losses, "quant_loss" = quantile(losses, quant)))
  }
  
}
