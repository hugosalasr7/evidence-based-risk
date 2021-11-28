# Evidence-Based Risk
Risk is a very popular and fun board game. I created this code to inform my decisions while playing it and increase my chances of winning. Specifically, I use Monte Carlo simulations to estimate the likelihood that a given attack will be successful. This code can produce the following calculations:

 1. The probability that, given a number of attacking and defending troops, the player that is defending will lose all of her/his troops.
 2. The (a) expected number and the (b) 95% confidence interval of troops that the attacker will lose if he was succesful in defeating his opponent.
 3. This code can be further generalized to create other quantities of interest. 


the functions available in this repository estimate the probability that, given a number of attacking and defending troops, the player that's defending will lose all of her/his troops. Additionally, it computes the expected number of troops that are expected to go down under this mission. 

For more information about the game and its rules, please visit the following [link](<https://www.ultraboardgames.com/risk/game-rules.php>).

<p align="center">
  <img src="https://github.com/hugosalasr7/evidence-based-risk/blob/main/risk.jpg" alt="drawing" width="400" />
</p>


## Relevant files:
- ```Risk_util_functions.R```: A script with a set of functions that can be used to simulate an attack in Risk. It contains all of the functions needed to compute the quantities of interest relevant for an attack during the game. 
- ```Risk_example_use.Rmd```: The code of a notebook where I show how ```Risk_util_functions.R``` can be used to inform decisionmaking while playing Risk.
- ```Risk_example_use.md```: The output of said notebook. 
