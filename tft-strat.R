library(tidyverse)

# number of players
# start_history: 
#     1 2 3 4
#    ---------
#   1 T V V T
#   2 V V V T
#  

payoffs <- matrix(c(-1, -1, -10, 0, 0, -10, -5, -5), nrow=2, ncol=4)

const_T <- c("T", "T")
const_V <- c("V", "V")

titfortat <- function(player, t, strat_history) {
  return(
    if(strat_history[3-player, t-1] == "V") "V" else "T" 
  )
}

M = 100
starting_strats <- c("T", "V")

# might cause overhead
global_strat_history <- matrix(rep(0, M*2), nrow=2, ncol=M)
global_strat_history[, 1] <- starting_strats

# simulate games
for(i in seq(2, M)) {
  # get strat for player 1 and 2
  strat_1 <- titfortat(1, i, global_strat_history)
  strat_2 <- titfortat(2, i, global_strat_history)
  global_strat_history[, i] <- c(strat_1, strat_2)
}

pay_1 <- 0
pay_2 <- 0
abs_min_payoff <- min(payoffs)*M
abs_max_payoff <- max(payoffs)*M
extr_pay <- max(abs(abs_min_payoff), abs(abs_max_payoff))

plot(0, 0)
# get payoffs
for(i in seq(1, M)) {
  curr_strat_1 <- if(global_strat_history[1, i] == "T") 1 else 2
  curr_strat_2 <- if(global_strat_history[2, i] == "T") 1 else 2
  f <- curr_strat_2*2-1
  g <- curr_strat_2*2
  pays <- payoffs[curr_strat_1, f:g]
  pay_1 <- pay_1 + pays[1]
  pay_2 <- pay_2 + pays[2]
  
  # standardize payoffs
  pay_1 <- pay_1 / extr_pay
  pay_2 <- pay_2 / extr_pay
  points(pay_1, pay_2)
}



