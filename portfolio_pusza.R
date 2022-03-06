library(tidyverse)

# number of trials
N <- 100
# number of stocks
M <- 2

# define covariance matrix and returns - hardcoded for now
C <- matrix(c(4, 0.5, 0.5, 7), ncol=M, nrow=M, byrow=T)
R <- matrix(c(0.05, 0.18), ncol=1)
rf <- 0.03

# generate random xs that make up the Nth portfolio
XS <- matrix(rep(c(rep(0, N)), M), ncol=N, nrow=M)



for(i in seq(1, N)) {
  t <- runif(M-1)
  x <- c(t, 1-sum(t))
  XS[, i] <- x
}

SD_R <- matrix(rep(c(0, 0), N), ncol=N, nrow=2)

# for each portfolio, calculate return and volatility (sd)
for(i in seq(1, N)) {
  tmp_x <- matrix(XS[, i], ncol=1)
  r <- t(tmp_x) %*% R
  sd <- t(tmp_x) %*% C %*% tmp_x
  SD_R[, i] <- c(sd, r)
}

# sort SD_R
SD_R <- SD_R[, order(SD_R[2, ], decreasing=F)]

# plot portfolio volatility ~ return
matplot(SD_R[1, ], SD_R[2, ], type="l")
