# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#                                                 
# S4 implementation of generalized iLUCK models   
# -- Tests for Class LuckModel incl. show method --             
#                                                 
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

library(luck)
library(testthat)

## Tests for constructor funktion:
set.seed(42)
bsp1 <- LuckModel(n0 = c(1,10), y0 = c(-5,5))
bsp1
expect_warning(expect_error(LuckModel(n0 = c(1,10,2), y0 = c(-5,5)), "n0 must *")) # error
expect_error(LuckModel(n0 = c(10,1), y0 = c(-5,5))) # error
expect_error(LuckModel(n0 = c(1,10), y0 = c(-5,5,15))) # error
expect_error(LuckModel(n0 = c(1,10), y0 = c(-5,15,5,10))) # error
bsp2 <- LuckModel(n0 = c(1,10), y0 = c(-5, 10, 5, 15)) 
bsp2
bsp3 <- LuckModel(n0 = 1, y0 = c(-5,5))
bsp3
bsp4 <- LuckModel(n0 = c(1,10), y0 = 5) 
bsp4
bsp5 <- LuckModel(n0 = c(1,10), y0 = c(-5,5), data = list(sum(rnorm(10)), 10))
bsp5
bsp5 <- LuckModel(n0 = c(1,10), y0 = c(-5,5), data = matrix(c(sum(rnorm(10)), 10), ncol = 2, byrow=FALSE))
bsp5
bsp6 <- LuckModel(n0 = c(1,10), y0 = c(-5,5,10,15), data = matrix(rep(c(20, 10), 2), ncol=2, byrow=TRUE))
bsp6
expect_error(LuckModel(n0 = c(1,10), y0 = c(-5,10,5,15), data = matrix(c(20, 10), ncol=2))) # error
bsp7 <- LuckModel(n0 = 1, y0 = 5)
bsp7


## Tests for accessor functions:
set.seed(42)
n0(bsp1)
n0(bsp1)[1]
n0(bsp1)[2]
y0(bsp2)
y0(bsp2)[1,1]
y0(bsp2)[1,2]
y0(bsp2)[3] # attention!
data(bsp1)
data(bsp5)
tauN(data(bsp5))
tauN(data(bsp5))[1]
tauN(data(bsp5))[2]
tau(data(bsp5))
n(data(bsp5))
data(bsp6)
tauN(data(bsp6))
tauN(data(bsp6))[1,1]
tauN(data(bsp6))[1,2]
tauN(data(bsp6))[3]  #attention!
tauN(data(bsp6))[1,]
tauN(data(bsp6))[,1]
tau(data(bsp6))
tau(data(bsp6))[1]
tau(data(bsp6))[2]
n(data(bsp6))
n(data(bsp6))[1]
n(data(bsp6))[2]


## Tests for replacement methods:
set.seed(42)
bsp1
expect_warning(expect_error(n0(bsp1) <- c(1,2,3)))
# because of invalid assignment to n0, no replacement takes place
n0(bsp1) <- c(1,2)
y0(bsp1) <- c(-1,1)
data(bsp1) <- list(tau=sum(rnorm(5)), n=5)
bsp1
bsp6
n0(bsp6)[1] <- 2
y0(bsp6)[2,1] <- -5
data(bsp6) <- list(tau=c(25,26), n=c(14,14))
expect_error(data(bsp6) <- list(tau=c(25,26), n=c(14,14,14))) # error
expect_error(data(bsp6) <- list(tau=c(25,26,27), n=c(14,14,14))) # error
expect_error(y0(bsp6) <- c(0,1)) # error
bsp6
bsp8 <- LuckModel(n = c(1,10), y = 1:10)


## Tests for show method:
bsp1
bsp2
bsp3
bsp4
bsp5
bsp6
bsp7
bsp8

#