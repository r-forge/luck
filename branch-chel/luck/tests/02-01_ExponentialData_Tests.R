# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#                                                 
# S4 implementation of generalized iLUCK models   
# -- Tests for Class ExponentialData incl. show method --             
#                                                 
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

library(luck)
library(testthat)

# constructor methods
# Given data vector
expdata1 <- ExponentialData(1:10)
expdata1 <- ExponentialData(data=1:10)
# Given mean and samplesize
expdata2 <- ExponentialData(2, 100)
expdata2 <- ExponentialData(mean=2, n=100)
expdata2 <- ExponentialData(mean=2, 100)
expdata2 <- ExponentialData(2, n=100)
# Simulating values from an exponential distribution
set.seed(1234)
expdata3 <- ExponentialData(2,100, sim=TRUE)
expdata3 <- ExponentialData(mean=2, n=100, sim=TRUE)
expdata3 <- ExponentialData(mean=2, 100, sim=TRUE)
expdata3 <- ExponentialData(2, n=100, sim=TRUE)
# Given LuckModelData-object
data1 <- LuckModelData(11,2)
expdata4 <- ExponentialData(data1)
# Given data vector AND (non-corresponding) mean and samplesize
expdata5 <- ExponentialData(5,3,c(3,2,3,4)) # TODO: should throw an error

# replacement method for rawData slot
set.seed(1235)
rawData(expdata1) <- rexp(10)
expect_error(tauN(expdata1) <- matrix(c(0,10), ncol=2))
# error: should not replace tauN if raw data is present
expect_error(tau(expdata1) <- 0) 
# error: should not replace tauN if raw data is present
expect_error(n(expdata1) <- 10)
# error: should not replace tauN if raw data is present
expect_error(tauN(expdata2) <- matrix(c(0,10), ncol=2))
# error: mean must be strictly greter than 0
tauN(expdata2) <- matrix(c(1,10), ncol=2)
tau(expdata2) <- 0.5
expect_error(tau(expdata2) <- -0.5) # error: mean must be strictly greter than 0
n(expdata2) <- 20

# show method
expdata1
expdata2
expdata3
expdata4
expdata5

#