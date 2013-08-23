# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#
# S4 implementation of generalized iLUCK models
# -- Tests for Class LuckModelData incl. show method --
#
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

#source(file="../R/00-01_LuckModelData.r")
#source(file="../R/00-02_LuckModel.r")
#source(file="../R/00-03_utilLuckModel.r")
#source(file="../R/00-04_unionHdiLuckModel.r")
#source(file="../R/00-05_plotLuckModel.r")
#source(file="../R/00-06_cdfplotLuckModel.r")
#source(file="../R/01-01_ScaledNormalData.r")
#source(file="../R/01-02_ScaledNormal.r")
#source(file="../R/02-01_ExponentialData.r")
#source(file="../R/02-02_Exponential.r")
library(luck)
library(testthat)

## Tests for constructor funktion:
# directly
# --------
data1 <- LuckModelData(5, 1)
data1
expect_error(LuckModelData(5, c(1,2)),
             "If tau is one-dimensional, n must be one-dimensional, too.") # error
expect_error(LuckModelData(5)) # error
data2 <- LuckModelData(1:5, 1)
data2
data3 <- LuckModelData(1:5, rep(1,5))
data3
expect_error(LuckModelData(1:5, rep(1,4)),
             "n must either have the same dimension as tau or be a single value.") # error
data4 <- LuckModelData(matrix(c(1:5, rep(1,5)), ncol = 2))
data4
expect_error(LuckModelData(matrix(c(1:5, rep(1,5)), ncol = 5))) # error
expect_error(LuckModelData(matrix(c(1:5, rep(1,5)), ncol = 5), 5)) # error
# named
# --------
LuckModelData(tau=5,n=1)
LuckModelData(n=1,tau=5)
expect_error(LuckModelData(tau=5, n=c(1,2))) # error
expect_error(LuckModelData(tau=5)) # error
expect_error(LuckModelData(n=5)) # error
LuckModelData(tau=1:5, n=1)
LuckModelData(tau=1:5, n=rep(1,5))
expect_error(LuckModelData(tau=1:5, n=rep(1,4))) # error
expect_error(LuckModelData(tau=matrix(c(1:5, rep(1,5)), ncol = 2))) # error
expect_error(LuckModelData(n=matrix(c(1:5, rep(1,5)), ncol = 2))) # error
# wrapped in a list
# --------
LuckModelData(list(5, 1))
expect_error(LuckModelData(list(5, c(1,2)))) # error
expect_error(LuckModelData(list(5))) # error
expect_error(LuckModelData(list())) # error
expect_error(LuckModelData(list(1,2,3))) # error
LuckModelData(list(1:5, 1))
LuckModelData(list(1:5, rep(1,5)))
expect_error(LuckModelData(list(1:5, rep(1,4)))) # error
LuckModelData(list(matrix(c(1:5, rep(1,5)), ncol = 2)))
expect_error(LuckModelData(list(matrix(c(1:5, rep(1,5)), ncol = 5)))) # error
expect_error(LuckModelData(list(matrix(c(1:5, rep(1,5)), ncol = 5), 5))) # error
# wrapped in a named list
# --------
LuckModelData(list(tau=5, n=1))
LuckModelData(list(n=1, tau=5))
LuckModelData(list(tau=5, 1))
LuckModelData(list(1, tau=5))
LuckModelData(list(5, n=1))
LuckModelData(list(n=1, 5))
expect_error(LuckModelData(list(tau=5, x=1))) # error
expect_error(LuckModelData(list(y=5, n=1))) # error
LuckModelData(list(tau=1:5, n=1))
LuckModelData(list(tau=1:5, n=rep(1,5)))
expect_error(ldata4e1 <- LuckModelData(list(tau=matrix(c(1:5, rep(1,5)), ncol = 2)))) # error


## Tests for accessor functions
tauN(data1)
tauN(data2)
tau(data1)
tau(data2)
n(data1)
n(data2)
rawData(data1)

# Tests for replacement functions
tauN(data1) <- matrix(c(10,1), ncol = 2)
data1
expect_error(tauN(data1) <- matrix(c(10,1), ncol = 1)) # error
tauN(data2)[1,1] <- 0
data2
tauN(data2)[,2] <- 10
data2
tau(data1) <- 11
data1
expect_error(tau(data1) <- c(11,12)) # error
tau(data2) <- 0:4
data2
expect_error(tau(data2) <- 0:5) # error
tau(data2) <- 0 # attention!
data2
tau(data2)[1] <- 1
data2
n(data1) <- 2
data1
expect_error(n(data1) <- c(1,2)) # error
n(data2) <- rep(9,5)
data2
expect_error(n(data2) <- rep(9,4)) # error
n(data2) <- 20 # attention!
data2
n(data2)[1] <- 10
data2

# Tests for show method
data1
data2
new("LuckModelData")
expect_error(LuckModelData(), "No arguments given *") # error
