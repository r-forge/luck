# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#                                                 
# S4 implementation of generalized iLUCK models   
# -- Tests for Class ScaledNormal incl. show method, unionHdi and cdfplot --             
#                                                 
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

library(luck)
library(testthat)
library(TeachingDemos)

# constructor function
expbsp1 <- ExponentialLuckModel(n0=c(1,2), y0=c(3,4))
expbsp2 <- ExponentialLuckModel(n0=2, y0=7, data=rep(7,7))
expbsp3 <- ExponentialLuckModel(
  LuckModel(y0=c(1,3), n0=c(1,2), data=list(tau=sum(rexp(5)), n=5)))
expbsp4 <- ExponentialLuckModel(n0=c(1,2), y0=6, data=rep(6,6))
expbsp5 <- ExponentialLuckModel(n0=c(1,2), y0=c(3,4), data=rep(4,10))
expbsp6 <- ExponentialLuckModel(n0=1, y0=c(1,5), data=rep(5,5)) 

expect_error(expbsp1e <- ExponentialLuckModel(bsp1))
# error: n0 and y0 must be positive to define an exponential distribution
expect_error(expbsp2e <- ExponentialLuckModel(bsp1, data=ExponentialData(mean=2, n=10)))
# error: n0 and y0 must be positive to define an exponential distribution  
expect_error(expbsp1e1 <- ExponentialLuckModel(bsp2)) # error: y0 must be one-dimensional                               
expect_error(expbsp1e2 <- ExponentialLuckModel(bsp2, data=ExponentialData(mean=2, n=10)))
# error: wrong dimension

# accessor and replacement methods
n0(expbsp3)
y0(expbsp3)
data(expbsp3)
n0(expbsp3) <- c(10,20)
expect_warning(expect_error(n0(expbsp1) <- c(10,20,30)))  # error: wrong n0 definition
y0(expbsp3) <- c(1,5)
expect_error(y0(expbsp1) <- c(-5,-5,5,5)) # error: wrong y0 definition
data(expbsp3) <- 1:6
data(expbsp3) <- data(expbsp1)
data(expbsp3) <- ExponentialData(mean=5, n=200)

# show method
expbsp1
expbsp2
expbsp3
expbsp4
expbsp5
expbsp6

# singleHdi
singleHdi(expbsp3, n=11, y=2, gamma=0.95)
expect_error(singleHdi(bsp1, n=1, y=0, gamma=0.95)) # error: bsp1 is a generic LuckModel
singleHdi(expbsp1, n=11, y=4, gamma=0.95)
singleHdi(expbsp1, n=35, y=3.29, gamma=0.95)

# unionHdi
unionHdi(expbsp3)
unionHdi(expbsp3, posterior=TRUE)$borders -> exphpd
cdfplot(expbsp3, control=controlList(posterior=TRUE))
lines(exphpd, rep(0,2), lwd=3)

# singleCdf
singleCdf(expbsp2, n = 1, y = 1, x = 1)
expect_error(singleCdf(bsp3)) # error: bsp1 is a generic LuckModel

# cdfplot
cdfplot(expbsp1)
cdfplot(expbsp1, xvec = 20) # plot over a number of 20 points
cdfplot(expbsp1, xvec = seq(0, 4, length.out = 80)) 
cdfplot(expbsp3)
cdfplot(expbsp3, control = controlList(posterior=TRUE))


#