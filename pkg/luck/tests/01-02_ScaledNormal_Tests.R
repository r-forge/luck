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

# constructor function
bsp1 <- LuckModel(n0 = c(1,10), y0 = c(-5,5))
n0(bsp1) <- c(1,2)
y0(bsp1) <- c(-1,1)
set.seed(42)
data(bsp1) <- list(tau=sum(rnorm(5)), n=5)
scnbsp1 <- ScaledNormalLuckModel(bsp1)
expect_error(scnbsp1e <- ScaledNormalLuckModel(bsp1, data=ScaledNormalData(mean=2, n=10)))
# error: bsp1 contains data

bsp2 <- LuckModel(n0 = c(1,10), y0 = c(-5, 10, 5, 15)) 
expect_error(scnbsp2e1 <- ScaledNormalLuckModel(bsp2))
# error: y0 in bsp2 has wrong dimension
expect_error(scnbsp2e2 <- ScaledNormalLuckModel(bsp2h, data=ScaledNormalData(mean=2, n=10)))
# error: dimensions do  not fit
scnbsp3 <- ScaledNormalLuckModel(n0=c(1,2), y0=c(3,4))
scnbsp4 <- ScaledNormalLuckModel(n0=c(1,2), y0=c(3,4), data=rep(4,10))
scnbsp5 <- ScaledNormalLuckModel(n0=1, y0=c(-5,5), data=rep(5,5))
scnbsp6 <- ScaledNormalLuckModel(n0=c(1,2), y0=6, data=rep(6,6))
scnbsp7 <- ScaledNormalLuckModel(n0=2, y0=7, data=rep(7,7))
paperbsp <- ScaledNormalLuckModel(n0=c(1,25), y0=c(3,4))

# accessor and replacement methods
n0(scnbsp1)
y0(scnbsp1)
data(scnbsp1)
n0(scnbsp1) <- c(10,20)
expect_warning(expect_error(n0(scnbsp1) <- c(10,20,30)))  # error: wrong n0 definition
y0(scnbsp1) <- c(-5,5)
expect_error(y0(scnbsp1) <- c(-5,-5,5,5)) # error: wrong y0 dimension
data(scnbsp1) <- 1:6
data(scnbsp1) <- data(scnbsp3)
data(scnbsp1) <- ScaledNormalData(mean=5, n=200)

# show method
scnbsp1
scnbsp3
paperbsp

# singleHdi 
singleHdi(scnbsp1, n=1, y=0, gamma=0.95)
expect_error(singleHdi(bsp1, n=1, y=0, gamma=0.95))
# error: bsp1 is a generic LuckModel and not of a subclass

# unionHdi
n0(scnbsp3) <- c(1,25)
data(scnbsp3) <- ScaledNormalData(mean=4,n=10)
unionHdi(scnbsp3)
unionHdi(scnbsp3, posterior=TRUE)$borders
# plot(scnbsp3, control=controlList(posterior=TRUE))
singleHdi(scnbsp3, n=11, y=4, gamma=0.95)
singleHdi(scnbsp3, n=35, y=3.29, gamma=0.95)

# singleCdf
singleCdf(scnbsp3, n = 1, y = 0, x = 0)
expect_error(singleCdf(bsp3, n = 1, y = 0, x = 0))
# error: bsp3 is a generic LuckModel and not of a subclass

# cdfplot
cdfplot(scnbsp3)
cdfplot(scnbsp3, xvec = 20)
cdfplot(scnbsp3, xvec = seq(0, 4, length.out = 80))
#