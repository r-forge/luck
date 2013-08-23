# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#                                                 
# S4 implementation of generalized iLUCK models   
# -- Tests for Class ScaledNormalData incl. show method --             
#                                                 
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

library(luck)
library(testthat)

# constructor methods
scndata1 <- ScaledNormalData(1:10)
scndata1 <- ScaledNormalData(data=1:10)
scndata2 <- ScaledNormalData(10, 5)
scndata2 <- ScaledNormalData(mean=10, n=5)
scndata2 <- ScaledNormalData(mean=10, 5)
scndata2 <- ScaledNormalData(10, n=5)
set.seed(8552)
scndata3 <- ScaledNormalData(10, 5, sim=TRUE)
scndata3 <- ScaledNormalData(mean=10, n=5, sim=TRUE)
scndata3 <- ScaledNormalData(mean=10, 5, sim=TRUE)
scndata3 <- ScaledNormalData(10, n=5, sim=TRUE)
data1 <- LuckModelData(5, 2)
scndata4 <- ScaledNormalData(data1)

# replacement methods
set.seed(8553)
rawData(scndata1) <- rnorm(10)
expect_error(tauN(scndata1) <- matrix(c(0,10), ncol=2)) # error: should not relace tauN if raw data is present
expect_error(tau(scndata1) <- 0)                        # error: should not relace tauN if raw data is present
expect_error(n(scndata1) <- 10)                         # error: should not relace tauN if raw data is present
tauN(scndata2) <- matrix(c(0,10), ncol=2)
tau(scndata2) <- 0.5
n(scndata2) <- 20

# show method
scndata1
scndata2
scndata3
scndata4

#