# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#                                                 
# S4 implementation of generalized iLUCK models   
# -- Tests for utility functions --             
#                                                 
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

library(luck)
library(testthat)

# Tests for wrapOptim()

testfu <- function (.n0y0, andarg) {
  if (andarg) return(.n0y0[1]^2 + .n0y0[2])
  else return (.n0y0[2]^2 + .n0y0[1])
}

# minimize
wrapOptim(par = c(1,1), fn = testfu, lower = c(1,1), upper = c(5,5), andarg = F)
wrapOptim(par = c(1,1), fn = testfu, lower = c(1,1), upper = c(5,5), andarg = T)

wrapOptim(par = c(1,1), fn = testfu, lower = c(1,1), upper = c(1,1), andarg = T)
wrapOptim(par = c(1,5), fn = testfu, lower = c(1,1), upper = c(1,5), andarg = T)
wrapOptim(par = c(2,1), fn = testfu, lower = c(1,1), upper = c(5,1), andarg = T)

# maximize
wrapOptim(par = c(1,1), fn = testfu, lower = c(1,1), upper = c(5,5), andarg = F, 
          control = list(fnscale=-1))
wrapOptim(par = c(1,1), fn = testfu, lower = c(1,1), upper = c(5,5), andarg = T,
          control = list(fnscale=-1))

wrapOptim(par = c(1,1), fn = testfu, lower = c(1,1), upper = c(1,1), andarg = T, control = list(fnscale=-1))
wrapOptim(par = c(1,5), fn = testfu, lower = c(1,1), upper = c(1,5), andarg = F, control = list(fnscale=-1))
wrapOptim(par = c(2,1), fn = testfu, lower = c(1,1), upper = c(5,1), andarg = T, control = list(fnscale=-1))

#