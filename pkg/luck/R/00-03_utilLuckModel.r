# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#
# S4 implementation of generalized iLUCK models
# -- utility functions --
#
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# updating functions (to calculate posterior parameters)
# ---------------------------------------------------------------------------- #
updateLuckY <- function (n0, y0, tau, n){ (n0*y0+tau)/(n0+n) }
updateLuckN <- function (n0, n){ n0+n }


# ---------------------------------------------------------------------------- #
# helper function to test if an object is a single positive integer number:
# ---------------------------------------------------------------------------- #
is.samplesize <- function (n) {
 is.numeric(n) && (length(n) == 1) && (floor(n) == n) && n > 0
}


# ---------------------------------------------------------------------------- #
# function to wrap optim() and optimize() for optimization on parameter sets of
# (generalized) (i)LUCK-models.
# Takes basically the same arguments as optim() with 2-dim. lower and upper
# bound. The first argument of the function to be optimized must be named
# ".n0y0". Depending if the two-dimensional bounds "lower" and "upper" coincide
# in none, one or both dimensions, optim() or optimize() are called (with their
# specific arguments) or the value of fn at lower (= upper) is returned,
# respectively.
# The return value is the return object of optim() (or pretends to be so). 
# ---------------------------------------------------------------------------- #

wrapOptim <- function (par, fn, method = "L-BFGS-B", control = list(fnscale = 1),
                       lower, upper, ...){
 if (all(lower != upper)) { # both n and y are interval-valued: use optim() directly
  .optimObject <- optim (par = par, fn = fn, method = method, control = control, #
                         lower = lower, upper = upper, ...)
 } else {
  if (all(lower == upper)) { # both n and y are singular: no optimization
   fn1 <- function (par, ...) fn(par, ...)
   # make fake optim object
   .optimObject = list (par = lower, value = fn1(par = lower, ...))
  } else {
   if (lower[1] == upper[1]) { # n singular, y interval-valued
    # create function to suit optimize()
    fn1 <- function (y0, ...) fn(.n0y0 = c(lower[1], y0), ...)
    if (control$fnscale > 0) { # minimize (see ?optim)
     .optimizeObject <- optimize (f = fn1, interval = c(lower[2], upper[2]), #
                                  tol = 1e-10, ...)
     # make fake optim object
     .optimObject <- list (value = .optimizeObject$objective, #
                           par = c(lower[1], .optimizeObject$minimum))
    } else { # maximize
     .optimizeObject <- optimize (f = fn1, interval = c(lower[2], upper[2]), #
                                  maximum = TRUE, tol = 1e-10, ...)
     # make fake optim object
     .optimObject <- list (value = .optimizeObject$objective, #
                           par = c(lower[1], .optimizeObject$maximum))
    }
   } else { # n interval-valued, y singular
    # create function to suit optimize()
    fn1 <- function (n0, ...) fn(.n0y0 = c(n0, lower[2]), ...)
    if (control$fnscale > 0) { # minimize (see ?optim)
     .optimizeObject <- optimize (f = fn1, interval = c(lower[1], upper[1]), #
                                  tol = 1e-10, ...)
     # make fake optim object
     .optimObject <- list (value = .optimizeObject$objective, #
                           par = c(.optimizeObject$minimum, lower[2]))
    } else { # maximize
     .optimizeObject <- optimize (f = fn1, interval = c(lower[1], upper[1]), #
                                  maximum = TRUE, tol = 1e-10, ...)
     # make fake optim object
     .optimObject <- list (value = .optimizeObject$objective, #
                           par = c(.optimizeObject$maximum, lower[2]))
    }
   } # end of one-dim. optimization  
  }
 }
 # return (fake) optim object
 .optimObject
}


#