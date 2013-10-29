# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#
# S4 implementation of generalized iLUCK models
# -- method to calculate unions of HD intervals --
#
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# unionHdi method for LuckModel objects
# -- can not be used for "plain" LuckModel objects, but for objects from all
#    subclasses
# -- uses wrapOptim() function (see utilLuckModel.r)
# ---------------------------------------------------------------------------- #

if(!isGeneric("unionHdi"))
 setGeneric("unionHdi", function(object, ...) standardGeneric("unionHdi"))

setMethod("unionHdi", "LuckModel", function(object, gamma = 0.95, posterior = FALSE) {
 # check of class of object
 if (class(object) == "LuckModel")
  stop("Union of HD intervals can only be calculated for LuckModel objects\n representing a certain distribution family!")
 # read out object slots
 .n0l <- n0(object)[1] # lower n0
 .n0u <- n0(object)[2] # upper n0
 .ydim <- dim(y0(object))[1]
 .y0l <- y0(object)[,1] # lower bounds are in the first column of y0
 .y0u <- y0(object)[,2] # upper bounds are in the second column of y0
 .tau <- tau(data(object)) # data: tau  (NULL if no data)
 .n   <- n(data(object))   # data: n    (NULL if no data)
 # check for data if posterior requested
 if (posterior && is.null(.tau))
  stop("Object must contain data if union of posterior HD intervals should be calculated")
 # --------------------------------------------------------------------------- #
 # define the optimizer function as needed by optim()
 .optimFunction <- function (.n0y0, .tau, .n, .gamma, #
                             .object, .posterior, .lower){
  # unwrap parameter vector over which is optimized (first argument!)
  .n0 <- .n0y0[1]
  .y0 <- .n0y0[2]
  # compute parameters of posterior if requested
  if (.posterior) {
   .usedn <- updateLuckN(n0=.n0, n=.n)
   .usedy <- updateLuckY(n0=.n0, y0=.y0, tau=.tau, n=.n)
  } else {
   .usedn <- .n0
   .usedy <- .y0
  }
  # return lower / upper value as computed by class-specific singleHdi()
  if (.lower) {
   return(singleHdi(object = .object, n = .usedn, y = .usedy, gamma = .gamma)[1])
  } else {
   return(singleHdi(object = .object, n = .usedn, y = .usedy, gamma = .gamma)[2])
  }
 } # end of optimizer function 
 # --------------------------------------------------------------------------- #
 # lowest lower border of H(P)D's via wrapOptim():
 .lowlowOpt <- wrapOptim(par = c(.n0l, .y0l),  # initial value for .n0y0: one corner of the prior set
                         fn = .optimFunction,  # function to be minimized over its first argument
                         # method = "L-BFGS-B",  # use method with box-constraints (default in wrapOptim)
                         lower = c(.n0l, .y0l), # lower bound for parameter vector n0y0
                         upper = c(.n0u, .y0u), # upper bound for parameter vector n0y0
                         # -> these two bounds span the rectangular set over which is optimized
                         .tau = .tau, .n = .n,   # \
                         .gamma = gamma,         #  arguments forwarded to
                         .object = object,       #  .optimFunction
                         .posterior = posterior, #  (besides first argument)
                         .lower = TRUE)          # /
 # highest upper border of H(P)D's via wrapOptim():
 .highupOpt <- wrapOptim(par = c(.n0l, .y0u),  # initial value for .n0y0: one corner of the prior set
                         fn = .optimFunction,  # function to be maximized over its first argument
                         # method = "L-BFGS-B",  # use method with box-constraints (default in wrapOptim)
                         control = list(fnscale=-1), # to make optim() maximize
                         lower = c(.n0l, .y0l), # lower bound for parameter vector n0y0
                         upper = c(.n0u, .y0u), # upper bound for parameter vector n0y0
                         # -> these two bounds span the rectangular set over which is optimized
                         .tau = .tau, .n = .n,   # \
                         .gamma = gamma,         #  arguments forwarded to
                         .object = object,       #  .optimFunction
                         .posterior = posterior, #  (besides first argument)
                         .lower = FALSE)         # /
 # extract information from objects returned by wrapOptim():
 # parameters at which extremes are obtained
 if (!posterior) { # prior HD interval: parameters from the wrapOptim() results
  .lowpars = list (n = .lowlowOpt$par[1], y = .lowlowOpt$par[2])
  .upppars = list (n = .highupOpt$par[1], y = .highupOpt$par[2])
 } else { # posterior HD interval: optimization was done over the (rectangle)
          # prior parameter set, so parameters from wrapOptim() results must be updated
  .lowpars = list (n = updateLuckN(n0 = .lowlowOpt$par[1], n = .n), #
                   y = updateLuckY(n0 = .lowlowOpt$par[1], #
                                   y0 = .lowlowOpt$par[2], tau = .tau, n = .n)) 
  .upppars = list (n = updateLuckN(n0 = .highupOpt$par[1], n = .n), #
                   y = updateLuckY(n0 = .highupOpt$par[1], #
                                   y0 = .highupOpt$par[2], tau = .tau, n = .n))
 }
 .returnObj <- list (borders = c(.lowlowOpt$value,   # borders of the union
                                 .highupOpt$value),  # of HD intervals
                     lowpars = .lowpars, #
                     upppars = .upppars)
 .returnObj
})

# Test: see distribution-specific definition of singleHdi()

#