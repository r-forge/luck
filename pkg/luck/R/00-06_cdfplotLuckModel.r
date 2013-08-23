# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#
# S4 implementation of generalized iLUCK models
# -- method to plot cumulative density functions --
#
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### presently suitable for one-dimensional densities only


# ---------------------------------------------------------------------------- #
# cdfplot method for LuckModel objects
# - can not be used for "plain" LuckModel objects, but for objects from all
#   subclasses
# - uses the same list of controls as plot method for LuckModel objects
#   (some switches like 'rectangle' are ignored, may be added in the future)
# - xvec: either a sequence to plot over or the number of points to plot over,
#   with default = 100. cdfplot() determines the plotting region by determining
#   the union of highest density intervals covering a probability of 1-epsilon,
#   where epsilon defaults to 1e-5.
# ---------------------------------------------------------------------------- #

if(!isGeneric("cdfplot"))
 setGeneric("cdfplot", function(object, ...) standardGeneric("cdfplot"))

setMethod("cdfplot", "LuckModel", function(object, xvec = 100, epsilon = 1e-5, #
                                           control = controlList(), ylim = c(0,1), vertdist = TRUE, ...) {
 # check of class of object
 if (class(object) == "LuckModel")
  stop("cdfs can only be plotted for LuckModel objects\n representing a model for a certain distribution!")
 # check of dimension of y0
 if (dim(y0(object))[1] != 1)
   stop("cdfplot() is suitable for one-dimensional densities only!")
 # read out object slots
 .n0l <- n0(object)[1] # lower n0
 .n0u <- n0(object)[2] # upper n0
 .ydim <- dim(y0(object))[1]
 .y0l <- y0(object)[,1] # lower bounds are in the first column of y0
 .y0u <- y0(object)[,2] # upper bounds are in the second column of y0
 .tau <- tau(data(object)) # data: tau  (NULL if no data)
 .n   <- n(data(object))   # data: n    (NULL if no data)
 # check for data if posterior requested
 if (control$posterior && is.null(.tau))
  stop("Object must contain data if posterior cdfs should be plotted!")
 # check xvec if supplied
 if (any(diff(xvec) < 0)) # also FALSE if xvec is a single number
  stop("xvec must be a vector of ascending values!")
 # if only a number given, take it as length of sequence and calculate
 # appropriate xvec with help from unionHdi;
 # a reversed sequence xvecrev is needed as well
 if (length(xvec) == 1){
  .xveclength <- xvec
  if (!control$posterior) { # prior
   .fromto <- unionHdi(object, gamma = 1-epsilon)$borders
  } else { # posterior
   .fromto <- unionHdi(object, gamma = 1-epsilon, posterior = TRUE)$borders
  }
  xvec <- seq(from = .fromto[1], to = .fromto[2], length.out = .xveclength)
  xvecrev <- seq(from = .fromto[2], to = .fromto[1], length.out = .xveclength)
 } else { # supplied xvec might not be equally spaced
  .xveclength <- length(xvec)
  xvecrev <- rep(NA, times = .xveclength)
  for (i in 1:.xveclength) xvecrev[i] <- xvec[.xveclength-i+1]
 }
 # --------------------------------------------------------------------------- #
 # define the optimizer function as needed by optim() (or wrapOptim())
 .optimFunction <- function (.n0y0, .tau, .n, .object, .x, .posterior){
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
  # return value of cdf as computed by class-specific singleCdf()
  singleCdf(object = .object, n = .usedn, y = .usedy, x = .x)
 } # end of optimizer function
 # --------------------------------------------------------------------------- #
 # calculate lower and upper values of cdfs (to be plotted with xvec and xvecrev, respectively)
 .yvec <- rep(NA, times = .xveclength) -> .yvecrev
 #.xvecrevy <- rep(NA, times = .xveclength)
 for (i in 1:.xveclength){
  .yvec[i] <- wrapOptim(par = c(.n0l, .y0l),  # initial value for .n0y0: one corner of the prior set
                        fn = .optimFunction,  # function to be minimized over its first argument
                        # method = "L-BFGS-B",  # use method with box-constraints (default in wrapOptim)
                        lower = c(.n0l, .y0l), # lower bound for parameter vector n0y0
                        upper = c(.n0u, .y0u), # upper bound for parameter vector n0y0
                        # -> these two bounds span the rectangular set over which is optimized
                        .tau = .tau, .n = .n,         # \
                        .object = object,             #  arguments forwarded to .optimFunction
                        .x = xvec[i],                 #  (besides first argument)
                        .posterior = control$posterior)$value # /
  .yvecrev[i] <- wrapOptim(par = c(.n0l , .y0l),  # initial value for .n0y0: one corner of the prior set
                           fn = .optimFunction,  # function to be maximized over its first argument
                           # method = "L-BFGS-B",  # use method with box-constraints (default in wrapOptim)
                           control = list(fnscale=-1), # to maximize
                           lower = c(.n0l, .y0l), # lower bound for parameter vector n0y0
                           upper = c(.n0u, .y0u), # upper bound for parameter vector n0y0
                           # -> these two bounds span the rectangular set over which is optimized
                           .tau = .tau, .n = .n,         # \
                           .object = object,             #  arguments forwarded to .optimFunction
                           .x = xvecrev[i],              #  (besides first argument)
                           .posterior = control$posterior)$value # /
 }
 # make an empty plot, because polygon() is not a first-level plot function
 plot(x = xvec, y = rep(0, times = .xveclength), #
      type = "n", xlab = "", ylab = "", col = 1, ylim = ylim, ...)
 # plot the shaded area
 polygon (c(xvec,  xvecrev,  xvec[1]),  # x coordinates
          c(.yvec, .yvecrev, .yvec[1]), # y coordinates
          col = control$polygonCol, border = control$borderCol,
          density = control$density, angle = control$angle, ...)
 if(vertdist){
  if (!control$posterior) { # prior
   lines(xvec, singleCdf(object = object, n = .n0l, y = .y0l, x = xvec))
   lines(xvec, singleCdf(object = object, n = .n0u, y = .y0l, x = xvec))
   lines(xvec, singleCdf(object = object, n = .n0l, y = .y0u, x = xvec))
   lines(xvec, singleCdf(object = object, n = .n0u, y = .y0u, x = xvec))
  } else { # posterior
   lines(xvec, singleCdf(object = object, n = updateLuckN(.n0l, .n), #
                                          y = updateLuckY(.n0l, .y0l, .tau, .n), x = xvec))
   lines(xvec, singleCdf(object = object, n = updateLuckN(.n0u, .n), #
                                          y = updateLuckY(.n0u, .y0l, .tau, .n), x = xvec))
   lines(xvec, singleCdf(object = object, n = updateLuckN(.n0l, .n), #
                                          y = updateLuckY(.n0l, .y0u, .tau, .n), x = xvec))
   lines(xvec, singleCdf(object = object, n = updateLuckN(.n0u, .n), #
                                          y = updateLuckY(.n0u, .y0u, .tau, .n), x = xvec))
  }
 }
 # TODO:
 # make polygon optional?
 # annotate
 # rect? to demonstrate loss of precision?
 # numbers?
})

# Test: see distribution-specific definition of singleCdf()

#
