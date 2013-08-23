# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#
# S4 implementation of generalized iLUCK models
# -- Classes for inference on data from a scaled normal N(mu,1)  --
#
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# Class for model: ScaledNormalLuckModel
# ---------------------------------------------------------------------------- #

# class definition: extends class LuckModel
setClass ("ScaledNormalLuckModel",
          contains = "LuckModel")

# ---------------------------------------------------------------------------- #
# Constructor function for ScaledNormalLuckModel
# ---------------------------------------------------------------------------- #

# arguments may either be the arguments for LuckModel()
# or an object of class LuckModel()
ScaledNormalLuckModel <- function (arg1 = NULL, n0 = NULL, y0 = NULL, data = new("ScaledNormalData")){
 if (all(is.null(c(arg1, n0, y0)))) {
  stop("No arguments given for ScaledNormalLuckModel()!")
 } else {
  # .data will stay a default object if no data argument was given.
  .data <- ScaledNormalData(data)
  # if arg1 is not a LuckModel, see if n0 and y0 are given.
  # If so, read out n0 and y0.
  if (!is(arg1, "LuckModel")){
   if (any(is.null(c(n0, y0))))
    stop("To define a ScaledNormalLuckModel both arguments n0 and y0 must be given!")
   .n0 <- n0
   .y0 <- y0
  } else { # if arg1 is a LuckModel, read out its slots
   .n0 <- n0(arg1)
   .y0 <- y0(arg1)
   if (!is.null(tauN(data(arg1)))) { # LuckModel contains data    
    if (!is.null(tauN(.data))) # data was given also in the constructor call
     stop("Object to be converted contains already some data.\n Replace this data later if it should be changed.")
    # overwrite default data object with data slot from arg1
    .data <- ScaledNormalData(data(arg1))
   }
  }
  # build a LuckModel object from the slots for checking inputs
  .object <- LuckModel(n0 = .n0, y0 = .y0, data = .data)
  # check if y0 is one-dimensional
  if (dim(y0(.object))[1] != 1)
   stop("For inference on data from a scaled normal, y0 must be one-dimensional!")
  # now create the ScaledNormalLuckModel object from the LuckModel object
  new("ScaledNormalLuckModel", n0 = n0(.object), y0 = y0(.object), data = .data)
 }
}


# accessor and replacement methods are inherited from LuckModel


# ---------------------------------------------------------------------------- #
# show (= print in S3) method for ScaledNormalLuckModel objects                                                             
# ---------------------------------------------------------------------------- #

# helper function to produce ScaledNormal specific text 
.pItextScN <- function(object) {
 .oneNoneY = paste("corresponding to a normal prior\n with mean", #
                   y0(object)[1,1], #                            
                   "and variance", #                             
                   1/n0(object)[1])
 .oneNtwoY = paste("corresponding to a set of normal priors\n with means in [", #
                   y0(object)[1,1], ";", y0(object)[1,2], #                    
                   "] and variance", #                                         
                   1/n0(object)[1])                                  
 .twoNoneY = paste("corresponding to a set of normal priors\n with mean", #
                   y0(object)[1,1], #                                    
                   "and variances in [", #                               
                   min(1/n0(object)[1], 1/n0(object)[2]), ";", #
                   max(1/n0(object)[1], 1/n0(object)[2]), "]")    
 .twoNtwoY = paste("corresponding to a set of normal priors\n with means in [", #                                   
                   y0(object)[1,1], ";", y0(object)[1,2], #                    
                   "] and variances in [", #                                   
                   min(1/n0(object)[1], 1/n0(object)[2]), ";", #
                   max(1/n0(object)[1], 1/n0(object)[2]), "]")    
 .genilucktree(object = object, oneNoneY = .oneNoneY, #
                                oneNtwoY = .oneNtwoY, #
                                twoNoneY = .twoNoneY, #
                                twoNtwoY = .twoNtwoY)
}


# show method uses helper function .showLuckModels (see show method for LuckModel)
setMethod("show", "ScaledNormalLuckModel", function(object){
 .showLuckModels(object = object, #
                 forInference = "for inference from scaled normal data\n", #
                 parameterInterpretation = .pItextScN(object))
})


# ---------------------------------------------------------------------------- #
# singleHdi: function returning highest density intervals for ScaledNormalLuckModel
# ---------------------------------------------------------------------------- #

if(!isGeneric("singleHdi"))
 setGeneric("singleHdi", function(object, ...) standardGeneric("singleHdi"))

# argument object is used only for method dispatching
setMethod("singleHdi", "ScaledNormalLuckModel", function(object, n, y, gamma) {
 # as the normal prior is symmetric, the quantiles can be calculated directly:
 .lqua <- (1-gamma)/2    # lower quantile
 .uqua <- gamma + .lqua  # upper quantile  
 .lhd <- qnorm (.lqua, mean=y, sd=1/sqrt(n)) # lower border
 .uhd <- qnorm (.uqua ,mean=y, sd=1/sqrt(n)) # upper border
 c(.lhd,.uhd) # return the HD interval
})


# ---------------------------------------------------------------------------- #
# singleCdf: function returning cdf values for ScaledNormalLuckModel
# ---------------------------------------------------------------------------- #

if(!isGeneric("singleCdf"))
 setGeneric("singleCdf", function(object, ...) standardGeneric("singleCdf"))

# argument object is used only for method dispatching
setMethod("singleCdf", "ScaledNormalLuckModel", function(object, n, y, x) {
 pnorm (x, mean = y, sd = 1/sqrt(n))
})


#