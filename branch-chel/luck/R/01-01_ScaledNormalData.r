# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#
# S4 implementation of generalized iLUCK models
# -- Classes for inference on data from a scaled normal N(mu,1)  --
#
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# Class for data: ScaledNormalData
# ---------------------------------------------------------------------------- #

# data class definition
setClass ("ScaledNormalData",
          contains = "LuckModelData")

# constructor function handling as input
#    an object of class LuckModelData (or some subclass)
# or an observed data vector
# or an observed mean and sample size
# or a mean and sample size to simulate data according to.
ScaledNormalData <- function (arg1 = NULL, arg2 = NULL, data = NULL,
                              mean = NULL, n = NULL, sim = FALSE){
 if (all(is.null(c(arg1, arg2, data, mean, n)))) {
  stop("No arguments given for ScaledNormalData()!")
 } else {
  # if arg1 is a LuckModelData object, turn it into a ScaledNormalData object directly
  if (is(arg1, "LuckModelData")) {
   return (new("ScaledNormalData", tauN = tauN(arg1), rawData = rawData(arg1)))
  } else {
  # see if a "ready" data vector is given either named or as a single unnamed argument
   if (is.vector(data) | (is.vector(arg1) & is.null(c(arg2, data, mean, n)))) {
    if (is.vector(data))
     arg1 <- data
    return (.ScaledNormalDataVector(data = arg1))
   } else { # no data vector is given   
    # check if some of the named arguments "mean" and "n" are given.
    # if so, write them in arg1 (mean) and arg2 (n)
    # if not, arg1 and arg2 give already the mean and sample size
    if (!is.null(mean) & is.samplesize(n)) { # mean and n are given
     arg1 <- mean  
     arg2 <- n
    }
    if (is.null(mean) & is.samplesize(n)) { # unnamed argument in arg1 is mean
     arg2 <- n     
    }
    if (!is.null(mean) & is.null(n)) {  # unnamed argument in arg1 is n
     arg2 <- arg1 
     arg1 <- mean
    }
    # check (again) if arg1 and arg2 are ok
    if (!is.vector(arg1))
     stop("mean must be a single value or, if used to simulate an inhomogeneous sample, a vector.")
    if (!is.samplesize(arg2))
     stop("n must be a proper sample size, i.e., a positive integer.")
    # take mean and n like this or simulate data according to them
    if (sim)
     return (.ScaledNormalDataSimulate(mean = arg1, n = arg2))
    else
     return (.ScaledNormalDataMeanN(mean = arg1, n = arg2))
   }
  } 
 }
}


# constructor function if an observed mean and a sample size is given:
.ScaledNormalDataMeanN <- function (mean, n) {
 if (length(mean) != 1)
  stop("Observed mean must be a single value.")
 # create tauN with LuckModelData constructor function
 .tauN <- tauN(LuckModelData(tau = mean*n, n = n))
 # create the object with rawdata = NULL
 new("ScaledNormalData", tauN = .tauN, rawData = NULL)
}

# constructor function if an observed data vector is given:
.ScaledNormalDataVector <- function (data){
 # create tauN with LuckModelData constructor function
 .tauN <- tauN(LuckModelData(tau = sum(data), n = length(data)))
 # create the object with rawdata = data
 new("ScaledNormalData", tauN = .tauN, rawData = matrix(data, ncol = 1))
}

# constructor function if data is simulated:
.ScaledNormalDataSimulate <- function (mean, n){
 # simulate data from a scaled normal distribution
 .data <- rnorm (n, mean = mean, sd = 1)
 # use then the data constructor function
 .ScaledNormalDataVector(data = .data)
}


# accessor methods are inherited from LuckModelData 

# replacement method for slot rawData
# (other replacement methods are inherited from LuckModelData)
if(!isGeneric("rawData<-"))
 setGeneric("rawData<-", function(object, value, ...) standardGeneric("rawData<-"))
setReplaceMethod("rawData", "ScaledNormalData", function(object, value){
 # create entirely new object with the new data
 ScaledNormalData(data = value)
})


# show method for class ScaledNormalData (no plot method)
setMethod("show", "ScaledNormalData", function(object){
 if (is.null(tauN(object))) {
  cat("Default data object with no data specified.\n")
 } else {
  .rawData <- rawData(object)
  .tau <- tau(object)
  .n <- n(object)
  if (is.null(.rawData)) {
   cat("ScaledNormalData object containing a mean of", .tau/.n, "for sample size", .n, ".\n")
  } else { # object has rawData
#   # test if tau or n fit to rawData  
#   if (length(.rawData) == .n & sum(.rawData) == .tau) {
    cat("ScaledNormalData object containing data of sample size", .n, "\n",
        "with mean", .tau/.n, "and variance", var(.rawData), ".\n")
#   } else { # tau or n do not fit to rawData!
#    cat("ScaledNormalData object where data slot and sample statistic slot are inconsistent!\n",
#        " data slot:             mean =", mean(.rawData), ", n =", length(.rawData), "\n",
#        " sample statistic slot: mean =", .tau/.n,        ", n =", .n, "\n",
#        "Do not manipulate sample statistic slots when object contains a data vector.\n")
#   }
  }
 }
})


#