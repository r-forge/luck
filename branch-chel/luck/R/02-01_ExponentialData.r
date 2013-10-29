# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#
# S4 implementation of generalized iLUCK models
# -- Classes for inference on data from an exponential distribution  --
#
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Class for data: ExponentialData
# ---------------------------------------------------------------------------- #

# data class definition
setClass ("ExponentialData",
		contains = "LuckModelData")

# constructor function handling as input
#    an object of class LuckModelData (or some subclass)
# or an observed data vector
# or an observed mean and sample size
# or a mean and sample size to simulate data according to.
ExponentialData <- function (arg1 = NULL, arg2 = NULL, data = NULL,
		mean = NULL, n = NULL, sim = FALSE){
	if (all(is.null(c(arg1, arg2, data, mean, n)))) {
		stop("No arguments given for ExponentialData()!")
	} else {
		# if arg1 is a LuckModelData object, turn it into an 
		# ExponentialData object directly
		if (is(arg1, "LuckModelData")) {
      if (!is.null(tau(arg1)) && (tau(arg1) <= 0 | length(tau(arg1)) != 1)) {
        stop("For an ExponentialData object, tau must be one-dimensional,
             and tau and/or data must be stricly larger than 0!")
      } else {
        return (new("ExponentialData", 
                    tauN = tauN(arg1), rawData = rawData(arg1)))
      }
		} else {
			# see if a "ready" data vector is given 
			# either named or as a single unnamed argument
			if (is.vector(data) | (is.vector(arg1) & is.null(c(arg2, 
										data, mean, n)))) {
				if (is.vector(data))
					arg1 <- data
        if (any(arg1 < 0)) {
          stop("Data for ExponentialData object must be strictly positive!")
        } else {
          return (.ExponentialDataVector(data = arg1))          
        }
			} else { # no data vector is given   
				# check if some of the named arguments "mean" and "n" are given.
				# if so, write them in arg1 (mean) and arg2 (n)
				# if not, arg1 and arg2 give already the mean and sample size
				if (!is.null(mean) & is.samplesize(n)) { # mean and n are given
				
					arg1 <- mean  
					arg2 <- n
				}
				if (is.null(mean) & is.samplesize(n)) { # unnamed argument 
					# in arg1 is mean
					arg2 <- n     
				}
				if (!is.null(mean) & is.null(n)) { # unnamed argument 
					# in arg1 is n
					arg2 <- arg1 
					arg1 <- mean
				}
				# check (again) if arg1 and arg2 are ok
				if (!is.vector(arg1))
					stop("mean must be a single value or, if used to simulate 
					an inhomogeneous sample, a vector.")
				if (any(arg1 < 0))
				  stop("mean(s) for ExponentialData must be strictly positive!")
				if (!is.samplesize(arg2))
					stop("n must be a proper sample size, i.e., 
					a positive integer.")
				# take mean and n like this or simulate data according to them
				if (sim)
					return (.ExponentialDataSimulate(mean = arg1, n = arg2))
				else
					return (.ExponentialDataMeanN(mean = arg1, n = arg2))
			}
		} 
	}
}


# constructor function if an observed mean and a sample size is given:
.ExponentialDataMeanN <- function (mean, n) {
	if (length(mean) != 1)
		stop("Observed mean must be a single value.")
	# create tauN with LuckModelData constructor function
	.tauN <- tauN(LuckModelData(tau = mean*n, n = n))
	# create the object with rawdata = NULL
	new("ExponentialData", tauN = .tauN, rawData = NULL)
}

# constructor function if an observed data vector is given:
.ExponentialDataVector <- function (data){
	# create tauN with LuckModelData constructor function
	.tauN <- tauN(LuckModelData(tau = sum(data), n = length(data)))
	# create the object with rawdata = data
	new("ExponentialData", tauN = .tauN, rawData = matrix(data, ncol = 1))
}

# constructor function if data is simulated:
.ExponentialDataSimulate <- function (mean, n){
	# simulate data from an exponential distribution
	.data <- rexp (n, rate = 1/mean)
	# use then the data constructor function
	.ExponentialDataVector(data = .data)
}


# accessor methods are inherited from LuckModelData 

# replacement method for slot rawData
# (other replacement methods are inherited from LuckModelData)
if(!isGeneric("rawData<-"))
	setGeneric("rawData<-", function(object, value, ...) standardGeneric("rawData<-"))
setReplaceMethod("rawData", "ExponentialData", function(object, value){
			# create entirely new object with the new data
			ExponentialData(data = value)
		})


# show method for class ExponentialData 
setMethod("show", "ExponentialData", function(object){
		# must hold: alpha, beta > 0
			.rawData <- rawData(object)
			.tau <- tau(object)
			.n <- n(object)
			if (is.null(.rawData)) {
			cat("ExponentialData object containing a mean of", .tau/.n,
							"for sample size", .n, ".\n")
			} else { # object has rawData
				cat("ExponentialData object containing data of sample size"
						, .n, "\n",
						"with mean", .tau/.n, "and variance", var(.rawData),
						".\n")
			}
			
		})

#