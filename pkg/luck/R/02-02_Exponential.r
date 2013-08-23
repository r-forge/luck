# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#
# S4 implementation of generalized iLUCK models
# -- Classes for inference on data from an exponential distribution  --
#
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# Class for model: ExponentialLuckModel
# ---------------------------------------------------------------------------- #

# class definition: extends class LuckModel
setClass ("ExponentialLuckModel",
		contains = "LuckModel")

# ---------------------------------------------------------------------------- #
# Constructor function for ExponentialLuckModel
# ---------------------------------------------------------------------------- #

# arguments may either be the arguments for LuckModel()
# or an object of class LuckModel()
ExponentialLuckModel <- function (arg1 = NULL, n0 = NULL, 
		y0 = NULL, data = new("ExponentialData")){
	if (all(is.null(c(arg1, n0, y0)))) {
		stop("No arguments given for ExponentialLuckModel()!")
	} else {
		# .data will stay a default object if no data argument was given.
		.data <- ExponentialData(data)
		# if arg1 is not a LuckModel, see if n0 and y0 are given.
		# If so, read out n0 and y0.
		if (!is(arg1, "LuckModel")){
			if (any(is.null(c(n0, y0))))
				stop("To define an 
				ExponentialLuckModel both arguments n0 and y0 must be given!")
			.n0 <- n0
			.y0 <- y0
		} else { # if arg1 is a LuckModel, read out its slots
			.n0 <- n0(arg1)
			.y0 <- y0(arg1)
			if (!is.null(tauN(data(arg1)))) { # LuckModel contains data    
				if (!is.null(tauN(.data))) # data was given also in the 
					# constructor call
					stop("Object to be converted 
					contains already some data.\n Replace 
					this data later if it should be changed.")
				# overwrite default data object with data slot from arg1
				.data <- ExponentialData(data(arg1))
			}
		}
		# build a LuckModel object from the slots for checking inputs
		.object <- LuckModel(n0 = .n0, y0 = .y0, data = .data)
		# check if y0 is one-dimensional
		if (dim(y0(.object))[1] != 1)
			stop("For inference on data from an exponential,
			 y0 must be one-dimensional!")
		# y0 and n0 must be positive because the parameter the gamma distr. must
		# be positive
		if (any( c(y0(.object), n0(.object)) <= 0)){
			stop("To define an ExponentialLuckModel 
				both arguments n0 and y0 must be strictly positive!")
		}else{		
		# now create the ExponentialLuckModel object from the LuckModel object
		new("ExponentialLuckModel", n0 = n0(.object), 
				y0 = y0(.object), data = .data)
		}
	}
}


# accessor and replacement methods are inherited from LuckModel

# the conjugate prior for data from an exp(lambda) is a gamma(n0+1, n0*y0)


# ---------------------------------------------------------------------------- #
# show (= print in S3) method for ExponentialLuckModel objects                                                             
# ---------------------------------------------------------------------------- #

# helper function to produce Gamma, specific text
.pItextExp <- function(object) {
	.oneNoneY = paste("corresponding to a Gamma prior\n with alpha =", #
			round(n0(object)[1] + 1, 3), #                            
			"and beta =", #                             
			round(y0(object)[1,1]*n0(object)[1], 3))
	.oneNtwoY = paste("corresponding to set of Gamma priors\n with alpha =", #
			round(n0(object)[1] + 1, 3),                     
			"and beta in [", #                         
			round(y0(object)[1,1]*n0(object)[1], 3), ";",
			round(y0(object)[1,2]*n0(object)[1], 3),
			"]")                         
	.twoNoneY = paste("corresponding to a set of Gamma priors\n with alpha in
			[", #
			round(n0(object)[1] + 1, 3), ";", round(n0(object)[2] + 1, 3), #                                    
			"] and beta in [", # 
			round(y0(object)[1,1]*n0(object)[1], 3),
			";", #
			round(y0(object)[1,1]*n0(object)[2], 3),
			"]")    
	.twoNtwoY = paste(
			"corresponding to a set of Gamma priors\n with alpha in [", #                                   
			round(n0(object)[1] + 1, 3), ";", round(n0(object)[2] + 1, 3), #                    
			"] and beta in [", #                                   
			round(y0(object)[1,1]*n0(object)[1], 3), 
			";", #
			round(y0(object)[1,2]*n0(object)[2], 3),
			"]")    
	.genilucktree(object = object, oneNoneY = .oneNoneY, #
			oneNtwoY = .oneNtwoY, #
			twoNoneY = .twoNoneY, #
			twoNtwoY = .twoNtwoY)
}

# show method uses helper function .showLuckModels 
# (see show method for LuckModel)
setMethod("show", "ExponentialLuckModel", function(object){
			.showLuckModels(object = object, #
					forInference = "for inference from exponential data\n", #
					parameterInterpretation = .pItextExp(object))
		})


# ---------------------------------------------------------------------------- #
# singleHdi: function returning highest density intervals for 
# 				ExponentialLuckModel
# ---------------------------------------------------------------------------- #

if(!isGeneric("singleHdi"))
	setGeneric("singleHdi", function(object, ...) standardGeneric("singleHdi"))

# argument object is used only for method dispatching
#require('TeachingDemos') # Version 2.8, Depends: R (>= 2.10), License: Artistic-2.0
setMethod("singleHdi", "ExponentialLuckModel", function(object, n, y, gamma) {
			qga <- function(x) qgamma(x, n+1, y*n)
			hpd(qga, conf= gamma)
			})


# ---------------------------------------------------------------------------- #
# singleCdf: function returning cdf values for ExponentialLuckModel
# ---------------------------------------------------------------------------- #

if(!isGeneric("singleCdf"))
	setGeneric("singleCdf", function(object, ...) standardGeneric("singleCdf"))

# argument object is used only for method dispatching
setMethod("singleCdf", "ExponentialLuckModel", function(object, n, y, x) {
			pgamma (x, shape= n+1,rate= n*y)
		})


#
