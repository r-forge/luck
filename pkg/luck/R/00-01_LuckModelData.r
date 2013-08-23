# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#
# S4 implementation of generalized iLUCK models
# -- Class LuckModelData incl. show method --
#
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# Slot Class for Class LuckModelData is OptionalMatrix:
# ---------------------------------------------------------------------------- #

setClassUnion ("OptionalMatrix",
               c("matrix", "NULL"))

# ---------------------------------------------------------------------------- #
# Class for LuckModel data
# ---------------------------------------------------------------------------- #

# Class for Data: LuckModelData
# class definition
setClass ("LuckModelData",
          representation = representation (tauN = "OptionalMatrix",
                                           rawData = "OptionalMatrix"),
          prototype (tauN = NULL, rawData = NULL))

# constructor function if argument is tauN matrix
.LuckModelDataMatrix <- function (tauN) {
 if (dim(tauN)[2] != 2)
  stop(paste("Matrix supplied to create LuckModelData must have two columns with the", #
             "\n  first giving sample statistic(s) tau(x) and the second giving sample size(s) n."))
 if (is.null(colnames(tauN)))
  colnames(tauN) <- c("tau", "n")
 if (any(tauN[,2] <= 0)) stop("n must be strictly positive!")
 return(new("LuckModelData", tauN = tauN))
}

# constructor function if arguments are tau (vector/single value) and n (vector/single value)
.LuckModelDataVectors <- function (tau, n) {
 if (length(tau) == 1) { # tau is onedimensional
  if (length(n) != 1){
   stop("If tau is one-dimensional, n must be one-dimensional, too.")
  } else { # n fits to tau
   .tauN <- matrix(c(tau, n), ncol = 2, byrow = TRUE)
  }
 } else { # tau is multidimensional
  if (length(n) == length(tau)){
   .tauN <- matrix(c(tau, n), ncol = 2)
  } else { # n has not the same dimension as tau
   if (length(n) == 1) { # single value for n supplied: repeat for each dimension of tau
    .tauN <- matrix(c(tau, rep(n, length(tau))), ncol = 2)
   } else { # n has not the same dimension as tau and is not a single value
    stop("n must either have the same dimension as tau or be a single value.")
   }
  }
 }
 if (any(.tauN[,2] <= 0)) stop("n must be strictly positive!")
 colnames(.tauN) <- c("tau", "n")
 new("LuckModelData", tauN = .tauN)
}

# general constructor function handling input types list, matrix and two vectors
LuckModelData <- function (arg1 = NULL, arg2 = NULL, tau = NULL, n = NULL) {
 if (all(is.null(c(arg1, arg2, tau, n)))) {
  stop('No arguments given for LuckModelData()!\n Construct a default LuckModelData object by new("LuckModelData") if you must.')
 } else {
  # check if the "named" arguments "tau" and "n" are given as vectors, if so put them in arg1 and arg2
  if (is.vector(tau) & is.vector(n)) { # both "named" arguments are given as vectors
   arg1 <- tau
   arg2 <- n
  }
  if (is.vector(tau) & is.null(n)) { # "tau" is given as vector, thus the unnamed n
   .n <- arg1                        # argument is in arg1
   arg1 <- tau
   arg2 <- .n
  }
  if (is.null(tau) & is.vector(n)) { # "n" is given as vector, thus the unnamed tau
   arg2 <- n                        # argument is already in arg1
  }
  # end of named argument treatment
  # check if arg1 is a list, if so put list element(s) in arg1 (and arg2)
  if (is.list(arg1)) {
   if (length(arg1) > 2 | length(arg1) == 0) {
    stop("List argument for LuckModelData() may have only one or two elements.")
   } else {
    .list <- arg1
    if (is.null(names(.list))) { # no names in the list
     arg1 <- .list[[1]]
     if (length(.list) == 2)
      arg2 <- .list[[2]]
    } else { # some names in the list
     # both right names are in the list
     if (all(names(.list) == c("tau", "n")) | all(names(.list) == c("n", "tau") )) {
      arg1 <- .list$tau
      arg2 <- .list$n
     } else { # only one of the right names ("tau" and "n") are in the list
      if (any(names(.list) == c("tau"))) { # "tau" is given
       arg1 <- .list$tau
       arg2 <- .list$"" # unnamed list element is taken as n (arg2 will be NULL
                        # if second list element has some wrong name)
      }
      if (any(names(.list) == c("n"))) { # "n" is given
       arg2 <- .list$n
       arg1 <- .list$"" # unnamed list element is taken as tau (arg2 will be NULL
                        # if other list element has some wrong name)
      }
     }
    } # end of named list processing
   } # end of 1/2 element list processing
  } # end of list treatment
  # now check if arg1 and arg2 are two vectors or a matrix and NULL
  # two vectors given
  if (is.vector(arg1) & is.vector(arg2)) {
   return(.LuckModelDataVectors(arg1, arg2))
  } else {
   # one matrix given
   if (is.matrix(arg1) & is.null(arg2)) {
    return(.LuckModelDataMatrix(arg1))
   } else {
    stop(paste("LuckModelData() needs either a matrix or (a list of) two vectors", #
               "\n  giving sample statistic tau(x) and sample size n."))
   }
  } # end of vector or matrix treatment
 } # end of non-NULL arguments treatment
} # end of general constructor function


# accessor methods

if(!isGeneric("tauN"))
 setGeneric("tauN", function(object, ...) standardGeneric("tauN"))
setMethod("tauN", "LuckModelData", function(object) object@tauN)

if(!isGeneric("tau"))
 setGeneric("tau", function(object, ...) standardGeneric("tau"))
setMethod("tau", "LuckModelData", function(object) object@tauN[,1])

if(!isGeneric("n"))
 setGeneric("n", function(object, ...) standardGeneric("n"))
setMethod("n", "LuckModelData", function(object) object@tauN[,2])

if(!isGeneric("rawData"))
 setGeneric("rawData", function(object, ...) standardGeneric("rawData"))
setMethod("rawData", "LuckModelData", function(object) object@rawData)


# replacement methods

if(!isGeneric("tauN<-"))
 setGeneric("tauN<-", function(object, ...) standardGeneric("tauN<-"))
setReplaceMethod("tauN", "LuckModelData", function(object, value){
 # tauN shuld not be changed if there is some raw data in the object
 if (!is.null(rawData(object)))
  stop("Do not replace (parts of) the sample statistic slot tauN\n", #
       "when object contains a data vector!\n Create a new data object instead.")
 # to sanitize replacement input of tauN, use LuckModelData constructor function       
 .object <- LuckModelData(value) # created object of class LuckModelData
                                 # has again NULL at slot rawData
 # if object was not of class LuckModelData, it should be converted to its
 # class again by the use of the respective construction class that can have
 # a LuckModelData object as argument
 if (class(object) != "LuckModelData")
  .object <- do.call(class(object), list(.object))
 .object                     
})

if(!isGeneric("tau<-"))
 setGeneric("tau<-", function(object, ...) standardGeneric("tau<-"))
setReplaceMethod("tau", "LuckModelData", function(object, value){
 tauN(object)[, 1] <- value
 object
})

if(!isGeneric("n<-"))
 setGeneric("n<-", function(object, ...) standardGeneric("n<-"))
setReplaceMethod("n", "LuckModelData", function(object, value){
 tauN(object)[, 2] <- value
 object
})


# show method for class LuckModelData (no plot method!)

setMethod("show", "LuckModelData", function(object){
 if (is.null(tauN(object))) {
  cat("Default data object with no data specified.\n")
 } else {
  .dim <- length(tau(object))
  .dimdata1 <- paste ("data object containing", .dim, "- dimensional data:\n")
  .dimdataj <- function (.j)
   paste("sample statistic tau(x) =", tau(object)[.j], "and sample size n =", n(object)[.j], "for dimension", .j, "\n")
  if (.dim == 1) {
   cat("data object with sample statistic tau(x) =", tau(object), "and sample size n =", n(object), "\n")
  } else {
   cat(.dimdata1, .dimdataj(1:.dim), "\n")
  }
 }
})


#