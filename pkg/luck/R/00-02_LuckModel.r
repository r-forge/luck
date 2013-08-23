# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#                                                 
# S4 implementation of generalized iLUCK models   
# -- Class LuckModel incl. show method --             
#                                                 
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# Class LuckModel                                                              
# ---------------------------------------------------------------------------- #

# class definition
setClass ("LuckModel",
          representation = representation (n0 = "matrix",
                                           y0 = "matrix",
                                           data = "LuckModelData"))

# constructor function
LuckModel <- function (n0, y0, data = new("LuckModelData")){
 # feeding n0:
 # n0 (the parameter giving the prior strength) should be a (1x2)-matrix,
 # with first element the lower bound and second element the upper bound. If a
 # single value is given (i.e. an iLUCK model is specified), it is replicated. 
 if (is.matrix(n0)) {
  .n0 <- n0
 } else {
  .n0 <- matrix(n0, ncol = 2, byrow = TRUE)
 }
 if (any(dim(.n0) != c(1,2)))
  stop ("\n n0 must either be a single value or have two elements forming an interval!")
 if (.n0[1,1] > .n0[1,2])
  stop ("\n Value of lower n0 larger than value of upper n0!")
 colnames(.n0)<- c("lower", "upper")
 # feeding y0:
 # y0 (the main prior parameter) should be a (px2)-matrix, with first
 # column giving the lower bound(s) and second column giving the upper bound(s)
 # of dimensions 1...p. If a single value is given, it is replicated. 
 # (e.g., if also n0 was not an interval, then a LUCK model would be specified)
 if (is.matrix(y0)) {
  .y0 <- y0
 } else {
  if ((floor(length(y0)/2) != length(y0)/2) & length(y0) != 1)
   stop ("\n y must either be a single value or have an even number of elements!")
  .y0 <- matrix(y0, ncol = 2)
 }
 if (any(.y0[,1] > .y0[,2]))
  stop (paste("\n Value of lower y0 larger than value of upper y0!", #
              "\n * If y0 supplied as matrix, then the first column must give lower bounds", #
              "\n   and the second column must give upper bounds.", #
              "\n * If y0 supplied as a single vector, vector must contain first the lower", #
              "\n   bounds for all dimensions and then the upper bounds for all dimensions."))
 colnames(.y0) <- c("lower", "upper")
 # feeding data:
 # Create the LuckModelData object from data if data is not already a LuckModelData
 # object (which is the case if no data was given, as then a LuckModelData
 # prototype has already been created in the function call).        
 # Then, if the LuckModelData object actually contains data, it is checked if
 # the dimension of tauN fits to the dimension of y0.
 # data may thus be all that LuckModelData() can eat, presumably a matrix or a list
 # of two vectors giving tau and n. 
 if (is(data, "LuckModelData"))
  .data <- data
 else
  .data <- LuckModelData(data)
 if (!is.null(tauN(.data)) && dim(tauN(.data))[1] != dim(.y0)[1]) 
  stop(paste("y0 and LuckModelData object do not fit together!",
             "\n  tau and y0 must have the same dimension."))
 # create LuckModel object 
 new("LuckModel", n0 = .n0, y0 = .y0, data = .data) 
}


# ---------------------------------------------------------------------------- #
# Access methods for LuckModel objects                                                             
# ---------------------------------------------------------------------------- #

if(!isGeneric("n0"))
 setGeneric("n0", function(object, ...) standardGeneric("n0"))
setMethod("n0", "LuckModel", function(object) object@n0)

if(!isGeneric("y0"))
 setGeneric("y0", function(object, ...) standardGeneric("y0"))
setMethod("y0", "LuckModel", function(object) object@y0)

if(!isGeneric("data"))
 setGeneric("data", function(object, ...) standardGeneric("data"))
setMethod("data", "LuckModel", function(object) object@data)


# ---------------------------------------------------------------------------- #
# Replacement methods for LuckModel objects                                                             
# ---------------------------------------------------------------------------- #

if(!isGeneric("n0<-"))
 setGeneric("n0<-", function(object, ...) standardGeneric("n0<-"))
setReplaceMethod("n0", "LuckModel", function(object, value){
 .y0 <- y0(object)
 .data <- data(object)
 do.call(class(object), list(n0 = value, y0 = .y0, data = .data))
# LuckModel(n0 = value, y0 = .y0, data = .data)
})

if(!isGeneric("y0<-"))
 setGeneric("y0<-", function(object, ...) standardGeneric("y0<-"))
setReplaceMethod("y0", "LuckModel", function(object, value){
 .n0 <- n0(object)
 .data <- data(object)
 do.call(class(object), list(n0 = .n0, y0 = value, data = .data))
# LuckModel(n0 = .n0, y0 = value, data = .data)
})

if(!isGeneric("data<-"))
 setGeneric("data<-", function(object, ...) standardGeneric("data<-"))
setReplaceMethod("data", "LuckModel", function(object, value){
 .n0 <- n0(object)
 .y0 <- y0(object)
 do.call(class(object), list(n0 = .n0, y0 = .y0, data = value))
# LuckModel(n0 = .n0, y0 = .y0, data = value)
})


# ---------------------------------------------------------------------------- #
# show (= print in S3) method for LuckModel objects                                                             
# ---------------------------------------------------------------------------- #

# function for case differentiation gen./i/luck-model
.genilucktree <- function (object, oneNoneY, oneNtwoY, twoNoneY, twoNtwoY) {
 if (n0(object)[1] == n0(object)[2]) {         # single n
  if (all(y0(object)[,1] == y0(object)[,2])) { # single n, single y
   oneNoneY
  } else {                                     # single n, interval-valued y
   oneNtwoY
  } 
 } else {                                      # interval-valued n
  if (all(y0(object)[,1] == y0(object)[,2])) { # interval-valued n, single y
   twoNoneY
  } else {                                     # interval-valued n, interval-valued y
   twoNtwoY
  }
 }
}


# workhorse function (also usable by subclasses)
.showLuckModels <- function (object, forInference = "", parameterInterpretation = NULL) {
 # define elementary text modules
 .geniluck  <- function (forInference)
                paste("generalized iLUCK model ", forInference, "with prior parameter set:", sep = "")
 .iluck     <- function (forInference)
                paste("iLUCK model ", forInference, "with prior parameter set:", sep = "")
 .luck      <- function (forInference)
                paste("LUCK model ", forInference, "with prior parameters:", sep = "")
 .twoN      <- paste ("\n  lower n0 =", n0(object)[1],"  upper n0 =", n0(object)[2])
 .oneN      <- paste ("\n  n0 =", n0(object)[1])
 .ydim      <- dim(y0(object))[1]
 .twoY      <- function (.dim) {
  if (.ydim == 1) paste ("\n  lower y0 =", y0(object)[.dim,1],"  upper y0 =", y0(object)[.dim,2])
  else paste ("\n  lower y0 =", y0(object)[.dim,1],"  upper y0 =", y0(object)[.dim,2], " for dimension ", .dim)
 }
 .oneY      <- function (.dim) {
  if (.ydim == 1) paste ("\n  y0 =", y0(object)[.dim,1])
  else paste ("\n  y0 =", y0(object)[.dim,1], " for dimension ", .dim)
 }
 .imprec    <- function (.dim) {
  if (.ydim == 1) paste ("\n giving a main parameter prior imprecision of", y0(object)[.dim,2]-y0(object)[.dim,1])
  else paste ("\n giving a main parameter prior imprecision of", y0(object)[.dim,2]-y0(object)[.dim,1], " for dimension", .dim)
 }
 # data switch
 .hasdata <- !is.null(tauN(data(object)))
 # parameterInterpretation switch
 .pI <- !is.null(parameterInterpretation)
 # arguments for the case differentiation function
 .oneNoneY <- function (...) { 
   if (!.hasdata) {                   # without data
    cat(.luck(forInference), .oneN, .oneY(1:.ydim), "\n")
    if (.pI) cat(parameterInterpretation, "\n")
   } else {                           # with data
    cat(.luck(forInference), .oneN, .oneY(1:.ydim), "\n")
    if (.pI) cat(parameterInterpretation, "\n")
    cat("and ")
    show(data(object))
   } 
 }
 .oneNtwoY <- function (...) {
   if (!.hasdata) {                   # without data
    cat(.iluck(forInference), .oneN, .twoY(1:.ydim), .imprec(1:.ydim), "\n")
    if (.pI) cat(parameterInterpretation, "\n")
   } else {                           # with data
    cat(.iluck(forInference), .oneN, .twoY(1:.ydim), .imprec(1:.ydim), "\n")
    if (.pI) cat(parameterInterpretation, "\n")
    cat("and ")
    show(data(object))
   }
 }
 .twoNoneY <- function (...) {
   if (!.hasdata) {                   # without data
    cat(.geniluck(forInference), .twoN, .oneY(1:.ydim), "\n")
    if (.pI) cat(parameterInterpretation, "\n")
   } else {                           # with data
    cat(.geniluck(forInference), .twoN, .oneY(1:.ydim), "\n")
    if (.pI) cat(parameterInterpretation, "\n")
    cat("and ")
    show(data(object))
   }
 }
 .twoNtwoY <- function (...) {
   if (!.hasdata) {                   # without data
    cat(.geniluck(forInference), .twoN, .twoY(1:.ydim), .imprec(1:.ydim), "\n")
    if (.pI) cat(parameterInterpretation, "\n")
   } else {                           # with data
    cat(.geniluck(forInference), .twoN, .twoY(1:.ydim), .imprec(1:.ydim), "\n")
    if (.pI) cat(parameterInterpretation, "\n")
    cat("and ")
    show(data(object))
   }
 }
 # use the case differentiation function
 # to produce the output
 .genilucktree(object = object, oneNoneY = .oneNoneY(), oneNtwoY = .oneNtwoY(), #
                                twoNoneY = .twoNoneY(), twoNtwoY = .twoNtwoY())
}

# actual show method
setMethod("show", "LuckModel", function(object){
 .showLuckModels(object = object)
})


#