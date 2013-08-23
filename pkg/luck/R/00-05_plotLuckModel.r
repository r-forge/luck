# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#                                                 
# S4 implementation of generalized iLUCK models   
# -- plot method for Class LuckModel --             
#                                                 
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# plot method for LuckModel objects
# ---------------------------------------------------------------------------- #

# helper function to build the list of controls #?? used also for cdfplot() ??
controlList <- function (plotdim = NA, posterior = FALSE, annotate = TRUE, rDigits = 2,
                         numbers = FALSE, rectangle = FALSE, polygonCol = "gray", #"green",
                         borderCol = 1, rectCol = 1, rectLty = 4, numCol = 1, 
                         density = NULL, angle = 45, plotSeqLength = 100) {
 list(plotdim = plotdim, posterior = posterior, annotate = annotate, rDigits = rDigits,
      numbers = numbers, rectangle = rectangle, polygonCol = polygonCol,
      borderCol = borderCol, rectCol = rectCol, rectLty = rectLty, numCol = numCol,
      density = density, angle = angle, plotSeqLength = plotSeqLength)
}

# plot method that calls the workhorse plot function once or several times
# depending on dimension of y; use plotdim in controlList to choose the dimension
setMethod("plot", c("LuckModel", "missing"), function(x, y, control = controlList(), add = FALSE, ...){
 # x is the LuckModel object, y is not needed (and therefore of class "missing")
 .object <- x
 # unwrap LuckModel object
 .n0l <- n0(.object)[1] # lower n0
 .n0u <- n0(.object)[2] # upper n0
 .ydim <- dim(y0(.object))[1]
 if (is.na(control$plotdim))
  .plotdim <- 1
 else 
  .plotdim <- control$plotdim
 .y0l <- y0(.object)[.plotdim,1] # lower y0 
 .y0u <- y0(.object)[.plotdim,2] # upper y0 
 # (of dimension specified in control$plotdim, default: first dimension)
 .tau <- tau(data(.object))[.plotdim] # data: tau  (NULL if no data)
 .n   <- n(data(.object))[.plotdim]   # data: n    (NULL if no data)
 # plot the specified dimension (of y) by calling the workhorse plot function
 # the first time
 .plotLuck(.n0l, .n0u, .y0l, .y0u, .tau, .n, control, .whichdim = .plotdim, .add = add, ...)
 # make annotations for the specified dimension if requested
 if (control$annotate)
  .plotLuckAugAnno(.n0l, .n0u, .y0l, .y0u, .tau, .n, .ydim, .plotdim, control, ...)
 # if y is multidimensional and no plotdim specified: call the workhorse plot 
 # function again for the other dimensions, those are added to the existing plot
 if (.ydim > 1 && is.na(control$plotdim)){
  for (j in 2:.ydim) {
   .y0l <- y0(.object)[j,1] # lower y0
   .y0u <- y0(.object)[j,2] # upper y0
   .plotLuck(.n0l, .n0u, .y0l, .y0u, .tau, .n, control, .whichdim = j, .add = TRUE, ...)
  } 
 }
})

# workhorse plot function -- see plot method for arguments...
.plotLuck <- function (.n0l, .n0u, .y0l, .y0u, .tau, .n, .controlList, .whichdim, .add, ...) {
 # read out possibly multidimensional arguments for color etc.
 # filling color of polygon
 if (length(.controlList$polygonCol) > 1)
  .polygonCol <- .controlList$polygonCol[.whichdim]
 else
  .polygonCol <- .controlList$polygonCol
 # border color of polygon
 if (length(.controlList$borderCol) > 1)
  .borderCol <- .controlList$borderCol[.whichdim]
 else
  .borderCol <- .controlList$borderCol
 # density of polygon shading lines:
 if (length(.controlList$density) > 1){
  # due to annoying bug of polygon(), a possible NA in the "density" vector
  # must be transformed to NULL (which itself can't be element of the vector),
  # because density=NA leads to a filled polygon even if col=NA or col=NULL
  if (is.na (.controlList$density[.whichdim]))
   .density <- NULL
  else 
   .density <- .controlList$density[.whichdim]
 } else {
  .density <- .controlList$density
 }
 # angle of polygon shading lines
 if (length(.controlList$angle) > 1)
  .angle <- .controlList$angle[.whichdim]
 else
  .angle <- .controlList$angle 
 # plot vectors for polygon(): prior parameter set
 if (.controlList$posterior == FALSE){
  .nseqf <- c(.n0l, .n0u) # forward "sequence" of prior n values
  .nseqb <- c(.n0u, .n0l) # backward "sequence" of prior n values
  .yseqf <- c(.y0l, .y0l) # "sequence" of prior y values matching .nseqf
  .yseqb <- c(.y0u, .y0u) # "sequence" of prior y values matching .nseqb
 } else { # plot vectors for posterior parameter set
  if (is.null(.tau)) stop ("No data specified in LUCK model object.")
  .fromto <- c(updateLuckN(.n0l, .n), updateLuckN(.n0u, .n))
  .nseqf <- seq(.fromto[1], .fromto[2], length.out = .controlList$plotSeqLength) # forward sequence of posterior n values
  .nseqb <- seq(.fromto[2], .fromto[1], length.out = .controlList$plotSeqLength) # backward sequence of posterior n values
  .yseqf <- updateLuckY(seq (.n0l, .n0u, length.out = .controlList$plotSeqLength), .y0l, .tau, .n)
  .yseqb <- updateLuckY(seq (.n0u, .n0l, length.out = .controlList$plotSeqLength), .y0u, .tau, .n)
 }
 # if .add == FALSE, make an empty plot, because polygon() is not a first-level plot function
 if (!.add){
  plot(x = c(.nseqf, .nseqb), y = c(.yseqf, .yseqb), type = "n", xlab = "", ylab = "", # xlab = "n", ylab = "y", #
       col = 1, ...)
 }
 # plotting the sequences with polygon()
 polygon(c(.nseqf, .nseqb), # x coordinates
         c(.yseqf, .yseqb), # y coordinates
         col = .polygonCol, border = .borderCol,
         density = .density, angle = .angle, ...)
 # easy plot augmentations
 if (.controlList$posterior & .controlList$rectangle)
  .plotLuckAugRect(.n0l, .n0u, .y0l, .y0u, .tau, .n, .controlList, .whichdim, ...)
 if (.controlList$numbers)
  .plotLuckAugNum(.n0l, .n0u, .y0l, .y0u, .tau, .n, .controlList, .whichdim, ...)
}


# ---------------------------------------------------------------------------- #
# helper functions for easy plot augmentation (e.g., annotations), to be called
# in the plot function via arguments in "control", the list of control
# parameters that can be produced with controlList()
# ---------------------------------------------------------------------------- #

# annotation
.plotLuckAugAnno <- function (.n0l, .n0u, .y0l, .y0u, .tau, .n, .ydim, .plotdim, .controlList, ...){
 .rD <- .controlList$rDigits
 #------- prior model -------#
 if (.controlList$posterior == FALSE){
  # axis labeling 
  if (.ydim == 1 | (.ydim > 1 && is.na(.controlList$plotdim))){ 
   mtext(text = bquote(n^(0)), 1, line = 2, adj = 0.5) 
   mtext(text = bquote(y^(0)), 2, line = 2, adj = 0.5)
  } else {
   mtext(text = bquote(n[.(.plotdim)]^(0)), 1, line = 2, adj = 0.5) 
   mtext(text = bquote(y[.(.plotdim)]^(0)), 2, line = 2, adj = 0.5)
  }
  # prior set annotation - only for 1dim or multidim with dim specified
  if (.ydim == 1 | (.ydim > 1 && !is.na(.controlList$plotdim))){
   if (.n0l == .n0u){ # for iLUCK models (a LUCK model would be just a dot...)
    title(main=bquote(paste("Set of priors: ",y^(0) %in%{}," [",.(round(.y0l,.rD))," ; ",.(round(.y0u,.rD)), #
                                     "] and ",n^(0)," = ",.(.n0l) )), col = 1, ... )
   } else { # annotation of prior set for generalized iLUCK models
    if (.y0l == .y0u) { # generalized iLUCK model with single y
     title(main=bquote(paste("Set of priors: ",y^(0)," = ",.(round(.y0l,.rD)), #
                                       " and ",n^(0) %in%{}," [",.(.n0l)," ; ",.(.n0u),"]")), col = 1, ... )
    } else { # generalized iLUCK model with interval-valued y
     title(main=bquote(paste("Set of priors: ",y^(0) %in%{}," [",.(round(.y0l,.rD))," ; ",.(round(.y0u,.rD)), #
                                      "] and ",n^(0) %in%{}," [",.(.n0l)," ; ",.(.n0u),"]")), col = 1, ... )
    } 
   }
  } 
 } else {
 #------- posterior model -------#
  # axis labeling 
  if (.ydim == 1 | (.ydim > 1 && is.na(.controlList$plotdim))){ 
   mtext(text = bquote(n^(n)), 1, line = 2, adj = 0.5) 
   mtext(text = bquote(y^(n)), 2, line = 2, adj = 0.5)
  } else {
   mtext(text = bquote(n[.(.plotdim)]^(n)), 1, line = 2, adj = 0.5) 
   mtext(text = bquote(y[.(.plotdim)]^(n)), 2, line = 2, adj = 0.5)
  }
  .n1l <- updateLuckN(.n0l, .n)
  .n1u <- updateLuckN(.n0u, .n)
  .y1lnly <- updateLuckY(n0 = .n0l, y0 = .y0l, tau = .tau, n = .n)
  .y1lnuy <- updateLuckY(n0 = .n0l, y0 = .y0u, tau = .tau, n = .n)
  .y1unly <- updateLuckY(n0 = .n0u, y0 = .y0l, tau = .tau, n = .n)
  .y1unuy <- updateLuckY(n0 = .n0u, y0 = .y0u, tau = .tau, n = .n)
  .y1l <- min(.y1lnly, .y1unly)
  .y1u <- max(.y1lnuy, .y1unuy)
  # posterior set annotation - only for 1dim or multidim with dim specified
  if (.ydim == 1 | (.ydim > 1 && !is.na(.controlList$plotdim))){
   if (.n1l == .n1u){ # for iLUCK models (a LUCK model would be just a dot...)
    title(main=bquote(paste("Set of posteriors: ",y^(n) %in%{}," [",
	 					   .(round(.y1l,.rD))," ; ",.(round(.y1u,.rD)), #
                                         "] and ",n^(n)," = ",.(.n1l) )), col = 1, ... )
   } else { # annotation of posterior set for generalized iLUCK models
    if (.y1l == .y1u) { # generalized iLUCK model with single y
     title(main=bquote(paste("Set of posteriors: ",y^(n)," = ",.(round(.y1l,.rD)), #
                                           " and ",n^(n) %in%{}," [",.(.n1l)," ; ",.(.n1u),"]")), col = 1, ... )
    } else { # generalized iLUCK model with interval-valued y
     title(main=bquote(paste("Set of posteriors: ",y^(n) %in%{}," [",.(round(.y1l,.rD))," ; ",.(round(.y1u,.rD)), #
                                          "] and ",n^(n) %in%{}," [",.(.n1l)," ; ",.(.n1u),"]")), col = 1, ... )
    } 
   } 
   # annotation for data situation
   #mtext(bquote(paste("Observation ",tilde(tau),"(x) = ",.(round(.tau/.n,.rD)), #
   #                   " with n = ",.(.n))), side=1, line=2, ...)# cex=1.0)
   title(sub=bquote(paste("Observation ",tilde(tau),"(x) = ",.(round(.tau/.n,.rD)), #
                          " with n = ",.(.n))), col = 1, ...)
  } 
 }
}

# rectangle posterior
.plotLuckAugRect <- function (.n0l, .n0u, .y0l, .y0u, .tau, .n, .controlList, .whichdim, ...){
 # read out possibly multidimensional arguments for rectCol and rectLty
 # rectCol
 if (length(.controlList$rectCol) > 1)
  .rectCol <- .controlList$rectCol[.whichdim]
 else
  .rectCol <- .controlList$rectCol
 # rectLty 
 if (length(.controlList$rectLty) > 1)
  .rectLty <- .controlList$rectLty[.whichdim]
 else
  .rectLty <- .controlList$rectLty
 # plot rectangular
 lines (x = updateLuckN(n0 = c(.n0l, .n0u, .n0u, .n0l, .n0l), n = .n), # n coordinates
        y = c( rep(min(updateLuckY(n0 = c(.n0l, .n0u), y0 = .y0l, tau = .tau, n = .n)), 2),
               rep(max(updateLuckY(n0 = c(.n0l, .n0u), y0 = .y0u, tau = .tau, n = .n)), 2),
                   min(updateLuckY(n0 = c(.n0l, .n0u), y0 = .y0l, tau = .tau, n = .n)) ),
        lty = .rectLty, col = .rectCol, ...) 
}

# numbers for y values
##TODO: add numbers for n values, too?
.plotLuckAugNum <- function (.n0l, .n0u, .y0l, .y0u, .tau, .n, .controlList, .whichdim, ...){
 .rD <- .controlList$rDigits
 # read out possibly multidimensional argument for number color
 if (length(.controlList$numCol) > 1)
  .numCol <- .controlList$numCol[.whichdim]
 else
  .numCol <- .controlList$numCol
 if (.controlList$posterior == FALSE){ # prior model
  # also n values?
  #text (c(.n0l, .n0u), c(.y0l, .y0l), labels=c(bquote(.(.n0l)), bquote(.(.n0u))), pos=1, ...)
  # y values
  text (c(.n0u, .n0u), c(.y0l, .y0u), #
        labels = c(bquote(.(round(.y0l,.rD))), bquote(.(round(.y0u,.rD)))), #
        pos = 4, col = .numCol, ...)
 } else { # posterior model
  .n1l <- updateLuckN(.n0l, .n)
  .n1u <- updateLuckN(.n0u, .n)
  .y1lnly <- updateLuckY(n0 = .n0l, y0 = .y0l, tau = .tau, n = .n)
  .y1lnuy <- updateLuckY(n0 = .n0l, y0 = .y0u, tau = .tau, n = .n)
  .y1unly <- updateLuckY(n0 = .n0u, y0 = .y0l, tau = .tau, n = .n)
  .y1unuy <- updateLuckY(n0 = .n0u, y0 = .y0u, tau = .tau, n = .n)
  .y1l <- min(.y1lnly, .y1unly)
  .y1u <- max(.y1lnuy, .y1unuy)
  # also n values?
  #text (c(.n1l, .n1u), c(.y1l, .y1l), labels=c(bquote(.(.n1l)), bquote(.(.n1u))), pos=1, ...)
  # y values
  text (c(.n1l, .n1l), c(.y1lnly, .y1lnuy), #
        labels = c(bquote(.(round(.y1lnly,.rD))), #
                   bquote(.(round(.y1lnuy,.rD)))), #
        pos = 2, col = .numCol, ...)
  text (c(.n1u, .n1u), c(.y1unly, .y1unuy), #
        labels = c(bquote(.(round(.y1unly,.rD))), #
                   bquote(.(round(.y1unuy,.rD)))), #
        pos = 4, col = .numCol, ...)
 }
}


#