\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot}
\alias{plot,LuckModel,missing-method}
\alias{plot,ExponentialLuckModel,missing-method}
\alias{plot,ScaledNormalLuckModel,missing-method}
\alias{controlList}
\title{Plotting canonical parameter sets}
\description{
For \code{\linkS4class{LuckModel}} objects, the function \code{plot}
gives a method to display prior or posterior sets of canonical parameters graphically.
The strength parameter \eqn{n^{(0)}}{n^(0)} resp. \eqn{n^{(n)}}{n^(n)} is plotted on the abscissa (x) axis,
the main parameter \eqn{y^{(0)}}{y^(0)} resp. \eqn{y^{(n)}}{y^(n)}  on the ordinate (y) axis.
Specific plotting options are given to \code{plot} via the argument \code{control} that is
a list of arguments that can be produced with the help of the function \code{controlList},
which is also described here.
}
\usage{
plot(x, y, control = controlList(), add = FALSE, \dots)

controlList(plotdim = NA, posterior = FALSE, annotate = TRUE, rDigits = 2,
            numbers = FALSE, rectangle = FALSE, polygonCol = "gray",
            borderCol = 1, rectCol = 1, rectLty = 4, numCol = 1, density = NULL,
            angle = 45, plotSeqLength = 100)
}
\arguments{
  \item{x}{
A \code{\linkS4class{LuckModel}} object.
}
  \item{y}{
\code{y} is not needed, and therefore of class \code{\linkS4class{missing}}. Any input will be ignored.
}
  \item{control}{
The list of controls to address options for appearance of the plot.
This includes whether the prior or the posterior parameter set should be drawn.
Default is the value of \code{controlList()}, a function that
returns a list containing the required elements with their default value.
The arguments for \code{controlList} is described below.
}
  \item{add}{
Whether the plot should be added to an existing plot or not.
Default is \code{FALSE}, i.e., a new plot device is opened.
}
  \item{\dots}{
Graphic parameters such as \code{xlim}, \code{ylim} etc. can be given as arguments.
For controlling colors, there are specialised arguments \code{boderCol} and \code{polygonCol}
to \code{controlList}, see below.
}
  \item{plotdim}{
Which dimension of the main parameter \eqn{y^{(0)}}{y^(0)} should be displayed in the ordinate (y axis).
With the default value \code{NA}, the sets for all dimensions are plotted in the same frame.
Specific dimensions are addressed by their row number in the \code{y0} slot of the
\code{\linkS4class{LuckModel}} object.
If \eqn{y^{(0)}}{y^(0)} is one-dimensional, i.e., \code{y0} has only one row,
then \code{NA} and \code{1} will have the same result,
and any other values will give an error.
}
  \item{posterior}{
Whether the prior or the posterior parameter set should be drawn.
For the default \code{FALSE}, the prior set is drawn.
If \code{posterior = TRUE}, the posterior parameter set is drawn.
In this case, it is necessary that the
\code{\linkS4class{LuckModel}} object contains data in order to calculate the posterior parameter set;
otherwise this results in an error.
}
  \item{annotate}{
Whether annotations for the parameter set in the plot margins should be displayed.
For \code{TRUE}, the default, the axes are labeled,
and information on the parameter sets is printed in the upper margin
(via \code{title(main=\dots)}).
}
  \item{rDigits}{
The number of digits to be displayed in the margin annotations (see \code{annotate}) and
in the set annotations (see \code{numbers} below).
Default is \code{2}.
}
  \item{numbers}{
Whether the extremes of \eqn{y^{(0)}}{y^(0)} resp. \eqn{y^{(n)}}{y^(n)}
should be printed in the plot next to the four corners of the parameter set.
Default is \code{FALSE}.
}
  \item{rectangle}{
Whether the rectangle containing the (non-rectangle) posterior parameter set should be drawn.
This can serve to illustrate that considering \eqn{y^{(n)}}{y^(n)} as an interval-valued parameter,
without bearing in mind that the range of \eqn{y^{(n)}}{y^(n)} changes with \eqn{y^{(n)}}{y^(n)},
amounts to a loss of information, and may result in incorrect posterior inferences
(too wide bounds). This is the reason why posterior sets are not
explicitely represented as \code{\linkS4class{LuckModel}} objects.
}
  \item{polygonCol}{
The colour to fill the parameter set with. Default is \code{"gray"},
other colours can be chosen as usual (see \code{\link{par}}), by text strings or numbers.
For multidimensional main parameters, also a vector of colours can be supplied.
}
  \item{borderCol}{
The colour of the lines delineating the paramer set. Default is \code{1} (black),
other colours can be chosen as usual, by text strings or numbers.
With \code{NA}, no borders are drawn.
For multidimensional main parameters, also a vector of colours can be supplied.
}
  \item{rectCol}{
The colour of the lines delineating the rectangle circumscribing the parameter set (see \code{rectangle}).
Default is \code{1} (black), other colours can be chosen as usual, by text strings or numbers.
For multidimensional main parameters, also a vector of colours can be supplied.
}
  \item{rectLty}{
The type of the lines delineating the rectangle circumscribing the paramer set (see \code{rectangle}).
Default is \code{4} (dashed), other line types can be chosen as usual (see \code{\link{par}}, entry \code{lty}).
For multidimensional main parameters, also a vector of line types can be supplied.
}
  \item{numCol}{
The colour to print the annotation numbers in the plot (see \code{numbers}).
Default is \code{1} (black), other colours can be chosen as usual, by text strings or numbers.
For multidimensional main parameters, also a vector of colours can be supplied.
}
  \item{density}{
As an alternative to filling the parameter set (see \code{polygonCol}), shading lines can be drawn instead.
\code{density} controls the density of shading lines, in lines per inch. (See \code{\link{polygon}}.)
Default is \code{NULL}, which means no shading.
For multidimensional main parameters, also a vector of shade densities can be supplied.
}
  \item{angle}{
The slope of the shading lines, given as an angle in degrees, counter-cockwise.
If \code{density} is not specified, this is ignored. Default is \code{45}.
For multidimensional main parameters, also a vector of angles can be supplied.
}
  \item{plotSeqLength}{
The number of points for plotting the posterior parameter set contours.
The higher the number, the more accurate the display of the set contours.
The default value of \code{100} should be enough for normal-sized graphs.
}
}
\value{
The function is used for its side effects (the plot).
}
\details{
The argument \code{control} can be created via the function \code{controlList},
its usage and arguments which control the appearance of the plot are described above.
It may be called with only one or a few named arguments,
such that the default values are assigned to the rest.

If the \code{\linkS4class{LuckModel}} object specifies
both \eqn{y^{(0)}}{y^(0)} and \eqn{n^{(0)}}{n^(0)} interval-valued,
then the prior parameter set is a simple rectangle.
If both \eqn{y^{(0)}}{y^(0)} and \eqn{n^{(0)}}{n^(0)} are single numbers,
the prior parameter set is drawn as a dot of the default line width.
The line width can be modified via the argument \code{lwd} to \code{plot()}
(see \code{\link{par}}).
If \eqn{n^{(0)}}{n^(0)} is interval-valued,
posterior parameter sets have usually less trivial shapes.

For multidimensional main parameters, \code{plotdim=NA} (the default)
will plot \eqn{y} against \eqn{n} for each dimension of \eqn{y} in the same plot,
such that the lower-dimension sets may be overplotted.
The plotting region, if not defined via \code{ylim}, is determined by the first dimension.
Usually, it might be more useful to draw a separate plot for each dimension,
using \code{plotdim=1}, \code{plotdim=2}, etc.
}
\references{
Gero Walter and Thomas Augustin (2009),
Imprecision and Prior-data Conflict in Generalized Bayesian Inference,
\emph{Journal of Statistical Theory and Practice} \bold{3}:255-271.
}
\author{
Gero Walter
}
\seealso{
\code{\link{luck}} for a general description of the package,
\code{\linkS4class{LuckModel}} for the class this plot method is for, and 
\code{\linkS4class{LuckModelData}} for the class for its \code{data} slot.
\code{\link{par}} for how to specify colors, line types and other graphical parameters.
}
\examples{
luck1 <- LuckModel(n0=c(2,10), y0=c(3, 4), data=LuckModelData(tau=sum(rnorm(5)), n=5))
# prior parameter set
plot(luck1)
# posterior parameter set with annotations
plot(luck1, control = controlList(posterior = TRUE, annotate = TRUE))
# low-resolution posterior parameter set with numbers
plot(luck1, control = controlList(posterior = TRUE, plotSeqLength = 10, numbers = TRUE))
luck2 <- LuckModel(n0 = c(1,10), y0 = c(-5, 10, 5, 15))
# two-dimensional main parameter, in one plot, using shading lines instead of colour filling
plot(luck2, ylim=c(-6,16), control=controlList(borderCol=c(1,2), #
     polygonCol=c("blue", "grey"), density=c(10,NA), angle=c(25,95)))
# posterior parameter set with rectangle and numbers, other graphical parameters
plot(luck1, control = controlList(posterior = TRUE, rectangle = TRUE, rectCol = 2, #
                                  rectLty = "2244", numbers = TRUE, numCol = 4), #
     xlim = c(0,15), ylim = c(0,10), cex.main = 0.8, cex = 0.5)
# adding the prior parameter set
plot(luck1, add = TRUE)
}
\keyword{methods}
\keyword{hplot}
