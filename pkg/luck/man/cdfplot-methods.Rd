\name{cdfplot-methods}
\alias{cdfplot}
\alias{cdfplot-methods}
\alias{cdfplot,LuckModel-method}
\alias{cdfplot,ExponentialLuckModel-method}
\alias{cdfplot,ScaledNormalLuckModel-method}
%\alias{singleCdf-methods}
%\alias{singleCdf,ExponentialLuckModel-method}
%\alias{singleCdf,ScaledNormalLuckModel-method}
%\alias{controlList}
\title{Plot sets of cumulative density functions for \code{\linkS4class{LuckModel}} objects}
\description{
This function plots the prior or posterior set of cumulative distribution functions
represented by a \code{\linkS4class{LuckModel}}.
It is currently implemented for one-dimensional distributions only.
To represent the set of distributions, a pointwise mininum curve and pointwise maximum curve is plotted,
i.e., for each \eqn{x}, \eqn{\min F(x)}{min F(x)} and \eqn{\max F(x)}{max F(x)}
are calculated and drawn for a range of \eqn{x} values,
and the area between them can be colored or shaded.
}
\usage{
cdfplot(object, xvec = 100, epsilon = 1e-5, control = controlList(),
        ylim = c(0,1), vertdist = TRUE, \dots)
}
\arguments{
  \item{object}{An object of a class extending \code{\linkS4class{LuckModel}},
  i.e. representing inference for data from a certain distribution,
  e.g., \code{\linkS4class{ScaledNormalLuckModel}}.
  (Plain \code{\linkS4class{LuckModel}} objects implement the general
  superstructure for canonical conjugate priors
  and thus do not specify a certain parametric family of priors.)
  }
  \item{xvec}{Either a sequence to plot over or the number of points to plot over,
  with default = 100. In the first case, \code{xvec} must be a vector of ascending values,
  in the latter, a positive integer larger than 2.
  If \code{xvec} is an integer, \code{cdfplot()} determines the plotting region by calculating
  the union of highest density intervals (see \code{\link{unionHdi}}),
  covering a probability of \code{1-epsilon} (see below).
  The plotting points are then evenly distributed over the resulting interval.
  }
  \item{epsilon}{
  \code{1-epsilon} is the coverage probability by which the plotting region is determined
  if \code{xvec} is an integer (see above). Defaults to \code{1e-5}.
  Ignored if \code{xvec} is a vector.
  }
  \item{control}{
  A list of controls to address options for appearance of the plot.
  Default is the value of \code{controlList()}, a function that
  returns a list containing the required elements with their default value.
  The function \code{controlList} is described in detail in
  \code{\link[=plot,LuckModel,missing-method]{plot}}.
  Relevant options are \code{posterior} (defaulting to \code{FALSE}),
  and the options for appearance of the filled or shaded area
  (\code{polygonCol}, \code{borderCol}, \code{density}, \code{angle}).
  Other arguments for \code{controlList} are currently ignored.
  }
  \item{ylim}{
  Vector with two elements giving the limit of the plotting region for the ordinate (y axis),
  as in usual plots (see, e.g., \code{\link{par}}).
  }
  \item{vertdist}{
  If the cdfs of the distributions corresponding to the four corners of the parameter set
  should be plotted. Defaults to \code{TRUE}.
  }
  \item{\dots}{
  Further arguments forwarded to \code{\link{plot}} and \code{\link{polygon}}. 
  }
}
\value{
The function is used for its side effects (the plot).
}
\details{
The minimum and maximum cdf curve drawn by this function are \emph{pointwise},
i.e., for each \eqn{x}, \eqn{\min F(x)}{min F(x)} and \eqn{\max F(x)}{max F(x)}
over the set of parametric distributions
are calculated and drawn for a range of \eqn{x} values.
The resulting curves usually do not correspond to a single parametric distribution
from the set of distributions, but for certain sets, this may nevertheless be the case.
An example for the former case is the prior plot for a \code{\linkS4class{ScaledNormalLuckModel}}
with both \eqn{n^{(0)}}{n^(0)} and \eqn{y^{(0)}}{y^(0)} interval-valued;
an example for the latter case is the prior plot for a \code{\linkS4class{ScaledNormalLuckModel}}
where \eqn{n^{(0)}}{n^(0)} is fixed and \eqn{y^{(0)}}{y^(0)} interval-valued
(see the examples below).
Comparison with the cdfs corresponding to the distributions in the four corners of the parameter set
(drawn with option \code{vertdist = TRUE}) may serve to illustrate this.

The function relies on the function \code{\link{singleCdf}},
which, for an object of a concrete subclass of \code{\linkS4class{LuckModel}},
returns values of the cdf for a single distribution of the prior's parametric family.
}
%\section{Methods}{
%\describe{
%\item{\code{signature(object = "ScaledNormalLuckModel")}}{
%Ignore this. I just tried to make \code{R CMD check} happy.
%}
%\item{\code{signature(object = "ExponentialLuckModel")}}{
%Ignore this. I just tried to make \code{R CMD check} happy.
%}
%}}
\author{
Gero Walter
}
\seealso{
\code{\link{luck}} for a general description of the package,
\code{\link{singleCdf}} for the cdf of a single distribution. 
}
\examples{
scn1 <- ScaledNormalLuckModel(n0=c(2,10), y0=c(3, 4), data=ScaledNormalData(mean=mean(rnorm(5)), n=5))
cdfplot(scn1)
scn2 <- ScaledNormalLuckModel(n0=2, y0=c(3, 4))
cdfplot(scn2, xvec=200)
cdfplot(scn1, control=controlList(posterior=TRUE, polygonCol=NA), vertdist=FALSE)
cdfplot(scn1, xvec=seq(2,5,by=0.01))
}

\keyword{methods}
\keyword{hplot}
