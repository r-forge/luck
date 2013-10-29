\name{LuckModel-class}
\Rdversion{1.1}
\docType{class}
\alias{LuckModel-class}
\alias{LuckModel}
%\alias{cdfplot,LuckModel-method}
\alias{data<-,LuckModel-method}
\alias{data,LuckModel-method}
\alias{data-methods}
\alias{data<--methods}
\alias{data}
\alias{data<-}
\alias{n0<-,LuckModel-method}
\alias{n0,LuckModel-method}
\alias{n0-methods}
\alias{n0<--methods}
\alias{n0}
\alias{n0<-}
%\alias{plot,LuckModel,missing-method}
%\alias{show,LuckModel-method}
%\alias{unionHdi,LuckModel-method}
\alias{y0<-,LuckModel-method}
\alias{y0,LuckModel-method}
\alias{y0-methods}
\alias{y0<--methods}
\alias{y0}
\alias{y0<-}

\title{Class \code{"LuckModel"} and its constructor function \code{LuckModel()}}
\description{
\code{"LuckModel"} objects describe an abstract set of conjugate priors for
imprecise Bayesian inference via the set of canonical parameters
\eqn{y^{(0)}}{y^(0)} and \eqn{n^{(0)}}{n^(0)}. Pure \code{LuckModel} objects
(that are not simultaneously instances of a subclass inheriting from
\code{LuckModel}) are useful only for studying the parameter update step and the
shapes of posterior parameter sets. For data-based inferences, classes inherting
from \code{LuckModel} must be used, see, e.g.,
\code{\linkS4class{ScaledNormalLuckModel}} and
\code{\linkS4class{ExponentialLuckModel}}.

Objects can be created using the constructor function \code{LuckModel()}
described below.
}
\usage{
LuckModel(n0, y0, data = new("LuckModelData"))
}
\arguments{
  \item{n0}{
  A (1x2)-\code{matrix}, with the first element the lower bound and the second
  element the upper bound for \eqn{n^{(0)}}{n^(0)}. Alternatively, a two-element
  vector containing lower and upper bound. If a single value is given, it is
  taken as lower and upper bound, such that a model with fixed
  \eqn{n^{(0)}}{n^(0)} is specified.
  }
  \item{y0}{
  A \code{matrix} with two columns and \code{p} rows, where \code{p} is the
  dimension of the canonical parameter \eqn{y^{(0)}}{y^(0)} (depending on the
  distribution family of the prior). The first column gives the lower, the
  second column the upper bound for \eqn{y^{(0)}}{y^(0)}. Lower and upper bound
  may coincide to specify a model with fixed \eqn{y^{(0)}}{y^(0)}.
  Alternatively, a vector containing first the lower bounds for all dimensions
  and then the upper bounds for all dimensions. If a single value is given, a
  LUCK model with one-dimensional and fixed \eqn{y^{(0)}}{y^(0)} is assumed.
  }
  \item{data}{
  An object of class \code{\linkS4class{LuckModelData}}, or, alternatively, a
  vector or list of arguments that its constructor function
  \code{\link[=LuckModelData]{LuckModelData()}} can handle. The
  \code{\linkS4class{LuckModelData}} object must fit the definition of the
  \code{LuckModel}, i.e. \code{tau} from the \code{\linkS4class{LuckModelData}}
  object must have the same dimension \code{p} as \code{y0}. Default value is a
  default \code{\linkS4class{LuckModelData}} object containing no data that can
  be replaced later by \code{data(object) <- }.
  }
}
\section{Slots of the resulting object}{
  \describe{
    \item{\code{n0}:}{\eqn{n^{(0)}}{n^(0)}, the "prior strength" parameter set,
    is stored as a (1x2)-\code{matrix}, with the first element the lower bound
    and the second element the upper bound.}
    \item{\code{y0}:}{The range of \eqn{y^{(0)}}{y^(0)}, the "main parameter",
    is stored as a (px2)-\code{matrix}, with the first column giving the lower
    bound(s) and the second column giving the upper bound(s) of dimensions
    1 to p.}
    \item{\code{data}:}{Object of class \code{\linkS4class{LuckModelData}},
    containing the sample statistic \eqn{\tau(x)} and the sample size \eqn{n}.
    For details, see \code{\linkS4class{LuckModelData}}.}
  }
}
\section{Methods for \code{LuckModel} objects}{
There are methods to access or replace the contents of the slots:
  \describe{
    \item{n0}{\code{signature(object = "LuckModel")}}
    \item{n0<-}{\code{signature(object = "LuckModel")}}
    \item{y0}{\code{signature(object = "LuckModel")}}
    \item{y0<-}{\code{signature(object = "LuckModel")}}
    \item{data}{\code{signature(object = "LuckModel")}}
    \item{data<-}{\code{signature(object = "LuckModel")}}
  }
There are methods to display \code{LuckModel}s by text or graphically: 
  \describe{
    \item{show}{\code{signature(object = "LuckModel")}: This is invoked when
    printing a \code{LuckModel}.}
    \item{plot}{\code{signature(x = "LuckModel", y = "missing")}: This plots the
    prior or posterior set of canonical parameters, with \eqn{n^{(0)}}{n^(0)}
    as the abscissa and \eqn{y^{(0)}}{y^(0)} as the ordinate. See
    \code{\link[=plot-methods]{plot}}.}
	}
There are two exemplary functions for inference tasks implemented so far. These
make sense for non-abstract subclasses of \code{LuckModel} only, and will result
in an error if called with a plain \code{LuckModel} as argument.
Examples for these are thus given in \code{\linkS4class{ScaledNormalLuckModel}}
and \code{\linkS4class{ExponentialLuckModel}} .
  \describe{
    \item{cdfplot}{\code{signature(object = "LuckModel")}: This displays the
    range of cumulative density functions as defined by the set of prior or
    posterior parameters, see \code{\link[=cdfplot-methods]{cdfplot}}.}
    \item{unionHdi}{\code{signature(object = "LuckModel")}: This calculates the
    union of highest density intervals for the prior or posterior set of
    distributions, see \code{\link[=unionHdi-methods]{unionHdi}}.}
  }
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
\code{\linkS4class{LuckModelData}} as the class for the \code{data} slot,
\code{\link{updateLuckY}} and \code{\link{updateLuckN}} for the functions
that calculate the posterior canonical parameters,
\code{\linkS4class{ScaledNormalLuckModel}} and
\code{\linkS4class{ExponentialLuckModel}} for non-abstract subclasses intended
for analysis of scaled normal or exponential data, respectively.
}
\examples{
# generate a generalized iLUCK model
luck1 <- LuckModel(n0=c(2,10), y0=c(3, 4))
luck1
# access and replace slots
n0(luck1)
n0(luck1) <- 5
y0(luck1) <- c(0,5)
data(luck1)
data(luck1) <- LuckModelData(tau=20, n=10)
# plot prior and posterior parameter sets
par(mfrow=c(1,2))
plot(luck1)
plot(luck1, control = controlList(posterior = TRUE))
par(mfrow=c(1,1))
}
\keyword{classes}
