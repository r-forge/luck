\name{ScaledNormalLuckModel-class}
\Rdversion{1.1}
\docType{class}
\alias{ScaledNormalLuckModel-class}
\alias{ScaledNormalLuckModel}
%\alias{cdfplot,ScaledNormalLuckModel-method}
\alias{data<-,ScaledNormalLuckModel-method}
\alias{data,ScaledNormalLuckModel-method}
\alias{data-ScaledNormalLuckModel}
\alias{data<--ScaledNormalLuckModel}
%\alias{data}
\alias{n0<-,ScaledNormalLuckModel-method}
\alias{n0,ScaledNormalLuckModel-method}
\alias{n0-ScaledNormalLuckModel}
\alias{n0<--ScaledNormalLuckModel}
%\alias{plot,ScaledNormalLuckModel,missing-method}
%\alias{show,ScaledNormalLuckModel-method}
%\alias{singleCdf,ScaledNormalLuckModel-method}
%\alias{singleHdi,ScaledNormalLuckModel-method}
\alias{unionHdi,ScaledNormalLuckModel-method}
\alias{y0<-,ScaledNormalLuckModel-method}
\alias{y0,ScaledNormalLuckModel-method}
\alias{y0-ScaledNormalLuckModel}
\alias{y0<--ScaledNormalLuckModel}


\title{Class \code{"ScaledNormalLuckModel"} and its constructor function \code{ScaledNormalLuckModel()}}
\description{
\code{"ScaledNormalLuckModel"} objects describe a set of conjugate Normal priors for
imprecise Bayesian inference about scaled Normal data \eqn{X[i] \sim N(\mu,1)}{X[i] ~ N(mu,1)}.
The set of Normal priors on \eqn{\mu}{mu} is defined
via the set of canonical parameters \eqn{y^{(0)}}{y^(0)} and \eqn{n^{(0)}}{n^(0)}.
\code{ScaledNormalLuckModel} extends the class \code{\linkS4class{LuckModel}}.
Objects can be created using the constructor function \code{ScaledNormalLuckModel()}
described below.
}
\usage{
ScaledNormalLuckModel(arg1 = NULL, n0 = NULL, y0 = NULL,
                      data = new("ScaledNormalData"))
}
\arguments{
  \item{arg1}{
  Used for treatment of unnamed arguments only, see Details.
}
  \item{n0}{
  A (1x2)-\code{matrix}, with the first element the lower bound and the second
  element the upper bound for \eqn{n^{(0)}}{n^(0)}. Alternatively, a two-element
  vector containing lower and upper bound. If a single value is given, it is
  taken as lower and upper bound, such that a model with fixed
  \eqn{n^{(0)}}{n^(0)} is specified. See also Details.
}
  \item{y0}{
  A (1x2)-\code{matrix}, with the first element the lower bound and the second
  element the upper bound for \eqn{y^{(0)}}{y^(0)}. Alternatively, a two-element
  vector containing lower and upper bound. If a single value is given, it is
  taken as lower and upper bound, such that a model with fixed
  \eqn{y^{(0)}}{y^(0)} is specified. See also Details.
}
  \item{data}{
  An object of class \code{\linkS4class{ScaledNormalData}}, or, alternatively, a
  vector or list of arguments that its constructor function
  \code{\link[=ScaledNormalData]{ScaledNormalData()}} can handle.
  Also, an object of class \code{\linkS4class{LuckModelData}}
  can be supplied if \code{tau} has dimension 1. Default value is a
  default \code{\linkS4class{ScaledNormalData}} object containing no data that can
  be replaced later by \code{data(object) <- }.
}
}
\details{
\code{ScaledNormalLuckModel} objects can be created by one of the two following ways:
  \itemize{
  \item By supplying a \code{\linkS4class{LuckModel}} object as the only (unnamed) argument.
  \item By supplying \code{n0} and \code{y0}, and possibly also \code{data}.
  }
With the data distributed as \eqn{X_i \sim N(\mu,1)}{X[i] ~ N(mu,1)},
the conjugate prior on \eqn{\mu}{mu} is defined as
\deqn{\mu \sim N(y^{(0)}, 1/n^{(0)}),}{%
      mu ~ N(y^(0), 1/n^(0)),}
such that the main parameter \eqn{y^{(0)}}{y^(0)} is the mean,
and the strength parameter \eqn{n^{(0)}}{n^(0)} is the inverse variance
of the Normal prior on \eqn{\mu}{mu}.

The set of priors is then defined as the set of Normal distributions as given above,
where \eqn{y^{(0)}}{y^(0)} and \eqn{n^{(0)}}{n^(0)} vary in sets with the bounds
given in \code{y0} and \code{n0}, respectively.
}
\section{Slots of the resulting object}{
  \describe{
    \item{\code{n0}:}{\eqn{n^{(0)}}{n^(0)}, the "prior strength" parameter set,
    taking here the role of the prior inverse variance of \eqn{\mu}{mu},
    is stored as a (1x2)-\code{matrix}, with the first element the lower bound
    and the second element the upper bound.}
    \item{\code{y0}:}{The range of \eqn{y^{(0)}}{y^(0)}, the "main parameter",
    taking here the role of the prior expectation of \eqn{\mu}{mu},
    is stored as a (1x2)-\code{matrix}, with the first element giving the lower
    bound and the second element the upper bound.}
    \item{\code{data}:}{Object of class \code{\linkS4class{ScaledNormalData}},
    containing the sample statistic \eqn{\tau(x) = n \cdot \bar{x}}{tau(x) = n*mean(x)}
    and the sample size \eqn{n}.
    For details, see \code{\linkS4class{ScaledNormalData}}.}
  }
}
\section{Methods for \code{ScaledNormalLuckModel} objects}{
There are methods to access or replace the contents of the slots:
  \describe{
    \item{n0}{\code{signature(object = "ScaledNormalLuckModel")}}
    \item{n0<-}{\code{signature(object = "ScaledNormalLuckModel")}}
    \item{y0}{\code{signature(object = "ScaledNormalLuckModel")}}
    \item{y0<-}{\code{signature(object = "ScaledNormalLuckModel")}}
    \item{data}{\code{signature(object = "ScaledNormalLuckModel")}}
    \item{data<-}{\code{signature(object = "ScaledNormalLuckModel")}}
  }
There are methods to display \code{ScaledNormalLuckModel}s by text or graphically: 
  \describe{
    \item{show}{\code{signature(object = "ScaledNormalLuckModel")}: This is invoked when
    printing a \code{ScaledNormalLuckModel}.}
    \item{plot}{\code{signature(x = "ScaledNormalLuckModel", y = "missing")}: This plots the
    prior or posterior set of parameters, with \eqn{n^{(0)}}{n^(0)}
    as the abscissa and \eqn{y^{(0)}}{y^(0)} as the ordinate. See
    \code{\link[=plot-methods]{plot}}.}
  }
There are two exemplary functions for inference tasks implemented so far. 
  \describe{
    \item{cdfplot}{\code{signature(object = "ScaledNormalLuckModel")}: This displays the
    range of cumulative density functions as defined by the set of prior or
    posterior parameters, see \code{\link[=cdfplot-methods]{cdfplot}}.}
    \item{unionHdi}{\code{signature(object = "ScaledNormalLuckModel")}: This calculates the
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
\code{\linkS4class{LuckModel}} for the general class describing the framework
of canonical conjugate priors for inference based on samples from an exponential family,
and \code{\linkS4class{ScaledNormalData}} for the class of the \code{data} slot.
}
\examples{
# directly generate a ScaledNormalLuckModel object with a ScaledNormalData object
scn1 <- ScaledNormalLuckModel(n0=c(1,2), y0=c(3,4), data=ScaledNormalData(mean=4, n=10))

# turn a LuckModel object into a ScaledNormalLuckModel object
luck1 <- LuckModel(n0=c(2,10), y0=c(3, 4))
scn2 <- ScaledNormalLuckModel(luck1)

# access and replace slots
n0(scn1)
y0(scn1)
data(scn1)
n0(scn1) <- c(1,25)
data(scn2) <- ScaledNormalData(mean=5, n=200, sim=TRUE)
data(scn2)
tauN(data(scn2))

# plot prior and posterior parameter sets (same as with LuckModel objects)
par(mfrow=c(1,2))
plot(scn1)
plot(scn1, control = controlList(posterior = TRUE))
par(mfrow=c(1,1))

# plot the set of cdfs
cdfplot(scn1)
cdfplot(scn1, control=controlList(posterior=TRUE))
cdfplot(scn1, xvec = seq(0, 4, length.out = 80))

# exemplary inference: union of highest density intervals
unionHdi(scn1)
unionHdi(scn1, posterior=TRUE)$borders
}
\keyword{classes}
