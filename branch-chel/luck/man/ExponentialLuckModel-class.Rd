\name{ExponentialLuckModel-class}
\Rdversion{1.1}
\docType{class}
\alias{ExponentialLuckModel-class}
\alias{ExponentialLuckModel}
%\alias{cdfplot,ExponentialLuckModel-method}
\alias{data<-,ExponentialLuckModel-method}
\alias{data,ExponentialLuckModel-method}
\alias{data-ExponentialLuckModel}
\alias{data<--ExponentialLuckModel}
%\alias{data}
\alias{n0<-,ExponentialLuckModel-method}
\alias{n0,ExponentialLuckModel-method}
\alias{n0-ExponentialLuckModel}
\alias{n0<--ExponentialLuckModel}
%\alias{plot,ExponentialLuckModel,missing-method}
%\alias{show,ExponentialLuckModel-method}
%\alias{singleCdf,ExponentialLuckModel-method}
%\alias{singleHdi,ExponentialLuckModel-method}
\alias{unionHdi,ExponentialLuckModel-method}
\alias{y0<-,ExponentialLuckModel-method}
\alias{y0,ExponentialLuckModel-method}
\alias{y0-ExponentialLuckModel}
\alias{y0<--ExponentialLuckModel}

\title{Class \code{"ExponentialLuckModel"} and its constructor function \code{ExponentialLuckModel()}}
\description{
\code{"ExponentialLuckModel"} objects describe a set of conjugate Gamma priors for
imprecise Bayesian inference about exponetially distributed data \eqn{X[i] \sim Exp(\lambda)}{X[i] ~ Exp(lambda)}.
The set of Gamma priors on \eqn{\lambda}{lambda} is defined
via the set of canonical parameters \eqn{y^{(0)}}{y^(0)} and \eqn{n^{(0)}}{n^(0)}.
\code{ExponentialLuckModel} extends the class \code{\linkS4class{LuckModel}}.
Objects can be created using the constructor function \code{ExponentialLuckModel()}
described below.
}
\usage{
ExponentialLuckModel(arg1 = NULL, n0 = NULL, y0 = NULL,
                     data = new("ExponentialData"))
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
  An object of class \code{\linkS4class{ExponentialData}}, or, alternatively, a
  vector or list of arguments that its constructor function
  \code{\link[=ExponentialData]{ExponentialData()}} can handle.
  Also, an object of class \code{\linkS4class{LuckModelData}}
  can be supplied if \code{tau} has dimension 1 and is strictly positive. Default value is a
  default \code{\linkS4class{ExponentialData}} object containing no data that can
  be replaced later by \code{data(object) <- }.
}
}
\details{
\code{ExponentialLuckModel} objects can be created by one of the two following ways:
  \itemize{
  \item By supplying a \code{\linkS4class{LuckModel}} object as the only (unnamed) argument.
  \item By supplying \code{n0} and \code{y0}, and possibly also \code{data}.
  }
With the data distributed as \eqn{X_i \sim Exp(\lambda)}{X[i] ~ Exp(lambda)},
the conjugate prior on \eqn{\mu}{mu} is defined as
\deqn{\lambda \sim Ga(n^{(0)}+1, n^{(0)} y^{(0)}),}{%
       lambda ~ Ga(n^(0)+1, n^(0) y^(0)),}
where \eqn{Ga(\alpha,\beta)}{Ga(alpha,beta)} is the
Gamma distribution with shape parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\beta}{beta}
and density
\deqn{p(\lambda) = \frac{\beta^\alpha}{\Gamma(\alpha)}\lambda^{\alpha-1} e^{-\beta\lambda} }{%
      p(\lambda) = (beta^alpha/G(alpha)) * lambda^(alpha-1) * e^(-beta*lambda)}
and \eqn{\Gamma}{G} is the Gamma function.

In the canonical parametrization in terms of \eqn{y^{(0)}}{y^(0)} and \eqn{n^{(0)}}{n^(0)}
used here, the main parameter \eqn{y^{(0)}}{y^(0)} gives the inverse mode of the gamma distribution.
\eqn{n^{(0)}}{n^(0)} can, as usually, be interpreted as a prior strength parameter. 

The set of priors is then defined as the set of Gamma distributions as given above,
where \eqn{y^{(0)}}{y^(0)} and \eqn{n^{(0)}}{n^(0)} vary in sets with the bounds
given in \code{y0} and \code{n0}, respectively.
}
\section{Slots of the resulting object}{
  \describe{
    \item{\code{n0}:}{\eqn{n^{(0)}}{n^(0)}, the "prior strength" parameter set,
    is stored as a (1x2)-\code{matrix}, with the first element the lower bound
    and the second element the upper bound.}
    \item{\code{y0}:}{The range of \eqn{y^{(0)}}{y^(0)}, the "main parameter",
    taking here the role of the inverse mode of \eqn{\lambda}{lambda},
    is stored as a (1x2)-\code{matrix}, with the first element giving the lower
    bound and the second element the upper bound.}
    \item{\code{data}:}{Object of class \code{\linkS4class{ExponentialData}},
    containing the sample statistic \eqn{\tau(x) = n \cdot \bar{x}}{tau(x) = n*mean(x)}
    and the sample size \eqn{n}.
    For details, see \code{\linkS4class{ExponentialData}}.}
  }
}
\section{Methods for \code{ExponentialLuckModel} objects}{
There are methods to access or replace the contents of the slots:
  \describe{
    \item{n0}{\code{signature(object = "ExponentialLuckModel")}}
    \item{n0<-}{\code{signature(object = "ExponentialLuckModel")}}
    \item{y0}{\code{signature(object = "ExponentialLuckModel")}}
    \item{y0<-}{\code{signature(object = "ExponentialLuckModel")}}
    \item{data}{\code{signature(object = "ExponentialLuckModel")}}
    \item{data<-}{\code{signature(object = "ExponentialLuckModel")}}
  }
There are methods to display \code{ExponentialLuckModel}s by text or graphically: 
  \describe{
    \item{show}{\code{signature(object = "ExponentialLuckModel")}: This is invoked when
    printing an \code{ExponentialLuckModel}.}
    \item{plot}{\code{signature(x = "ExponentialLuckModel", y = "missing")}: This plots the
    prior or posterior set of parameters, with \eqn{n^{(0)}}{n^(0)}
    as the abscissa and \eqn{y^{(0)}}{y^(0)} as the ordinate. See
    \code{\link[=plot-methods]{plot}}.}
  }
There are two exemplary functions for inference tasks implemented so far. 
  \describe{
    \item{cdfplot}{\code{signature(object = "ExponentialLuckModel")}: This displays the
    range of cumulative density functions as defined by the set of prior or
    posterior parameters, see \code{\link[=cdfplot-methods]{cdfplot}}.}
    \item{unionHdi}{\code{signature(object = "ExponentialLuckModel")}: This calculates the
    union of highest density intervals for the prior or posterior set of
    distributions, see \code{\link[=unionHdi-methods]{unionHdi}}.}
  }
}
\references{
Gero Walter and Thomas Augustin (2009),
Imprecision and Prior-data Conflict in Generalized Bayesian Inference,
\emph{Journal of Statistical Theory and Practice} \bold{3}:255-271.\cr
\cr
Norbert Krautenbacher (2011),
Ein Beitrag zur generalisierten Bayes-Inferenz: Erweiterung und Anwendung der Implementierung der generalized iLUCK-models
(A contribution to generalized Bayesian Inference: Extension and Application of the implementation of generalized iLUCK-models),
Diploma Thesis, Department of Statistics, LMU Munich.
}
\author{
Norbert Krautenbacher
}
\seealso{
\code{\link{luck}} for a general description of the package,
\code{\linkS4class{LuckModel}} for the general class describing the framework
of canonical conjugate priors for inference based on samples from an exponential family,
and \code{\linkS4class{ExponentialData}} for the class of the \code{data} slot.
}
\examples{
# directly generate a ExponentialLuckModel object with a ExponentialData object
exp1 <- ExponentialLuckModel(n0=c(1,2), y0=c(3,4))

# turn a LuckModel object into a ExponentialLuckModel object
luck1 <- LuckModel(n0=c(2,10), y0=c(3, 4))
exp2 <- ExponentialLuckModel(luck1)

# access and replace slots
n0(exp1)
y0(exp1)
n0(exp1) <- c(1,25)
set.seed(12345)
data(exp2) <- ExponentialData(mean=5, n=20, sim=TRUE)
data(exp2)
tauN(data(exp2))

# plot prior and posterior parameter sets (same as with LuckModel objects)
par(mfrow=c(1,2))
plot(exp2)
plot(exp2, control = controlList(posterior = TRUE))
par(mfrow=c(1,1))

# plot the set of cdfs
cdfplot(exp2)
cdfplot(exp2, control=controlList(posterior=TRUE))

# exemplary inference: union of highest density intervals
unionHdi(exp2)
unionHdi(exp2, posterior=TRUE)$borders
}
\keyword{classes}
