\name{singleCdf-methods}
\alias{singleCdf-methods}
\alias{singleCdf}
\alias{singleCdf,ExponentialLuckModel-method}
\alias{singleCdf,ScaledNormalLuckModel-method}
\title{Values of the cumulative density function}
\description{
Returns values of the cdf for a single distribution defined by canonical parameters
used in the definition of \code{\linkS4class{LuckModel}}s.
}
\usage{
singleCdf(object, n, y, x)
}
\arguments{
  \item{object}{
  An object of a class extending \code{\linkS4class{LuckModel}},
  e.g., \code{\linkS4class{ScaledNormalLuckModel}}.
  (Plain \code{\linkS4class{LuckModel}} objects implement the general
  superstructure for canonical conjugate priors
  and thus do not specify a certain parametric family of priors.)
  }
  \item{n, y}{
  The canonical parameters \eqn{n^{(0)}}{n^(0)} resp. \eqn{y^{(0)}}{y^(0)}
  defining the distribution for which to calculate values of the cdf.
  }
  \item{x}{
  (Vector of) \eqn{x} values for which to calculate the cdf \eqn{F(x)}.
  }
}
\value{
A vector of the same size of \code{x}.
}
\details{
The \code{object} is needed for method dispatching only;
its class tells \code{singleCdf} the parametric family the object represents,
i.e., how to interpret the canonical parameters \eqn{n^{(0)}}{n^(0)} and \eqn{y^{(0)}}{y^(0)}
and which functional form the cdf has for that family.
\code{n} and \code{y} may thus be any value that is admissible for the parametric family,
and need not be within the set of parameters defined in \code{object}.
}
\author{
Gero Walter
}
\seealso{
\code{\link{luck}} for a general description of the package,
\code{\link{cdfplot}} for plotting sets of cdfs. 
}
\examples{
scn1 <- ScaledNormalLuckModel(n0=c(2,10), y0=c(3, 4))
singleCdf(scn1,10,4,seq(0,8,by=0.1))
}
\keyword{methods}
\keyword{univar}
