\name{singleHdi-methods}
\alias{singleHdi-methods}
\alias{singleHdi,ExponentialLuckModel-method}
\alias{singleHdi,ScaledNormalLuckModel-method}
\alias{singleHdi}
\title{Highest density intervals for \code{\linkS4class{LuckModel}}s}
\description{
Returns the bounds of a highest density (HD) interval for a single distribution
defined by the canonical parameters used in the definition of \code{\linkS4class{LuckModel}}s.
}
\usage{
singleHdi(object, n, y, gamma)
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
  \item{gamma}{
  The coverage probability of the HD interval.
  }
}
\value{
A two-element vector containing the lower and the upper bound of the HD interval.
}
\details{
The \code{object} is needed for method dispatching only;
its class tells \code{singleHdi} the parametric family the object represents,
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
\code{\link{unionHdi}} for calculating the union of prior or posterior HD intervals
for a certain \code{\linkS4class{LuckModel}}.
}
\examples{
scn1 <- ScaledNormalLuckModel(n0=c(2,10), y0=c(-5,5))
singleHdi(scn1, n=1, y=0, gamma=0.95)
ex1 <- ExponentialLuckModel(n0=5, y0=c(3,4))
singleHdi(ex1, n=1, y=5, gamma=0.95)
}
\keyword{methods}
\keyword{univar}
