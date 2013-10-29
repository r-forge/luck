\name{unionHdi-methods}
\alias{unionHdi-methods}
\alias{unionHdi,LuckModel-method}
\alias{unionHdi}
\title{ Calculate the union of highest density intervals for \code{\linkS4class{LuckModel}} objects }
\description{
This function calculates the union of prior or posterior highest density (HD) intervals for
a \code{\linkS4class{LuckModel}} object, i.e. it gives the lowest lower border and the highest upper border
of HD intervals over the set of parametric distributions
defined by a \code{\linkS4class{LuckModel}} object.
}
\usage{
unionHdi(object, gamma = 0.95, posterior = FALSE)
}
\arguments{
  \item{object}{An object of a class extending \code{\linkS4class{LuckModel}},
  i.e. representing inference for data from a certain distribution,
  e.g., \code{\linkS4class{ScaledNormalLuckModel}}.
  (Plain \code{\linkS4class{LuckModel}} objects implement the general
  superstructure for canonical conjugate priors
  and thus do not specify a certain parametric family of priors.)
  }
  \item{gamma}{The coverage probability for each of the HD intervals
  forming the union. The union then has at least the coverage probability of \code{gamma}.
  Default is 0.95, i.e. 95\%.
  }
  \item{posterior}{Whether the prioror the posterior union of HD intervals should be calculated.
  For \code{posterior = TRUE}, the \code{\linkS4class{LuckModel}} must contain a
  \code{\linkS4class{LuckModelData}} object.
  }
}
\details{
The function makes use of \code{\link{wrapOptim}} to optimize over the prior or posterior parameter set,
where for each parameter combination from the set, the function \code{\link{singleHdi}} is used
to calculate the highest density interval.
}
\value{
A list with the elements
\item{borders}{A two-element vector containing the lowest lower and the highest upper HD bound.}
\item{lowpars}{The canonical parameters for which the lowest lower HD bound was attained,
itself a list with elements \code{n} and \code{y}.}
\item{upppars}{The canonical parameters for which the highest upper HD bound was attained,
itself a list with elements \code{n} and \code{y}.}
}
\author{
Gero Walter
}
\seealso{
\code{\link{singleHdi}} for the method to calculate the single highest density intervals,
\code{\link{luck}} for a general description of the package,
\code{\linkS4class{LuckModel}} for its central class.
}
\examples{
scn3 <- ScaledNormalLuckModel(n0=c(1,25), y0=c(3,4))
unionHdi(scn3)
data(scn3) <- ScaledNormalData(mean=4,n=10)
unionHdi(scn3, posterior=TRUE)$borders
}
\keyword{methods}
\keyword{univar}
