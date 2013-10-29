\name{updateLuckN}
\alias{updateLuckN}
\title{
Calculate the posterior canonical parameter \eqn{n^{(n)}}{n^(n)}
}
\description{
This function serves to calculate the posterior canonical parameter \eqn{n^{(n)}}{n^(n)}
from its prior counterpart \eqn{n^{(0)}}{n^(0)} and the sample size \eqn{n},
by
\deqn{ n^{(n)} =  n^{(0)} + n }{%
       n^{n} =  n^{0} + n}
Together with \code{\link{updateLuckY}}, this formula thus executes the Bayesian update step.
It is mostly used internally to determine posterior characteristics and inferences,
e.g., when plotting the posterior parameter set via \code{\link[=plot,LuckModel,missing-method]{plot}},
or when calculating the posterior union of highest density intervals via \code{\link{unionHdi}}.
}
\usage{
updateLuckN(n0, n)
}
\arguments{
  \item{n0}{
  The prior canonical parameter \eqn{n^{(0)}}{n^(0)},
  which can be taken from the slot \code{n0} of \code{\linkS4class{LuckModel}} objects.
}
  \item{n}{
  The sample size \eqn{n},
  which can be taken form the slot \code{n} of \code{\linkS4class{LuckModelData}} objects.
}
}
\value{
The posterior canonical parameter \eqn{n^{(n)}}{n^(n)} (numeric).
}
\author{
Gero Walter
}
\seealso{
\code{\link{luck}} for a general description of the package,
\code{\linkS4class{LuckModel}} for its central class.
}
