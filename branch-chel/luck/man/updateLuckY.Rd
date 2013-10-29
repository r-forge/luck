\name{updateLuckY}
\alias{updateLuckY}
\title{
Calculate the posterior canonical parameter \eqn{y^{(n)}}{y^(n)}
}
\description{
This function serves to calculate the posterior canonical parameter \eqn{y^{(n)}}{y^(n)}
from its prior counterpart \eqn{y^{(0)}}{y^(0)}, \eqn{n^{(0)}}{n^(0)}, the sample statistic \eqn{\tau(x)},
and the sample size \eqn{n}, by
\deqn{ y^{(n)} =  \frac{n^{(0)} y^{(0)} + \tau(x)}{n^{(0)} + n} }{%
       y^{n} =  (n^{0}y^{0} + \tau(x))/(n^{0} + n)}
Together with \code{\link{updateLuckN}}, this formula thus executes the Bayesian update step.
It is mostly used internally to determine posterior characteristics and inferences,
e.g., when plotting the posterior parameter set via \code{\link[=plot,LuckModel,missing-method]{plot}},
or when calculating the posterior union of highest density intervals via \code{\link{unionHdi}}.
}
\usage{
updateLuckY(n0, y0, tau, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n0}{
  The prior canonical parameter \eqn{n^{(0)}}{n^(0)},
  which can be taken from the slot \code{n0} of \code{\linkS4class{LuckModel}} objects.
}
  \item{y0}{
  The prior canonical parameter \eqn{y^{(0)}}{y^(0)}, possibly vectorial,
  which can be taken from the slot \code{y0} of \code{\linkS4class{LuckModel}} objects.
}
  \item{tau}{
  The sample statistic \eqn{\tau(x)}, possibly vectorial of the same dimension as \code{y0},
  which can be taken from the slot \code{tau} of \code{\linkS4class{LuckModelData}} objects.
}
  \item{n}{
  The sample size \eqn{n},
  which can be taken form the slot \code{n} of \code{\linkS4class{LuckModelData}} objects.
}
}
\value{
The posterior canonical parameter \eqn{y^{(n)}}{y^(n)} (possibly vectorial).
}
\author{
Gero Walter
}
\seealso{
\code{\link{luck}} for a general description of the package,
\code{\linkS4class{LuckModel}} for its central class.
}
