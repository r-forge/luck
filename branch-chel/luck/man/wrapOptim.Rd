\name{wrapOptim}
\alias{wrapOptim}
\title{
Function to address \code{optim} and \code{optimize} with the same set of arguments.
}
\description{
This function takes basically the same arguments as \code{\link{optim}}
with a lower and upper bound for optimization as required by the method \code{"L-BFGS-B"},
and is intended for optimization over the parameter set of a \code{\linkS4class{LuckModel}}.
}
\usage{
wrapOptim(par, fn, method = "L-BFGS-B", control = list(fnscale = 1),
          lower, upper, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par}{
  Initial values for the parameters to be optimzed over,
  typically one of the "corners" of the prior parameter set
  defined by a \code{\linkS4class{LuckModel}}.
}
  \item{fn}{
  The function to be minimized or maximized. The first argument must be named \code{.n0y0},
  and contains the two canonical parameters \eqn{n^{(0)}}{n^(0)} and \eqn{y^{(0)}}{y^(0)} of a 
  \code{\linkS4class{LuckModel}} as a vector, typically \code{c(n0,y0)}.
}
  \item{method}{
  The optimization method to be used, see \code{\link{optim}}.
  Default is \code{"L-BFGS-B"} as the one offering box-constrained optimization.
}
  \item{control}{
  A list of control parameters, see \code{\link{optim}}.
  Default is \code{list(fnscale = 1)}, making \code{wrapOptim} perform a minimization.
  (\code{fnscale=-1} performs minimization.)
}
  \item{lower}{
  Lower bound for the optimization, in the same format as \code{par} and in the function \code{fn},
  i.e. as a vector containing first the lower bound for the \eqn{n^{(0)}}{n^(0)}
  and second the lower bound for the \eqn{y^{(0)}}{y^(0)} parameter.
}
  \item{upper}{
  Upper bound for the optimization, in the same format as \code{par} and in the function \code{fn},
  i.e. as a vector containing first the upper bound for the \eqn{n^{(0)}}{n^(0)}
  and second the upper bound for the \eqn{y^{(0)}}{y^(0)} parameter.
}
  \item{\dots}{
  Further arguments to be passed to \code{\link{optim}}, \code{\link{optimize}}, or \code{fn}.
}
}
\details{
Depending wether \code{lower} and \code{upper} coincide in none
or either of \eqn{n^{(0)}}{n^(0)} or \eqn{y^{(0)}}{y^(0)},
\code{\link{optim}} or \code{\link{optimize}} are called.
If \code{lower} and \code{upper} coincide in both elements,
then no optimization is performed and the value of \code{fn}
at \code{lower} (=\code{upper}) is returned.

This function was programmed before the method \code{"Brent"} in \code{\link{optim}} existed,
which seems to provide a similar functionality.
}
\value{
The return value is the return object of \code{\link{optim}} (or pretends to be so).
See \code{\link{optim}} for details.
}
\author{
Gero Walter
}
\seealso{
\code{\link{luck}} for a general description of the package,
\code{\link{optim}},
\code{\link{optimize}}
}
\examples{
# function to optimize over
testfn <- function (.n0y0, arg2) {
  if (arg2) return(.n0y0[1]^2 + .n0y0[2])
  else return (.n0y0[2]^2 + .n0y0[1])
}
# minimize
wrapOptim(par = c(1,1), fn = testfn, lower = c(1,1), upper = c(5,5), arg2 = FALSE)
wrapOptim(par = c(1,1), fn = testfn, lower = c(1,1), upper = c(5,5), arg2 = TRUE)
wrapOptim(par = c(1,1), fn = testfn, lower = c(1,1), upper = c(1,1), arg2 = TRUE)
wrapOptim(par = c(1,5), fn = testfn, lower = c(1,1), upper = c(1,5), arg2 = TRUE)
wrapOptim(par = c(2,1), fn = testfn, lower = c(1,1), upper = c(5,1), arg2 = TRUE)
# maximize
wrapOptim(par = c(1,1), fn = testfn, lower = c(1,1), upper = c(5,5), arg2 = FALSE, 
          control = list(fnscale=-1))
}
\keyword{optimize}
