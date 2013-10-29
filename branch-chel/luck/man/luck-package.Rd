\name{luck-package}
\alias{luck-package}
\alias{luck}
\docType{package}
\title{
Generalized iLUCK Models for Bayesian Inference with Sets of Priors
}
\description{
Generalized iLUCK-models are a way to define sets of priors based on conjugate
priors that allow for easy inference and a reasonable behaviour in case of
prior-data conflict.}
\details{
Generalized iLUCK-models belong to the domain of imprecise (or interval)
probability models. Instead of a single prior distribution, a \emph{set} of
priors is used to model prior information. This set is updated element by
element by Bayes' rule to form the set of posterior distributions, on which all
inferences can be based. The package offers a general framework to handle sets
of conjugate priors and methods for inferences based on them.

For sample distributions belonging to the regular, canonical exponential family
(Bernardo and Smith 2000, pp. 202 and 272f), there is a general construction
method to obtain a conjugate prior. The resulting prior for any distibution of
this family can be expressed via the canonical parameters \eqn{y^{(0)}}{y^(0)}
and \eqn{n^{(0)}}{n^(0)}, forming a general superstructure for conjugate priors.
The canonical parameters \eqn{y^{(0)}}{y^(0)} and \eqn{n^{(0)}}{n^(0)} are
usually transformations of the commonly used parameters of the prior. In most
cases, \eqn{y^{(0)}}{y^(0)} can be seen as parameter of location and is called
"main prior parameter" here. \eqn{n^{(0)}}{n^(0)} determines the spread of the
prior and can be seen as an inverse variance parameter.

In the canonical parametrization with \eqn{y^{(0)}}{y^(0)} and
\eqn{n^{(0)}}{n^(0)}, a prior is updated to the posterior by the updating the
prior parameters \eqn{y^{(0)}}{y^(0)} and \eqn{n^{(0)}}{n^(0)} to their
posterior counterparts by
\deqn{n^{(n)} = n^{(0)} + n}{%
      n^(n) = n^(0) + n,}
\deqn{y^{(n)} = \frac{n^{(n)}y^{(n)} + \tau(x)}{n^{(0)} + n},}{%
      y^(n) = (n^(0)*y^(0) + tau(x))/(n^(0) + n),}
where \eqn{\tau(x)} is a sufficient statistic of the data vector \eqn{x} derived
during the construction of the prior, and \eqn{n} is the sample size. Due to the
role of \eqn{n^{(0)}}{n^(0)} in the update step, it is also called "prior
strength parameter".

In the generalized Bayesian approach as described in Walter and Augustin (2009),
sets of priors are created by considering sets of these parameters: a prior with
\ldots
\itemize{
  \item both \eqn{y^{(0)}}{y^(0)} and \eqn{n^{(0)}}{n^(0)} as single numbers is
  a LUCK-model
  \item \eqn{y^{(0)}}{y^(0)} interval-valued and \eqn{n^{(0)}}{n^(0)} as single
  number is an iLUCK-model
  \item both \eqn{y^{(0)}}{y^(0)} and \eqn{n^{(0)}}{n^(0)} interval-valued is a
  generalized iLUCK-model.
}

The package thus implements the general structure of priors in the canonical
form where none, one or both \eqn{y^{(0)}}{y^(0)} and \eqn{n^{(0)}}{n^(0)} can
be interval-valued. Implemented in the \code{S4} class system of \strong{R}, the
\code{\linkS4class{LuckModel}} class contains next to slots for
\eqn{n^{(0)}}{n^(0)} and \eqn{y^{(0)}}{y^(0)} also an optional data slot,
supplying the data in the form needed (\eqn{\tau(x)}) for the update step.
The data is supplied by the \code{\linkS4class{LuckModelData}} class in the
needed form.

For a concrete data distribution, special cases of this general structure are
needed. One must thus define a class extending the
\code{\linkS4class{LuckModel}} class, and also a class extending
\code{\linkS4class{LuckModelData}}, such that raw data can be easily converted
to the needed form. Currently, this has been done for data from a scaled normal
distribution, i.e. \eqn{x ~ N(\mu, 1)}{x \sim N(\mu, 1)}, with the classes
\code{\linkS4class{ScaledNormalLuckModel}} and
\code{\linkS4class{ScaledNormalData}}, and for data from an exponential
distribution, i.e. \eqn{x ~ Exp(\lambda)}{x \sim Exp(\lambda)}, with the classes
\code{\linkS4class{ExponentialLuckModel}} and
\code{\linkS4class{ExponentialData}}.

A vignette giving step-by-step instructions to programming classes for a new
sample distribution will be included in future versions of the package.

As the posterior parameter sets resulting from updating all the priors in a
prior set are not two-dimensional intervals anymore, posterior sets are not
explicitely represented as \code{\linkS4class{LuckModel}} objects. Whenever
posterior quantities are of interest (specified in methods by the option
\code{"posterior = TRUE"}), the range of these quantities are calculated by
minimizing and maximizing over the updated parameters, which in turn can be done
by a box-constrained optimization over the set of prior parameters. This is why
the data object is directly included in the \code{\linkS4class{LuckModel}}
object. For the box-constrained optimization, a helper function called
\code{\link{wrapOptim}} is used to be able to treat all cases (none, one or both
canonical parameters are interval-valued) in the same way. 

For illustrations of and workings with LuckModel objects, some methods have been
written. First, there are methods to display and print plain
\code{\linkS4class{LuckModel}} objects (existing only on the superstructure
level):
\itemize{
  \item The \code{show} method prints the contents of a
  \code{\linkS4class{LuckModel}} object in more readable form. If the object
  contains a \code{\linkS4class{LuckModelData}} object, this is printed along as
  well, resorting on a \code{show} method for \code{\linkS4class{LuckModelData}}
  objects. (\code{show} methods are the \code{S4} equivalent to \code{print}
  methods for \code{S3} objects.)
  \item the \code{plot} method represents the prior or posterior parameter sets
  graphically.
}
Second, for concrete data distributions, there are methods for working with and
displaying the resulting sets of prior/posterior distributions:
\itemize{
  \item The \code{show} methods for \code{\linkS4class{LuckModel}} and
  \code{\linkS4class{LuckModelData}} are specialized in order to explain the
  meaning of the canonical parameters \eqn{n} and \eqn{y}.
  \item \code{\link[=unionHdi-methods]{unionHdi}} calculates the union of highest
  density intervals for a specialized LuckModel object. This method relies on a
  function \code{\link[=singleHdi-methods]{singleHdi}} that gives a highest
  density interval for a single parameter combination \eqn{(n, y)} for the
  respective conjugate prior or posterior distribution. Therefore, for each
  specialized \code{\linkS4class{LuckModel}} class,
  \code{\link[=singleHdi-methods]{singleHdi}} must be provided.
  \item \code{\link[=cdfplot-methods]{cdfplot}} plots the set of cumulative
  density functions for a specialized \code{\linkS4class{LuckModel}}. Again,
  this method relies on a function \code{\link[=singleCdf-methods]{singleCdf}}
  that gives values of the cdf for a single parameter combination \eqn{(n, y)}
  for the respective conjugate prior distribution.
}
}
\author{
Gero Walter, Norbert Krautenbacher

Maintainer: Gero Walter <gero.walter@stat.uni-muenchen.de>
}
\references{
Gero Walter and Thomas Augustin (2009),
Imprecision and Prior-data Conflict in Generalized Bayesian Inference,
\emph{Journal of Statistical Theory and Practice} \bold{3}:255-271.\cr

Norbert Krautenbacher (2011),
\emph{Ein Beitrag zur generalisierten Bayes-Inferenz: Erweiterung und Anwendung
der Implementierung der generalized iLUCK-models}
(A Contribution to Generalized Bayesian Inference: Extension and Application of
the Implementation of Generalized iLUCK models, in German),
Bachelor's thesis, LMU Munich.
}
\keyword{ package }
\keyword{ models }
\keyword{ htest }
\seealso{
See \code{\linkS4class{LuckModel}} and \code{\linkS4class{LuckModelData}} for
the abstract framework, and \code{\linkS4class{ScaledNormalLuckModel}} and
\code{\linkS4class{ScaledNormalData}}, or
\code{\linkS4class{ExponentialLuckModel}} and
\code{\linkS4class{ExponentialData}}, for the use of the framework in case of
data from a scaled normal, or exponential distribution, respectively.\cr

See \code{\link[=cdfplot-methods]{cdfplot}} or
\code{\link[=unionHdi-methods]{unionHdi}} for examples of inferences based on
sets of distributions.
}
\examples{
# generate an abstract generalized iLUCK model
ex1 <- LuckModel(n0=c(1,10), y0=c(0,5))
ex1
# with abstract data object
data1 <- LuckModelData(tau=11, n=2)
data1
ex1d <- ex1
data(ex1d) <- data1
ex1d

# generate a generalized iLUCK model for scaled normal data
ex2 <- ScaledNormalLuckModel(ex1, data=rnorm(mean=4, sd=1, n=10))
ex2

# plot the posterior set of canonical parameters
plot(ex2, control = controlList(posterior = TRUE))

# plot the prior and posterior set of cdf functions
par(mfrow = c(1,2))
cdfplot(ex2)
cdfplot(ex2, control = controlList(posterior = TRUE))
par(mfrow = c(1,1))
# calculate the union of hpd intervals
unionHdi(ex2, posterior=TRUE)$borders
}
\keyword{package}
