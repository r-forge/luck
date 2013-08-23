\name{ExponentialData-class}
\Rdversion{1.1}
\docType{class}
\alias{ExponentialData-class}
\alias{ExponentialData}
\alias{n<-,ExponentialData-method}
\alias{n,ExponentialData-method}
\alias{rawData<-,ExponentialData-method}
\alias{rawData,ExponentialData-method}
%\alias{show,ExponentialData-method}
\alias{tau<-,ExponentialData-method}
\alias{tau,ExponentialData-method}
\alias{tauN<-,ExponentialData-method}
\alias{tauN,ExponentialData-method}

\title{Class \code{"ExponentialData"} and its constructor function \code{ExponentialData()}}
\description{
\code{ExponentialData} objects represent data from an exponential distribution.
For inferences based on this data in the framework of generalized iLUCK models,
the class \code{\linkS4class{ExponentialLuckModel}} can be used.
\code{ExponentialData} extends the class \code{\linkS4class{LuckModelData}}.
Objects can be created using the constructor function \code{ExponentialData()}
described below.
}
\usage{
ExponentialData(arg1 = NULL, arg2 = NULL, data = NULL, mean = NULL, n = NULL,
                sim = FALSE)
}
\arguments{
  \item{arg1}{
  Used for treatment of unnamed arguments only, see Details.
}
  \item{arg2}{
  Used for treatment of unnamed arguments only, see Details.
}
  \item{data}{
  A vector of observations \eqn{x} from an exponential distribution,
  i.e., \eqn{x \sim Exp(\lambda)}{x ~ Exp(lambda).
  All elements must thus be strictly positive.}
}
  \item{mean}{
  The mean of \eqn{n} observations from an exponential distribution.
  Must be strictly positive, and supplied together with \code{n}.
}
  \item{n}{
  The sample size of observations from an exponential distribution
  with mean given in \code{mean}.
}
  \item{sim}{
  If \code{TRUE}, data are simulated according to \code{mean} and \code{n}
  and stored in the object. Defaults to \code{FALSE}.
}
}
\details{
\code{ExponentialData} objects can be created using one of the following ways:
  \itemize{
  \item By supplying a \code{\linkS4class{LuckModelData}} as the only (unnamed) argument.
  \item By supplying the mean and the sample size of a sample in \code{mean} and \code{n}, respectively.
  one of the arguments may be unnamed.
  \item By supplying a vector of observations in \code{data}.
  \item By setting \code{sim = TRUE}, and supplying a mean and a sample size
  in \code{mean} and \code{n}, respectively, a vector of observations is simulated.
  }
}
\section{Slots of the resulting object}{
  \describe{
    \item{\code{tauN}:}{A matrix with two named columns, with the first giving
    the sample statistic \eqn{\tau(x) = n \cdot \bar{x}}{tau(x) = n*mean(x)},
    and the second giving the corresponding sample size \code{n}.}
    \item{\code{rawData}:}{The data vector, if data was provided or simulated.
    From this data, \eqn{\tau(x)} and \eqn{n} in \code{tauN} are calculated.}
  }
}
\section{Methods}{
There are methods to access or replace the contents of the slots:
  \describe{
    \item{tauN}{\code{signature(object = "ExponentialData")} }
    \item{tauN<-}{\code{signature(object = "ExponentialData")} }
    \item{tau}{\code{signature(object = "ExponentialData")} }
    \item{tau<-}{\code{signature(object = "ExponentialData")} }
    \item{n}{\code{signature(object = "ExponentialData")} }
    \item{n<-}{\code{signature(object = "ExponentialData")} }
    \item{rawData}{\code{signature(object = "ExponentialData")} }
    \item{rawData<-}{\code{signature(object = "ExponentialData")} }
   }
When the slot \code{rawData} is replaced using \code{rawData<-},
the contents of the slot \code{tauN} are recalculated.
Attempts to replace (parts of) \code{tauN} if the object
contains data in slot \code{rawData} results in an error.\cr
There is a method to display \code{ExponentialData} objects by text: 
  \describe{
    \item{show}{\code{signature(object = "ExponentialData")}: This is invoked
    when printing an \code{ExponentialData} object, or an \code{\linkS4class{ExponentialLuckModel}} object
    containing data.}
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
and \code{\linkS4class{ExponentialLuckModel}} for the model using this data representation.
}
\examples{
# generate an ExponentialData object
set.seed(8553)
x <- rexp(10)
expdata1 <- ExponentialData(data = x)
# also possible:
expdata2 <- ExponentialData(mean = mean(x), n = length(x))
expdata3 <- ExponentialData(mean=2, n=100)
scndata4 <- ExponentialData(mean = 5, n = 10, sim = TRUE)
data1 <- LuckModelData(tau=20, n=10)
scndata5 <- ExponentialData(data1)
# access and replace slots
tauN(expdata1)
rawData(expdata1) <- rexp(n = 20, rate = 1/3)
expdata1
tau(expdata2)
tau(expdata2) <- 4 
n(expdata2)
n(expdata2) <- 100
}
\keyword{classes}
