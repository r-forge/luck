\name{ScaledNormalData-class}
\Rdversion{1.1}
\docType{class}
\alias{ScaledNormalData-class}
\alias{ScaledNormalData}
\alias{n<-,ScaledNormalData-method}
\alias{n,ScaledNormalData-method}
\alias{rawData<-,ScaledNormalData-method}
\alias{rawData,ScaledNormalData-method}
%\alias{show,ScaledNormalData-method}
\alias{tau<-,ScaledNormalData-method}
\alias{tau,ScaledNormalData-method}
\alias{tauN<-,ScaledNormalData-method}
\alias{tauN,ScaledNormalData-method}

\title{Class \code{"ScaledNormalData"} and its constructor function \code{ScaledNormalData()}}
\description{
\code{ScaledNormalData} objects represent data from a scaled Normal distribution
(a Normal, or Gaussian, distribution with variance \eqn{1}).
For inferences based on this data in the framework of generalized iLUCK models,
the class \code{\linkS4class{ScaledNormalLuckModel}} can be used.
\code{ScaledNormalData} extends the class \code{\linkS4class{LuckModelData}}.
Objects can be created using the constructor function \code{ScaledNormallData()}
described below.
}
\usage{
ScaledNormalData(arg1 = NULL, arg2 = NULL, data = NULL, mean = NULL, n = NULL,
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
  A vector of observations \eqn{x} from a Normal distribution with variance \eqn{1},
  i.e., \eqn{x \sim N(\mu,1)}{x ~ N(mu,1).}
}
  \item{mean}{
  The mean of \eqn{n} observations from a Normal distribution with variance \eqn{1}.
  Must be supplied together with \code{n}.
}
  \item{n}{
  The sample size of observations from a Normal distribution with variance \eqn{1}
  and mean given in \code{mean}.
}
  \item{sim}{
  If \code{TRUE}, data are simulated according to \code{mean} and \code{n}
  and stored in the object. Defaults to \code{FALSE}.
}
}
\details{
\code{ScaledNormalData} objects can be created using one of the following ways:
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
    the sample statistic \eqn{\tau(x) = n \cdot \bar{x}}{tau(x) = n*mean(x)}, and the second giving the corresponding
    sample size \code{n}.}
    \item{\code{rawData}:}{The data vector, if data was provided or simulated.
    From this data, \eqn{\tau(x)} and \eqn{n} in \code{tauN} are calculated.}
  }
}
\section{Methods for \code{ScaledNormalData} objects}{
There are methods to access or replace the contents of the slots:
  \describe{
    \item{tauN}{\code{signature(object = "ScaledNormalData")} }
    \item{tauN<-}{\code{signature(object = "ScaledNormalData")} }
    \item{tau}{\code{signature(object = "ScaledNormalData")} }
    \item{tau<-}{\code{signature(object = "ScaledNormalData")} }
    \item{n}{\code{signature(object = "ScaledNormalData")} }
    \item{n<-}{\code{signature(object = "ScaledNormalData")} }
    \item{rawData}{\code{signature(object = "ScaledNormalData")} }
    \item{rawData<-}{\code{signature(object = "ScaledNormalData")} }
   }
When the slot \code{rawData} is replaced using \code{rawData<-},
the contents of the slot \code{tauN} are recalculated.
Attempts to replace (parts of) \code{tauN} if the object
contains data in slot \code{rawData} results in an error.\cr
There is a method to display \code{ScaledNormalData} objects by text: 
  \describe{
    \item{show}{\code{signature(object = "ScaledNormalData")}: This is invoked
    when printing a \code{ScaledNormalData} object, or a \code{\linkS4class{ScaledNormalLuckModel}} object
    containing data.}
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
and \code{\linkS4class{ScaledNormalLuckModel}} for the model using this data representation.
}
\examples{
# generate a ScaledNormalData object
set.seed(8553)
x <- rnorm(10)
scndata1 <- ScaledNormalData(data = x)
# also possible:
scndata2 <- ScaledNormalData(mean = mean(x), n = length(x))
scndata3 <- ScaledNormalData(mean = 0, n = 10, sim = TRUE)
data1 <- LuckModelData(tau=20, n=10)
scndata4 <- ScaledNormalData(data1)
# access and replace slots
tauN(scndata1)
rawData(scndata1) <- rnorm(n = 20, mean=3)
scndata1
tau(scndata2)
tau(scndata2) <- 0
n(scndata2)
n(scndata2) <- 100
}
\keyword{classes}
