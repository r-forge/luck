\name{LuckModelData-class}
\Rdversion{1.1}
\docType{class}
\alias{LuckModelData-class}
\alias{LuckModelData}
\alias{n<-,LuckModelData-method}
\alias{n,LuckModelData-method}
\alias{n-methods}
\alias{n<--methods}
\alias{n}
\alias{n<-}
\alias{rawData<-,LuckModelData-method}
\alias{rawData,LuckModelData-method}
\alias{rawData-methods}
\alias{rawData<--methods}
\alias{rawData}
\alias{rawData<-}
%\alias{show,LuckModelData-method}
\alias{tau<-,LuckModelData-method}
\alias{tau,LuckModelData-method}
\alias{tau-methods}
\alias{tau<--methods}
\alias{tau}
\alias{tau<-}
\alias{tauN<-,LuckModelData-method}
\alias{tauN,LuckModelData-method}
\alias{tauN-methods}
\alias{tauN<--methods}
\alias{tauN}
\alias{tauN<-}

\title{Class \code{"LuckModelData"} and its constructor function \code{LuckModelData()}}
\description{
\code{LuckModelData} objects are an abstract data representation for inference
with generalized iLUCK models as implemented in \code{\linkS4class{LuckModel}}.
Pure \code{LuckModelData} objects (that are not simultaneously instances of a
subclass inheriting from \code{LuckModelData}) are useful only for studying the
parameter update step and the shapes of posterior parameter sets of generalized
iLUCK models. For data-based inferences, classes inherting from
\code{LuckModelData} must be used, see, e.g.,
\code{\linkS4class{ScaledNormalData}} and \code{\linkS4class{ExponentialData}}.

Objects can be created using the constructor function \code{LuckModelData()}
described below.
}
\usage{
LuckModelData(arg1 = NULL, arg2 = NULL, tau = NULL, n = NULL)
}
\arguments{
  \item{tau}{
  A numeric vector containing the sample statistic(s) \eqn{\tau(x)}, the length
  of which depends on the dimension of \eqn{\tau(x)} for the sample
  distribution.
}
  \item{n}{
  A numeric vector containing the sample size(s) belonging to the sample
  statistic(s) \eqn{\tau(x)} as supplied in \code{tau}. \code{n} must either
  have the same dimension as \code{tau} or be a single value, the latter of
  which is the usual case.
}
  \item{arg1}{
  Used for treatment of unnamed arguments only, see Details.
}
  \item{arg2}{
  Used for treatment of unnamed arguments only, see Details.
}
}
\details{
To create a \code{LuckModelData} object, \code{LuckModelData()} must be called
with either of the following argument (sets):
\itemize{
  \item \code{tau} \emph{and} \code{n} as described above, both, or either, or
  none of which may given as named arguments. If called with two unnamed
  arguments, \code{LuckModelData()} assumes the first to be \code{tau} and the
  second to be \code{n}.
  \item A matrix containing two columns, with the first giving the sample
  statistic(s) \eqn{\tau(x)}, and the second giving the corresponding sample
  size(s) \eqn{n}. (Usually, \eqn{n} is the same for al dimensions of
  \eqn{\tau(x)}.)
  \item A list containing \eqn{\tau(x)} and \eqn{n}, one or both of which may
  be named \code{tau} or \code{n} to identify which is which. If the list
  elements have no names, the first is taken as as \code{tau}, and the second
  as \code{n}.
}
}

\section{Slots of the resulting object}{
  \describe{
    \item{\code{tauN}:}{A matrix with two named columns, with the first giving
    the sample statistic(s) \code{tau}, and the second giving the corresponding
    sample size(s) \code{n}. In a default \code{LuckModelData} object,
    \code{tauN} is \code{NULL}.}
    \item{\code{rawData}:}{Slot to store data vector(s) from which
    \eqn{\tau(x)} and \eqn{n} can be determined. For pure \code{LuckModelData}
    objects (that are not simultaneously instances of a subclass inheriting
    from \code{LuckModelData}), \code{rawData} is \code{NULL}.}
  }
}
\section{Methods}{
There are methods to access or replace the contents of the slots:
  \describe{
    \item{tauN}{\code{signature(object = "LuckModelData")} }
    \item{tauN<-}{\code{signature(object = "LuckModelData")} }
    \item{tau}{\code{signature(object = "LuckModelData")} }
    \item{tau<-}{\code{signature(object = "LuckModelData")} }
    \item{n}{\code{signature(object = "LuckModelData")} }
    \item{n<-}{\code{signature(object = "LuckModelData")} }
    \item{rawData}{\code{signature(object = "LuckModelData")} }
    \item{rawData<-}{\code{signature(object = "LuckModelData")} }
   }
The replacement function for \code{rawData} is implemented at the level of the
subclasses inheriting from \code{LuckModelData} to allow for
distribution-specific consistency checks.\cr
There is a method to display \code{LuckModelData} objects by text: 
  \describe{
    \item{show}{\code{signature(object = "LuckModelData")}: This is invoked
    when printing a \code{LuckModelData} object, or a \code{LuckModel} object
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
\code{\linkS4class{LuckModel}} for the models using this data representation,
and \code{\linkS4class{ScaledNormalData}} or
\code{\linkS4class{ExponentialData}} for non-abstract subclasses intended for
analysis of scaled normal or exponential data, respectively.
}
\examples{
# generate a LuckModelData object
data1 <- LuckModelData(tau=20, n=10)
data1
data1 <- LuckModelData(20, 10)
data2 <- LuckModelData(tau=c(50,20), n=10)
data2 <- LuckModelData(c(50,20), 10)
data2 <- LuckModelData(list(tau=c(50,20), 10))
data2 <- LuckModelData(matrix(c(50,20,10,10), ncol=2))
# access and replace slots
tauN(data1)
tau(data2)
tau(data2) <- c(0,0)
n(data2)
n(data2) <- c(5,5)
}
\keyword{classes}
\keyword{data}
