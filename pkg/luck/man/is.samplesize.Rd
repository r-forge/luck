\name{is.samplesize}
\alias{is.samplesize}
\title{
Function to check if the argument is a sample size, i.e., a positive integer.
}
\description{
A little helper function to check if the argument is a sample size, i.e.,
a single natural number larger than zero.
This function is used to check the input to
\code{\linkS4class{ScaledNormalData}} and \code{\linkS4class{ExponentialData}}.
}
\usage{
is.samplesize(n)
}
\arguments{
  \item{n}{
  The argument to test. Vectors return \code{"FALSE"}.
}
}
\value{
\code{"TRUE"} if is \code{n} is numeric, positive, integer, and has length \code{1};
otherwise \code{"FALSE"}.
}
\author{
Gero Walter
}
\seealso{
\code{\link{is.integer}} checks for R's internal type \code{integer},
and not if its argument is an integer in the mathematical sense.
}
\examples{
is.samplesize(1)    # TRUE
is.samplesize(1:10) # FALSE
}
