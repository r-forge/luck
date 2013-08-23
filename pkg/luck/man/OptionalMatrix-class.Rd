\name{OptionalMatrix-class}
\Rdversion{1.1}
\docType{class}
\alias{OptionalMatrix-class}

\title{Class \code{"OptionalMatrix"}}
\description{
Objects of this class are either a \code{\link{matrix}} or \code{NULL}.
This is the class of the slots \code{tauN} and \code{rawData}
in \code{\linkS4class{LuckModelData}} objects.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Methods}{
No methods defined with class "OptionalMatrix" in the signature.
There is nothing to see here. Go away.
}
\author{
Gero Walter
}
\note{
Sincerely, I have no idea why \code{\link{promptPackage}},
in producing a skeleton man page for this class, asks me to write a documentation for this class.
It's internal. You don't need to know about it.
All in all, writing documentation for S4 classes and methods is a mess,
and doesn't blend very well with usual documentation in R.
If I may give you some advice, don't bother about S4 classes,
unless you really, really, really, really want to use them.\cr
\cr
Sorry about this rant.
}
\seealso{
\code{\link{luck}} for a general description of the package,
\code{\linkS4class{LuckModel}} for its central class.
}
\examples{
showClass("OptionalMatrix")
}
\keyword{classes}
