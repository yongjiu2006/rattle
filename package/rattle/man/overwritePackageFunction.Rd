\name{overwritePackageFunction}
\alias{overwritePackageFunction}
\title{Overwrite an internal function of a package.}
\description{

  Rattle supports a plugin infrastructure to allow other package writers
  to fine tune the Rattle interface and behaviour. This function
  provides a simple mechanism to allow these packages to define their
  own versions of functions that will replace the same function in Rattle.

}
\usage{
overwritePackageFunction(fname, fun, pkg)
}
\arguments{
  \item{fname}{a string naming the Rattle function to overwrite.}
  \item{fun}{the actual function to overwrite the Rattle function.}
  \item{pkg}{the package in which the function will be overwritten.}
}
\details{

  Refer to the Rattle home page in the URL below for a growing reference
  manual for using Rattle, which overwritePackageFunction is part of.

}

\references{Package home page: \url{http://rattle.togaware.com}}

\author{\email{Graham.Williams@togaware.com}}

\seealso{

  \code{\link{rattle}}.

}
\keyword{environment}
