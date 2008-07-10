\name{RStat}
\alias{RStat}
\title{Display the RStat User Interface}
\description{

  The RStat user interface, based on Rattle but badged differently and
  tuned for integration with a business intelligence product, uses the
  RGtk2 package to present an intuitive point and click interface for
  data mining, extensively building on the excellent collection of R
  packages for data manipulation, exploration, analysis, and evaluation.

}
\usage{
RStat(csvname=NULL, ...)
}
\arguments{
  \item{csvname}{the name of a CSV file to load into RStat on startup.}

  \item{...}{arguments passed on to rattle.}
}
\details{

  Refer to the Rattle home page in the URL below for a growing reference
  manual for using Rattle, on which RStat is based.

  Whilst the underlying functionality of Rattle is built upon a vast
  collection of other R packages, Rattle itself provides a collection of
  utility functions used within Rattle. These are made available through
  loading the rattle package into your R library. The See Also section
  lists these utility functions that may be useful outside of Rattle.
  
  Rattle can initialise some options using a .Rattle file if the folder
  in which Rattle is started. The currently supported options are
  .RATTLE.DATA, .RATTLE.SCORE.IN, and .RATTLE.SCORE.OUT.
  
  If the environment variable RATTLE\_DATA is defined then that is set
  as the default CSV file name to load. Otherwise, if .RATTLE.DATA is
  defined then that will be used as the CSV file to load. Otherwise, if
  csvname is provided then that will be used.}

\references{Package home page: \url{http://rattle.togaware.com}}

\author{\email{Graham.Williams@togaware.com}}

\seealso{

  \code{\link{rattle}}, \code{\link{evaluateRisk}},
  \code{\link{genPlotTitleCmd}}, \code{\link{plotRisk}}.

}
\examples{
# You can start rattle with a path to a csv file to pre-specify the
# dataset. You then need to click Execute to load the data.

RStat(system.file("csv", "audit.csv", package = "rattle"))
}
\keyword{environment}
