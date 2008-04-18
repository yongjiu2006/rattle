\name{rattle}
\alias{rattle}
\title{Display the Rattle User Interface}
\description{

  The Rattle user interface uses the RGtk2 package to present an
  intuitive point and click interface for data mining, extensively
  building on the excellent collection of R packages for data
  manipulation, exploration, analysis, and evaluation.

}
\usage{
rattle(csvname=NULL)
}
\arguments{
  \item{csvname}{the name of a CSV file to load into Rattle on startup.}
}
\details{

  Refer to the Rattle home page in the URL below for a growing reference
  manual for using Rattle.

  Whilst the underlying functionality of Rattle is built upon a vast
  collection of other R packages, Rattle itself provides a collection of
  utility functions used within Rattle. These are made available through
  loading the rattle package into your R library. The See Also section
  lists these utility functions that may be useful outside of Rattle.
  
  If the environment variable RATTLE\_DATA is defined then that is set
  as the default CSV file name to load, unless csvname is provided.}

\references{Package home page: \url{http://rattle.togaware.com}}

\author{\email{Graham.Williams@togaware.com}}

\seealso{

  \code{\link{evaluateRisk}}, \code{\link{genPlotTitleCmd}},
  \code{\link{plotRisk}}.

}
\examples{
## You can start rattle with a path to a csv file to pre-specify the
## dataset. You then need to click Execute to load the data.

\dontrun{rattle(system.file("csv", "audit.csv", package = "rattle"))}
}
\keyword{environment}
