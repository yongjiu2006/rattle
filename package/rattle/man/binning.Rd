\name{binning}
\alias{binning}
\title{
  Perform binning over numeric data
}
\description{
  
  Perform binning.
  
}
\usage{
binning(x, bins=4, method=c("quantile", "kmeans"),
                     labels=NULL, ordered=TRUE)
}
\arguments{
  
  \item{x}{the numeric data to bin.}

  \item{bins}{the number of bins to use.}

  \item{method}{whether to use "quantile" or "kmeans" binning.}

  \item{labels}{the labels or names to use for each of the bins.}

  \item{ordered}{whether to build an ordered factor or not.}
  
}
\details{

  Bin the provided nmeric data into the specified number of bins using
  one of the supported methods. The bins will have the names specified
  by labels, if supplied. The result can optionally be an ordered
  factor.
  
}
\value{A factor is returned.}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{Daniele Medri and \email{Graham.Williams@togaware.com}}
