\name{write.rstat}
\alias{write.rstat}
\title{
  Write a CSV file but with RStat conventiosn for missing values.
}
\description{
  
  Wriate a standard CSV file except with "." to indicate missing for
  factors and "" for numeric.  This is the WebFOCUS standard.
  
}
\usage{
write.rstat(x, file="", ...)
}
\arguments{
  
  \item{x}{the object to be written, preferably a matrix or data
    frame. If not, it is attempted to coerce 'x' to a data frame.}

  \item{file}{either a character string naming a file or a connection open
    for writing.  '""' indicates output to the console.}

  \item{...}{arguments to 'write.csv.}
  
}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
