\name{rattle.print.rpart}
\alias{rattle.print.rpart}
\title{
  Modified printing of rpart decision tree
}
\description{
  
  Display a textual veiw of an rpart decision tree.
  
}
\usage{
rattle.print.rpart(x, minlength = 0, spaces = 2, cp,
                   digits = getOption("digits"), ...)
}
\arguments{
  
  \item{x}{An rpart object.}

  \item{minlength}{The minimum length.}

  \item{spaces}{How much to indent.}

  \item{cp}{Complexity parameter}

  \item{digits}{Number of digits to display for numbers.}

  \item{...}{Other arguments.}

}
\details{

  Print decision tree.

}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\keyword{tree}
