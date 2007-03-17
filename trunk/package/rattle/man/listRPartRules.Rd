\name{listRPartRules}
\alias{listRPartRules}
\title{
  List the rules corresponding to the rpart decision tree
}
\description{
  
  Display a list of rules for an rpart decision tree.
  
}
\usage{
listRPartRules(model, compact=FALSE)
}
\arguments{
  
  \item{model}{an rpart model.}

  \item{compact}{whether to list cateogricals compactly.}

}
\details{

  Traverse a decision tree to generate the equivalent set of rules, one
  rule for each path from the root node to a leaf node.

}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\examples{
\dontrun{listRPartRules(ds.rpart)}
}
\keyword{tree}
