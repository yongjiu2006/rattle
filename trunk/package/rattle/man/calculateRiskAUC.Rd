\name{calculateRiskAUC}
\alias{calculateRiskAUC}
\title{
  Determine the area under the risk and recall curves of a risk chart
}
\description{
  
  Given the evaluation returned by evaluateRisk, calculate the area
  under the risk and recall curves, to use as a metric to compare the
  performance of a model.
  
}
\usage{
calculateRiskAUC(ev)
}
\arguments{
  
  \item{ev}{an evaluation matrix as returned by evaluateRisk.}

}
\details{

  The two areas are returned.

}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\seealso{\code{\link{evaluateRisk}}.}
\examples{
## this is usually used in the context of the evaluateRisk function
\dontrun{ev <- evaluateRisk(predicted, actual, risk)}

## imitate this output here
ev <- data.frame(Caseload=c(1.0, 0.8, 0.6, 0.4, 0.2, 0),
                 Precision=c(0.15, 0.18, 0.21, 0.25, 0.28, 0.30),
                 Recall=c(1.0, 0.95, 0.80, 0.75, 0.5, 0.0),
                 Risk=c(1.0, 0.98, 0.90, 0.77, 0.30, 0.0))

## Calculate the areas unde the Risk and the Recall curves.
calculateRiskAUC(ev)

}
\keyword{hplot}
