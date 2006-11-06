\name{audit}
\docType{data}
\alias{audit}
\title{Sample dataset for illustration Rattle functionality.}
\description{
  
  The audit dataset is an artificially constructed dataset that has some
  of the characteristics of a true audit dataset for modelling
  productive and non-productive audits. It is used to illustrate binary
  classification.

  The target variable is \code{Adjusted}, an integer which is either 0
  (for non-producitve audits) or 1 (for productive audits). Productive
  audits are those that result in an adjustment being made to a client's
  claims. The dollar value of those adjustments is also recorded (as the
  \code{Adjusted} column).

  The independent variables include \code{Age}, type of
  \code{Employment}, level of \code{Education}, \code{Marital} status,
  \code{Occupation}, level of \code{Income}, \code{Sex}, amount of
  \code{Deductions} being claimed, \code{Hours} worked per week, and
  country in which they have a bank \code{Account}.

  An identifier is included as the \code{ID} variable.

  The dataset is quite small, consisting of just 2000 entities. It
  primary purpose is to illustrate modelling in Rattle, so a minimally
  sized dataset is suitable.
}
\format{
  A data frame.
}
\keyword{datasets}
