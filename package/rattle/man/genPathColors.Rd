\name{genPathColors}
\alias{genPathColors}
\title{
  Generate a smooth transition from one color to another.
}
\description{
  
  Given a starting colour, an intermediate colour, and an end colour,
  generate a smooth colour transistion, useful for plotting.

}
\usage{
genPathColors(n, path=c('cyan', 'white', 'magenta'), interp=c('rgb','hsv'))
}
\arguments{
  
  \item{n}{Number of colours to generate.}

  \item{path}{The colours to range from, through, to.}

  \item{interp}{How the colours are to be interpreted.}

}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\examples{
plot(1:10, col=genPathColors(10, c("red", "white", "blue")), pch=19)

}
\keyword{hplot}
