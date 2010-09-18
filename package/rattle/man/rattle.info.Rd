\name{rattle.info}
\alias{rattle.info}
\title{Print version information for Rattle, R, associated packages and system.}
\description{

  Display system information, including versions of Rattle and R,
  operating system, and other packages used by Rattle. Useful for
  reporting bugs.

}
\usage{
rattle.info(all.dependencies=FALSE,
            include.not.installed=FALSE,
            include.not.available=FALSE)
}
\arguments{
  
  \item{all.dependencies}{If TRUE then check the full dependency graph
    for Rattle and list all of those packages (which may take quite a
    few seconds to compute), or else just list those key packages that
    Rattle Depends on and Suggests.}

  \item{include.not.installed}{If TRUE then make mention of any packages
    that are not installed, but are available.}

  \item{include.not.available}{If TRUE then make mention of any packages
    that are not available from CRAN.}

}
\details{

  This is a support function to list useful information to provide the
  developers with information about the system environment when running
  Rattle. It is intended to provide the information that is useful in
  reporting bugs.

  It also lists the currently installed version of a number of packages
  that Rattle makes use of as well as checking for any updates available
  for those packages. If updates are found then a command is generated
  and printed so that a user can simply copy and paste the command to
  update the relevant packages.

}

\references{Package home page: \url{http://rattle.togaware.com}}

\author{\email{Graham.Williams@togaware.com}}

\seealso{

  \code{\link{rattle}}.

}

\keyword{environment}
