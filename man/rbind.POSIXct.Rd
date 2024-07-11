\name{rbind.POSIXct}
\alias{rbind.POSIXct}
\alias{cbind.POSIXct}
\title{rbind and cbind methods for POSIXct}
\description{Lets you rbind and cbind POSIXct matrices in the obvious way.
}
\usage{
\method{rbind}{POSIXct}(...) # rbind.POSIXct
\method{cbind}{POSIXct}(...) # cbind.POSIXct
}
\arguments{\item{ ...}{things to bind}
}
\value{A matrix of class \code{POSIXct}, which will display nicely thanks to the \pkg{nicetime} package.
}
\examples{
st <- structure( Sys.time() + Hours( 0:3), dim=c(2, 2))
#     [,1]                      [,2]
#[1,] "2012-09-07 14:22:30 EST" "2012-09-07 16:22:30 EST"
#[2,] "2012-09-07 15:22:30 EST" "2012-09-07 17:22:30 EST"
rbind( st, st)
#     [,1]                      [,2]
#[1,] "2012-09-07 14:22:30 EST" "2012-09-07 16:22:30 EST"
#[2,] "2012-09-07 15:22:30 EST" "2012-09-07 17:22:30 EST"
#[3,] "2012-09-07 14:22:30 EST" "2012-09-07 16:22:30 EST"
#[4,] "2012-09-07 15:22:30 EST" "2012-09-07 17:22:30 EST"
}
\keyword{misc}
