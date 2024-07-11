\name{Hours}
\alias{Hours}
\alias{Minutes}
\alias{Seconds}
\alias{Days}
\alias{Weeks}
\alias{Years}
\title{Sorting out times and time difference}
\description{These functions enforce seconds-based calculations of time differences. \code{Weeks}, \code{Days}, \code{Hours}, \code{Minutes}, and \code{Seconds} all return the obvious number of \bold{seconds}, together with the attribute \code{unit="secs"}. Along with the other modifications made by the \pkg{nicetime} package when it loads, this means you always get units of "secs" with \code{POSIXct} calculations, rather than base-R's random habit of sometimes make the units "days" etc. You \emph{can} still force \code{difftime} to calculate in different units, via its \code{units} argument, but the default is now "secs" rather than "auto".
}
\usage{
 Weeks( x)
 Days( x)
 Hours( x)
 Minutes( x)
 Seconds( x)
 Years( x, roughly=TRUE)
}
\arguments{
\item{  x}{numeric vector in the units of the name of the called function.}
\item{  roughly}{if TRUE, assume 365 days per year; if FALSE, assume 365.25 (Julian calendar).}
}
\value{A numeric vector of the same length as \code{x}, giving the equivalent timespan in seconds, together with an attribute \code{unit="secs"}. Be slightly careful what you do with it; there's no class, S3 or otherwise, so operations such as \code{*} and \code{/} will have meaningless units (see \bold{Examples}).
}
\author{Mark Bravington}
\examples{
#Hours( 2)
##[1] 7200
##attr(,"units")
##[1] "secs"
#Years(1) / Days(1)
#[1] 365
#attr(,"units")
#[1] "secs"
### well kinda sorta...
}
\keyword{misc}
