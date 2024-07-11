\name{monseq}
\alias{monseq}
\title{Months spanning a time range}
\description{Returns a vector of POSIXct dates corresponding to the month-starts that span a time interval--- i.e. the last month before and the first month after. Useful for graph axes.
}
\usage{monseq( trange, tz='UTC')
}
\arguments{
\item{ trange}{POSIXct vector}
\item{ tz}{Time zone string, by default "UTC"}
}
\value{POSIXct vector, of length zero iff \code{length(trange)==0}, otherwise of length at least 2.
}
\examples{
monseq( Sys.time()) # length-2 POSIXct vector
}
\keyword{misc}
