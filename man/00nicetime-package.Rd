\name{nicetime-package}
\alias{nicetime}
\alias{nicetime-package}
\docType{package}
\title{Nicer handling of POSIXct objects}
\description{
Package \pkg{nicetime} provides nicer handling of \code{POSIXct} variables. To do these good things, it has to overwrite the base-R versions of \code{difftime}, \code{+.POSIXt}, \code{-.POSIXt}, \code{head.matrix}, and \code{tail.matrix} (or it used to do that, anyway; not sure nowadays). This is wicked, vile behaviour, which will never ever be allowed on CRAN; I find it extremely useful. Note that \code{nicetime} is quite an old package, and I have not tried to update it or check it since about 2014 (there might be better ways to do some of the sneaky bits). It may well have some dicey features; nevertheless, it might still be useful. Use at your own risk, like always but more so. But it does give you:

\itemize{
\item sensible arithmetic without nanny watching, e.g. \code{Sys.time() + Hours(1)}
\item proper display of POSIXct matrices and arrays, including via \code{head}, which show up wrong with base-R
\item and you can \code{rbind} or \code{cbind} them
\item POSIXct matrices can be incorporated into \code{data.frames}
}

Functions \code{\link{Weeks}}, \code{\link{Days}}, \code{\link{Hours}}, \code{\link{Minutes}}, and \code{\link{Seconds}} allow the definition of time intervals (all in units of "secs", so no more nasty surprises when the units arbitrarily change) which now work nicely with \code{POSIXct} objects. There are also (new and CRAN-legit) methods for \code{\link{rbind.POSIXct}}, \code{\link{cbind.POSIXct}}, and \code{length<-.POSIXct}, to facilicate the matrix-handling. You can also now have matrices of POSIXct objects \emph{inside} data.frames.

\code{\link{ydays}} and \code{\link{years}} are convenient extractors that do what you'd expect, analogous to \code{months} in base R{}. Note that \code{\link{years}} is lower-case unlike e.g. \code{\link{Weeks}}, and as such cannot be used to "define" a set number of years--- that is not a well-defined operation because of leap-years.

\code{\link{monseq}} is useful for plotting axes; it returns a POSIXct vector of the first day of each month that spans the range of its argument.

These functions used to be in \code{mvbutils} and/or \code{handy2}, but I moved them out because they are non-core to \code{mvbutils} and some break CRAN policy by overwriting; I am trying to keep a low profile with \code{mvbutils} CRANwise. The obvious alternative might be \code{handy2} (my usual repository for the unCRANnable) but these few functions form a specific group that is worth documenting--- \code{handy2} is mostly an undocumented mess from the general user's PoV, and is likely to remain so. So I made a new package.

Eventually I might get round to writing a general "units" package that has a \code{units} S3 class and keeps track of physical dimensionality; then time-intervals would be a natural inclusion. But I haven't yet.
\subsection{Notes}{
To \emph{construct} a matrix of POSIXct variables, you can't just call \code{matrix}-- that would be too easy, wouldn't it? See \bold{Examples}. I could avoid this by making \code{matrix} generic (like the \pkg{gmp} package does, for example), but I was worried about slowing down a basic function.

\code{difftime}, \code{+.POSIXt}, and \code{-.POSIXt} are modified to behave more consistently and forgivingly. Results won't break code that doesn't make invalid assumptions.

\code{print.POSIXct}, \code{format.POSIXct}, \code{as.data.frame.POSIXt}, and the new methods \code{\link{rbind.POSIXct}} & \code{\link{cbind.POSIXct}} & \code{length<-.POSIXct}, are modified/created to honour any matrix/array shape of \code{POSIXct} objects. Without this mod, R{} forces you to either hide the matrix shape if you want to see the POSIXity, or to discard POSIXity if you want to see the matricity. I'm not sure that a \code{POSIXlt} matrix makes sense, so I haven't provided comparable mods for \code{POSIXlt} yet.

\code{head.default} and \code{tail.default} now call \code{head/tail.matrix} if passed any matrix (i.e. any object with a \code{dim} attribute of length 2). This is almost certainly what you want. Although there are already \code{head/tail.matrix} methods, by default they won't be invoked for 2D objects that have a non-default S3 class, such as \code{POSIXct} objects. This mod fixes the problem.

The mere existence of a \code{\link{cbind.POSIXct}} method means that you have to wrap \code{POSIXct} objects in a call to \code{data.frame} when you \code{cbind} them to a \code{data.frame}. Without this, for example, \code{cbind( data.frame( x=1), Sys.time())} will return a \code{matrix} not a \code{data.frame}; you need \code{cbind( data.frame( x=1), data.frame( Sys.time()))}. This is a consequence of R{}'s dispatch rules for \code{rbind/cbind}, and it is good practice for \emph{all} objects to wrap them with \code{data.frame} when \code{cbind}ing to a \code{data.frame}.
}
}
\section{Usage arguments and values}{
A \emph{time-interval} is a classless numeric object with a \code{units} attribute equal to "secs", "mins", "hours", "days", or "weeks".

Usage, arguments, and values are as you'd expect for the As per usual, except:

\itemize{
\item the result of \code{-.POSIXt} is a time-interval (i.e. not class "difftime", which I recommend avoiding)
\item you can add/subtract time-intervals to each other
\item you can add/subtract time-intervals to \code{POSIXct} objects, to get a \code{POSIXct} result
\item the default unit for \code{difftime} is "secs" not "auto" (but I never use \code{difftime}, since \code{a-b} is a lot simpler and nanny is not involved)
\item [This should probably be done in a different package. But no-one has complained yet, so the mods will stay until/unless enough people do...] this will change!!!
}

 -
}
\examples{
Sys.time()-Sys.time()
#[1] 0
#attr(,"units")
#[1] "secs"
st <- structure( Sys.time() + Hours( 0:3), dim=c( 2, 2))
#     [,1]                      [,2]
#[1,] "2012-09-07 13:45:59 EST" "2012-09-07 15:45:59 EST"
#[2,] "2012-09-07 14:45:59 EST" "2012-09-07 16:45:59 EST"
Sys.time() + structure( Hours( 1:4), dim=c(2, 2))
#     [,1]                      [,2]
#[1,] "2012-09-07 14:59:47 EST" "2012-09-07 16:59:47 EST"
#[2,] "2012-09-07 15:59:47 EST" "2012-09-07 17:59:47 EST"
data.frame( st= structure( Sys.time() + Hours( 0:3), dim=c( 2, 2)))
#                st.V1               st.V2
#1 2012-09-07 13:49:52 2012-09-07 15:49:52
#2 2012-09-07 14:49:52 2012-09-07 16:49:52
st <- structure( Sys.time() + Hours( 0:3), dim=c(2, 2))
#     [,1]                      [,2]
#[1,] "2012-09-07 14:22:30 EST" "2012-09-07 16:22:30 EST"
#[2,] "2012-09-07 15:22:30 EST" "2012-09-07 17:22:30 EST"
rbind( st, st)
#     [,1]                      [,2]
#[1,] "2012-09-07 13:45:59 EST" "2012-09-07 15:45:59 EST"
#[2,] "2012-09-07 14:45:59 EST" "2012-09-07 16:45:59 EST"
#[3,] "2012-09-07 13:45:59 EST" "2012-09-07 15:45:59 EST"
#[4,] "2012-09-07 14:45:59 EST" "2012-09-07 16:45:59 EST"
cbind( data.frame( x=1), Sys.time()) # wrong!
#   [,1] [,2]
# x 1    1346992426
cbind( data.frame( x=1), data.frame( Sys.time())) # right!
#   x          Sys.time..
# 1 1 2012-09-07 14:33:20
# Base-R version without 'nicetime' doesn't show the matrix property:
# structure( rep( Sys.time(), 4), dim=c( 2, 2))
# [1] "2012-09-07 13:48:31 EST" "2012-09-07 13:48:31 EST" "2012-09-07 13:48:31 EST"
# [4] "2012-09-07 13:48:31 EST"
}
\keyword{misc}
