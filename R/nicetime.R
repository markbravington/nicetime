# This is package nicetime 

".onLoad" <-
function( libname, pkgname) {
#### nicetime #####

  assign.to.base( 'difftime', hack( difftime, 
      units=c( 'secs', 'mins', 'hours', 'days', 'weeks', 'auto')))
  assign.to.base( '+.POSIXt', mvb.plus.POSIXt)
  assign.to.base( '-.POSIXt', mvb.minus.POSIXt)
  assign.to.base( 'print.POSIXct', mvb.print.POSIXt) # POSIXlt too?
  assign.to.base( 'format.POSIXct', mvb.format.POSIXct)
  assign.to.base( 'head.default', mvb.head.default)
  assign.to.base( 'tail.default', mvb.tail.default)
  assign.to.base( 'as.data.frame.POSIXct', mvb.as.data.frame.POSIXct)
}


"cbind.POSIXct" <-
function( ...)
  wrap.bind.call( cbind, ...)


"Days" <-
function( x){
  24*Hours(x)
}


"Hours" <-
function( x){
  Minutes( 60*x)
}


"length<-.POSIXct" <-
function( x, value) {
  att <- attributes( x) %without.name% cq( dim, dimnames, names)
  x <- unclass( x)
  length( x) <- value
  attributes( x) <- att
return( x)
}


"Minutes" <-
function( x){
  Seconds( 60*x)
}


"monseq" <-
function( trange, tz='UTC') {
  if( trange %is.not.a% 'POSIXt')
    class( trange) <- 'POSIX' %&% cq( t, ct)
  if( !length( trange))
return( trange)

  if( length( trange)==1)
    trange <- rep( trange, 2)

  lt <- as.POSIXlt( trange, tz=tz)
  inc.month <- lt$mday[2]>1 
  lt$mday[] <- 1
  lt$hour[] <- lt$min[] <- lt$sec[] <- 0
  mreps <- seq( from = lt$mon[1], to = lt$mon[2] + inc.month + 12 * diff( lt$year), by=1)
  lt <- rep( lt[1], length( mreps))
  lt$mon <- mreps %% 12
  lt$year <- lt$year[1] + (mreps %/% 12)
  as.POSIXct( lt)
}


"mvb.as.data.frame.POSIXct" <-
function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) {
# MVB mod
  if( length( dim( x))==2) {
    orig <- attr( x, 'origin')
    clx <- class( x)
    x <- as.data.frame.matrix( x)
    for( i in seq_along( x)) {
      oldClass( x[[i]]) <- clx
      attr( x[[i]], 'origin') <- orig
    }
return( x)
  }
    
  force(nm)
  nrows <- length(x)
  if (is.null(row.names)) {
      if (nrows == 0L) 
          row.names <- character(0L)
      else if (length(row.names <- names(x)) == nrows && !anyDuplicated(row.names)) {
      }
      else row.names <- .set_row_names(nrows)
  }
  if (!is.null(names(x))) 
      names(x) <- NULL
  value <- list(x)
  if (!optional) 
      names(value) <- nm
  attr(value, "row.names") <- row.names
  class(value) <- "data.frame"
  value
}


"mvb.format.POSIXct" <-
function (x, format = "", tz = "", usetz = FALSE, ...) {
# MVB mod to cope with array of POSIXct
stopifnot( inherits( x, "POSIXct")) 
  if (missing(tz) && !is.null(tzone <- attr(x, "tzone"))) 
    tz <- tzone
  structure(format.POSIXlt(as.POSIXlt(x, tz), format, usetz, ...), 
      names = names(x), dim=dim( x), dimnames=dimnames( x))
}


"mvb.head.default" <-
function (x, n = 6L, ...) {
  # MVB mod to respect matrix structure of non-default S3 objects eg POSIXct
  if( length( dim( x))==2){
    mc <- match.call()
    mc[[1]] <- `head.matrix`
    # mc[[1]] <- call( '::', quote( utils), quote( head.matrix))
    return( eval( mc, parent.frame()))
  }
  stopifnot(length(n) == 1L)
  n <- if (n < 0L) 
      max(length(x) + n, 0L)
    else 
      min(n, length(x))
  x[seq_len(n)]
}


"mvb.minus.POSIXt" <-
function (e1, e2) {
  # Hacked by MVB in 1/2008 to avoid difftime
  if( !inherits( e1, "POSIXt"))
stop("Can only subtract from POSIXt objects")

  if (nargs() == 1) 
stop("unary - is not defined for \"POSIXt\" objects")

  pure.tdiff <- inherits( e2, "POSIXt")
  if( pure.tdiff)
    e2 <- as.POSIXct( e2) # guaranteed seconds
  
  e1 <- as.POSIXct( e1)
  # value <- c( unclass(e1) - Seconds( unclass( e2)))
  # Removed c() Dec 2010 to allow matrices-- dunno side-effects?
  value <- unclass(e1) - Seconds( unclass( e2))
  
  if( !pure.tdiff) {# Changed 12/2010 to keep class order (POSIXt or POSIXct first?) consistent with changes in R
    # and 1/2013 to remove otiose 'units'
    attr( value, 'units') <- NULL
    oldClass( value) <- class( e1)
  }
return( value)
}


"mvb.plus.POSIXt" <-
function (e1, e2) {
  # Hacked by MVB in 1/2008 to avoid difftime
  if (nargs() == 1) 
return(e1)

  if( inherits( e1, "POSIXt") && inherits( e2, "POSIXt"))
stop("binary '+' is not defined for 'POSIXt' objects")

  if( inherits( e2, 'POSIXt')) {
    temp <- e1
    e1 <- e2
    e2 <- temp
  }

  if( inherits( e1, "POSIXlt"))
    e1 <- as.POSIXct(e1) # get tzone OK
  
  # Changed 12/2010 to keep class order (POSIXt or POSIXct first?) consistent with changes in R
  # and 1/2013 to avoid adding otiose 'units' attribute
  e2 <- Seconds( e2)
  attr( e2, 'units') <- NULL
  structure( unclass( e1) + e2, class = class( e1), tzone = attr( e1, 'tzone'))
}


"mvb.print.POSIXt" <-
function( x, ...) { 
  # Keeps matrix/ array structure without losing POSIXity
  thrub <- format( x, usetz=TRUE, ...)
  dim( thrub) <- dim( x)
  dimnames( thrub) <- dimnames( x)
  print( thrub, ...)
return( invisible( x))
}


"mvb.tail.default" <-
function (x, n = 6L, ...) {
  # MVB mod to respect matrix structure of non-default S3 objects eg POSIXct
  if( length( dim( x))==2) {
    mc <- match.call()
    mc[[1]] <- `tail.matrix`
return( eval( mc, parent.frame()))
  }

  stopifnot(length(n) == 1L)
  xlen <- length(x)
  n <- if (n < 0L) 
      max(xlen + n, 0L)
  else min(n, xlen)
  x[seq.int(to = xlen, length.out = n)]
}


"rbind.POSIXct" <-
function( ...)
  wrap.bind.call( rbind, ...)


"Seconds" <-
function( x){
  if( !is.null( attr( x, 'units')))
    x <- switch( attr(x, "units"), secs = x, mins = 60 * x, 
        hours = 60 * 60 * x, days = 60 * 60 * 24 * x, 
        weeks = 60 * 60 * 24 * 7 * x)
  attr( x, 'units') <- 'secs'
  x
}


"Weeks" <-
function( x) Days( 7*x)


"wrap.bind.call" <-
function( BIND, ...){
  l <- list(...)
  atts <- lapply( l, function( x) { 
    a <- attributes( x) %without.name% cq( dim, dimnames)
    if( length( a)) {
      onama <- order( names( a))
      a <- a[ onama]
    }
    a
  })
  sambo <- all( sapply( atts, my.all.equal, y=atts[[1]]))
  l <- lapply( l, function( x) { attributes( x) <- attributes( x)[ cq( dim, dimnames)]; x })
  rb <- do.call( BIND, l)
  if( sambo)
    attributes( rb) <- c( attributes( rb), atts[[1]])
return( rb)
}


"ydays" <-
function( x, ...) UseMethod( 'ydays')


"ydays.POSIXt" <-
function( x, ystart='1-1', ...) {
# If 'ystart' is a character, then it will be converted to a date in this format:
# 12-25 is Xmas. No year part
# First day of year, ie 1st Jan, is day 1-- whereas posix has day 0...

  if( is.character( ystart))
    ystart <- '2001-' %&% ystart
  yd <- as.POSIXlt( x)$yday - as.POSIXlt( ystart)$yday + 1
  yd[ yd<0] <- yd[ yd<0] + 365 # leap years ignored
  yd
}


"years" <-
function( x, ...) UseMethod( 'years')


"Years" <-
function( x, roughly=TRUE) {
  (if( roughly) 365 else 365.25) * Days( x)
}


"years.Date" <-
function( x, ...) as.integer( format( x, '%Y'))


"years.POSIXt" <-
function( x, start='1-1', has.Jan=TRUE, ...) {
  x <- as.POSIXlt( x)
  years <- x$year+1900
  if( is.character( start))
    start <- '0000-' %&% start
  start.day <- as.POSIXlt( start)$yday
  prev <- x$yday < start.day
  years[ prev] <- years[ prev] - 1
  if( has.Jan && start.day>0)
    years <- years+1
    
  years
}

