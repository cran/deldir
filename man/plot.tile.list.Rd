\name{plot.tile.list}
\alias{plot.tile.list}
\title{ Plot Dirchlet/Voronoi tiles }
\description{ A method for \code{plot}.  Plots (sequentially)
the tiles associated with each point in the set being tessellated. }
\usage{
plot.tile.list(x, verbose = FALSE, close=FALSE, pch=1, polycol=NA,
               showpoints=TRUE, ...)
\method{plot}{tile.list}(x, verbose = FALSE, close=FALSE, pch=1,
                         polycol=NA, showpoints=TRUE, ...)
}
\arguments{
  \item{x}{ A list of the tiles in a tessellation, as produced
	the function \code{\link{tile.list}()}.}
  \item{verbose}{ Logical scalar; if \code{TRUE} the tiles are
	plotted one at a time (with a ``Go?'' prompt after each)
	so that the process can be watched.}
  \item{close}{ Logical scalar; if \code{TRUE} the outer edges of
	of the tiles (i.e. the edges of the enclosing rectangle)
	are drawn.  Otherwise tiles on the periphery of the
	tessellation are left ``open''.}
  \item{pch}{The plotting character for plotting the points of the
	pattern which was tessellated.  Ignored if \code{showpoints}
	is \code{FALSE}.}
  \item{polycol}{Optional vector of integers (or \code{NA}s);
	the \eqn{i}-th entry indicates with which colour to fill
	the \eqn{i}-th tile.  Note that an \code{NA} indicates
        the use of no colour at all.}
  \item{showpoints}{Logical scalar; if \code{TRUE} the points of
	the pattern which was tesselated are plotted.}
  \item{...}{ Optional arguments; not used.  There for consistency
        with the generic \code{plot} function.}
}
\section{Warning}{
   The default value for \code{verbose} was formerly \code{TRUE};
   it is now \code{FALSE}.
}
\value{NULL; side effect is a plot.}
\author{ Rolf Turner }
\seealso{
\code{\link{tile.list}()}
}
\examples{
	x <- runif(20)
	y <- runif(20)
	z <- deldir(x,y,rw=c(0,1,0,1))
	w <- tile.list(z)
	plot(w)
	ccc <- rep(2:5,5)
	plot(w,polycol=ccc,close=TRUE)
}
\keyword{ hplot }
