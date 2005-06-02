\name{plot.tile.list}
\alias{plot.tile.list}
\title{ Plot Dirchlet/Voronoi tiles }
\description{ A method for \code{plot}.  Plots (sequentially)
the tiles associated with each point in the set being tessellated. }
\usage{
plot.tile.list(x, verbose = TRUE, ...)
}
\arguments{
  \item{x}{ A list of the tiles in a tessellation, as produced
	the function \code{\link{tile.list}()}.}
  \item{verbose}{ Logical scalar; if \code{TRUE} the tiles are
	plotted one at a time (with a ``Go?'' prompt after each)
	so that the process can be watched.}
  \item{...}{ Optional arguments; not used.  There for consistency
        with the generic \code{plot} function.}
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
}
\keyword{ hplot }
