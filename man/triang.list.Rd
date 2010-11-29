\name{triang.list}
\alias{triang.list}
\title{
  Produce a list of Delaunay triangles.
}
\description{
  From an object of class \dQuote{deldir} produces a list
  of the Delaunay triangles in the triangulation of a set
  of points in the plane.
}
\usage{
triang.list(object)
}
\arguments{
  \item{object}{
  An object of class \dQuote{deldir} as produced by \code{\link{deldir}()}.
}
}
\value{
  A list each of whose components is a 3-x-2 data frame corresponding
  to one of the Delaunay triangles specified by \dQuote{object}.  The
  rows of each such data frame consist of the coordinates of the
  vertices of the corresponding Delaunay triangle.

  The returned value has an attribute \dQuote{rw} consisting of the
  enclosing rectangle of the triangulation.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
  \url{http://www.math.unb.ca/~rolf}
}
\note{
   The code of this function was taken more-or-less directly
   from code written by Adrian Baddeley for the \dQuote{delaunay()}
   function in the \dQuote{spatstat} package.
}
\seealso{ \code{\link{deldir}()} \code{\link{plot.triang.list}()}
          \code{\link{tile.list}()} \code{\link{plot.tile.list}()} }

\examples{
set.seed(42)
x <- runif(20)
y <- runif(20)
d <- deldir(x,y)
ttt <- triang.list(d)
}
\keyword{spatial}
