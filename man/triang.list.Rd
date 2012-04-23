\name{triang.list}
\alias{triang.list}
\title{
  Create a list of Delaunay triangles
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
  A list each of whose components is a \eqn{3 \times 2}{3 x 2}
  or \eqn{3 \times 3}{3 x 3} data frame corresponding
  to one of the Delaunay triangles specified by \dQuote{object}.  The
  rows of each such data frame consist of the coordinates of the
  vertices of the corresponding Delaunay triangle, and possibly the
  values of the \dQuote{auxiliary} variable or \dQuote{weight} \code{z}
  associated with the vertices (if such values were supplied in the
  call to \code{deldir()}).

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
x   <- runif(20)
y   <- runif(20)
z   <- sample(1:100,20)
d   <- deldir(x,y,z=z)
ttt <- triang.list(d)
}
\keyword{spatial}
