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
  A list each of whose components is a \eqn{3 \times 3}{3 x 3},
  \eqn{3 \times 4}{3 x 4} or \eqn{3 \times 5}{3 x 5} data frame
  corresponding to one of the Delaunay triangles specified by
  \dQuote{object}.  The rows of each such data frame correspond
  to the vertices of the corresponding Delaunay triangle. The
  columns are:
  \itemize{
  \item \code{ptNum} (the point number of the vertex in the original
  list of points being triangulated)
  \item \code{ptType} (the type of the vertex; \dQuote{data}
  or \dQuote{dummy}; present only if there were any dummy points
  specified)
  \item \code{x} (the \eqn{x}-coordinate of the vertex)
  \item \code{y} (the \eqn{x}-coordinate of the vertex)
  \item \code{z} (the \dQuote{auxiliary value} or \dQuote{weight}
  \code{z} associated with the vertex; present only if such values
  were supplied in the call to \code{deldir()})
  }

  The returned value has an attribute \dQuote{rw} consisting of the
  enclosing rectangle of the triangulation.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
% \url{https://www.stat.auckland.ac.nz/~rolf}
}
\note{
   The code of this function was taken more-or-less directly
   from code written by Adrian Baddeley for the \dQuote{delaunay()}
   function in the \dQuote{spatstat} package.
}
\seealso{ \code{\link{deldir}()}, \code{\link{plot.triang.list}()},
          \code{\link{tile.list}()}, \code{\link{plot.tile.list}()} }

\examples{
set.seed(42)
x   <- runif(20)
y   <- runif(20)
z   <- sample(1:100,20)
d   <- deldir(x,y,z=z)
ttt <- triang.list(d)
}
\keyword{spatial}
