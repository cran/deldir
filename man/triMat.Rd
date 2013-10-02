\name{triMat}
\alias{triMat}
\title{
  Produce matrix of triangle vertex indices.
}
\description{
  Lists the indices of the vertices of each Delaunay triangle in
  the triangulation of a planar point set.  The indices are listed
  (in increasing numeric order) as the rows of an \eqn{n \times 3}{n x 3}
  matrix where \eqn{n} is the number of Delaunay triangles in the
  triangulation.
}

\usage{
triMat(object)
}
\arguments{
  \item{object}{
  An object of class \code{deldir} (as produced by the funtion
  \code{\link{deldir}()}) specifying the Delaunay triangulation
  and Dirichlet (Voronoi) tesselation of a planar point set.
}
}
\details{
  This function was suggested by Robin Hankin of the School of
  Mathematical and Computing Sciences at Auckland University of
  Technology.
}
\value{
  An \eqn{n \times 3}{n x 3} matrix where \eqn{n} is the number
  of Delaunay triangles in the triangulation specified by \code{object}.
  The \eqn{i^{th}}{i-th} row consists of the indices (in the original
  list of points being triangulated) of vertices of the \eqn{i^{th}}{i-th}
  Delaunay triangle.  The indices are listed in increasing numeric
  order in each row.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
  \url{http://www.stat.auckland.ac.nz/~rolf}
}
\seealso{ \code{\link{deldir}()}
          \code{\link{triang.list}()}
	  \code{\link{plot.triang.list}()}
}
\examples{
set.seed(42)
x    <- runif(10)
y    <- runif(10)
ddxy <- deldir(x,y)
M    <- triMat(ddxy)
}
\keyword{spatial}
