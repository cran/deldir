\name{tile.list}
\alias{tile.list}
\title{ Create a list of tiles in a tessellation }
\description{
For each point in the set being tessellated produces a list
entry describing the Dirichlet/Voronoi tile containing that
point.
}
\usage{ tile.list(object) }
\arguments{
  \item{object}{ An object of class \code{deldir} as produced
   by the function \code{\link{deldir}()}.}
}
\value{
A list with one entry for each of the points in the set
being tesselated.  Each entry is in turn a list with
components

\item{pt}{The coordinates of the point whose tile is being described.}
\item{x}{The \code{x} coordinates of the vertices of the tile, in
anticlockwise order.}
\item{y}{The \code{y} coordinates of the vertices of the tile, in
anticlockwise order.}
\item{bp}{Vector of logicals indicating whether the tile vertex is a
``real'' vertex, or a \emph{boundary point}, i.e. a point where the
tile edge intersects the boundary of the enclosing rectangle}
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
  \url{http://www.math.unb.ca/~rolf}
}
\section{Acknowledgement}{
The author expresses sincere thanks to Majid Yazdani who found and
pointed out a serious bug in \code{tile.list} in a previous version
(0.0-5) of the \code{deldir} package.
}

\section{Warning}{
The set of vertices of each tile may be ``incomplete''.  Only
vertices which lie within the enclosing rectangle, and ``boundary
points'' are listed.

Note that the enclosing rectangle may be specified by the user
in the call to \code{\link{deldir}()}.

In contrast to some earlier versions of \code{deldir}, the corners
of the enclosing rectangle are now include as vertices of tiles.
I.e. a tile which in fact extends beyond the rectangular window
and contains a corner of that window will have that corner added
to its list of vertices.  Thus the corresponding polygon is the
intersection of the tile with the enclosing rectangle.
}

\seealso{ \code{\link{deldir}()}, \code{\link{plot.tile.list}()}}

\examples{
	x <- runif(20)
	y <- runif(20)
	z <- deldir(x,y)
	w <- tile.list(z)

	z <- deldir(x,y,rw=c(0,1,0,1))
	w <- tile.list(z)

	z <- deldir(x,y,rw=c(0,1,0,1),dpl=list(ndx=2,ndy=2))
	w <- tile.list(z)

}
\keyword{spatial}
