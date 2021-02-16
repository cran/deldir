\name{tile.centroids}
\Rdversion{1.1}
\alias{tile.centroids}
\title{
Compute centroids of Dirchlet (Voronoi) tiles
}
\description{
Given a list of Dirichlet tiles, as produced by \code{tile.list()},
produces a data frame consisting of the centroids of those tiles.
}
\usage{
tile.centroids(tl)
}
\arguments{
  \item{tl}{A list of the tiles (produced by \code{tile.list()})
  in a Dirichlet tessellation of a set of planar points.}
}
\value{
  A data frame with two columns named \code{x} and \code{y}.
  Each row of this data frame consitutes the centroid of one
  of the Dirichlet tiles.
}
\references{
URL http://en.wikipedia.org/wiki/Centroid
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
% \url{https://www.stat.auckland.ac.nz/~rolf}
}

\seealso{ \code{\link{tile.list}()} }
\examples{
set.seed(42)
x <- runif(20)
y <- runif(20)
d <- deldir(x,y)
l <- tile.list(d)
g <- tile.centroids(l)
\dontrun{
plot(l,close=TRUE)
points(g,pch=20,col="red")
}
}
\keyword{ spatial }
