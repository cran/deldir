\name{tilePerim}
\alias{tilePerim}
\title{
   Calculate tile perimeters.
}
\description{
   Calculates the perimeters of all of the Dirichlet (Voronoi)
   tiles in a tessellation of a set of planar points.  Also
   calculates the sum and the mean of these perimeters.
}
\usage{
tilePerim(object,inclbdry=TRUE)
}
\arguments{
  \item{object}{
  An object of class \code{tile.list} (as produced by
  \code{\link{tile.list}()} specifying the Dirichlet (Voronoi)
  tiles in a tessellation of a set of planar points.
}
  \item{inclbdry}{
  Logical scalar.  Should boundary segments (edges of tiles
  at least one of whose endpoints lies on the enclosing
  rectangle \code{rw} (see \code{\link{deldir}()}) be included
  in the perimeter?
}
}
\value{
   A list with components
   \item{perimeters}{
   A vector consisting of the values of the perimeters of
   the Dirichlet tiles in the tessellation.
   }
   \item{totalPerim}{
   The sum of \code{perimeters}.
   }
   \item{meanPerim}{
   The mean of \code{perimeters}.
   }
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
  \url{https://www.stat.auckland.ac.nz/~rolf}
}
\note{
   Function added at the request of Haozhe Zhang.
}

\seealso{
  \code{\link{tile.list}()}, \code{\link{plot.tile.list}()}
}
\examples{
    x <- runif(20)
    y <- runif(20)
    z <- deldir(x,y,rw=c(0,1,0,1))
    w <- tile.list(z)
    p1 <- tilePerim(w)
    p0 <- tilePerim(w,inclbdry=FALSE)
    p1$totalPerim - p0$totalPerim # Get 4 = the perimeter of rw.
    ss <- apply(as.matrix(z$dirsgs[,1:4]),1,
                function(x){(x[1]-x[3])^2 + (x[2]-x[4])^2})
    2*sum(sqrt(ss)) - p0$totalPerim # Get 0; in tilePerim() each interior
                                    # edge is counted twice.
}
\keyword{spatial}
