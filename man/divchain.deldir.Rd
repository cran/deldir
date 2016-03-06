\name{divchain.deldir}
\alias{divchain.deldir}
\title{
    Dividing chain; \dQuote{deldir} method.
}
\description{
   Create the \dQuote{dividing chain} of a Dirchlet tesselation.
   The tessellation must have been created from a set of points
   having associated categorical \dQuote{weights}.  The dividing
   chain consists of those edges of Dirichlet tiles which separate
   points having different values of the given weights.
}

\usage{
   \method{divchain}{deldir}(x, ...)
}

\arguments{
  \item{x}{
      An object of class \dQuote{deldir}.  This object must have
      been created in such a way that the points of the set being
      tessellated have associate categorical \dQuote{weights}.  That is,
      \code{\link{deldir}()} must have been called with a \emph{factor}
      valued \code{z} argument or the \code{x} argument to  \code{deldir()}
      must have had an appropriate component which could be taken to
      be \code{z}.
  }
  \item{\dots}{
      Not used.
  }
}
\value{
  An object of class \dQuote{divchain} consisting of a
  data frame with columns named \dQuote{x0}, \dQuote{y0},
  \dQuote{x1},\dQuote{y1}, \dQuote{v01}, \dQuote{v02}, \dQuote{v03},
  \dQuote{v11}, \dQuote{v12} and \dQuote{v13}.

  The columns named \dQuote{x0} and \dQuote{y0} consist of the
  coordinates of one endpoint of an edge of a Dirichlet tile and
  the columns named \dQuote{x1} and \dQuote{y1} consist of the
  coordinates of the other endpoint.

  The columns named \dQuote{vij}, i = 0, 1, j = 1, 2, 3, consist
  of the indices of the vertices of the Delaunay triangles
  whose circumcentres constitute the respective endpoints of the
  corresponding edge of a Dirichlet tile.  The entries of column
  \dQuote{vi3} may (also) take the values $-1, -2, -3$, and $-4$.
  This will be the case if the circumcentre in question lay outside
  of the rectangular window \code{rw} (see \code{\link{deldir}()})
  enclosing the points being tessellated.  In these circumstances the
  corresponding endpoint of the tile edge is the intersection of the
  line joining the two circumcentres with the boundary of \code{rw},
  and the numeric value of the entry of column \dQuote{vi3} indicates
  which side.  The numbering follows the convention for numbering
  the sides of a plot region in \code{R}: 1 for the bottom side,
  2 for the left side, 3 for the top side and 4 for the right side.

  Note that the triple of vertices uniquely identify the endpoint
  of the tile edge.

  The object has an attribute \code{rw} which is equal to the
  specification of the rectangular window within which the
  class \dQuote{deldir} object \code{x} was constructed.
  (See \code{\link{deldir}()}.)
}

\author{
  Rolf Turner
  \email{r.turner@auckland.ac.nz}
% \url{https://www.stat.auckland.ac.nz/~rolf}
}
\note{
  This function was created in response to a question asked
  on \code{stackoverflow.com} by a user named \dQuote{Dan}.
}

\seealso{
  \code{\link{divchain.default}()} \code{\link{deldir}()}
  \code{\link{plot.divchain}()}
}
\examples{
   set.seed(42)
   x   <- runif(50)
   y   <- runif(50)
   z   <- factor(kmeans(cbind(x,y),centers=4)$cluster)
   dxy <- deldir(x,y,z=z,rw=c(0,1,0,1))
   dc  <- divchain(dxy)
}

\keyword{ spatial }
