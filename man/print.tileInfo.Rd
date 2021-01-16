\name{print.tileInfo}
\alias{print.tileInfo}
\title{
   Print a summary of tile information.
}
\description{
   Print a reasonably readable summary of an object of class
   \code{tileInfo} as produced by the \code{tileInfo()} function.
}
\usage{
\method{print}{tileInfo}(x, digits = 4, ...)
}
\arguments{
  \item{x}{An object of class \code{tileInfo} as produced by the
  \code{tileInfo()} function.
}
  \item{digits}{
  The (maximum) number of decimal digits to which the output
  is to be printed.
}
  \item{\dots}{
  Not used.  Present for compatibility with the generic
  \code{print()} function.
}
}
\details{
  The list produced by \code{tileInfo()} is a bit messy and
  hard to comprehend, especially if there is a large number
  of tiles.  This print method produces a screen display which
  is somewhat more perspicuous.
 
  There are four components to the display:

  \itemize{
  \item A matrix, each row of which is the vector of edge
  lengths of the tile.  The number of columns is the \emph{maximum}
  of the lengths of the edge length vectors.  Rows corresponding
  to shorter vectors are filled in with blanks.  The row names
  of the matrix indicate the number of the point corresponding to
  the tile.  Note that this number is the index of the point in
  the original sequence of points that is being tessellated.
  \item A table of the edge counts of the tiles.
  \item A simple print out of the areas of the tiles (rounded
  to a maximum of \code{digits} decimal digits).
  \item A simple print out of the perimeters of the tiles (rounded
  to a maximum of \code{digits} decimal digits).
  }

  This screen display is for \dQuote{looking at} only.  In
  order to do further calculations on the output of \code{tileInfo}
  it is necessary to delve into the bowels of \code{x} and
  extract the relevant bits.

  In order to get a decent looking display you may (if there are
  tiles with a large number of edges) need to widen the window in
  which you are displaying the output and increase the value of the
  \code{width} option.  E.g. use \code{options(width=120)}.
}
\value{
  None.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}

\seealso{
  \code{\link{tileInfo}()}
}
\examples{
set.seed(179)
x    <- runif(100)
y    <- runif(100)
dxy  <- deldir(x,y,rw=c(0,1,0,1))
ixy1 <- tileInfo(dxy)
print(ixy1)
ixy2 <- tileInfo(dxy,bndry=TRUE)
print(ixy2)
if(require(polyclip)) {
    CP <- list(x=c(0.49,0.35,0.15,0.20,0.35,0.42,
                   0.43,0.62,0.46,0.63,0.82,0.79),
               y=c(0.78,0.86,0.79,0.54,0.58,0.70,
                   0.51,0.46,0.31,0.20,0.37,0.54))
    ixy3 <- tileInfo(dxy,clipp=CP)
    options(width=120) # And enlarge the console window.
    print(ixy3) # 33 tiles are retained.
    print(ixy3$perimeters$perComps) # The tiles for points 9 and 94 have
                                    # been split into two components.
}
}
\keyword{ utilities }
