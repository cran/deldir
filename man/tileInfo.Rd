\name{tileInfo}
\alias{tileInfo}
\title{
   Extract information from a tile list.
}
\description{
   Produces a summary of information about the tiles in an
   object of class \code{deldir} as produced by the function
   \code{deldir()}.
}
\usage{
tileInfo(object, bndry = FALSE)
}
\arguments{
  \item{object}{ An object of class \code{deldir} as produced
  by the function \code{deldir()}.
}
  \item{bndry}{
  Logical scalar.  If \code{TRUE} then the \dQuote{boundary} tiles
  (those tiles having edges forming part of the \dQuote{rectangular
  window} enclosing the tessellation) are included in the summary.
  Otherwise they are not included.
}
}
\value{
  A list with components:
  \item{indivTiles}{This is itself a list with one entry for each
  tile in \dQuote{object}.  It is in fact a \emph{named} list,
  the names being of form \code{tile.n}, where \code{n} is equal to
  the value of \code{ptNum} (see below) corresponding to the tile.
  The entries of \code{indivTiles} are themselves in turn lists with
  entries \code{edgeLengths} (a vector of the lengths of the edges
  of the tiles), \code{numEdges} (an integer equal to the number
  of edges of the tile), \code{area} (a positive number equal to
  the area of the tile) and \code{ptNum} (an integer equal to the
  number of the point determining the tile).  Note that \code{ptNum}
  is the number of the point in the \emph{original} sequence of
  points that were tessellated.}
  \item{allEdgeCounts}{An integer vector of the edge counts (given
  by \code{numEdge} for each tile) of the tiles.}
  \item{tabEdgeCounts}{A table of \code{numEdge}.}
  \item{allEdgeLengths}{A vector of all of the tile edge lengths;
  a catenation of the \code{edgeLengths} components of the entries
  of \code{indivTiles}.  Note that there will be many duplicate
  lengths since each tile edge is, in general, an edge of \emph{two}
  tiles.}
  \item{Areas}{A vector of the areas of the tiles.}
  \item{uniqueEdgeLengths}{A vector of the lengths of the tiles edges
  with the duplicates (which occur in \code{allEdgeLengths}) being
  eliminated. Each tile edge is represented only once.}
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}

\seealso{
 \code{\link{deldir}()} \code{\link{tile.list}()} \code{\link{print.tileInfo}()}
}
\examples{
set.seed(42)
x    <- runif(20)
y    <- runif(20)
dxy  <- deldir(x,y,rw=c(0,1,0,1))
ixy1 <- tileInfo(dxy)
ixy2 <- tileInfo(dxy,bndry=TRUE)
}
\keyword{ spatial }
