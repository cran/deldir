\name{plot.tile.list}
\alias{plot.tile.list}
\title{ Plot Dirchlet (Voronoi) tiles }
\description{ A method for \code{plot}.  Plots (sequentially)
the tiles associated with each point in the set being tessellated. }
\usage{
\method{plot}{tile.list}(x, verbose = FALSE, close = FALSE, pch = 1,
                         fillcol = getCol(x,warn=warn),col.pts = NULL,
                         border=NULL, showpoints = TRUE, add = FALSE,
                         asp = 1, clipp = NULL, xlab = "x", ylab = "y",
                         main = "", warn = FALSE, ...)
}
\arguments{
  \item{x}{ A list of the tiles in a tessellation, as produced
  the function \code{\link{tile.list}()}.}
  \item{verbose}{ Logical scalar; if \code{TRUE} the tiles are
  plotted one at a time (with a ``Go?'' prompt after each)
  so that the process can be watched.}
  \item{close}{ Logical scalar; if \code{TRUE} the outer edges of
  of the tiles (i.e. the edges which are contained in the enclosing
  rectangle) are drawn.  Otherwise tiles on the periphery of the
  tessellation are left ``open''.}
  \item{pch}{The plotting character (or vector of plotting
  characters) with which to plot the points of the pattern which
  was tessellated.  Ignored if \code{showpoints} is \code{FALSE}.}
  \item{fillcol}{Optional vector (possibly of length 1, i.e. a scalar)
  whose entries can be interpreted as colours by \code{\link{col2rgb}()}.
  The \eqn{i}-th entry indicates with which colour to fill
  the \eqn{i}-th tile.  Note that an \code{NA} entry indicates
  the use of no colour at all.  This argument will be replicated
  to have length equal to the number of tiles.
  }
  \item{col.pts}{Optional vector like unto \code{fillcol} whose
  entries can be interpreted as colours by \code{\link{col2rgb}()}.
  The \eqn{i}-th entry indicates with which colour to plot the
  \eqn{i}-th point.  This argument will be replicated to have length
  equal to the number of tiles.  Ignored if \code{showpoints} is
  \code{FALSE}.
  }
  \item{border}{A \emph{scalar} indicating the colour with which to plot
  the tile boundaries.  Defaults to black unless all of the
  fill colours specified by \code{fillcol} are black, in which
  case it defaults to white.  If \code{length(border) > 1} then
  an error is given.
  }
  \item{showpoints}{Logical scalar; if \code{TRUE} the points of
  the pattern which was tesselated are plotted.}
  \item{add}{
  Logical scalar; should the plot of the tiles be added
  to an existing plot?  }
  \item{asp}{The aspect ratio of the plot; integer scalar or
  \code{NA}.  Set this argument equal to \code{NA} to allow the data
  to determine the aspect ratio and hence to make the plot occupy the
  complete plotting region in both \code{x} and \code{y} directions.
  This is inadvisable; see the \bold{Warnings}.}
  \item{clipp}{An object specifying a polygon to which the tessellation
  being plotted should be clipped.  It should consist either of:
  \itemize{
    \item a list containing two components x and y giving the
    coordinates of the vertices of a single polygon. The last vertex
    should not repeat the first vertex. Or:
    \item a list of list(x,y) structures giving the coordinates of
    the vertices of several polygons.
  }
  If this argument is provided then the plot of the tessellation
  is \dQuote{clipped} to the polygon specified by \code{clipp}.
  }
  \item{xlab}{
   Label for the \code{x}-axis (used only if \code{add} is \code{FALSE}).}
  \item{ylab}{
   Label for the \code{y}-axis (used only if \code{add} is \code{FALSE}).}
  \item{main}{
   A title for the plot (used only if \code{add} is \code{FALSE}).}
  \item{warn}{Logical scalar passed to the internal function \code{getCol()}.
  Should a warning be issued if the \code{z} components of the entries
  of \code{x} cannot all be interpreted as colours.  See \bold{Details}.
  }
  \item{...}{ Optional arguments; not used.  There for consistency
  with the generic \code{plot} function.}
}
  \section{Warnings}{
  \itemize{
  \item The behaviour of this function with respect to
  \dQuote{clipping} has changed substantially since the previous
  release of \code{deldir}, i.e. 1.1-0.  The argument \code{clipwin}
  has been re-named \code{clipp} (\dQuote{p} for \dQuote{polygon}).
  Clipping is now effected via the new package \code{polyclip}.
  The \code{spatstat} package is no longer used.  The argument
  \code{use.gpclib} has been eliminated, since \code{gpclib} (which
  used to be called upon by \code{spatstat} has been superceded by
  \code{polyclip} which has an unrestrictive license.

  \item As of release 0.1-1 of the \code{deldir} package, the
  argument \code{fillcol} to this function \emph{replaces} the old
  argument \code{polycol}, but behaves somewhat differently.

  \item The argument \code{showrect} which was present in versions
  of this function prior to release 0.1-1 has been eliminated.
  It was redundant.

  \item As of release 0.1-1 the \code{col.pts} argument \emph{might}
  behave somewhat differently from how it behaved in the past.

  \item The arguments \code{border}, \code{clipp}, and \code{warn}
  are new as of release 0.1-1.

  \item The user is \emph{strongly advised} not to set the value of
  \code{asp} but rather to leave \code{asp} equal to its default
  value of \code{1}.  Any other value distorts the tesselation
  and destroys the perpendicular appearance of lines which are
  indeed perpendicular.  (And conversely can cause lines which
  are not perpendicular to appear as if they are.)
  }
}
\section{Notes}{

  The new behaviour in respect of the colours with which to fill
  the plotted tiles, and the argument \code{clipp} were added at
  the request of Chris Triggs.

  The argument \code{asp} was added at the request of Zubin
  Dowlaty.
}
\value{NULL; side effect is a plot.}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
  \url{http://www.stat.auckland.ac.nz/~rolf}
}
\seealso{ \code{\link{deldir}()}, \code{\link{tile.list}()}
          \code{\link{triang.list}()} \code{\link{plot.triang.list}()} }
\examples{
  set.seed(42)
  x <- runif(20)
  y <- runif(20)
  z <- deldir(x,y,rw=c(0,1,0,1))
  w <- tile.list(z)
  plot(w)
  ccc <- heat.colors(20) # Or topo.colors(20), or terrain.colors(20)
                         # or cm.colors(20), or rainbow(20).
  plot(w,fillcol=ccc,close=TRUE)
  if(require(polyclip)) {
     CP <- list(x=c(0.49,0.35,0.15,0.20,0.35,0.42,
                          0.43,0.62,0.46,0.63,0.82,0.79),
                      y=c(0.78,0.86,0.79,0.54,0.58,0.70,
                          0.51,0.46,0.31,0.20,0.37,0.54))
     plot(w,clipp=CP,showpoints=FALSE,fillcol=topo.colors(20))
  }
}
\keyword{ hplot }
