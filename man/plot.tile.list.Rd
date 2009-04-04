\name{plot.tile.list}
\alias{plot.tile.list}
\title{ Plot Dirchlet/Voronoi tiles }
\description{ A method for \code{plot}.  Plots (sequentially)
the tiles associated with each point in the set being tessellated. }
\usage{
\method{plot}{tile.list}(x, verbose = FALSE, close=FALSE, pch=1,
                         polycol=NA, showpoints=TRUE, asp=1, ...)
}
\arguments{
  \item{x}{ A list of the tiles in a tessellation, as produced
  the function \code{\link{tile.list}()}.}
  \item{verbose}{ Logical scalar; if \code{TRUE} the tiles are
  plotted one at a time (with a ``Go?'' prompt after each)
  so that the process can be watched.}
  \item{close}{ Logical scalar; if \code{TRUE} the outer edges of
  of the tiles (i.e. the edges of the enclosing rectangle)
  are drawn.  Otherwise tiles on the periphery of the
  tessellation are left ``open''.}
  \item{pch}{The plotting character for plotting the points of the
  pattern which was tessellated.  Ignored if \code{showpoints}
  is \code{FALSE}.}
  \item{polycol}{Optional vector of integers (or \code{NA}s);
  the \eqn{i}-th entry indicates with which colour to fill
  the \eqn{i}-th tile.  Note that an \code{NA} indicates
  the use of no colour at all.}
  \item{showpoints}{Logical scalar; if \code{TRUE} the points of
  the pattern which was tesselated are plotted.}
  \item{asp}{The aspect ratio of the plot; integer scalar or
  \code{NA}.  Set this argument equal to \code{NA} to allow the data
  to determine the aspect ratio and hence to make the plot occupy the
  complete plotting region in both \code{x} and \code{y} directions.
  This is inadvisable; see the \bold{Warnings}.}
  \item{...}{ Optional arguments; not used.  There for consistency
  with the generic \code{plot} function.}
}
\section{Warnings}{
  The default value for \code{verbose} was formerly \code{TRUE};
  it is now \code{FALSE}.

  The user is \emph{strongly advised} not to set the value of
  \code{asp} but rather to leave \code{asp} equal to its default
  value of \code{1}.  Any other value distorts the tesselation
  and destroys the perpendicular appearance of lines which are
  indeed perpendicular.  (And conversely can cause lines which
  are not perpendicular to appear as if they are.)

  The argument \code{asp} is present ``just because it can be''.
}
\value{NULL; side effect is a plot.}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
  \url{http://www.math.unb.ca/~rolf}
}
\seealso{
\code{\link{tile.list}()}
}
\examples{
  x <- runif(20)
  y <- runif(20)
  z <- deldir(x,y,rw=c(0,1,0,1))
  w <- tile.list(z)
  plot(w)
  ccc <- heat.colors(20) # Or topo.colors(20), or terrain.colors(20)
                         # or cm.colors(20), or rainbox(20).
  plot(w,polycol=ccc,close=TRUE)
}
\keyword{ hplot }
