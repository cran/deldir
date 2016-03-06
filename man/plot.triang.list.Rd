\name{plot.triang.list}
\alias{plot.triang.list}
\title{ Plot Delaunay triangles }
\description{ A method for \code{plot}.  Plots the triangles of
a Delaunay triangulation of a set of points in the plane.}
\usage{
\method{plot}{triang.list}(x, showrect = FALSE, add = FALSE,
              xlab = "x", ylab = "y", main = "", asp = 1, ...)
}
\arguments{
  \item{x}{
   An object of class \dQuote{triang.list} as produced by
   \code{\link{triang.list}()}.
}
  \item{showrect}{
   Logical scalar; show the enclosing rectangle \code{rw} (see
   \code{\link{deldir}()}) be plotted?
}
  \item{add}{
   Logical scalar; should the plot of the triangles be added
   to an existing plot?
}
  \item{xlab}{
   Label for the \code{x}-axis.
}
  \item{ylab}{
   Label for the \code{y}-axis.
}
  \item{main}{
   A title for the plot (used only if \code{add} is \code{FALSE}).
}
  \item{asp}{The aspect ratio of the plot; integer scalar or
  \code{NA}.  Set this argument equal to \code{NA} to allow the data
  to determine the aspect ratio and hence to make the plot occupy the
  complete plotting region in both \code{x} and \code{y} directions.
  This is inadvisable; see the \bold{Warnings}.}
  \item{\dots}{
  Arguments passed to \code{\link{polygon}()} which does the actual
  plotting of the triangles.
}
}
\section{Warnings}{
  The user is \emph{strongly advised} not to set the value of
  \code{asp} but rather to leave \code{asp} equal to its default
  value of \code{1}.  Any other value distorts the tesselation
  and destroys the perpendicular appearance of lines which are
  indeed perpendicular.  (And conversely can cause lines which
  are not perpendicular to appear as if they are.)

  The argument \code{asp} was added at the request of Zubin
  Dowlaty.
}
\value{
   None.  This function has the side effect of producing (or
   adding to) a plot.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
% \url{https://www.stat.auckland.ac.nz/~rolf}
}

\seealso{ \code{\link{deldir}()}, \code{\link{plot.triang.list}()},
          \code{\link{tile.list}()}, \code{\link{plot.tile.list}()} }
\examples{
set.seed(42)
x <- runif(20)
y <- runif(20)
d <- deldir(x,y)
ttt <- triang.list(d)
plot(ttt,border="red",showrect=TRUE)
sss <- tile.list(d)
plot(sss)
plot(ttt,add=TRUE,border="blue")
}
\keyword{ spatial }
