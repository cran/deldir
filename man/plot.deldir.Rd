\name{plot.deldir}
\alias{plot.deldir}
\title{
Plot objects produced by deldir
}
\description{
This is a method for plot.
}
\usage{
\method{plot}{deldir}(x,add=FALSE,wlines=c('both','triang','tess'),
                      wpoints=c('both','real','dummy','none'),
                      number=FALSE,cex=1,nex=1,col=NULL,lty=NULL,
                      pch=NULL,xlim=NULL,ylim=NULL,axes=FALSE,
                      xlab=if(axes) 'x' else '',ylab=if(axes) 'y' else'',
                      showrect=FALSE,...)
}
\arguments{
\item{x}{
An object of class "deldir" as constructed by the function deldir.
}
\item{add}{
logical argument; should the plot be added to an existing plot?
}
\item{wlines}{
"which lines?".  I.e.  should the Delaunay triangulation be plotted
(wlines='triang'), should the Dirichlet tessellation be plotted
(wlines='tess'), or should both be plotted (wlines='both', the
default) ?
}
\item{wpoints}{
"Which points to plot?".  I.e.  should the real points be plotted
(wpoints='real'), should the dummy points be plotted
(wpoints='dummy'), should both be plotted (wpoints='both', the
default) or should no points be plotted (wpoints='none')?
}
\item{number}{
Logical argument, defaulting to \code{FALSE}; if \code{TRUE} then the
points plotted will be labelled with their index numbers
(corresponding to the row numbers of the matrix "summary" in the
output of deldir).
}
\item{cex}{
The value of the character expansion argument cex to be used
with the plotting symbols for plotting the points.
}
\item{nex}{
The value of the character expansion argument cex to be used by the
text function when numbering the points with their indices.  Used only
if number=\code{TRUE}.
}
\item{col}{
The colour numbers for plotting the triangulation, the tesselation,
the data points, the dummy points, and the point numbers, in that
order; defaults to c(1,1,1,1,1).  If fewer than five numbers are
given, they are recycled.  (If more than five numbers are given, the
redundant ones are ignored.)
}
\item{lty}{
The line type numbers for plotting the triangulation and the
tesselation, in that order; defaults to 1:2.  If only one value is
given it is repeated.  (If more than two numbers are given, the
redundant ones are ignored.)
}
\item{pch}{
The plotting symbols for plotting the data points and the dummy
points, in that order; may be either integer or character; defaults
to 1:2.  If only one value is given it is repeated.  (If more than
two values are given, the redundant ones are ignored.)
}
\item{xlim}{
The limits on the x-axis.  Defaults to rw[1:2] where rw is the
rectangular window specification returned by deldir().
}
\item{ylim}{
The limits on the y-axis.  Defaults to rw[3:4] where rw is the
rectangular window specification returned by deldir().
}
\item{axes}{
Logical scalar.  Should axes be drawn on the plot?
}
\item{xlab}{
Label for the x-axis.  Defaults to \code{x} if \code{axes} is
\code{TRUE} and to the empty string if \code{axes} is \code{FALSE}.
Ignored if \code{add=TRUE}.
}
\item{ylab}{
Label for the y-axis.  Defaults to \code{y} if \code{axes} is
\code{TRUE} and to the empty string if \code{axes} is \code{FALSE}.
Ignored if \code{add=TRUE}.
}
\item{showrect}{
Logical scalar; show the enclosing rectangle \code{rw} (see
\code{\link{deldir}()}) be plotted? }
\item{...}{
Further plotting parameters to be passed to \code{plot()}
\code{segments()} or \code{points()} or \code{text()}.  Unlikely to
be used.
}
}
\section{Side Effects}{
A plot of the points being triangulated is produced or added to
an existing plot.  As well, the edges of the Delaunay
triangles and/or of the Dirichlet tiles are plotted.  By default
the triangles are plotted with solid lines (lty=1) and the tiles
with dotted lines (lty=2).
}
\details{
The points in the set being triangulated are plotted with distinguishing
symbols.  By default the real points are plotted as circles (pch=1) and the
dummy points are plotted as triangles (pch=2).
}
\seealso{ \code{\link{deldir}()} }
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
% \url{https://www.stat.auckland.ac.nz/~rolf}
}
\examples{
\dontrun{
try <- deldir(x,y,list(ndx=2,ndy=2),c(0,10,0,10))
plot(try)
#
deldir(x,y,list(ndx=4,ndy=4),plot=TRUE,add=TRUE,wl='te',
       col=c(1,1,2,3,4),num=TRUE)
# Plots the tesselation, but does not save the results.
try <- deldir(x,y,list(ndx=2,ndy=2),c(0,10,0,10),plot=TRUE,wl='tr',
              wp='n')
# Plots the triangulation, but not the points, and saves the
# returned structure.
}
}
\keyword{ hplot }
% Converted by Sd2Rd version 1.21.
