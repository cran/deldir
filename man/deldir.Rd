\name{deldir}
\alias{deldir}
\title{
   Delaunay triangulation and Dirichlet tessellation
}
\description{
This function computes the Delaunay triangulation (and hence the
Dirichlet or Voronoi tesselation) of a planar point set according
to the second (iterative) algorithm of Lee and Schacter ---
see REFERENCES.  The triangulation is made to be with respect to
the whole plane by \code{suspending} it from so-called ideal points
(-Inf,-Inf), (Inf,-Inf) (Inf,Inf), and (-Inf,Inf).  The triangulation
is also enclosed in a finite rectangular window.  A set of dummy
points may be added, in various ways, to the set of data points
being triangulated.
}
\usage{
deldir(x, y, dpl=NULL, rw=NULL, eps=1e-09, sort=TRUE, plotit=FALSE,
       digits=6, \dots)
}
\arguments{
\item{x,y}{
The coordinates of the point set being triangulated. These can be
given by two arguments x and y which are vectors or by a single
argument x which is a list with components \code{x} and \code{y}.
}
\item{dpl}{
A list describing the structure of the dummy points to be added to
the data being triangulated.  The addition of these dummy points
is effected by the auxilliary function dumpts().  The list may
have components:
\itemize{
\item \code{ndx}: The x-dimension of a rectangular grid; if either
ndx or ndy is null, no grid is constructed.
\item \code{ndy}: The y-dimension of the aforementioned rectangular
grid.
\item \code{nrad}: The number of radii or \dQuote{spokes}, emanating from
each data point, along which dummy points are to be added.
\item \code{nper}: The number of dummy points per spoke.
\item \code{fctr}: A factor determining the length of each spoke;
each spoke is of length equal to fctr times the mean nearest
neighbour distance of the data.  (This distance is calculated by
the auxilliary function mnnd().)
\item \code{x}: A vector of x-coordinates of \dQuote{ad hoc} dummy points
\item \code{y}: A vector of the corresponding y-coordinates of
\dQuote{ad hoc} dummy points
}

}
\item{rw}{
The coordinates of the corners of the rectangular window enclosing
the triangulation, in the order (xmin, xmax, ymin, ymax).  Any data
points (including dummy points) outside this window are discarded.
If this argument is omitted, it defaults to values given by the range
of the data, plus and minus 10 percent.
}
\item{eps}{
A value of epsilon used in testing whether a quantity is zero, mainly
in the context of whether points are collinear.  If anomalous errors
arise, it is possible that these may averted by adjusting the value
of eps upward or downward.
}
\item{sort}{
Logical argument; if \code{TRUE} (the default) the data (including dummy
points) are sorted into a sequence of \dQuote{bins} prior to triangulation;
this makes the algorithm slightly more efficient.  Normally one would
set sort equal to \code{FALSE} only if one wished to observe some of the
fine detail of the way in which adding a point to a data set affected
the triangulation, and therefore wished to make sure that the point
in question was added last.  Essentially this argument would get used
only in a de-bugging process.
}
\item{plotit}{
Logical argument; if \code{TRUE} a plot is produced.  The nature
of the plot may be controlled by using the \code{\dots} argument
to pass appropriate arguments to \code{plot.deldir()}.  Without
\dQuote{further instruction} a plot of the points being triangulated
and of both the triangulation and the tessellation is produced;
}
\item{digits}{
The number of decimal places to which all numeric values in the
returned list should be rounded.  Defaults to 6.
}
\item{...}{
Auxilliary arguments add, wlines, wpoints, number, nex, col, lty,
pch, xlim, and ylim (and possibly other plotting parameters) may be
passed to plot.deldir through \code{\dots} if plotit=\code{TRUE}.
}
}
\value{
A list (of class \code{deldir}), invisible if plotit=\code{TRUE}, with components:

\item{delsgs}{
a matrix with 6 columns.  The first 4 entries of each row are the
coordinates of the points joined by an edge of a Delaunay
triangle, in the order (x1,y1,x2,y2).  The last two entries are the
indices of the two points which are joined.
}
\item{dirsgs}{
a data frame with 8 columns.  The first 4 entries of each row are
the coordinates of the endpoints of one the edges of a Dirichlet
tile, in the order (x1,y1,x2,y2).  The fifth and sixth entries
are the indices of the two points, in the set being triangulated,
which are separated by that edge. The seventh and eighth entries are
logical values.  The seventh indicates whether the first endpoint
of the corresponding edge of a Dirichlet tile is a boundary point
(a point on the boundary of the rectangular window).  Likewise for
the eighth entry and the second endpoint of the edge.
}
\item{summary}{
a matrix with 9 columns and \code{n.data + n.dum}rows (see below).
The rows correspond to the points in the set being triangulated.
The column names are \code{x} (the x-coordinate of the point),
\code{y} (the y-coordinate), \code{n.tri} (the number of Delaunay
triangles emanating from the point), \code{del.area} (1/3 of the
total area of all the Delaunay triangles emanating from the point),
\code{del.wts} (the corresponding entry of the \code{del.area}
column divided by the sum of this column); \code{n.tside} (the
number of sides --- within the rectangular window --- of the
Dirichlet tile surrounding the point), \code{nbpt} (the number of
points in which the Dirichlet tile intersects the boundary of the
rectangular window), \code{dir.area} (the area of the Dirichlet tile
surrounding the point), and \code{dir.wts} (the corresponding entry
of the \code{dir.area} column divided by the sum of this column).

Note that the factor of 1/3 associated with the del.area column
arises because each triangle occurs three times --- once for
each corner.
}

\item{n.data}{
the number of real (as opposed to dummy) points in the set which was
triangulated, with any duplicate points eliminated.  The first n.data
rows of \code{summary} correspond to real points.
}
\item{n.dum}{
the number of dummy points which were added to the set being triangulated,
with any duplicate points (including any which duplicate real points)
eliminated.  The last n.dum rows of \code{summary} correspond to dummy
points.
}
\item{del.area}{
the area of the convex hull of the set of points being triangulated,
as formed by summing the \code{del.area} column of \code{summary}.
}
\item{dir.area}{
the area of the rectangular window enclosing the points being triangulated,
as formed by summing the \code{dir.area} column of \code{summary}.
}
\item{rw}{
the specification of the corners of the rectangular window enclosing
the data, in the order (xmin, xmax, ymin, ymax).
}}
\note{
If ndx >= 2 and ndy >= 2, then the rectangular window IS the convex
hull, and so the values of del.area and dir.area (if the latter is
not \code{NULL}) are identical.
}
\section{Side Effects}{
If plotit==\code{TRUE} a plot of the triangulation and/or tessellation is produced
or added to an existing plot.
}
\details{
This package is a (straightforward) adaptation of the Splus library
section ``delaunay'' to R.  That library section is an implementation
of the Lee-Schacter algorithm, which was originally written as a
stand-alone Fortran program in 1987/88 by Rolf Turner, while with the
Division of Mathematics and Statistics, CSIRO, Sydney, Australia.  It
was re-written as an Splus function (using dynamically loaded Fortran
code), by Rolf Turner while visiting the University of Western
Australia, May, 1995.

Further revisions were made December 1996. The author gratefully
acknowledges the contributions, assistance, and guidance of Mark
Berman, of D.M.S., CSIRO, in collaboration with whom this project
was originally undertaken.  The author also acknowledges much useful
advice from Adrian Baddeley, formerly of D.M.S., CSIRO (now of CMIS,
CSIRO and Adjunct Professor of Statistics at the University of
Western Australia).  Daryl Tingley of the Department of Mathematics
and Statistics, University of New Brunswick provided some helpful
insight.  Special thanks are extended to Alan Johnson, of the
Alaska Fisheries Science Centre, who supplied two data sets which
were extremely valuable in tracking down some errors in the code.

Don MacQueen, of Lawrence Livermore National Lab, wrote an Splus
driver function for the old stand-alone version of this software.
That driver, which was available on Statlib, is now deprecated in
favour of the current package ``delaunay'' package.  Don also
collaborated in the preparation of that package.

See the \code{ChangeLog} for information about further revisions
and bug-fixes.
}

\section{Warning}{
The process for determining if points are duplicated changed
between versions 0.1-9 and 0.1-10.  Previously there was an
argument \code{frac} for this function, which defaulted to
0.0001.  Points were deemed to be duplicates if the difference in
\code{x}-coordinates was less than \code{frac} times the width
of \code{rw} and \code{y}-coordinates was less than \code{frac}
times the height of \code{rw}.  This process has been changed to
one which uses \code{\link{duplicated}()} on the data frame whose
columns are \code{x} and \code{y}.

As a result it may happen that points which were previously
eliminated as duplicates will no longer be eliminated.  (And
possibly vice-versa.)
}

\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
  \url{http://www.math.unb.ca/~rolf}
}

\references{
Lee, D. T., and Schacter, B. J.  Two algorithms for constructing a
Delaunay triangulation, Int. J. Computer and Information Sciences,
Vol. 9, No. 3, 1980, pp. 219 -- 242.

Ahuja, N. and Schacter, B. J. (1983). Pattern Models.  New York: Wiley.
}
\seealso{
plot.deldir
}
\examples{
x   <- c(2.3,3.0,7.0,1.0,3.0,8.0)
y   <- c(2.3,3.0,2.0,5.0,8.0,9.0)
try <- deldir(x,y,list(ndx=2,ndy=2),c(0,10,0,10))
# Puts dummy points at the corners of the rectangular
# window, i.e. at (0,0), (10,0), (10,10), and (0,10)
\dontrun{
try <- deldir(x,y,list(ndx=2,ndy=2),c(0,10,0,10),plot=TRUE,wl='tr')
}
# Plots the triangulation which was created (but not the tesselation).
}
\keyword{spatial}
