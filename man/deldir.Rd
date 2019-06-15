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
       digits=6, z=NULL, zdum=NULL, suppressMsge=FALSE, \dots)
}
\arguments{
\item{x,y}{
These arguments specify the coordinates of the point set being
triangulated or tessellated. These can be given by two separate
arguments x and y which are vectors or by a single argument x
which is either a data frame or a generic list, possibly one of
class \code{ppp}.  (See package \code{spatstat}.)

If \code{x} is a data frame then the \code{x} coordinates of the
points to be triangulated or tessellated are taken to be the column
of this data frame which is named \dQuote{x} if there is one,
else the first column of the data frame which is not named either
\dQuote{y} or \dQuote{z}. The \code{y} coordinates are taken to be
the column of this data frame which is named \dQuote{y} if there is
one.  If there is no column named \dQuote{y} but there are columns
named \dQuote{x} and \dQuote{z} then the \code{y} coordinates
are taken to be the first \dQuote{other} column.  If there no
columns named either \dQuote{x} or \dQuote{y}, then the \code{x}
coordinates are taken to be the first column not named \dQuote{z}
and the \code{y} coordinates are taken to be the \emph{second}
column not named \dQuote{z}.

If there is a column named \dQuote{z} and if the argument \code{z}
(see below) is NULL, then this the column named \dQuote{z} is taken
to be the value of \code{z}.

If \code{x} is a list (but not a data frame) then it must have
components named \code{x} and \code{y}, and possibly a component
named \code{z}.  The \code{x} and \code{y} components give the
\code{x} and \code{y} coordinates respectively of the points to
be triangulated or tessellated. If \code{x} is \emph{not} of class
\code{ppp}, if it has a component \code{z} and if argument \code{z}
is \code{NULL}, then the \code{z} argument is set equal to this
component \code{z}.  If \code{x} \emph{is} of class \dQuote{ppp}, if
the argument \code{z} is \code{NULL}, if \code{x} is \dQuote{marked}
(see package \code{spatstat}) and if the marks of \code{x} are a
vector or a factor (as opposed to a data frame) then the \code{z}
argument is set equal to these marks.  In this case \code{x}
should \emph{not} have a component \code{z}, and at any rate such
a component would be ignored.
}
\item{dpl}{
A list describing the structure of the dummy points to be added to
the data being triangulated.  The addition of these dummy points
is effected by the auxiliary function dumpts().  The list may
have components:
\itemize{
\item \code{ndx}: The x-dimension of a rectangular grid; if either
ndx or ndy is null, no grid is constructed.
\item \code{ndy}: The y-dimension of the aforementioned rectangular
grid.
\item \code{nrad}: The number of radii or \dQuote{spokes}, emanating from
each data point, along which dummy points are to be added.
\item \code{nper}: The number of dummy points per spoke.
\item \code{fctr}: A numeric \dQuote{multiplicative factor}
determining the length of each spoke; each spoke is of length equal
to fctr times the mean nearest neighbour distance of the data.
(This distance is calculated by the auxiliary function mnnd().)
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
set \code{sort} equal to \code{FALSE} only if one wished to observe some of the
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
\item{z}{
An optional vector of \dQuote{auxiliary} values or \dQuote{weights}
associated with the respective points. (\bold{NOTE:} These
\dQuote{weights} are values associated with the points and hence
with the tiles of the tessellation produced.  They \bold{DO NOT}
affect the tessellation, i.e.  the tessellation produced is the
same as is it would be if there were no weights.  The \code{deldir}
package \bold{DOES NOT} do weighted tessellation.  The so-called
weights in fact need not be numeric.)

If \code{z} is left \code{NULL} then it is taken to be the third
column of \code{x}, if \code{x} is a data frame or to be the \code{z}
component of \code{x} if \code{x} is a generic list. If \code{z}
is left \code{NULL} and if \code{x} is of class \dQuote{ppp} and is
\dQuote{marked} (see package \code{spatstat}) and if in addition
the marks are atomic (i.e. \emph{not} a data frame) then \code{z}
is taken to be the marks of \code{x}.
}
\item{zdum}{
Values of \code{z} to be associated with any dummy points that are
created. See \bold{Warnings}.
}
\item{suppressMsge}{
Logical scalar indicating whether a message (alerting the user to
changes from previous versions of \code{deldir}) should be
suppressed.
}
\item{...}{
Auxiliary arguments \code{add}, \code{wlines}, \code{wpoints},
\code{number}, \code{nex}, \code{col}, \code{lty}, \code{pch},
\code{xlim}, and \code{ylim} (and possibly other plotting parameters)
may be passed to \code{\link{plot.deldir}()} through \code{\dots}
if \code{plotit=TRUE}.
}
}
\value{
A list (of class \code{deldir}), invisible if \code{plotit=TRUE}, with components:

\item{delsgs}{
A data frame with 6 columns.  The first 4 entries of each row are the
coordinates of the points joined by an edge of a Delaunay triangle,
in the order \code{(x1,y1,x2,y2)}.  The last two entries are the indices
of the two points which are joined.
}
\item{dirsgs}{
A data frame with 10 columns.  The first 4 entries of each
row are the coordinates of the endpoints of one the edges of a
Dirichlet tile, in the order \code{(x1,y1,x2,y2)}.  The fifth and
sixth entries, in the columns named \code{ind1} and \code{ind2},
are the indices of the two points, in the set being triangulated,
which are separated by that edge. The seventh and eighth entries,
in the columns named \code{bp1} and \code{bp2} are logical values.
The entry in column \code{bp1} indicates whether the first endpoint
of the corresponding edge of a Dirichlet tile is a boundary point
(a point on the boundary of the rectangular window).  Likewise for
the entry in column \code{bp2} and the second endpoint of the edge.

The nineth and tenth entries, in columns named \code{thirdv1}
and \code{thirdv2} are the indices of the respective third
vertices of the Delaunay triangle whose circumcentres constitute
the corresponding endpoints of the edge under consideration.
(The other two vertices of the triangle in question are indexed by
the entries of columns \code{ind1} and \code{ind2}.)

The entries of columns \code{thirdv1} and \code{thirdv2} may (also)
take the values $-1, -2, -3$, and $-4$.  This will be the case
if the circumcentre in question lies outside of the rectangular
window \code{rw}.  In these circumstances the corresponding
endpoint of the tile edge is the intersection of the line joining
the two circumcentres with the boundary of \code{rw}, and the
numeric value of the entry of column \dQuote{thirdv1} (respectively
\dQuote{thirdv2}) indicates which side.  The numbering follows the
convention for numbering the sides of a plot region in \code{R}:
1 for the bottom side, 2 for the left hand side, 3 for the top side
and 4 for the right hand side.

Note that the entry in column \code{thirdv1} will be negative if and
only if the corresponding entry in column \code{bp1} is \code{TRUE}.
Similarly for columns \code{thirdv2} and \code{bp2}.
}

\item{summary}{
a data frame with 9, 10 or 11 columns and \code{n.data + n.dum} rows
(see below).  The rows correspond to the points in the set being
triangulated.  Note that the row names are the indices of the points
in the orginal sequence of points being triangulated/tessellated.
Usually these will be the sequence 1, 2, ..., npd ("n plus dummy").
However if there were \emph{duplicated} points then the row name
corresponding to a point is the \emph{first} of the indices of
the set of duplicated points in which the given point appears.
The columns are:
\itemize{
\item \code{x} (the \eqn{x}-coordinate of the point)
\item \code{y} (the \eqn{y}-coordinate of the point)
\item \code{pt.type} (a character vector with entries \dQuote{data}
and \dQuote{dummy}; present only if \code{n.dum > 0})
\item \code{z} (the auxiliary values or \dQuote{weights}; present
only if these were specified)
\item \code{n.tri} (the number of Delaunay triangles emanating from
the point)
\item \code{del.area} (1/3 of the total area of all the Delaunay
triangles emanating from the point)
\item \code{del.wts} (the corresponding entry of the \code{del.area}
column divided by the sum of this column)
\item \code{n.tside} (the number of sides --- within the rectangular
window --- of the Dirichlet tile surrounding the point)
\item \code{nbpt} (the number of points in which the Dirichlet tile
intersects the boundary of the rectangular window)
\item \code{dir.area} (the area of the Dirichlet tile surrounding
the point)
\item \code{dir.wts} (the corresponding entry of the \code{dir.area}
column divided by the sum of this column).
}

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
}
\item{ind.orig}{A vector of the indices of the points (x,y) in the
set of coordinates initially supplied (as data points or as dummy
points) to \code{deldir()} before duplicate points (if any) were
removed.  These indices are used by \code{\link{triang.list}()}.
}}
\section{Remark:}{
If ndx >= 2 and ndy >= 2, then the rectangular window IS the convex
hull, and so the values of del.area and dir.area (if the latter is
not \code{NULL}) are identical.
}
\section{Side Effects}{
If \code{plotit=TRUE} a plot of the triangulation and/or tessellation
is produced or added to an existing plot.
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
advice from Adrian Baddeley, formerly of D.M.S., CSIRO (now
Professor of Statistics at Curtin University).
Daryl Tingley of the Department of Mathematics
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

\section{Notes on error messages}{

In the underlying Fortran code, error traps have been set for
17 different errors, which are identified by an error number
\code{nerror}.  When one of these traps detects an error, the
value of \code{nerror} is passed back along the call stack to the
\code{R} function \code{deldir()} that calls the Fortran subroutines.
(I.e. to \emph{this} function, the documentation of which you are
currently reading.)  The \code{deldir()} function then prints out a
message and returns (invisibly) a \code{NULL} value.  The  message
consists only of the value of \code{nerror}.  A glossary of the
meanings of the values of \code{nerror} is to be found in the file
\code{err.list}, located in the top level of the package directory
(\dQuote{folder} if you are a Windoze weenie).

Note that the values 4, 14 and 15 of \code{nerror} do not cause
\code{deldir()} to return a \code{NULL} value but rather cause
a message to be printed, storage (memory) to be re-allocated
(increased) and \code{deldir()} to be re-started so as to take
advantage of the increased amount of storage.

In version \code{0.1-16} of \code{deldir} a new error trap was
introduced, and this new trap triggers a genuine error and does
so in a direct and perspicuous manner.

This new error trap relates to \dQuote{triangle problems}.  It was
drawn to my attention by Adam Dadvar (on 18 December, 2018) that in
some data sets collinearity problems may cause the \dQuote{triangle
finding} procedure, used by the algorithm to successively add new
points to a tessellation, to go into an infinite loop.  A symptom of
the collinearity is that the vertices of a putative triangle appear
\emph{not} to be in anticlockwise order irrespective of whether
they are presented in the order \code{i, j, k} or \code{k, j, i}.
The result of this anomaly is that the procedure keeps alternating
between moving to \dQuote{triangle} \code{i, j, k} and moving to
\dQuote{triangle} \code{k, j, i}, forever.

The new error trap, set in \code{trifnd}, the triangle finding
subroutine, detects such occurrences of \dQuote{clockwise in either
orientation} vertices.  The trap causes the \code{deldir()} function
to throw an error rather than disappearing into a black hole.
The error is thrown \dQuote{directly} rather than via passing a
\code{nerror} number back up the call stack.  The facility for
triggering an error in this manner was not available when the
\code{deldir} package was originally written.  In the reasonably
near future the \code{deldir} package will be adjusted so that all
error traps throw errors in the \dQuote{direct} manner, and use of
the \code{nerror} numbers will be eliminated.

When an error of the \dQuote{triangle problems} nature occurs, a
\emph{possible} remedy is to increase the value of the \code{eps}
argument of \code{deldir()}. (See the \bold{Examples}.)  There may
conceiveably be other problems that lead to infinite loops and so I
have put in another error trap to detect whether the procedure has
inspected more triangles than actually exist, and if so to throw
an error.

Note that the strategy of increasing the value of \code{eps}
is \emph{probably} the appropriate one in most (if not all)
of the cases where errors of this nature arise.  (Similarly this
strategy is \emph{probably} the appropriate response to errors with
\code{nerror} equal to 3, 12 and 13.) However it is impossible to
be sure.  The intricacy and numerical delicacy of triangulations
is too great for anyone to be able to foresee all the possibilities
that could arise.

If there is any doubt as the appropriateness of the \dQuote{increase
\code{eps}} strategy, the user is advised to do his or her best to
explore the data set, graphically or by other means, and thereby
determine what is actually going on and why problems are occurring.
}

\section{Warnings}{
\enumerate{
\item The process for determining if points are duplicated
changed between versions 0.1-9 and 0.1-10.  Previously there
was an argument \code{frac} for this function, which defaulted
to 0.0001.  Points were deemed to be duplicates if the difference
in \code{x}-coordinates was less than \code{frac} times the width
of \code{rw} and \code{y}-coordinates was less than \code{frac}
times the height of \code{rw}.  This process has been changed to
one which uses \code{\link{duplicated}()} on the data frame whose
columns are \code{x} and \code{y}.

As a result it may happen that points which were previously
eliminated as duplicates will no longer be eliminated.  (And
possibly vice-versa.)

\item The components \code{delsgs} and \code{summary} of the value
returned by \code{deldir()} are now \emph{data frames} rather than
matrices.  The component \code{summary} was changed to allow the
\dQuote{auxiliary} values \code{z} to be of arbitrary mode (i.e.
not necessarily numeric).  The component \code{delsgs} was then
changed for consistency.  Note that the other \dQuote{matrix-like}
component \code{dirsgs} has been a data frame since time immemorial.

A message alerting the user to the foregoing two items is printed
out the first time that \code{deldir()} is called with
\code{suppressMsge=FALSE} in a given session.  In succeeding
calls to \code{deldir()} in the same session, no message is printed.
(I.e. the \dQuote{alerting} message is printed \emph{at most once}
in any given session.)

The \dQuote{alerting} message is \emph{not} produced via the
\code{warning()} function, so \code{suppressWarnings()} will
\emph{not} suppress its appearance.  To effect such suppression
(necessary only on the first call to \code{deldir()} in a
given session) one must set the \code{suppressMsge} argument of
\code{deldir} equal to \code{TRUE}.

\item If any dummy points are created, and if a vector \code{z}, of
\dQuote{auxiliary} values or \dQuote{weights} associated with the
points being triangulated, is supplied, then it is up to the user to
supply the corresponding auxiliary values or weights associated with
the dummy points.  These values should be supplied as \code{zdum}.
If \code{zdum} is not supplied then the auxiliary values or weights
associated with the dummy points are all taken to be missing values
(i.e. \code{NA}).
}
}

\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
% \url{https://www.stat.auckland.ac.nz/~rolf}
}

\references{
Lee, D. T., and Schacter, B. J.  Two algorithms for constructing a
Delaunay triangulation, Int. J. Computer and Information Sciences,
Vol. 9, No. 3, 1980, pp. 219 -- 242.

Ahuja, N. and Schacter, B. J. (1983). Pattern Models.  New York: Wiley.
}
\seealso{
  \code{\link{plot.deldir}()}, \code{\link{tile.list}()}, \code{\link{triang.list}()}
}
\examples{
x    <- c(2.3,3.0,7.0,1.0,3.0,8.0)
y    <- c(2.3,3.0,2.0,5.0,8.0,9.0)

# Let deldir() choose the rectangular window.
dxy1 <- deldir(x,y)

# User chooses the rectangular window.
dxy2 <- deldir(x,y,rw=c(0,10,0,10))

# Put dummy points at the corners of the rectangular
# window, i.e. at (0,0), (10,0), (10,10), and (0,10)
dxy3 <- deldir(x,y,dpl=list(ndx=2,ndy=2),rw=c(0,10,0,10))

# Plot the triangulation created (but not the tesselation).
\dontrun{
dxy2 <- deldir(x,y,rw=c(0,10,0,10),plot=TRUE,wl='tr')
}

# Auxiliary values associated with points; 4 dummy points to be
# added so 4 dummy "z-values" provided.
z    <- c(1.63,0.79,2.84,1.56,0.22,1.07)
zdum <- rep(42,4)
dxy4 <- deldir(x,y,dpl=list(ndx=2,ndy=2),rw=c(0,10,0,10),z=z,zdum=zdum)

# Example of collinearity error.
\dontrun{
    dniP <- deldir(niProperties) # Throws an error
}
    dniP <- deldir(niProperties,eps=1e-8) # No error.
}
\keyword{spatial}
