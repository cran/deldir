.BG
.FN deldir
.TL
Construct the Delaunay triangulation and the Dirichlet
(or Voronoi) tessellation of a planar point set.
.DN
This function computes the Delaunay triangulation (and hence the
Dirichlet tesselation) of a planar point set according to the second
(iterative) algorithm of Lee and Schacter --- see REFERENCES.  The
triangulation is made to be with respect to the whole plane by
`suspending' it from so-called ideal points (-Inf,-Inf), (Inf,-Inf)
(Inf,Inf), and (-Inf,Inf).  The triangulation is also enclosed in a
finite rectangular window.  A set of dummy points may
be added, in various ways, to the set of data points being triangulated.
.CS
deldir(x, y, dpl=NULL, rw=NULL, eps=1e-09, frac=0.0001,
       sort=T, plot=F, digits=6, ...)
.RA
.AG x,y
The coordinates of the point set being triangulated. These can be
given by two arguments x and y which are vectors or by a single
argument x which is a list with components "x" and "y".
.OA
.AG dpl
A list describing the structure of the dummy points to be added
to the data being triangulated.  The addition of these dummy points
is effected by the auxilliary function dumpts().  The list may have
components:
.PP
ndx: The x-dimension of a rectangular grid; if either ndx or ndy is null,
no grid is constructed.
.PP
ndy: The y-dimension of the aforementioned rectangular grid.
.PP
nrad: The number of radii or "spokes", emanating from each data point,
along which dummy points are to be added.
.PP
nper: The number of dummy points per spoke.
.PP
fctr: A factor determining the length of each spoke; each spoke is of
length equal to fctr times the mean nearest neighbour distance of the data.
(This distance is calculated by the auxilliary function mnnd().)
.PP
x: A vector of x-coordinates of "ad hoc" dummy points
.PP
y: A vector of the corresponding y-coordinates of "ad hoc" dummy points
.PP
.AG rw
The coordinates of the corners of the rectangular window enclosing
the triangulation, in the order (xmin, xmax, ymin, ymax).  Any data
points (including dummy points) outside this window are discarded.
If this argument is omitted, it defaults to values given by the range
of the data, plus and minus 10 percent.
.AG eps
A value of epsilon used in testing whether a quantity is zero, mainly
in the context of whether points are collinear.  If anomalous errors
arise, it is possible that these may averted by adjusting the value
of eps upward or downward.
.AG frac
A value specifying the tolerance used in eliminating duplicate
points; defaults to 0.0001. Points are considered duplicates if
abs(x1-x2) < frac*(xmax-xmin) AND abs(y1-y2) < frac*(ymax-ymin).
.AG sort
Logical argument; if TRUE (the default) the data (including dummy
points) are sorted into a sequence of "bins" prior to triangulation;
this makes the algorithm slightly more efficient.  Normally one would
set sort equal to FALSE only if one wished to observe some of the
fine detail of the way in which adding a point to a data set affected
the triangulation, and therefore wished to make sure that the point
in question was added last.  Essentially this argument would get used
only in a de-bugging process.
.AG plot
Logical argument; if TRUE a plot of the triangulation and tessellation
is produced; the default is FALSE.
.AG digits
The number of decimal places to which all numeric values in the
returned list should be rounded.  Defaults to 6.
.AG ...
Auxilliary arguments add, wlines, wpoints, number, nex, col, lty,
pch, xlim, and ylim may be passed to plot.deldir through "..." if
plot=T.
.RT
A list (of class `deldir'), invisible if plot=T, with components:
.RC delsgs
a matrix with 6 columns.  The first 4 entries of each row are the
coordinates of the points joined by an edge of a Delaunay
triangle, in the order (x1,y1,x2,y2).  The last two entries are the
indices of the two points which are joined.
.RC dirsgs
a data frame with 8 columns.  The first 4 entries of each row are the
coordinates of the endpoints of one the edges of a Dirichlet tile, in
the order (x1,y1,x2,y2).  The fifth and sixth entries are the indices
of the two points, in the set being triangulated, which are separated
by that edge. The seventh and eighth entries are logical values.  The
seventh indicates whether the first endpoint of the corresponding
edge of a Dirichlet tile is a boundary point (a point on the boundary
of the rectangular window).  Likewise for the eighth entry and the
second endpoint of the edge.
.RC summary
a matrix with 9 columns, and (n.data + n.dum) rows (see below).
These rows correspond to the points in the set being triangulated.
The columns are named "x" (the x-coordinate of the point), "y" (the
y-coordinate), "n.tri" (the number of Delaunay triangles emanating
from the point), "del.area" (1/3 of the total area of all the
Delaunay triangles emanating from the point), "del.wts" (the
corresponding entry of the "del.area" column divided by the sum of
this column), "n.tside" (the number of sides --- within the
rectangular window --- of the Dirichlet tile surrounding the point),
"nbpt" (the number of points in which the Dirichlet tile intersects
the boundary of the rectangular window), "dir.area" (the area of the
Dirichlet tile surrounding the point), and "dir.wts" (the
corresponding entry of the "dir.area" column divided by the sum of
this column).  Note that the factor of 1/3 associated with the
del.area column arises because each triangle occurs three times ---
once for each corner.
.RC n.data
the number of real (as opposed to dummy) points in the set which was
triangulated, with any duplicate points eliminated.  The first n.data
rows of "summary" correspond to real points.
.RC n.dum
the number of dummy points which were added to the set being triangulated,
with any duplicate points (including any which duplicate real points)
eliminated.  The last n.dum rows of "summary" correspond to dummy
points.
.RC del.area
the area of the convex hull of the set of points being triangulated,
as formed by summing the "del.area" column of "summary".
.RC dir.area
the area of the rectangular window enclosing the points being triangulated,
as formed by summing the "dir.area" column of "summary".
.RC rw
the specification of the corners of the rectangular window enclosing
the data, in the order (xmin, xmax, ymin, ymax).
.SH NOTE
If ndx >= 2 and ndy >= 2, then the rectangular window IS the convex
hull, and so the values of del.area and dir.area are identical.
.SE
If plot==T a plot of the triangulation and/or tessellation is produced
or added to an existing plot.
.DT
The code to effect this implementation of the Lee-Schacter algorithm
was originally written in 1987/88 by Rolf Turner, while with the
Division of Mathematics and Statistics, CSIRO, Sydney, Australia.  It
was re-written to adapt the implementation from a stand-alone Fortran
program to an Splus function, by Rolf Turner while visiting the
University of Western Australia, May, 1995.  Further revisions made
December 1996. The author gratefully acknowledges the contributions,
assistance, and guidance of Mark Berman, of D.M.S., CSIRO, in
collaboration with whom this project was originally undertaken.  The
author also acknowledges much useful advice from Adrian Baddeley,
formerly of D.M.S., CSIRO (now Professor of Statistics at the
University of Western Australia).  Daryl Tingley of the Department of
Mathematics and Statistics, University of New Brunswick provided some
helpful insight.  Special thanks are extended to Alan Johnson, of the
Alaska Fisheries Science Centre, who supplied two data sets which
were extremely valuable in tracking down some errors in the code.

Don MacQueen, of Lawrence Livermore National Lab, wrote an Splus
driver function for the old stand-alone version of this software.
That driver, which was available on Statlib, is now deprecated in
favour of this current package.  Don also collaborated in the
preparation of this current package.

.SH REFERENCES
Lee, D. T., and Schacter, B. J.  "Two algorithms for constructing a
Delaunay triangulation", Int. J. Computer and Information
Sciences, Vol. 9, No. 3, 1980, pp. 219 -- 242.

Ahuja, N. and Schacter, B. J. (1983). Pattern Models.  New York: Wiley.
.SA
plot.deldir
.EX
x   <- c(2.3,3.0,7.0,1.0,3.0,8.0)
y   <- c(2.3,3.0,2.0,5.0,8.0,9.0)
try <- deldir(x,y,list(ndx=2,ndy=2),c(0,10,0,10))
# Puts dummy points at the corners of the rectangular
# window, i.e. at (0,0), (10,0), (10,10), and (0,10)
try <- deldir(x,y,list(ndx=2,ndy=2),c(0,10,0,10),plot=T,wl='tr')
# Plots the triangulation which was created (but not the tesselation).
.KW
.WR
