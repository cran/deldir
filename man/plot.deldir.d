.BG
.FN plot.deldir
.TL
Produce a plot of the Delaunay triangulation and Dirichlet (Voronoi)
tesselation of a planar point set, as constructed by the function deldir.
.DN
This is a method for plot.
.CS
plot.deldir(object, add=F, wlines='both', wpoints='both',
            number=F, cex=0.5, nex=0.75, col=NULL, lty=NULL,
            pch=NULL, xlim=NULL, ylim=NULL)

plot(object, add=F, wlines='both', wpoints='both',
     number=F, cex=0.5, nex=0.75, col=NULL, lty=NULL,
     pch=NULL, xlim=NULL, ylim=NULL)

.RA
.AG object
An object of class "deldir" as constructed by the function deldir.
.OA
.AG add
logical argument; should the plot be added to an existing plot?
.AG wlines
"which lines?".  I.e.  should the Delaunay triangulation be plotted
(wlines='triang'), should the Dirichlet tessellation be plotted
(wlines='tess'), or should both be plotted (wlines='both', the
default) ?
.AG wpoints
"which points?".  I.e.  should the real points be plotted
(wpoints='real'), should the dummy points be plotted
(wpoints='dummy'), should both be plotted (wpoints='both', the
default) or should no points be plotted (wpoints='none')?
.AG number
Logical argument, defaulting to FALSE; if TRUE then the points plotted
will be labelled with their index numbers (corresponding to the row
numbers of the matrix "summary" in the output of deldir).
.AG cex
The value of the character expansion argument cex to be used
with the plotting symbols for plotting the points.
.AG nex
The value of the character expansion argument cex to be used by the
text function when numbering the points with their indices.  Used only
if number=T.
.AG col
the colour numbers for plotting the triangulation, the tesselation,
the data points, the dummy points, and the point numbers, in that
order; defaults to c(1,1,1,1,1).  If fewer than five numbers are
given, they are recycled.  (If more than five numbers are given, the
redundant ones are ignored.)
.AG lty
the line type numbers for plotting the triangulation and the
tesselation, in that order; defaults to 1:2.  If only one value is
given it is repeated.  (If more than two numbers are given, the
redundant ones are ignored.)
.AG pch
the plotting symbols for plotting the data points and the dummy
points, in that order; may be either integer or character; defaults
to 1:2.  If only one value is given it is repeated.  (If more than
two values are given, the redundant ones are ignored.)
.AG xlim
the limits on the x-axis.  Defaults to rw[1:2] where rw is the
rectangular window specification returned by deldir().
.AG ylim
the limits on the y-axis.  Defaults to rw[3:4] where rw is the
rectangular window specification returned by deldir().
.SE
A plot of the points being triangulated is produced or added to
an existing plot.  As well, the edges of the Delaunay
triangles and/or of the Dirichlet tiles are plotted.  By default
the triangles are plotted with solid lines (lty=1) and the tiles
with dotted lines (lty=2).
.DT
The points in the set being triangulated are plotted with distinguishing
symbols.  By default the real points are plotted as circles (pch=1) and the
dummy points are plotted as triangles (pch=2).
.SA
deldir
.EX
try <- deldir(x,y,list(ndx=2,ndy=2),c(0,10,0,10))
plot(try)
#
deldir(x,y,list(ndx=4,ndy=4),plot=T,add=T,wl='te',
       col=c(1,1,2,3,4),num=T)
# Plots the tesselation, but does not save the results.
try <- deldir(x,y,list(ndx=2,ndy=2),c(0,10,0,10),plot=T,wl='tr',
              wp='n')
# Plots the triangulation, but not the points, and saves the returned
structure.
.KW
.WR
