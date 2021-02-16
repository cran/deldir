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
                      number=FALSE,cex=1,nex=1,cmpnt_col=NULL,cmpnt_lty=NULL,
                      pch=NULL,xlim=NULL,ylim=NULL,axes=FALSE,
                      xlab=if(axes) 'x' else '',ylab=if(axes) 'y' else'',
                      showrect=FALSE,asp=1,...)
}
\arguments{
\item{x}{
An object of class "deldir" as returned by the function deldir.
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
\item{cmpnt_col}{
A vector or list specifying the colours to be used for plotting
the (up to six) different components of the graphic, explictly the
triangulation, the tesselation, the data points, the dummy points,
the point numbers and the enclosing rectangle (\code{x$rw}) in that
order.  The components of this vector or list may be named, with the
names chosen from \code{"tri","tess","data","dummy","num","rect".}
The default is \code{c(tri=1,tess=1,data=1,dummy=1,num=1,rect=1)}.
If the vector or list is not named, the component names are assumed
to be from the default list of names in the given order.

Colours may be specified as positive integers, corresponding to
the entries of the current \code{\link{palette}()}, or as names of
colours (from the list given by \code{link{colors}()}), or  in terms
of their RGB components with strings of the form \code{"#RRGGBB"}.
(See \bold{Color Specification} in \code{\link{par}()}.)

If fewer than six colours are given, the missing components are
filled in with \code{1} or \code{palette()[1]} as is appropriate.
If \code{cmpnt_col} does not have names then the components are
simply recycled.  If more than six colours are given, the redundant
ones are ignored (i.e. only the first six are used).
}
\item{cmpnt_lty}{
A vector or list (of length two) of line type numbers for plotting
the two components of the graphic that consist of lines, i.e. the
triangulation and the tesselation.  The components of this vector
or list may have names (\code{"tri","tess"}).  The default is
c(tri=1,tess=1).  If a single value is given the missing one
is filled in as \code{1}.  If more than two numbers are given,
the redundant ones are ignored, i.e. only the first two are used.
See \code{\link{par}()} for the meaning of these numbers.
}
\item{pch}{
The plotting symbols for plotting the data points and the dummy
points, in that order; may be either integer or character; defaults
to 1:2.  If only one value is given it is repeated.  (If more than
two values are given, the redundant ones are ignored.)
}
\item{xlim}{
The limits on the x-axis.  Defaults to \code{rw[1:2]} where \code{rw} is the
rectangular window specification returned by deldir().
}
\item{ylim}{
The limits on the y-axis.  Defaults to \code{rw[3:4]} where \code{rw} is the
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
\item{asp}{
The aspect ratio of the plot; integer scalar or \code{NA}.  Set this
argument equal to \code{NA} to allow the data to determine the aspect
ratio and hence to make the plot occupy the complete plotting region
in both \code{x} and \code{y} directions.  This is inadvisable;
see the \bold{Warning}.  The \code{asp} argument is ignored if
\code{add} is \code{TRUE}.
}
\item{...}{
Further plotting parameters (e.g. \code{lw}, \code{col}) to be passed
to \code{segments()}.  (Conceivably one might also use \code{...}
to supply additional arguments to be passed to \code{points()}
or \code{text()} but this is unlikely.)  Note that if \code{col}
is specified as one of the \code{...} arguments then this is used
as the \code{col} argument of \code{segments} (and the values of
\code{cmpnt_col[1:2]} are ignored.
}
}
\section{Side Effects}{
A plot of the edges of the Delaunay triangles and/or of the Dirichlet
tiles is produced or added to an existing plot.  By default the
triangles are plotted with solid lines (lty=1) and the tiles with
dotted lines (lty=2).  In addition the (data and dummy) points
being triangulated may be plotted.
}
\details{
The points in the set being triangulated are plotted with distinguishing
symbols.  By default the real points are plotted as circles (pch=1) and the
dummy points are plotted as triangles (pch=2).
}
\section{Warning}{
In previous versions of the \code{deldir} package, the aspect
ratio was not set.  Instead, the shape of the plotting region was
set to be square (\code{pty="s"}).  This practice was suboptimal.
To reproduce previous behaviour set \code{asp=NA} (and possibly
\code{pty="s"}) in the call to \code{plot.deldir()}.  Users, unless
they \emph{really} understand what they are doing and why they are
doing it, are now \emph{strongly advised} not to set the value of
\code{asp} but rather to leave \code{asp} equal to its default value
of \code{1}.  Any other value may distort the tesselation and destroy
the perpendicular appearance of lines which are indeed perpendicular.
(And conversely may cause lines which are not perpendicular to
appear as if they are.)
}
\seealso{ \code{\link{deldir}()} }
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\examples{
x <- c(2.3,3.0,7.0,1.0,3.0,8.0) + 0.5
y <- c(2.3,3.0,2.0,5.0,8.0,9.0) + 0.5
dxy <- deldir(x,y,dpl=list(x=c(1,10,10,1),y=c(1,1,10,10)),rw=c(0,11,0,11))
plot(dxy)

# Plots the tesselation, but does not save the results:
deldir(x,y,dpl=list(x=c(1,10,10,1),y=c(1,1,10,10),rw=c(0,11,0,11)),plot=TRUE,
       wl='tess',cmpnt_col=c(1,1,2,3,4,5),num=TRUE)

# Plots the triangulation, but not the tessellation or the points,
# and saves the returned structure:

dxy <- deldir(x,y,dpl=list(x=c(1,10,10,1),y=c(1,1,10,10)),rw=c(0,11,0,11),plot=TRUE,
       wl='triang',wp="none")

# Plot everything:
plot(dxy,cmpnt_col=c("orange","green","red","blue","black","black"),cmpnt_lty=1,
     number=TRUE,nex=1.5,pch=c(19,9),showrect=TRUE,axes=TRUE)

# Complicated example from He Huang:
# "Linguistic distances".
vldm <- c(308.298557,592.555483,284.256926,141.421356,449.719913,
          733.976839,591.141269,282.842712,1.414214,732.562625)
ldm <- matrix(nrow=5,ncol=5)
ldm[row(ldm) > col(ldm)]  <- vldm
ldm[row(ldm) <= col(ldm)] <- 0
ldm <- (ldm + t(ldm))/2
rownames(ldm) <- LETTERS[1:5]
colnames(ldm) <- LETTERS[1:5]

# Data to be triangulated.
id <- c("A","B","C","D","E")
x  <- c(0.5,1,1,1.5,2)
y  <- c(5.5,3,7,6.5,5)
dat_Huang <- data.frame(id = id, x = x, y = y)

# Form the triangulation/tessellation.
dH <- deldir(dat_Huang)

# Plot the triangulation with line widths proportional
# to "linguistic distances".
all_col <- rainbow(100)
i       <- pmax(1,round(100*vldm/max(vldm)))
use_col <- all_col[i]
ind     <- as.matrix(dH$delsgs[,c("ind1","ind2")])
lwv <- ldm[ind]
lwv <- 10*lwv/max(lwv)
plot(dH,wlines="triang",col=use_col,wpoints="none",
     lw=lwv,asp=NA)
with(dH,text(x,y,id,cex=1.5))
}
\keyword{ hplot }
