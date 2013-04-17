deldir <- local({

EnvSupp <- new.env()

function(x,y,dpl=NULL,rw=NULL,eps=1e-9,sort=TRUE,
                   plotit=FALSE,digits=6,z=NULL, zdum=NULL,
                   suppressMsge=FALSE,...) {
# Function deldir
#
#   Copyright (C) 1996 by T. Rolf Turner
#
#   Permission to use, copy, modify, and distribute this software and
#   its documentation for any purpose and without fee is hereby
#   granted, provided that the above copyright notice appear in all
#   copies and that both that copyright notice and this permission
#   notice appear in supporting documentation.
#
# ORIGINALLY PROGRAMMED BY: Rolf Turner in 1987/88, while with the
# Division of Mathematics and Statistics, CSIRO, Sydney, Australia.
# Re-programmed by Rolf Turner to adapt the implementation from a
# stand-alone Fortran program to an S function, while visiting the
# University of Western Australia, May 1995.  Further revised
# December 1996.
# 
# Function to compute the Delaunay Triangulation (and hence the
# Dirichlet Tesselation) of a planar point set according to the
# second (iterative) algorithm of Lee and Schacter, International
# Journal of Computer and Information Sciences, Vol. 9, No. 3, 1980,
# pages 219 to 242.

# The triangulation is made to be with respect to the whole plane by
# `suspending' it from `ideal' points
# (-R,-R), (R,-R) (R,R), and (-R,R), where R --> infinity.

# It is also enclosed in a finite rectangle (whose boundaries truncate any
# infinite Dirichlet tiles) with corners (xmin,ymin) etc.  This rectangle
# is referred to elsewhere as `the' rectangular window.
if(exists("deldirMsgeDone",envir=EnvSupp)) suppressMsge <- TRUE
if(!suppressMsge){
    cat(paste("\n     PLEASE NOTE:  The components \"delsgs\" and \"summary\"",
        "of the", "\n     object returned by deldir() are now",
        "DATA FRAMES rather than","\n     matrices (as they were prior",
        "to release 0.0-18).",    "\n     See help(\"deldir\").\n",
        "\n     PLEASE NOTE: The process that deldir() uses for determining\n",
        "    duplicated points has changed from that used in version\n",
        "    0.0-9 of this package (and previously). See help(\"deldir\").\n\n"))
    assign("deldirMsgeDone","xxx",envir=EnvSupp)
}

# If the first argument is a list, extract components x and y (and
# possibly z).
if(is.list(x)) {
	if(all(!is.na(match(c('x','y'),names(x))))) {
		y <- x$y
                z <- if(!is.null(x$z)) x$z else z
		x <- x$x
	}
	else {
		stop("Argument \"x\" is a list but lacks x and/or y components.\n")
		return()
	}
}
haveZ <- !is.null(z)

# Check that lengths match.
n <- length(x)
if(n!=length(y)) stop("Lengths \"x\" and \"y\" do not match.\n")
if(haveZ) {
	if(n!=length(z))
		stop("Length of \"z\" does not match lengths of \"x\" and \"y\".\n")
}

# If a data window is specified, get its corner coordinates
# and truncate the data by this window.  Discard any constraint
# segments either of whose endpoints fall outside this window.
if(!is.null(rw)) {
	xmin <- rw[1]
	xmax <- rw[2]
	ymin <- rw[3]
	ymax <- rw[4]
	drop <- (1:n)[x<xmin|x>xmax|y<ymin|y>ymax]
	if(length(drop)>0) {
		x <- x[-drop]
		y <- y[-drop]
		n <- length(x)
	}
}

# If corners of the window are not specified, form them from
# the minimum and maximum of the data +/- 10%:
else {
	xmin <- min(x)
	xmax <- max(x)
	ymin <- min(y)
	ymax <- max(y)
	xdff <- xmax-xmin
	ydff <- ymax-ymin
	xmin <- xmin-0.1*xdff
	xmax <- xmax+0.1*xdff
	ymin <- ymin-0.1*ydff
	ymax <- ymax+0.1*ydff
	rw   <- c(xmin,xmax,ymin,ymax)
}

# Add the dummy points:
if(!is.null(dpl)) {
	dpts <- dumpts(x,y,dpl,rw)
	x    <- dpts$x
	y    <- dpts$y
        if(haveZ) {
		ndum <- length(x)-length(z)
		if(!is.null(zdum)) {
			if(length(zdum) != ndum)
				stop("The z dummy points are of the wrong length.\n")
		} else {
			zdum <- rep(NA,ndum)
		}
		z <- c(z,zdum)
	}
}

# Eliminate duplicate points:
iii <- !duplicatedxy(x,y)
ndm <- sum(iii[-(1:n)])
n   <- sum(iii[1:n])
x   <- x[iii]
y   <- y[iii]

# Make space for the total number of points (real and dummy) as
# well as 4 ideal points and 4 extra corner points which get used
# (only) by subroutines dirseg and dirout in the ``output'' process
# (returning a description of the triangulation after it has been
# calculated):
npd  <- n + ndm
ntot <- npd + 4               # ntot includes the 4 ideal points but
                              # but NOT the 4 extra corners
x <- c(rep(0,4),x,rep(0,4))
y <- c(rep(0,4),y,rep(0,4))

# Set up fixed dimensioning constants:
ntdel  <- 4*npd
ntdir  <- 3*npd

# Set up dimensioning constants which might need to be increased:
madj <- max(20,ceiling(3*sqrt(ntot)))
tadj <- (madj+1)*(ntot+4)
ndel <- madj*(madj+1)/2
tdel <- 6*ndel
ndir <- ndel
tdir <- 8*ndir

# Call the master subroutine to do the work:
repeat {
	tmp <- .Fortran(
			'master',
			x=as.double(x),
			y=as.double(y),
			sort=as.logical(sort),
			rw=as.double(rw),
			npd=as.integer(npd),
			ntot=as.integer(ntot),
			nadj=integer(tadj),
			madj=as.integer(madj),
			ind=integer(npd),
			tx=double(npd),
			ty=double(npd),
			ilist=integer(npd),
			eps=as.double(eps),
			delsgs=double(tdel),
			ndel=as.integer(ndel),
			delsum=double(ntdel),
			dirsgs=double(tdir),
			ndir=as.integer(ndir),
			dirsum=double(ntdir),
			nerror=integer(1),
			PACKAGE='deldir'
		)

# Check for errors:
	nerror <- tmp$nerror
	if(nerror < 0) break

	else {
		if(nerror==4) {
			cat('nerror =',nerror,'\n')
			cat('Increasing madj and trying again.\n')
			madj <- ceiling(1.2*madj)
			tadj <- (madj+1)*(ntot+4)
			ndel <- max(ndel,madj*(madj+1)/2)
			tdel <- 6*ndel
			ndir <- ndel
			tdir <- 8*ndir
			}
		else if(nerror==14|nerror==15) {
			cat('nerror =',nerror,'\n')
			cat('Increasing ndel and ndir and trying again.\n')
			ndel <- ceiling(1.2*ndel)
			tdel <- 6*ndel
	                ndir <- ndel
	                tdir <- 8*ndir
		}
		else {
			cat('nerror =',nerror,'\n')
			return(invisible())
		}
	}
}

# Collect up the results for return:
ndel             <- tmp$ndel
delsgs           <- round(t(as.matrix(matrix(tmp$delsgs,nrow=6)[,1:ndel])),digits)
delsgs           <- as.data.frame(delsgs)
names(delsgs)    <- c('x1','y1','x2','y2','ind1','ind2')
delsum           <- matrix(tmp$delsum,ncol=4)
del.area         <- sum(delsum[,4])
delsum           <- round(cbind(delsum,delsum[,4]/del.area),digits)
del.area         <- round(del.area,digits)
ndir             <- tmp$ndir
dirsgs           <- round(t(as.matrix(matrix(tmp$dirsgs,nrow=8)[,1:ndir])),digits)
dirsgs           <- as.data.frame(dirsgs)
dirsum           <- matrix(tmp$dirsum,ncol=3)
dir.area         <- sum(dirsum[,3])
dirsum           <- round(cbind(dirsum,dirsum[,3]/dir.area),digits)
dir.area         <- round(dir.area,digits)
names(dirsgs)    <- c('x1','y1','x2','y2','ind1','ind2','bp1','bp2')
mode(dirsgs$bp1) <- 'logical'
mode(dirsgs$bp2) <- 'logical'
allsum           <- as.data.frame(cbind(delsum,dirsum))
names(allsum)    <- c('x','y','n.tri','del.area','del.wts',
                              'n.tside','nbpt','dir.area','dir.wts')
if(haveZ) allsum <- cbind(allsum[,1:2],z=z,allsum[,3:9])
rw               <- round(rw,digits)

# Aw' done!!!
rslt <- list(delsgs=delsgs,dirsgs=dirsgs,summary=allsum,n.data=n,
             n.dum=ndm,del.area=del.area,dir.area=dir.area,rw=rw)
class(rslt) <- 'deldir'
if(plotit) {
	plot(rslt,...)
	return(invisible(rslt))
} else return(rslt)
}
}
)
