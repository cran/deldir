deldir <- local({

EnvSupp <- new.env()

function(x,y,dpl=NULL,rw=NULL,eps=1e-9,sort=TRUE,plot=FALSE,
         round=TRUE,digits=6,z=NULL, zdum=NULL,suppressMsge=FALSE,...) {
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
mess <- NULL
#
# Example message:
# mess <- paste("\n     Go stick your head right up a dead bear's",
#               "\n     pohutukawa.  Or something like that.\n\n")
#
if(!(is.null(mess) | suppressMsge)){
    message(mess)
    assign("deldirMsgeDone","xxx",envir=EnvSupp)
}

#
# If the first argument is a data frame, extract the column
# named "z", if there is one, to be the "z weights".  Remove
# this column from the first argument.  Extract the column named
# "x", if there is one, to be the "x" coordinates, otherwise take
# the "x" coordinates to be the first column which is isn't named
# "y".  Extract the column named "y", if there is one, to be the
# "y" coordinates, otherwise take the "y" coordinates to be the
# first column which is isn't named "x".
if(is.data.frame(x)) {
        if(ncol(x) < 2)
            stop(paste("If \"x\" is a data frame it must have\n",
                       "at least two columns.\n"))
        j  <- match("z",names(x))
        if(!is.na(j)) {
            if(is.null(z)) z <- x[,j]
            x <- x[,-j]
        }
        j <- match(c("x","y"),names(x))
        if(all(is.na(j))){
            j <- 1:2
        } else {
            if(is.na(j[2])) j[2] <- if(j[1]==1) 2 else 1
            if(is.na(j[1])) j[1] <- if(j[2]==1) 2 else 1
        }
        y <- x[,j[2]]
        x <- x[,j[1]]
} else if(inherits(x,"ppp")) {

# If the first argument is an object of class "ppp", extract the x and y
# coordinates from this object. If this object is "marked" and
# if the marks are a vector or a factor and if z is NULL, then set
# z equal to the marks.
    if(is.null(z)) {
        marx <- x$marks
        ok   <- !is.null(marx) && (is.vector(marx) | is.factor(marx))
        if(ok) z <- x$marks
    }
    y <- x$y
    x <- x$x
} else if(is.list(x)) {

# If the first argument is a list (but not a data frame) extract
# components x and y (and possibly z).
    if(all(!is.na(match(c('x','y'),names(x))))) {
       	y <- x$y
        z <- if(is.null(z) && !inherits(x,"ppp")) x$z else z
        x <- x$x
    }
    else {
        stop("Argument \"x\" is a list but lacks x and/or y components.\n")
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

# If a data window is specified, turn it into a 4-tuple (if necessary).
# Otherwise, if x is of class "ppp", form the data window as the bounding
# box of the window of that
if(!is.null(rw)) {
   if(inherits(rw,"owin")) {
       xr <- rw$window$xrange
       yr <- rw$window$yrange
       rw <- c(xr,yr)
   }

# Apparently --- according to Michael Chirico 24/09/2015 --- the
# following will accommodate the bounding box of a collection of
# polygons as structured in the "sp" package.
   if(is.matrix(rw)) rw <- as.vector(t(rw))
} else if(inherits(x,"ppp")) {
   rw <- c(x$window$xrange, x$window$yrange)
}

# If a data window now exists, get its corner coordinates
# and truncate the data by this window.
if(!is.null(rw)) {
	xmin <- rw[1]
	xmax <- rw[2]
	ymin <- rw[3]
	ymax <- rw[4]
        ind.orig <- 1:n
	drop     <- ind.orig[x<xmin|x>xmax|y<ymin|y>ymax]
	if(length(drop)>0) {
		x <- x[-drop]
		y <- y[-drop]
                ind.orig <- ind.orig[-drop]
                if(haveZ) z <- z[-drop]
	}
}

# If the rectangular window is (still) not specified, form its corners
# from the minimum and maximum of the data +/- 10%:
if(is.null(rw)) {
    if(length(x)==0)
        stop("No points and no rectangular window specified.\n")
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
    ind.orig <- 1:n
}
nn <- length(x) # Could be different from "n" if the data were
                # clipped to rw.

# Add the dummy points:
if(!is.null(dpl)) {
    dpts <- dumpts(x,y,dpl,rw)
    x    <- dpts$x
    y    <- dpts$y
    ndm  <- length(x) - nn
    ind.orig <- c(ind.orig,nn+(1:ndm))
    if(haveZ) {
        if(!is.null(zdum)) {
            if(length(zdum) != ndm)
                stop("The z dummy points are of the wrong length.\n")
        } else {
            zdum <- rep(NA,ndm)
        }
        z <- c(z,zdum)
    }
} else ndm <- 0

# Eliminate duplicate points:
iii <- duplicatedxy(x,y)
if(any(iii)) {
    kkk <- !iii
    ndm <- sum(kkk[-(1:nn)])
    nn  <- sum(kkk[1:nn])
    if(haveZ) {
        jjj <- duplicated(data.frame(x=x,y=y,z=z))
        if(sum(jjj) < sum(iii)) {
            whinge <- paste("There were different z \"weights\" corresponding to\n",
                            "duplicated points.\n",sep="")
            warning(whinge)
        }
        z   <- z[kkk]
    }
    x   <- x[kkk]
    y   <- y[kkk]
    ind.orig <- ind.orig[!iii]
}

# Toadal length of coordinate vectors ("n plus dummy").
npd  <- nn + ndm
if(length(x) != npd) stop("Alles up-gefucken ist!\n")

# Check there are sufficiently many points to triangulate/tessellate.
if(npd <= 1) {
    whinge <- paste("There is at most one point, data or dummy, inside\n",
                    " the given rectangular window. Thus there are\n",
                    " insufficiently many points to triangulate/tessellate.\n")
    stop(whinge)
}

# Sort the coordinates into "bins".  There are approximately
# sqrt(npd) such bins.  The vector "ind" (index) keeps track of the
# re-ordering; if ind[i] == j then i is the index of a point in
# the original sequence of points and j is the index of the same
# point in the bin sorted sequence.  The vector "rind" (reverse
# index) does the opposite; if rind[i] == j then i is the position
# of a point in the bin sorted sequence and j is its position in
# the original sequence.  Thus ind[rind[k]] = k and rind[ind[k]] = k
# for all k.  So xs[ind] (where xs is the bin sorted sequence of
# x's) is equal to x, the original sequence of x's.  Likewise ys[ind]
# (where ys is the bin sorted sequence of y's) is equal to y, the
# original sequence of y's. Conversely x[rind] = xs and y[rind] = ys.
#
# Added 20/03/2017:  I think I rather made a meal of this.  The
# vector of indices "rind" is just order(ind); ind is a permutation
# of 1, 2, ..., npd.  Thus rind[ind] = ind[rind] = 1, 2, ..., npd.
# However there's no real harm done, so I won't change the shaganappi
# code at this stage.
if(sort) {
    xy   <- binsrtR(x,y,rw)
    x    <- xy$x
    y    <- xy$y
    ind  <- xy$ind
    rind <- xy$rind
} else {
    ind  <- 1:npd
    rind <- 1:npd
}

# Make space for the total number of points (real and dummy) as
# well as 4 ideal points and 4 extra corner points which get used
# (only) by subroutines dirseg and dirout in the ``output'' process
# (returning a description of the triangulation after it has been
# calculated):
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
tdir <- 10*ndir

# Call the master subroutine to do the work:
repeat {
	tmp <- .Fortran(
			'master',
			x=as.double(x),
			y=as.double(y),
			rw=as.double(rw),
			npd=as.integer(npd),
			ntot=as.integer(ntot),
			nadj=integer(tadj),
			madj=as.integer(madj),
			eps=as.double(eps),
			delsgs=double(tdel),
			ndel=as.integer(ndel),
			delsum=double(ntdel),
			dirsgs=double(tdir),
			ndir=as.integer(ndir),
			dirsum=double(ntdir),
                        incAdj=integer(1),
                        incSeg=integer(1),
			PACKAGE='deldir'

		)

# Check for problems with insufficient storage:
    incAdj <- tmp$incAdj
    incSeg <- tmp$incSeg
    if(incAdj==0 & incSeg==0) break
    if(incAdj==1) {
        nmadj <- ceiling(1.2*madj)
        wrds <- paste('Increasing madj from',madj,'to',nmadj,
                      'and trying again.')
        message(wrds)
        madj <- nmadj
        tadj <- (madj+1)*(ntot+4)
        ndel <- max(ndel,madj*(madj+1)/2)
        tdel <- 6*ndel
        ndir <- ndel
        tdir <- 10*ndir
    }
    if(incSeg==1) {
        nndel <- ceiling(1.2*ndel)
        wrds <-paste('Increasing ndel and ndir from',ndel,
                     'to',nndel,'and trying again.')
        message(wrds)
        ndel <- nndel
        tdel <- 6*ndel
        ndir <- ndel
        tdir <- 10*ndir
    }
}

# Collect up the results for return:
ndel             <- tmp$ndel
delsgs           <- if(round) {
                        round(t(as.matrix(matrix(tmp$delsgs,nrow=6)[,1:ndel])),digits)
                    } else {
                        t(as.matrix(matrix(tmp$delsgs,nrow=6)[,1:ndel]))
                    }
delsgs           <- as.data.frame(delsgs)
names(delsgs)    <- c('x1','y1','x2','y2','ind1','ind2')
delsum           <- matrix(tmp$delsum,ncol=4)
del.area         <- sum(delsum[,4])
delsum           <- if(round) {
                        round(cbind(delsum,delsum[,4]/del.area),digits)
                    } else {
                        cbind(delsum,delsum[,4]/del.area)
                    }
del.area         <- if(round) round(del.area,digits) else del.area
ndir             <- tmp$ndir
dirsgs           <- if(round) {
                        round(t(as.matrix(matrix(tmp$dirsgs,nrow=10)[,1:ndir])),digits)
                    } else {
                        t(as.matrix(matrix(tmp$dirsgs,nrow=10)[,1:ndir]))
                    }
dirsgs           <- as.data.frame(dirsgs)
dirsum           <- matrix(tmp$dirsum,ncol=3)
dir.area         <- sum(dirsum[,3])
dirsum           <- if(round) {
                        round(cbind(dirsum,dirsum[,3]/dir.area),digits)
                    } else {
                        cbind(dirsum,dirsum[,3]/dir.area)
                    }
dir.area         <- if(round) round(dir.area,digits) else dir.area
names(dirsgs)    <- c('x1','y1','x2','y2','ind1','ind2','bp1','bp2',
                      'thirdv1','thirdv2')
mode(dirsgs$bp1) <- 'logical'
mode(dirsgs$bp2) <- 'logical'
allsum           <- as.data.frame(cbind(delsum,dirsum))
names(allsum)    <- c('x','y','n.tri','del.area','del.wts',
                              'n.tside','nbpt','dir.area','dir.wts')

# The foregoing results are in terms of the indices of the bin sorted coordinates.
# Put things in terms of the indices of the original coordinates.
delsgs$ind1 <- rind[delsgs$ind1]
delsgs$ind2 <- rind[delsgs$ind2]
dirsgs$ind1 <- rind[dirsgs$ind1]
dirsgs$ind2 <- rind[dirsgs$ind2]
dirsgs$thirdv1  <- with(dirsgs,ifelse(thirdv1<0,thirdv1,rind[abs(thirdv1)]))
dirsgs$thirdv2  <- with(dirsgs,ifelse(thirdv2<0,thirdv2,rind[abs(thirdv2)]))

# The points in "allsum" appear in bin-sorted order; rearrange
# the rows of "allsum" so that the points appear in the original order.
allsum          <- allsum[ind,]

# The following is a furphy --- it just makes the rownames into
# 1, 2, ..., n.  At this point the rownames of "allsum" were
# (1:n)[ind].  So we're getting (1:n)[ind][rind] = ind[rind]
# = 1:n !!!
# rownames(allsum) <- rownames(allsum)[rind]
# So we could just set rownames(allsum) <- 1:nrow(allsum) and
# get the same effect.  However that does not take account of
# *duplicated* points.  So it is better to use ind.orig.  Note that
# the resulting rowname corresponding to a point is the index (in
# the original sequence of points) of the *first* in its sequence
# of duplicated points.

rownames(allsum) <- ind.orig

# Put in an indicator of point type if there were any
# dummy points added.
i1 <- if(ndm > 0) {
    data.frame(pt.type=c(rep("data",nn),rep("dummy",ndm)))
} else {
    as.data.frame(matrix(nrow=nn,ncol=0))
}
i2 <- if(haveZ) {
    data.frame(z=z)
} else {
    as.data.frame(matrix(nrow=nn+ndm,ncol=0))
}

allsum <- cbind(allsum[,1:2],i1,i2,allsum[,3:9])
rw     <- if(round) round(rw,digits) else rw

# Aw' done!!!
rslt <- list(delsgs=delsgs,dirsgs=dirsgs,summary=allsum,n.data=nn,
             n.dum=ndm,del.area=del.area,dir.area=dir.area,rw=rw,
             ind.orig=ind.orig)
attr(rslt,"round") <- round
attr(rslt,"digits") <- if(round) digits else NA
class(rslt) <- 'deldir'
if(plot) {
	plot(rslt,...)
	return(invisible(rslt))
} else return(rslt)
}
}
)
