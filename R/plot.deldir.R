plot.deldir <- local({

fixColours <- function(cmpnt_col) {
    col_nms <- c("tri","tess","data","dummy","num","rect")
    if(is.null(cmpnt_col)) {
        cmpnt_col <- rep(1,6)
        names(cmpnt_col) <- col_nms
    } else {
        cmpnt_col <- as.vector(cmpnt_col)
        if(length(cmpnt_col) > 6) cmpnt_col <- cmpnt_col[1:6]
        if(!is.null(names(cmpnt_col))) {
            if(!all(names(cmpnt_col) %in% col_nms)) {
                stop("Argument \"cmpnt_col\" has incorrect names.\n")
            }
            ctmp <- rep(NA,6)
            names(ctmp) <- col_nms
            ctmp[names(cmpnt_col)] <- cmpnt_col
            cmpnt_col <- ctmp
            if(any(is.na(cmpnt_col))) {
                mde <- mode(cmpnt_col)
                switch(EXPR=mde,
                   character={cmpnt_col[is.na(cmpnt_col)] <- palette()[1]},
                   numeric={cmpnt_col[is.na(cmpnt_col)] <- 1},
                   stop("Argument \"cmpnt_col\" is of an inappropriate mode.\n")
                )
            }
        } else {
            cmpnt_col <- rep(cmpnt_col,length.out=6)
            names(cmpnt_col) <- col_nms
        }
    }
    cmpnt_col
}

fixLines <- function(cmpnt_lty) {
    lty_nms <- c("tri","tess")
    if(is.null(cmpnt_lty)) {
        cmpnt_lty <- rep(1,2)
        names(cmpnt_lty) <- lty_nms
    } else {
        cmpnt_lty <- as.vector(cmpnt_lty)
        if(length(cmpnt_lty) > 2) cmpnt_lty <- cmpnt_lty[1:2]
        if(mode(cmpnt_lty) != "numeric") {
            stop("Argument \"cmpnt_lty\" must be of numeric mode.\n")
        }
        if(!is.null(names(cmpnt_lty))) {
            if(!all(names(cmpnt_lty) %in% lty_nms)) {
                stop("Argument \"cmpnt_lty\" has incorrect names.\n")
            }
            ltmp <- rep(NA,2)
            names(ltmp) <- lty_nms
            ltmp[names(cmpnt_lty)] <- cmpnt_lty
            cmpnt_lty <- ltmp
            if(any(is.na(cmpnt_lty))) {
                cmpnt_lty[is.na(cmpnt_lty)] <- 1
            }
        } else {
            cmpnt_lty <- rep(cmpnt_lty,length.out=2)
            names(cmpnt_lty) <- lty_nms
        }
    }
    cmpnt_lty
}

function(x,add=FALSE,wlines=c('both','triang','tess'),
                        wpoints=c('both','real','dummy','none'),
                        number=FALSE,cex=1,nex=1,cmpnt_col=NULL,cmpnt_lty=NULL,
                        pch=NULL,xlim=NULL,ylim=NULL,axes=FALSE,
                        xlab=if(axes) 'x' else '',ylab=if(axes) 'y' else'',
                        showrect=FALSE,asp=1,...)
{
#
# Function plot.deldir to produce a plot of the Delaunay triangulation
# and Dirichlet tesselation of a point set, as produced by the
# function deldir().
#

# Check that x is of the appropriate class.
if(!inherits(x, "deldir")) 
        stop("Argument \"x\" is not of class deldir.\n")

wlines    <- match.arg(wlines)
wpoints   <- match.arg(wpoints)
cmpnt_col <- fixColours(cmpnt_col)
cmpnt_lty <- fixLines(cmpnt_lty)
pch       <- if(is.null(pch)) 1:2 else rep(pch,length.out=2)

plot.del <- switch(wlines,both=TRUE,triang=TRUE,tess=FALSE)
plot.dir <- switch(wlines,both=TRUE,triang=FALSE,tess=TRUE)
plot.rl  <- switch(wpoints,both=TRUE,real=TRUE,dummy=FALSE,none=FALSE)
plot.dum <- switch(wpoints,both=TRUE,real=FALSE,dummy=TRUE,none=FALSE)

delsgs <- x$delsgs
dirsgs <- x$dirsgs
n      <- x$n.data
rw     <- x$rw

if(plot.del) {
	x1<-delsgs[,1]
	y1<-delsgs[,2]
	x2<-delsgs[,3]
	y2<-delsgs[,4]
} else {
    x1 <- y1 <- x2 <- y2 <- numeric(0)
}

if(plot.dir) {
	u1<-dirsgs[,1]
	v1<-dirsgs[,2]
	u2<-dirsgs[,3]
	v2<-dirsgs[,4]
} else {
    u1 <- v1 <- u2 <- v2 <- numeric(0)
}

X<-x$summary[,"x"]
Y<-x$summary[,"y"]

if(!add) {
	if(is.null(xlim)) xlim <- rw[1:2]
	if(is.null(ylim)) ylim <- rw[3:4]
        x.all <- c(x1,x2,u1,u2,X)
        y.all <- c(y1,y2,v1,v2,Y)
        pty <- list(...)$pty
        if(!is.null(pty)) {
            OP <- par(pty=pty)
            on.exit(par(OP))
        }
	plot(x.all,y.all,type='n',xlim=xlim,ylim=ylim,
     		xlab=xlab,ylab=ylab,axes=axes,asp=asp)
}

if(plot.del) {
    dotargs <- list(...)
    if(is.null(dotargs$col)) dotargs$col <- cmpnt_col[1]
    if(is.null(dotargs$lty)) dotargs$lty <- cmpnt_lty[1]
    arhgs <- c(list(x1,y1,x2,y2),dotargs)
    do.call(segments,arhgs)
}
if(plot.dir) {
    dotargs <- list(...)
    if(is.null(dotargs$col)) dotargs$col <- cmpnt_col[2]
    if(is.null(dotargs$lty)) dotargs$lty <- cmpnt_lty[2]
    arhgs <- c(list(u1,v1,u2,v2),dotargs)
    do.call(segments,arhgs)
}
if(plot.rl) {
	x.real <- X[1:n]
	y.real <- Y[1:n]
	points(x.real,y.real,pch=pch[1],col=cmpnt_col[3],cex=cex,...)
}
if(plot.dum) {
	x.dumm <- X[-(1:n)]
	y.dumm <- Y[-(1:n)]
	points(x.dumm,y.dumm,pch=pch[2],col=cmpnt_col[4],cex=cex,...)
}
if(number) {
	xoff <- 0.02*diff(range(X))
	yoff <- 0.02*diff(range(Y))
	text(X+xoff,Y+yoff,1:length(X),cex=nex,col=cmpnt_col[5],...)
}
if(showrect) do.call(rect,c(as.list(x$rw)[c(1,3,2,4)],list(border=cmpnt_col[6])))
invisible()
}
})
