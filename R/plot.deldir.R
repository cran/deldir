plot.deldir <- function(x,add=F,wlines=c('both','triang','tess'),
                        wpoints=c('both','real','dummy','none'),
                        number=F,cex=1,nex=1,col=NULL,lty=NULL,
                        pch=NULL,xlim=NULL,ylim=NULL,...)
{
#
# Function plot.deldir to produce a plot of the Delaunay triangulation
# and Dirichlet tesselation of a point set, as produced by the
# function deldir().
#

wlines  <- match.arg(wlines)
wpoints <- match.arg(wpoints)

if(is.null(class(x)) || class(x)!='deldir') {
	cat('Argument is not of class deldir.\n')
	return(invisible())
}

col <- if(is.null(col)) c(1,1,1,1,1) else rep(col,length.out=5)
lty <- if(is.null(lty)) 1:2 else rep(lty,length.out=2)
pch <- if(is.null(pch)) 1:2 else rep(pch,length.out=2)

plot.del <- switch(wlines,both=T,triang=T,tess=F)
plot.dir <- switch(wlines,both=T,triang=F,tess=T)
plot.rl  <- switch(wpoints,both=T,real=T,dummy=F,none=F)
plot.dum <- switch(wpoints,both=T,real=F,dummy=T,none=F)

delsgs <- x$delsgs
dirsgs <- x$dirsgs
n      <- x$n.data
rw     <- x$rw

if(plot.del) {
	x1<-delsgs[,1]
	y1<-delsgs[,2]
	x2<-delsgs[,3]
	y2<-delsgs[,4]
}

if(plot.dir) {
	u1<-dirsgs[,1]
	v1<-dirsgs[,2]
	u2<-dirsgs[,3]
	v2<-dirsgs[,4]
}

X<-x$summary[,1]
Y<-x$summary[,2]

if(!add) {
	pty.save <- par()$pty
	on.exit(par(pty=pty.save))
	par(pty='s')
	if(is.null(xlim)) xlim <- rw[1:2]
	if(is.null(ylim)) ylim <- rw[3:4]
	plot(0,0,type='n',xlim=xlim,ylim=ylim,
     		xlab='x',ylab='y',axes=F,...)
	axis(side=1)
	axis(side=2)
}

if(plot.del) segments(x1,y1,x2,y2,col=col[1],lty=lty[1],...)
if(plot.dir) segments(u1,v1,u2,v2,col=col[2],lty=lty[2],...)
if(plot.rl) {
	x.real <- X[1:n]
	y.real <- Y[1:n]
	points(x.real,y.real,pch=pch[1],col=col[3],cex=cex,...)
}
if(plot.dum) {
	x.dumm <- X[-(1:n)]
	y.dumm <- Y[-(1:n)]
	points(x.dumm,y.dumm,pch=pch[2],col=col[4],cex=cex,...)
}
if(number) {
	xoff <-0.02*diff(range(X))
	yoff <-0.02*diff(range(Y))
	text(X+xoff,Y+yoff,1:length(X),cex=nex,col=col[5],...)
}
invisible()
}
