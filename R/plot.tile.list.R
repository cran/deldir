plot.tile.list <- function(x,verbose=TRUE,...)
{
object <- x
if(!inherits(object,"tile.list"))
        stop("Argument \"object\" is not of class tile.list.\n")
n <- length(object)
x.all <- unlist(lapply(object,function(w){c(w$pt[1],w$x)}))
y.all <- unlist(lapply(object,function(w){c(w$pt[2],w$y)}))
x.pts <- unlist(lapply(object,function(w){w$pt[1]}))
y.pts <- unlist(lapply(object,function(w){w$pt[2]}))
plot(x.pts,y.pts,xlim=range(x.all),ylim=range(y.all),xlab='x',ylab='y',pch=21)
for(i in 1:n) {
	x  <- object[[i]]$x
	y  <- object[[i]]$y
	bp <- object[[i]]$bp
	ni <- length(x)
	nim1 <- ni - 1
	for(j in 1:nim1) {
		do.it <- (ni==2) | (!(bp[j] & bp[j+1]))
		if(do.it) segments(x[j],y[j],x[j+1],y[j+1])
	}
	do.it <- (!(bp[ni] & bp[1]))
	if(do.it) segments(x[ni],y[ni],x[1],y[1])
        if(verbose & i < n) readline('Go? ')
        points(object[[i]]$pt[1],object[[i]]$pt[2],pch=3)
}
invisible()
}

