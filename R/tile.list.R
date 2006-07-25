tile.list <- function(object)
{
if(!inherits(object,"deldir"))
        stop("Argument \"object\" is not of class deldir.\n")
rw  <- object$rw
c0  <- 0.5*(rw[c(1,3)] + rw[c(2,4)])
ddd <- object$dirsgs
sss <- object$summary
npts <- nrow(sss)
rslt <- list()
for(i in 1:npts) {
        m  <- as.matrix(rbind(ddd[ddd$ind1==i,1:4],ddd[ddd$ind2==i,1:4]))
        bp1 <- c(ddd[ddd$ind1==i,7],ddd[ddd$ind2==i,7])
        bp2 <- c(ddd[ddd$ind1==i,8],ddd[ddd$ind2==i,8])
	m1 <- cbind(m[,1:2,drop=FALSE],0+bp1)
	m2 <- cbind(m[,3:4,drop=FALSE],0+bp2)
        m  <- rbind(m1,m2)
        pt <- sss[i,1:2]
        theta <- atan2(m[,2]-pt[2],m[,1]-pt[1])
	theta <- ifelse(theta>0,theta,theta+2*pi)
        theta.0 <- sort(unique(theta))
        m <- m[match(theta.0,theta),]
	bp <- as.logical(m[,3])
        x  <- m[,1]
        y  <- m[,2]
        xynew <- list(x=x,y=y,bp=bp,joff=0) 
        ni    <- length(bp)
	if(ni==2) xynew <- get.cnrpt(1,xynew,c0)
        else for(j in 1:ni) {
		jprev <- if(j > 1) j-1 else ni
		jnext <- if (j < ni) j + 1 else 1
		if (!bp[jprev] & bp[j] & bp[jnext]) {
			xynew <- get.cnrpt(j, xynew,c0)
                }
        }
        rslt[[i]] <- list(pt = pt, x = xynew$x, y = xynew$y,
                          bp = xynew$bp)
}
class(rslt) <- "tile.list"
rslt
}
