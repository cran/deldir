tile.list <- function(object)
{
if(!inherits(object,"deldir"))
        stop("Argument \"object\" is not of class deldir.\n")
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
        theta.0 <- sort(unique(theta))
	m.save <- m
        m <- m[match(theta.0,theta),]
        rslt[[i]] <- list(pt=sss[i,1:2],x=m[,1],y=m[,2],
                          bp=as.logical(m[,3]))
}
class(rslt) <- "tile.list"
rslt
}

