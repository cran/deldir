tile.list <- function (object) 
{
    if (!inherits(object, "deldir")) 
        stop("Argument \"object\" is not of class deldir.\n")
    rw <- object$rw
    x.crnrs <- rw[c(1,2,2,1)]
    y.crnrs <- rw[c(3,3,4,4)]
    ddd <- object$dirsgs
    sss <- object$summary
    npts <- nrow(sss)
    x <- sss[,"x"]
    y <- sss[,"y"]
    i.crnr <- get.cnrind(x,y,rw)
    rslt <- list()
    for (i in 1:npts) {
        m <- as.matrix(rbind(ddd[ddd$ind1 == i, 1:4], ddd[ddd$ind2 == i, 1:4]))
        bp1 <- c(ddd[ddd$ind1 == i, 7], ddd[ddd$ind2 == i, 7])
        bp2 <- c(ddd[ddd$ind1 == i, 8], ddd[ddd$ind2 == i, 8])
        m1 <- cbind(m[, 1:2, drop = FALSE], 0 + bp1)
        m2 <- cbind(m[, 3:4, drop = FALSE], 0 + bp2)
        m <- rbind(m1, m2)
        pt <- sss[i, 1:2]
        theta <- atan2(m[, 2] - pt[2], m[, 1] - pt[1])
        theta <- ifelse(theta > 0, theta, theta + 2 * pi)
        theta.0 <- sort(unique(theta))
        mm <- m[match(theta.0, theta), ]
        xx <- mm[, 1]
        yy <- mm[, 2]
        bp <- as.logical(mm[, 3])
# Add corner points if necessary:
        ii <- i.crnr%in%i
	xx <- c(xx,x.crnrs[ii])
	yy <- c(yy,y.crnrs[ii])
        bp <- c(bp,rep(TRUE,sum(ii)))
        rslt[[i]] <- acw(list(pt=pt, x = unname(xx), y = unname(yy), bp = bp))
    }
    class(rslt) <- "tile.list"
    attr(rslt,"rw") <- object$rw
    rslt
}
