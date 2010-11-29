triang.list <- function (object) 
{
    stopifnot(inherits(object,"deldir"))
    a <- object$delsgs[, 5]
    b <- object$delsgs[, 6]
    tlist <- matrix(integer(0), 0, 3)
    for(i in seq(nrow(object$summary))) {
      # find all Delaunay neighbours of i
      jj <- c(b[a==i], a[b==i])
      jj <- sort(unique(jj))
      # select those with a higher index than i
      jj <- jj[jj > i]
      # find pairs of neighbours which are Delaunay neighbours
      # (thus, triangles where the first numbered vertex is i)
      if(length(jj) > 0)
        for(j in jj) {
          kk <- c(b[a == j], a[b == j])
          kk <- kk[(kk %in% jj) & (kk > j)]
          if(length(kk) > 0)
            for(k in kk)
              # add (i,j,k) to list of triangles (i < j < k)
              tlist <- rbind(tlist, c(i, j, k))
        }
    }
    x <- object$summary[,"x"]
    y <- object$summary[,"y"]
    xtri <- matrix(x[tlist], nrow(tlist), 3)
    ytri <- matrix(y[tlist], nrow(tlist), 3)
    ztri <- ytri - min(y)
    dx <- cbind(xtri[, 2] - xtri[, 1], xtri[, 3] - xtri[, 2], 
        xtri[, 1] - xtri[, 3])
    zm <- cbind(ztri[, 1] + ztri[, 2], ztri[, 2] + ztri[, 3], 
        ztri[, 3] + ztri[, 1])
    negareas <- apply(dx * zm, 1, sum)
    clockwise <- (negareas > 0)
    if (any(clockwise)) {
        xc <- xtri[clockwise, ]
        yc <- ytri[clockwise, ]
        xtri[clockwise, ] <- xc[, c(1, 3, 2)]
        ytri[clockwise, ] <- yc[, c(1, 3, 2)]
    }
    rslt <- list()
    for(i in 1:nrow(xtri)) {
	rslt[[i]] <- data.frame(x=xtri[i,],y=ytri[i,])
    }
    attr(rslt,"rw") <- object$rw
    class(rslt) <- "triang.list"
    rslt
}
