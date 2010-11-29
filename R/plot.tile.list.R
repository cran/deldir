plot.tile.list <- function (x, verbose = FALSE, close = FALSE, pch = 1,
                            polycol = NA, showpoints = TRUE, showrect=FALSE,
                            add=FALSE, asp = 1, xlab = "x", ylab = "y", main = "", ...) 
{
    object <- x
    if (!inherits(object, "tile.list")) 
        stop("Argument \"object\" is not of class tile.list.\n")
    n <- length(object)
    if(showrect) {
	rw <- attr(object,"rw")
        rx <- rw[1:2]
        ry <- rw[3:4]
    } else {
        x.all <- unlist(lapply(object, function(w) {
            c(w$pt[1], w$x)
        }))
        y.all <- unlist(lapply(object, function(w) {
            c(w$pt[2], w$y)
        }))
        rx <- range(x.all)
        ry <- range(y.all)
    }
    x.pts <- unlist(lapply(object, function(w) {
        w$pt[1]
    }))
    y.pts <- unlist(lapply(object, function(w) {
        w$pt[2]
    }))
    if(!add) plot(0, 0, type = "n", asp = asp, xlim=rx, ylim=ry, xlab = xlab,
                  ylab = ylab, main = main)
    polycol <- apply(col2rgb(polycol,TRUE),2,
                     function(x){do.call(rgb,as.list(x/255))})
    polycol <- rep(polycol, length = length(object))
    hexbla  <- do.call(rgb,as.list(col2rgb("black",TRUE)/255))
    hexwhi  <- do.call(rgb,as.list(col2rgb("white",TRUE)/255))
    ptcol <- ifelse(polycol == hexbla,hexwhi,hexbla)
    lnwid <- ifelse(polycol == hexbla, 2, 1)
    for (i in 1:n) {
        inner <- !any(object[[i]]$bp)
        if (close | inner) 
            polygon(object[[i]], col = polycol[i], border = ptcol[i], 
                lwd = lnwid[i])
        else {
            x <- object[[i]]$x
            y <- object[[i]]$y
            bp <- object[[i]]$bp
            ni <- length(x)
            for (j in 1:ni) {
                jnext <- if (j < ni) 
                  j + 1
                else 1
                do.it <- mid.in(x[c(j, jnext)], y[c(j, jnext)], 
                  rx, ry)
                if (do.it) 
                  segments(x[j], y[j], x[jnext], y[jnext], col = ptcol[i], 
                    lwd = lnwid[i])
            }
        }
        if (verbose & showpoints) 
            points(object[[i]]$pt[1], object[[i]]$pt[2], pch = pch, 
                col = ptcol[i])
        if (verbose & i < n) 
            readline("Go? ")
    }
    if (showpoints & !verbose) 
        points(x.pts, y.pts, pch = pch, col = ptcol)
    if(showrect) {
	do.call(rect,as.list(rw)[c(1,3,2,4)])
    }
    invisible()
}
