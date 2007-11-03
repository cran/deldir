`plot.tile.list` <-
function (x, verbose = FALSE, close = FALSE, pch = 1, polycol = NA, 
    showpoints = TRUE, asp = 1, ...) 
{
    object <- x
    if (!inherits(object, "tile.list")) 
        stop("Argument \"object\" is not of class tile.list.\n")
    n <- length(object)
    x.all <- unlist(lapply(object, function(w) {
        c(w$pt[1], w$x)
    }))
    y.all <- unlist(lapply(object, function(w) {
        c(w$pt[2], w$y)
    }))
    x.pts <- unlist(lapply(object, function(w) {
        w$pt[1]
    }))
    y.pts <- unlist(lapply(object, function(w) {
        w$pt[2]
    }))
    rx <- range(x.all)
    ry <- range(y.all)
    plot(x.all, y.all, type = "n", asp = asp, xlab = "x", ylab = "y")
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
    if (showpoints) 
        points(x.pts, y.pts, pch = pch, col = ptcol)
    invisible()
}
