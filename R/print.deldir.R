print.deldir <- function(x,digits=NULL,...) {
    cat("\n")
    cat("Delaunay triangulation and Dirchlet tessellation\n")
    kount <- if(x$n.dum) {
        paste0("of ",x$n.data," data points and ",x$n.dum," dummy points.\n")
    } else {
        paste0("of ",x$n.data," data points.\n")
    }
    cat(kount)
    cat("\n")
    if(is.null(digits)) {
        rw <- x$rw
        dirA <- x$dir.area
        delA <- x$del.area
    } else {
        rw <- round(x$rw,digits)
        dirA <- round(x$dir.area,digits)
        delA <- round(x$del.area,digits)
    }
    RW <- paste0("[",rw[1],",",rw[2],"]"," x ",
                 "[",rw[3],",",rw[4],"]")
    cat("Enclosing rectangular window:\n")
    cat(RW,"\n")
    cat("\n")
    cat("Area of rectangular window (total area of\n")
    cat("Dirichlet tiles):\n")
    cat(dirA,"\n")
    cat("\n")
    cat("Area of convex hull of points (total area of\n")
    cat("Delaunay triangles):\n")
    cat(delA,"\n")
    cat("\n")
    invisible()
}
