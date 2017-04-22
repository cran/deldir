tileInfo <- function(object,bndry=FALSE) {
#
# Function to provide a summary of information about the tiles
# of a Dirichlet (Voronoi) tessellation.
#
# First check that we really are looking at a tessellation/triangulation.
    if (!inherits(object, "deldir")) 
        stop("Argument \"object\" is not of class \"deldir\".\n")
# List the tiles in the tessellation.
    tl <- tile.list(object)

# Required info:
# * for each tile, the number of edges
# * for each tile, a vector of the lengths of the edges
# * a tabulation of the numbers of edges of tiles
# * a vector of all lengths of edges (with repetitions)
# * a vector of lengths of _unique_ edges
# * the area of each tile
# * adjust plot.tile.list() so that the number of the point/tile
#   is plotted.

    foo <- function(tile){
        x  <- tile$x
        y  <- tile$y
        x1 <- c(x,x[1])
        y1 <- c(y,y[1])
        ledge <- sqrt(((x1[-1] - x))^2 + ((y1[-1] - y))^2)
        nedge <- length(ledge)
        list(edgeLengths=ledge,numEdges=nedge,area=tile$area,ptNum=tile$ptNum)
    }
    
    ok <- if(bndry) rep(TRUE,length(tl)) else sapply(tl,function(x){!any(x$bp)})
    xxx <- lapply(tl[ok],foo)
    ptNums <- sapply(xxx,function(x){x$ptNum})
    nms <- paste("tile",ptNums,sep=".")
    names(xxx) <- nms

# Extract and tabulate the edge counts.
    allnedge <- sapply(xxx,function(x){x$numEdges})
    tabnedge <- table(allnedge)

# Extract and combine the edge lengths.  Note that there
# will be duplication since many edges are edges of *two* tiles.
    all.lengths <- unlist(lapply(xxx,function(x){x$edgeLengths}))

# Extract the tile areas into a single vector.
    areas <- sapply(xxx,function(x){x$area})

# Now go back to the deldir object to get the unique edge lengths.

    d   <- object$dirsgs
    ok  <- if(bndry) rep(TRUE,nrow(d)) else !(d$bp1 | d$bp2)
    ue  <- with(d,sqrt((x1-x2)^2 + (y1-y2)^2))

# Pack up and go home.
    rslt <- list(indivTiles=xxx,allEdgeCounts=allnedge,tabEdgeCounts=tabnedge,
                 allEdgeLengths=all.lengths,Areas=areas,uniqueEdgeLengths=ue)
    class(rslt) <- "tileInfo"
    rslt
}
