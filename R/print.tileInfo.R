print.tileInfo <- function(x,digits=4,...) {
#
# Matrix of edge lengths:
lel <- lapply(x$indivTiles,function(u){u$edgeLengths})
nel <- sapply(lel,length)
pNs <- sapply(x$indivTiles,function(u){u$ptNum})
m   <- length(nel)
n   <- max(nel)
M   <- matrix("",nrow=m,ncol=n)
ld  <- ceiling(log10(max(unlist(lel)))) + digits + 1
fmt <- paste0("%",ld,".",digits,"f")
for(i in 1:m) {
    M[i,1:nel[i]] <- sprintf(fmt,lel[[i]])
}
rownames(M) <- paste0("point ",pNs,": ")
colnames(M) <- rep("",n)
cat("\nEdge lengths:\n")
cat("=============\n")
print(M,quote=FALSE)

# Table of edgecounts:
tec <- x$tabEdgeCounts
names(attr(tec,"dimnames")) <- ""
mode(tec) <- "character"
tec <- c("  ",tec)
cat("\nTable of edge counts:\n")
cat("=====================\n\n")
print(tec,quote=FALSE)

# Areas:
cat("\nTile areas:\n")
cat("===========\n\n")
print(round(x$Areas,digits=digits))
cat("\n")

# Perimeters:
cat("\nTile perimeters:\n")
cat("================\n\n")
print(round(x$perimeters$perimeters,digits=digits))
cat("\n")

invisible()
}
