get.cnrpt <- function(j,xxx,cntr) {
#
j  <- j+xxx$joff
n  <- length(xxx$x)
jnext <- if(j<n) j+1 else 1
x <- c(xxx$x[j],xxx$x[jnext])
y <- c(xxx$y[j],xxx$y[jnext])

# Get the pair of points which is in use into anti-clockwise order.
theta <- atan2(y-cntr[2],x-cntr[1])
theta <- ifelse(theta>0,theta,theta+2*pi)
x     <- x[order(theta)]
y     <- y[order(theta)]

# Go round the rectangle from SW --> SE --> NE --> NW
# SW:
if (x[1] < x[2] & y[1] > y[2])      xy <- c(x[1], y[2])
# SE:
else if (x[1] < x[2] & y[1] < y[2]) xy <- c(x[2], y[1])
# NE:
else if (x[1] > x[2] & y[1] < y[2]) xy <- c(x[1], y[2])
# NW:
else if (x[1] > x[2] & y[1] > y[2]) xy <- c(x[2], y[1])
# Not a corner:
else return(xxx)

if(n==2) {
	xxx$x <- x
	xxx$y <- y
}

xxx$x    <- append(xxx$x, xy[1], after = j)
xxx$y    <- append(xxx$y, xy[2], after = j)
xxx$bp   <- append(xxx$bp, TRUE, after = j)
xxx$joff <- xxx$joff+1
xxx
}
