\name{divchain}
\alias{divchain}
\title{
   Dividing chain; generic.
}
\description{
   Generic function for creating the \dQuote{dividing chain}
   of a Dirchlet tesselation.  The tessellation must have been
   created from a set of points having associated categorical
   \dQuote{weights}.  The dividing chain consists of those edges
   of Dirichlet tiles which separate points having different values
   of the given weights.
}

\usage{
   divchain(x, ...)
}
\arguments{
  \item{x}{
     Either an object specifying coordinates or an object of
     class \dQuote{deldir}.
}
  \item{\dots}{
      Arguements to be passed to the appropriate method for
      this generic function.
}
}
\details{
     If \code{x} is a (numeric) vector it will be taken to be
     the \dQuote{x} coordinates of the points being tessellated.
     In this case the \dots\ arguments must contain a vector
     \code{y} specifying the \dQuote{y} coordinates.
     The \dots\ arguments must also contain a \emph{factor}
     \code{z} specifying the relevant \dQuote{weights} argument.
     The argument \code{x} may also be a data frame or list
     from which the coordinates and the weights will be extracted.
     See \code{\link{divchain.default}()} for details.

     If \code{x} is an object of class class \dQuote{deldir}
     then it must have been created with an appropriate (factor)
     \dQuote{weights} argument, otherwise an error is given.

}
\value{
     An object of class \dQuote{divchain}.  See \code{\link{divchain.deldir}()}
     for details.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
% \url{https://www.stat.auckland.ac.nz/~rolf}
}
\note{
   This function was created in response to a question asked
   on \code{stackoverflow.com} by a user named \dQuote{Dan}.
}

\seealso{
\code{\link{divchain.default}()} \code{\link{divchain.deldir}()}
\code{\link{deldir}()} \code{\link{plot.divchain}()}
}
\examples{
   set.seed(42)
   x   <- runif(50)
   y   <- runif(50)
   z   <- factor(kmeans(cbind(x,y),centers=4)$cluster)
   dc1 <- divchain(x,y,z,rw=c(0,1,0,1))
   dxy <- deldir(x,y,z=z,rw=c(0,1,0,1))
   dc2 <- divchain(dxy)
}
\keyword{ spatial }
