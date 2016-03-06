\name{divchain.default}
\alias{divchain.default}
\title{
   Dividing chain; default method.
}
\description{
   Create the \dQuote{dividing chain} of the Dirchlet tesselation
   of a given set of points having distinguishing (categorical)
   \dQuote{weights}.  This dividing chain consists of those edges
   of Dirichlet tiles which separate points having different values
   of the given weights.
}

\usage{
  \method{divchain}{default}(x, y, z, ...)
}

\arguments{
\item{x,y}{
   These provide the coordinates of the set of points being
   tesselated.  Argument \code{x} may be a data frame or a
   list, in particular one of class \code{ppp}.  (See the
   \code{spatstat} package.)
   For a full description see the discussion of these arguments in the
   help for \code{\link{deldir}()}.
}
\item{z}{
   A \emph{factor} specifying \dQuote{auxiliary} values or
   \dQuote{weights} If this argument is left \code{NULL} then it
   is extracted, if possible, from the components of \code{x}.
   See \code{\link{deldir}()} for further details.
}
   
  \item{\dots}{
      Other arguments to be passed to \code{deldir}.
}
}
\value{
   An object of class \code{divchain}.  See
   \code{\link{divchain.deldir}()} for details.
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
\code{\link{divchain.deldir}()} \code{\link{deldir}()}
\code{\link{plot.divchain}()}
}
\examples{
   set.seed(42)
   x    <- runif(50)
   y    <- runif(50)
   z    <- factor(kmeans(cbind(x,y),centers=4)$cluster)
   dcxy <- divchain(x,y,z,rw=c(0,1,0,1))
}

\keyword{ spatial }
