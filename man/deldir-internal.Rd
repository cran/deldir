\name{deldir-internal}
\alias{[.tile.list}
\alias{[.triang.list}
\alias{acw}
\alias{binsrtR}
\alias{doClip}
\alias{dumpts}
\alias{findNewInOld}
\alias{get.cnrind}
\alias{getCol}
\alias{mid.in}
\alias{mnndR}
\alias{prelimtlist}
\alias{tilePerim0}
\alias{verGetter}
\title{Internal deldir functions}
\description{
  Internal deldir functions.
}
\usage{
\method{[}{tile.list}(x,i,\dots)
\method{[}{triang.list}(x,i,\dots)
acw(xxx)
dumpts(x,y,dpl,rw)
doClip(object,clipp,rw)
binsrtR(x,y,rw)
findNewInOld(xnew,xold,ynew,yold,tolerance=sqrt(.Machine$double.eps))
get.cnrind(x,y,rw)
getCol(x,warn=FALSE)
mid.in(x,y,rx,ry)
mnndR(x,y)
prelimtlist(object)
tilePerim0(object,inclbdry=TRUE)
verGetter()
}
\details{
  These functions are auxiliary and are not intended to be called by
  the user.
}
\keyword{internal}
