C Output from Public domain Ratfor, version 1.03
      subroutine pred(kpr,i,j,nadj,madj,ntot)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj)
      dimension ndi(1)
      ndi(1) = 0
      n = nadj(i,0)
      if(n.eq.0)then
      call intpr("Adjacency list of i is empty, and so cannot contain j.
     *",-1,ndi,0)
      call rexit("Bailing out of pred.")
      endif
      do23002 k = 1,n 
      if(j.eq.nadj(i,k))then
      km = k-1
      if(km.lt.1)then
      km = n
      endif
      kpr = nadj(i,km)
      return
      endif
23002 continue
23003 continue
      call intpr("Adjacency list of i does not contain j.",-1,ndi,0)
      call rexit("Bailing out of pred.")
      end
