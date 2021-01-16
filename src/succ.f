C Output from Public domain Ratfor, version 1.03
      subroutine succ(ksc,i,j,nadj,madj,ntot)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj)
      dimension ndi(1)
      ndi(1) = 0
      n = nadj(i,0)
      if(n.eq.0)then
      call intpr("Adjacency list of i is empty, and so cannot contain j.
     *",-1,ndi,0)
      call rexit("Bailing out of succ.")
      endif
      do23002 k = 1,n 
      if(j.eq.nadj(i,k))then
      kp = k+1
      if(kp.gt.n)then
      kp = 1
      endif
      ksc = nadj(i,kp)
      return
      endif
23002 continue
23003 continue
      ndi(1) = i
      call intpr("i =",-1,ndi,1)
      ndi(1) = j
      call intpr("j =",-1,ndi,1)
      call intpr("Adjacency list of i does not contain j.",-1,ndi,0)
      call rexit("Bailing out of succ.")
      end
