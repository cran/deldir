C Output from Public domain Ratfor, version 1.03
      subroutine adjchk(i,j,adj,nadj,madj,ntot)
      dimension nadj(-3:ntot,0:madj)
      logical adj
      dimension ndi(1)
      ndi(1) = 0
      adj = .false.
      ni = nadj(i,0)
      if(ni.gt.0)then
      do23002 k = 1,ni 
      if(j.eq.nadj(i,k))then
      adj = .true.
      goto 23003
      endif
23002 continue
23003 continue
      endif
      nj = nadj(j,0)
      if(nj.gt.0)then
      do23008 k = 1,nj 
      if(i.eq.nadj(j,k))then
      if(adj)then
      return
      else
      call intpr("Contradictory adjacency lists.",-1,ndi,0)
      call rexit("Bailing out of adjchk.")
      endif
      endif
23008 continue
23009 continue
      endif
      if(adj)then
      call intpr("Contradictory adjacency lists.",-1,ndi,0)
      call rexit("Bailing out of adjchk.")
      endif
      return
      end
