      subroutine adjchk(i,j,adj,nadj,madj,ntot,nerror)
      dimension nadj(-3:ntot,0:madj)
      logical adj
      nerror = -1
      adj = .false.
      ni = nadj(i,0)
      if(.not.(ni.gt.0))goto 23000
      do 23002 k = 1,ni 
      if(.not.(j.eq.nadj(i,k)))goto 23004
      adj = .true.
      goto 23003
23004 continue
23002 continue
23003 continue
23000 continue
      nj = nadj(j,0)
      if(.not.(nj.gt.0))goto 23006
      do 23008 k = 1,nj 
      if(.not.(i.eq.nadj(j,k)))goto 23010
      if(.not.(adj))goto 23012
      return
23012 continue
      nerror = 1
      return
23013 continue
23010 continue
23008 continue
23006 continue
      if(.not.(adj))goto 23014
      nerror = 1
      return
23014 continue
      return
      end
