C Output from Public domain Ratfor, version 1.03
      subroutine insrt1(i,j,kj,nadj,madj,ntot,incadj)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj)
      incadj = 0
      n = nadj(i,0)
      if(n.eq.0)then
      nadj(i,0) = 1
      nadj(i,1) = j
      return
      endif
      kk = n+1
      if(kk.gt.madj)then
      incadj = 1
      return
      endif
23004 if(kk.gt.kj)then
      nadj(i,kk) = nadj(i,kk-1)
      kk = kk-1
      goto 23004
      endif
23005 continue
      nadj(i,kj) = j
      nadj(i,0) = n+1
      return
      end
