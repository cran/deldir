      subroutine insrt1(i,j,kj,nadj,madj,ntot,nerror)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj)
      nerror = -1
      n = nadj(i,0)
      if(.not.(n.eq.0))goto 23000
      nadj(i,0) = 1
      nadj(i,1) = j
      return
23000 continue
      kk = n+1
      if(.not.(kk.gt.madj))goto 23002
      nerror = 4
      return
23002 continue
23004 if(.not.(kk.gt.kj))goto 23005
      nadj(i,kk) = nadj(i,kk-1)
      kk = kk-1
      goto 23004
23005 continue
      nadj(i,kj) = j
      nadj(i,0) = n+1
      return
      end
