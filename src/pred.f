      subroutine pred(kpr,i,j,nadj,madj,ntot,nerror)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj)
      nerror = -1
      n = nadj(i,0)
      if(.not.(n.eq.0))goto 23000
      nerror = 5
      return
23000 continue
      do 23002 k = 1,n 
      if(.not.(j.eq.nadj(i,k)))goto 23004
      km = k-1
      if(.not.(km.lt.1))goto 23006
      km = n
23006 continue
      kpr = nadj(i,km)
      return
23004 continue
23002 continue
      nerror = 6
      return
      end
