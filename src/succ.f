      subroutine succ(ksc,i,j,nadj,madj,ntot,nerror)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj)
      nerror = -1
      n = nadj(i,0)
      if(.not.(n.eq.0))goto 23000
      nerror = 9
      return
23000 continue
      do 23002 k = 1,n 
      if(.not.(j.eq.nadj(i,k)))goto 23004
      kp = k+1
      if(.not.(kp.gt.n))goto 23006
      kp = 1
23006 continue
      ksc = nadj(i,kp)
      return
23004 continue
23002 continue
      nerror = 10
      return
      end
