      subroutine delet1(i,j,nadj,madj,ntot)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj)
      n = nadj(i,0)
      do 23000 k = 1,n 
      if(.not.(nadj(i,k).eq.j))goto 23002
      do 23004 kk = k,n-1 
      nadj(i,kk) = nadj(i,kk+1) 
23004 continue
      nadj(i,n) = 0
      nadj(i,0) = n-1
      return
23002 continue
23000 continue
      end
