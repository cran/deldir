      subroutine delet(i,j,nadj,madj,ntot,nerror)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj)
      logical adj
      call adjchk(i,j,adj,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23000
      return
23000 continue
      if(.not.(adj))goto 23002
      call delet1(i,j,nadj,madj,ntot)
      call delet1(j,i,nadj,madj,ntot)
23002 continue
      return
      end
