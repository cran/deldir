      subroutine insrt(i,j,nadj,madj,x,y,ntot,nerror,eps)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      logical adj
      call adjchk(i,j,adj,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23000
      return
23000 continue
      if(.not.(adj))goto 23002
      return
23002 continue
      call locn(i,j,kj,nadj,madj,x,y,ntot,eps)
      call locn(j,i,ki,nadj,madj,x,y,ntot,eps)
      call insrt1(i,j,kj,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt.0))goto 23004
      return
23004 continue
      call insrt1(j,i,ki,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt.0))goto 23006
      return
23006 continue
      return
      end
