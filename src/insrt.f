C Output from Public domain Ratfor, version 1.03
      subroutine insrt(i,j,nadj,madj,x,y,ntot,eps,incadj)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      logical adj
      call adjchk(i,j,adj,nadj,madj,ntot)
      if(adj)then
      return
      endif
      call locn(i,j,kj,nadj,madj,x,y,ntot,eps)
      call locn(j,i,ki,nadj,madj,x,y,ntot,eps)
      call insrt1(i,j,kj,nadj,madj,ntot,incadj)
      if(incadj.eq.1)then
      return
      endif
      call insrt1(j,i,ki,nadj,madj,ntot,incadj)
      if(incadj.eq.1)then
      return
      endif
      return
      end
