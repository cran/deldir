C Output from Public domain Ratfor, version 1.03
      subroutine swap(j,k1,k2,shdswp,nadj,madj,x,y,ntot,eps,incadj)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      logical shdswp
      call adjchk(k1,k2,shdswp,nadj,madj,ntot)
      if(.not.shdswp)then
      return
      endif
      call pred(k,k1,k2,nadj,madj,ntot)
      call succ(kk,k2,k1,nadj,madj,ntot)
      if(kk.ne.k)then
      shdswp = .false.
      return
      endif
      call qtest(j,k1,k,k2,shdswp,x,y,ntot,eps)
      if(shdswp)then
      call delet(k1,k2,nadj,madj,ntot)
      call insrt(j,k,nadj,madj,x,y,ntot,eps,incadj)
      if(incadj.eq.1)then
      return
      endif
      endif
      return
      end
