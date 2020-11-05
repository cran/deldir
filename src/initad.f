C Output from Public domain Ratfor, version 1.03
      subroutine initad(j,nadj,madj,x,y,ntot,eps,ntri,incadj)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      integer tau(3)
      call trifnd(j,tau,nedge,nadj,madj,x,y,ntot,eps,ntri)
      if(nedge.ne.0)then
      ip = nedge
      i = ip-1
      if(i.eq.0)then
      i = 3
      endif
      call pred(k,tau(i),tau(ip),nadj,madj,ntot)
      call succ(kk,tau(ip),tau(i),nadj,madj,ntot)
      call delet(tau(i),tau(ip),nadj,madj,ntot)
      if(k.eq.kk)then
      call insrt(j,k,nadj,madj,x,y,ntot,eps,incadj)
      endif
      endif
      do23006 i = 1,3 
      call insrt(j,tau(i),nadj,madj,x,y,ntot,eps,incadj)
23006 continue
23007 continue
      return
      end
