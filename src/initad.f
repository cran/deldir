      subroutine initad(j,nadj,madj,x,y,ntot,eps,nerror)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      integer tau(3)
      call trifnd(j,tau,nedge,nadj,madj,x,y,ntot,eps,nerror)
      if(.not.(nerror .gt. 0))goto 23000
      return
23000 continue
      if(.not.(nedge.ne.0))goto 23002
      ip = nedge
      i = ip-1
      if(.not.(i.eq.0))goto 23004
      i = 3
23004 continue
      call pred(k,tau(i),tau(ip),nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23006
      return
23006 continue
      call succ(kk,tau(ip),tau(i),nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23008
      return
23008 continue
      call delet(tau(i),tau(ip),nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23010
      return
23010 continue
      if(.not.(k.eq.kk))goto 23012
      call insrt(j,k,nadj,madj,x,y,ntot,nerror,eps)
23012 continue
      if(.not.(nerror .gt. 0))goto 23014
      return
23014 continue
23002 continue
      do 23016 i = 1,3 
      call insrt(j,tau(i),nadj,madj,x,y,ntot,nerror,eps)
      if(.not.(nerror .gt. 0))goto 23018
      return
23018 continue
23016 continue
      return
      end
