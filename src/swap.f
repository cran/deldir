      subroutine swap(j,k1,k2,shdswp,nadj,madj,x,y,ntot,eps,nerror)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      logical shdswp
      call adjchk(k1,k2,shdswp,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23000
      return
23000 continue
      if(.not.(.not.shdswp))goto 23002
      return
23002 continue
      call pred(k,k1,k2,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23004
      return
23004 continue
      call succ(kk,k2,k1,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23006
      return
23006 continue
      if(.not.(kk.ne.k))goto 23008
      shdswp = .false.
      return
23008 continue
      call qtest(j,k1,k,k2,shdswp,x,y,ntot,eps,nerror)
      if(.not.(nerror .gt. 0))goto 23010
      return
23010 continue
      if(.not.(shdswp))goto 23012
      call delet(k1,k2,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23014
      return
23014 continue
      call insrt(j,k,nadj,madj,x,y,ntot,nerror,eps)
      if(.not.(nerror .gt. 0))goto 23016
      return
23016 continue
23012 continue
      return
      end
