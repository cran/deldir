      subroutine qtest1(h,i,j,k,x,y,ntot,eps,shdswp,nerror)
      implicit double precision(a-h,o-z)
      dimension x(-3:ntot), y(-3:ntot)
      integer h
      logical shdswp
      call acchk(i,j,k,shdswp,x,y,ntot,eps)
      if(.not.(.not.shdswp))goto 23000
      return
23000 continue
      xh = x(h)
      yh = y(h)
      xj = x(j)
      yj = y(j)
      call circen(h,i,k,x0,y0,x,y,ntot,eps,shdswp,nerror)
      if(.not.(nerror.gt.0))goto 23002
      return
23002 continue
      if(.not.(shdswp))goto 23004
      return
23004 continue
      a = x0-xh
      b = y0-yh
      r2 = a*a+b*b
      a = x0-xj
      b = y0-yj
      ch = a*a+b*b
      if(.not.(ch.lt.r2))goto 23006
      shdswp = .true.
      goto 23007
23006 continue
      shdswp = .false.
23007 continue
      return
      end
