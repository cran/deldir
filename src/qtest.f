      subroutine qtest(h,i,j,k,shdswp,x,y,ntot,eps,nerror)
      implicit double precision(a-h,o-z)
      dimension x(-3:ntot), y(-3:ntot)
      integer h
      logical shdswp
      nerror = -1
      if(.not.(i.le.0))goto 23000
      ii = 1
      goto 23001
23000 continue
      ii = 0
23001 continue
      if(.not.(j.le.0))goto 23002
      jj = 1
      goto 23003
23002 continue
      jj = 0
23003 continue
      if(.not.(k.le.0))goto 23004
      kk = 1
      goto 23005
23004 continue
      kk = 0
23005 continue
      ijk = ii*4+jj*2+kk
      I23006 = (ijk)
      if(.not.(I23006.eq.( 7)))goto 23007
      shdswp = .true.
      return
23007 continue
      if(.not.(I23006.eq.( 6)))goto 23008
      ss = 1 - 2*mod(-j,2)
      xh = x(h)
      yh = y(h)
      xk = x(k)
      yk = y(k)
      test =(xh*yk+xk*yh-xh*yh-xk*yk)*ss
      if(.not.(test.gt.0))goto 23009
      shdswp = .true.
      goto 23010
23009 continue
      shdswp = .false.
23010 continue
      if(.not.(shdswp))goto 23011
      call acchk(j,k,h,shdswp,x,y,ntot,eps)
23011 continue
      return
23008 continue
      if(.not.(I23006.eq.( 5)))goto 23013
      shdswp = .true.
      return
23013 continue
      if(.not.(I23006.eq.( 4)))goto 23014
      call acchk(j,k,h,shdswp,x,y,ntot,eps)
      return
23014 continue
      if(.not.(I23006.eq.( 3)))goto 23015
      ss = 1 - 2*mod(-j,2)
      xi = x(i)
      yi = y(i)
      xh = x(h)
      yh = y(h)
      test = (xh*yi+xi*yh-xh*yh-xi*yi)*ss
      if(.not.(test.gt.0.))goto 23016
      shdswp = .true.
      goto 23017
23016 continue
      shdswp = .false.
23017 continue
      if(.not.(shdswp))goto 23018
      call acchk(h,i,j,shdswp,x,y,ntot,eps)
23018 continue
      return
23015 continue
      if(.not.(I23006.eq.( 2)))goto 23020
      shdswp = .false.
      return
23020 continue
      if(.not.(I23006.eq.( 1)))goto 23021
      call acchk(h,i,j,shdswp,x,y,ntot,eps)
      return
23021 continue
      if(.not.(I23006.eq.( 0)))goto 23022
      call qtest1(h,i,j,k,x,y,ntot,eps,shdswp,nerror)
      return
23022 continue
      nerror = 7
      return
23006 continue
      end
