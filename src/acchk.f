      subroutine acchk(i,j,k,anticl,x,y,ntot,eps)
      implicit double precision(a-h,o-z)
      dimension x(-3:ntot), y(-3:ntot), xt(3), yt(3)
      logical anticl
      if(.not.(i.le.0))goto 23000
      i1 = 1
      goto 23001
23000 continue
      i1 = 0
23001 continue
      if(.not.(j.le.0))goto 23002
      j1 = 1
      goto 23003
23002 continue
      j1 = 0
23003 continue
      if(.not.(k.le.0))goto 23004
      k1 = 1
      goto 23005
23004 continue
      k1 = 0
23005 continue
      ijk = i1*4+j1*2+k1
      xt(1) = x(i)
      yt(1) = y(i)
      xt(2) = x(j)
      yt(2) = y(j)
      xt(3) = x(k)
      yt(3) = y(k)
      call cross(xt,yt,ijk,cprd)
      if(.not.(cprd .gt. eps))goto 23006
      anticl = .true.
      goto 23007
23006 continue
      anticl = .false.
23007 continue
      return
      end
