      subroutine testeq(a,b,eps,value)
      implicit double precision(a-h,o-z)
      logical value
      if(.not.(abs(b).le.eps))goto 23000
      if(.not.(abs(a).le.eps))goto 23002
      value = .true.
      goto 23003
23002 continue
      value = .false.
23003 continue
      return
23000 continue
      if(.not.(abs(a).gt.10.*abs(b).or.abs(a).lt.0.1*abs(b)))goto 23004
      value = .false.
      return
23004 continue
      c = a/b
      if(.not.(abs(c-1.).le.eps))goto 23006
      value = .true.
      goto 23007
23006 continue
      value = .false.
23007 continue
      return
      end
