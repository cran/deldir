      subroutine cross(x,y,ijk,cprd)
      implicit double precision(a-h,o-z)
      dimension x(3), y(3)
      zero = 0.d0
      one = 1.d0
      two = 2.d0
      four = 4.d0
      I23000 = (ijk)
      if(.not.(I23000.eq.( 0)))goto 23001
      smin = -one
      do 23002 i = 1,3 
      ip = i+1
      if(.not.(ip.eq.4))goto 23004
      ip = 1
23004 continue
      a = x(ip) - x(i)
      b = y(ip) - y(i)
      s = a*a+b*b
      if(.not.(smin .lt. zero .or. s .lt. smin))goto 23006
      smin = s
23006 continue
23002 continue
      goto 23000
23001 continue
      if(.not.(I23000.eq.( 1)))goto 23008
      x(2) = x(2) - x(1)
      y(2) = y(2) - y(1)
      x(1) = zero
      y(1) = zero
      cn = sqrt(x(2)**2+y(2)**2)
      x(2) = x(2)/cn
      y(2) = y(2)/cn
      smin = one
      goto 23000
23008 continue
      if(.not.(I23000.eq.( 2)))goto 23009
      x(3) = x(3) - x(1)
      y(3) = y(3) - y(1)
      x(1) = zero
      y(1) = zero
      cn = sqrt(x(3)**2+y(3)**2)
      x(3) = x(3)/cn
      y(3) = y(3)/cn
      smin = one
      goto 23000
23009 continue
      if(.not.(I23000.eq.( 3)))goto 23010
      x(1) = zero
      y(1) = zero
      smin = 2
      goto 23000
23010 continue
      if(.not.(I23000.eq.( 4)))goto 23011
      x(3) = x(3) - x(2)
      y(3) = y(3) - y(2)
      x(2) = zero
      y(2) = zero
      cn = sqrt(x(3)**2+y(3)**2)
      x(3) = x(3)/cn
      y(3) = y(3)/cn
      smin = one
      goto 23000
23011 continue
      if(.not.(I23000.eq.( 5)))goto 23012
      x(2) = zero
      y(2) = zero
      smin = two
      goto 23000
23012 continue
      if(.not.(I23000.eq.( 6)))goto 23013
      x(3) = zero
      y(3) = zero
      smin = two
      goto 23000
23013 continue
      if(.not.(I23000.eq.( 7)))goto 23014
      smin = four
23014 continue
23000 continue
      a = x(2)-x(1)
      b = y(2)-y(1)
      c = x(3)-x(1)
      d = y(3)-y(1)
      cprd = (a*d - b*c)/smin
      return
      end
