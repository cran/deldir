      subroutine inddup(x,y,n,rw,frac,dup)
      implicit double precision(a-h,o-z)
      logical dup(n)
      dimension x(n), y(n), rw(4)
      xtol = frac*(rw(2)-rw(1))
      ytol = frac*(rw(4)-rw(3))
      dup(1) = .false.
      do 23000 i = 2,n 
      dup(i) = .false.
      do 23002 j = 1,i-1 
      dx = abs(x(i)-x(j))
      dy = abs(y(i)-y(j))
      if(.not.(dx .lt. xtol .and. dy .lt. ytol))goto 23004
      dup(i) = .true.
      goto 23003
23004 continue
23002 continue
23003 continue
23000 continue
      return
      end
