      subroutine mnnd(x,y,n,dminbig,dminav)
      implicit double precision(a-h,o-z)
      dimension x(n), y(n)
      dminav = 0.d0
      do 23000 i = 1,n 
      dmin = dminbig
      do 23002 j = 1,n 
      if(.not.(i.ne.j))goto 23004
      d = (x(i)-x(j))**2 + (y(i)-y(j))**2
      if(.not.(d .lt. dmin))goto 23006
      dmin = d
23006 continue
23004 continue
23002 continue
      dminav = dminav + sqrt(dmin)
23000 continue
      dminav = dminav/n
      return
      end
