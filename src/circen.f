C Output from Public domain Ratfor, version 1.03
      subroutine circen(i,j,k,x0,y0,x,y,ntot,eps,collin)
      implicit double precision(a-h,o-z)
      dimension x(-3:ntot), y(-3:ntot), xt(3), yt(3)
      dimension indv(3)
      dimension xtmp(1)
      dimension ndi(1)
      logical collin
      ndi(1) = 0
      xt(1) = x(i)
      yt(1) = y(i)
      xt(2) = x(j)
      yt(2) = y(j)
      xt(3) = x(k)
      yt(3) = y(k)
      ijk = 0
      call cross(xt,yt,ijk,cprd)
      if(abs(cprd) .lt. eps)then
      collin = .true.
      else
      collin = .false.
      endif
      a = x(j) - x(i)
      b = y(j) - y(i)
      c = x(k) - x(i)
      d = y(k) - y(i)
      c1 = sqrt(a*a+b*b)
      c2 = sqrt(c*c+d*d)
      a = a/c1
      b = b/c1
      c = c/c2
      d = d/c2
      if(collin)then
      alpha = a*c+b*d
      if(alpha.gt.0)then
      indv(1) = i
      indv(2) = j
      indv(3) = k
      call intpr("Point numbers:",-1,indv,3)
      xtmp(1) = alpha
      call dblepr("Test value:",-1,xtmp,1)
      call intpr("Points are collinear but in the wrong order.",-1,ndi,0
     *)
      call rexit("Bailing out of circen.")
      endif
      return
      endif
      crss = a*d - b*c
      x0 = x(i) + 0.5*(c1*d - c2*b)/crss
      y0 = y(i) + 0.5*(c2*a - c1*c)/crss
      return
      end
