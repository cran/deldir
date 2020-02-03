C Output from Public domain Ratfor, version 1.03
      subroutine qtest1(h,i,j,k,x,y,ntot,eps,shdswp,nerror)
      implicit double precision(a-h,o-z)
      dimension x(-3:ntot), y(-3:ntot), xt(3), yt(3), indv(3)
      dimension itmp(1)
      dimension xtmp(1)
      integer h
      logical shdswp, collin
      xt(1) = x(h)
      yt(1) = y(h)
      xt(2) = x(i)
      yt(2) = y(i)
      xt(3) = x(k)
      yt(3) = y(k)
      nid = 0
      call cross(xt,yt,nid,cprd)
      collin = (abs(cprd) .lt. eps)
      if(collin)then
      a = xt(2) - xt(1)
      b = yt(2) - yt(1)
      c = xt(3) - xt(1)
      d = yt(3) - yt(1)
      c1 = sqrt(a*a+b*b)
      c2 = sqrt(c*c+d*d)
      a = a/c1
      b = b/c1
      c = c/c2
      d = d/c2
      alpha = a*c+b*d
      if(alpha.gt.0)then
      itmp(1) = 1
      call intpr("error detected in qtest1",-1,itmp,0)
      indv(1) = i
      indv(2) = j
      indv(3) = k
      itmp(1) = h
      call intpr("Point being added, h:",-1,itmp,1)
      call intpr("now, other vertex, nxt:",-1,indv,3)
      xtmp(1) = alpha
      call dblepr("Test value:",-1,xtmp,1)
      call rexit("Points are collinear but h not between i and k.")
      endif
      shdswp = .true.
      endif
      xh = x(h)
      yh = y(h)
      xj = x(j)
      yj = y(j)
      call circen(h,i,k,x0,y0,x,y,ntot,eps,shdswp,nerror)
      if(nerror.gt.0)then
      return
      endif
      if(shdswp)then
      return
      endif
      a = x0-xh
      b = y0-yh
      r2 = a*a+b*b
      a = x0-xj
      b = y0-yj
      ch = a*a + b*b
      if(ch.lt.r2)then
      shdswp = .true.
      else
      shdswp = .false.
      endif
      return
      end
