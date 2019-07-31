C Output from Public domain Ratfor, version 1.03
      subroutine crossutil(i,j,k,x,y,ntot,eps,collin)
      implicit double precision(a-h,o-z)
      dimension x(-3:ntot), y(-3:ntot)
      dimension xt(3), yt(3)
      logical collin
      xt(1) = x(i)
      yt(1) = y(i)
      xt(2) = x(j)
      yt(2) = y(j)
      xt(3) = x(k)
      yt(3) = y(k)
      i1 = 0
      if(j.le.0)then
      j1 = 1
      else
      j1 = 0
      endif
      if(k.le.0)then
      k1 = 1
      else
      k1 = 0
      endif
      ijk = i1*4+j1*2+k1
      call cross(xt,yt,ijk,cprd)
      collin = (abs(cprd) .lt. eps)
      return
      end
