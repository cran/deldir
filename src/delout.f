C Output from Public domain Ratfor, version 1.03
      subroutine delout(delsum,nadj,madj,x,y,ntot,nn)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      dimension delsum(nn,4)
      do23000 i = 1,nn 
      area = 0.
      np = nadj(i,0)
      xi = x(i)
      yi = y(i)
      npt = np
      do23002 k = 1,np 
      kp = k+1
      if(kp.gt.np)then
      kp = 1
      endif
      if(nadj(i,k).le.0.or.nadj(i,kp).le.0)then
      npt = npt-1
      endif
23002 continue
23003 continue
      do23008 j1 = 1,np 
      j = nadj(i,j1)
      if(j.le.0)then
      goto 23008
      endif
      xj = x(j)
      yj = y(j)
      call succ(k,i,j,nadj,madj,ntot)
      if(k.le.0)then
      goto 23008
      endif
      xk = x(k)
      yk = y(k)
      call triar(xi,yi,xj,yj,xk,yk,tmp)
      area = area+tmp/3.
23008 continue
23009 continue
      delsum(i,1) = xi
      delsum(i,2) = yi
      delsum(i,3) = npt
      delsum(i,4) = area
23000 continue
23001 continue
      return
      end
