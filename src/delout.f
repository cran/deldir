      subroutine delout(delsum,nadj,madj,x,y,ntot,npd,ind,nerror)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      dimension delsum(npd,4), ind(npd)
      do 23000 i1 = 1,npd 
      area = 0.
      i = ind(i1)
      np = nadj(i,0)
      xi = x(i)
      yi = y(i)
      npt = np
      do 23002 k = 1,np 
      kp = k+1
      if(.not.(kp.gt.np))goto 23004
      kp = 1
23004 continue
      if(.not.(nadj(i,k).le.0.or.nadj(i,kp).le.0))goto 23006
      npt = npt-1
23006 continue
23002 continue
      do 23008 j1 = 1,np 
      j = nadj(i,j1)
      if(.not.(j.le.0))goto 23010
      goto 23008
23010 continue
      xj = x(j)
      yj = y(j)
      call succ(k,i,j,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23012
      return
23012 continue
      if(.not.(k.le.0))goto 23014
      goto 23008
23014 continue
      xk = x(k)
      yk = y(k)
      call triar(xi,yi,xj,yj,xk,yk,tmp)
      area = area+tmp/3.
23008 continue
      delsum(i1,1) = xi
      delsum(i1,2) = yi
      delsum(i1,3) = npt
      delsum(i1,4) = area
23000 continue
      return
      end
