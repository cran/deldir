C Output from Public domain Ratfor, version 1.03
      subroutine dirout(dirsum,nadj,madj,x,y,ntot,nn,rw,eps)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      dimension dirsum(nn,3), rw(4)
      dimension ndi(1)
      logical collin, intfnd, bptab, bptcd, rwu
      ndi(1) = 0
      xmin = rw(1)
      xmax = rw(2)
      ymin = rw(3)
      ymax = rw(4)
      do23000 i = 1,nn 
      area = 0.
      nbpt = 0
      npt = 0
      np = nadj(i,0)
      do23002 j1 = 1,np 
      j = nadj(i,j1)
      call pred(k,i,j,nadj,madj,ntot)
      call succ(l,i,j,nadj,madj,ntot)
      call circen(i,k,j,a,b,x,y,ntot,eps,collin)
      if(collin)then
      call intpr("Vertices of triangle are collinear.",-1,ndi,0)
      call rexit("Bailing out of dirout.")
      endif
      call circen(i,j,l,c,d,x,y,ntot,eps,collin)
      if(collin)then
      call intpr("Vertices of triangle are collinear.",-1,ndi,0)
      call rexit("Bailing out of dirout.")
      endif
      call stoke(a,b,c,d,rw,tmp,sn,eps)
      area = area+sn*tmp
      xi = x(i)
      xj = x(j)
      yi = y(i)
      yj = y(j)
      if(yi.ne.yj)then
      slope = (xi - xj)/(yj - yi)
      rwu = .true.
      else
      slope = 0.d0
      rwu = .false.
      endif
      call dldins(a,b,slope,rwu,ai,bi,rw,intfnd,bptab,nedge)
      if(intfnd)then
      call dldins(c,d,slope,rwu,ci,di,rw,intfnd,bptcd,nedge)
      if(.not.intfnd)then
      call intpr("Line from midpoint to circumcenter",-1,ndi,0)
      call intpr("does not intersect rectangle boundary!",-1,ndi,0)
      call intpr("But it HAS to!!!",-1,ndi,0)
      call rexit("Bailing out of dirout.")
      endif
      if(bptab .and. bptcd)then
      xm = 0.5*(ai+ci)
      ym = 0.5*(bi+di)
      if(xmin.lt.xm.and.xm.lt.xmax.and.ymin.lt.ym.and.ym.lt.ymax)then
      nbpt = nbpt+2
      npt = npt+1
      endif
      else
      npt = npt + 1
      if(bptab .or. bptcd)then
      nbpt = nbpt+1
      endif
      endif
      endif
23002 continue
23003 continue
      dirsum(i,1) = npt
      dirsum(i,2) = nbpt
      dirsum(i,3) = area
23000 continue
23001 continue
      return
      end
