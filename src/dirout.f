      subroutine dirout(dirsum,nadj,madj,x,y,ntot,npd,rw,ind,eps,nerror)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      dimension dirsum(npd,3), ind(npd), rw(4)
      logical collin, intfnd, bptab, bptcd
      xmin = rw(1)
      xmax = rw(2)
      ymin = rw(3)
      ymax = rw(4)
      do 23000 i1 = 1,npd 
      area = 0.
      nbpt = 0
      npt = 0
      i = ind(i1)
      np = nadj(i,0)
      xi = x(i)
      yi = y(i)
      do 23002 j1 = 1,np 
      j = nadj(i,j1)
      xj = x(j)
      yj = y(j)
      xij = 0.5*(xi+xj)
      yij = 0.5*(yi+yj)
      call pred(k,i,j,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23004
      return
23004 continue
      call succ(l,i,j,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23006
      return
23006 continue
      call circen(i,k,j,a,b,x,y,ntot,eps,collin,nerror)
      if(.not.(nerror.gt.0))goto 23008
      return
23008 continue
      if(.not.(collin))goto 23010
      nerror = 13
      return
23010 continue
      call circen(i,j,l,c,d,x,y,ntot,eps,collin,nerror)
      if(.not.(nerror.gt.0))goto 23012
      return
23012 continue
      if(.not.(collin))goto 23014
      nerror = 13
      return
23014 continue
      call stoke(a,b,c,d,rw,tmp,sn,eps,nerror)
      if(.not.(nerror .gt. 0))goto 23016
      return
23016 continue
      area = area+sn*tmp
      call dldins(a,b,xij,yij,ai,bi,rw,intfnd,bptab)
      if(.not.(intfnd))goto 23018
      call dldins(c,d,xij,yij,ci,di,rw,intfnd,bptcd)
      if(.not.(.not.intfnd))goto 23020
      nerror = 17
      return
23020 continue
      if(.not.(bptab .and. bptcd))goto 23022
      xm = 0.5*(ai+ci)
      ym = 0.5*(bi+di)
      if(.not.(xmin.lt.xm.and.xm.lt.xmax.and.ymin.lt.ym.and.ym.lt.ymax))
&     goto 23024
      nbpt = nbpt+2
      npt = npt+1
23024 continue
      goto 23023
23022 continue
      npt = npt + 1
      if(.not.(bptab .or. bptcd))goto 23026
      nbpt = nbpt+1
23026 continue
23023 continue
23018 continue
      dirsum(i1,1) = npt
      dirsum(i1,2) = nbpt
      dirsum(i1,3) = area
23002 continue
23000 continue
      return
      end
