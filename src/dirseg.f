      subroutine dirseg(dirsgs,ndir,nadj,madj,x,y,ntot,rw,eps,ind,
&     nerror)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      dimension dirsgs(8,ndir), rw(4), ind(1)
      logical collin, adjace, intfnd, bptab, bptcd, goferit
      nerror = -1
      xmin = rw(1)
      xmax = rw(2)
      ymin = rw(3)
      ymax = rw(4)
      a = xmax-xmin
      b = ymax-ymin
      c = sqrt(a*a+b*b)
      npd = ntot-4
      nstt = npd+1
      i = nstt
      x(i) = xmin-c
      y(i) = ymin-c
      i = i+1
      x(i) = xmax+c
      y(i) = ymin-c
      i = i+1
      x(i) = xmax+c
      y(i) = ymax+c
      i = i+1
      x(i) = xmin-c
      y(i) = ymax+c
      do 23000 j = nstt,ntot 
      call addpt(j,nadj,madj,x,y,ntot,eps,nerror)
      if(.not.(nerror .gt. 0))goto 23002
      return
23002 continue
23000 continue
      kseg = 0
      do 23004 i1 = 2,npd 
      i = ind(i1)
      do 23006 j1 = 1,i1-1 
      j = ind(j1)
      call adjchk(i,j,adjace,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23008
      return
23008 continue
      if(.not.(adjace))goto 23010
      xi = x(i)
      yi = y(i)
      xj = x(j)
      yj = y(j)
      xij = 0.5*(xi+xj)
      yij = 0.5*(yi+yj)
      call pred(k,i,j,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23012
      return
23012 continue
      call circen(i,k,j,a,b,x,y,ntot,eps,collin,nerror)
      if(.not.(nerror .gt. 0))goto 23014
      return
23014 continue
      if(.not.(collin))goto 23016
      nerror = 12
      return
23016 continue
      call dldins(a,b,xij,yij,ai,bi,rw,intfnd,bptab)
      if(.not.(.not.intfnd))goto 23018
      nerror = 16
      return
23018 continue
      call succ(l,i,j,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23020
      return
23020 continue
      call circen(i,j,l,c,d,x,y,ntot,eps,collin,nerror)
      if(.not.(nerror .gt. 0))goto 23022
      return
23022 continue
      if(.not.(collin))goto 23024
      nerror = 12
      return
23024 continue
      call dldins(c,d,xij,yij,ci,di,rw,intfnd,bptcd)
      if(.not.(.not.intfnd))goto 23026
      nerror = 16
      return
23026 continue
      goferit = .false.
      if(.not.(bptab .and. bptcd))goto 23028
      xm = 0.5*(ai+ci)
      ym = 0.5*(bi+di)
      if(.not.(xmin.lt.xm.and.xm.lt.xmax.and.ymin.lt.ym.and.ym.lt.ymax))
&     goto 23030
      goferit = .true.
23030 continue
23028 continue
      if(.not.((.not.bptab).or.(.not.bptcd)))goto 23032
      goferit = .true.
23032 continue
      if(.not.(goferit))goto 23034
      kseg = kseg + 1
      if(.not.(kseg .gt. ndir))goto 23036
      nerror = 15
      return
23036 continue
      dirsgs(1,kseg) = ai
      dirsgs(2,kseg) = bi
      dirsgs(3,kseg) = ci
      dirsgs(4,kseg) = di
      dirsgs(5,kseg) = i1
      dirsgs(6,kseg) = j1
      if(.not.(bptab))goto 23038
      dirsgs(7,kseg) = 1.d0
      goto 23039
23038 continue
      dirsgs(7,kseg) = 0.d0
23039 continue
      if(.not.(bptcd))goto 23040
      dirsgs(8,kseg) = 1.d0
      goto 23041
23040 continue
      dirsgs(8,kseg) = 0.d0
23041 continue
23034 continue
23010 continue
23006 continue
23004 continue
      ndir = kseg
      return
      end
