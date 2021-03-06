C Output from Public domain Ratfor, version 1.03
      subroutine dirseg(dirsgs,ndir,nadj,madj,npd,x,y,ntot,rw,eps,ntri,i
     *ncadj,incseg)
      implicit double precision(a-h,o-z)
      logical collin, adjace, intfnd, bptab, bptcd, goferit, rwu
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      dimension dirsgs(10,ndir), rw(4)
      dimension ndi(1)
      ndi(1) = 0
      incseg = 0
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
      do23000 j = nstt,ntot 
      call addpt(j,nadj,madj,x,y,ntot,eps,ntri,incadj)
      if(incadj.eq.1)then
      return
      endif
      ntri = ntri + 3
23000 continue
23001 continue
      kseg = 0
      do23004 i = 2,npd 
      do23006 j = 1,i-1 
      call adjchk(i,j,adjace,nadj,madj,ntot)
      if(adjace)then
      call pred(k,i,j,nadj,madj,ntot)
      call circen(i,k,j,a,b,x,y,ntot,eps,collin)
      if(collin)then
      call intpr("Vertices of triangle are collinear.",-1,ndi,0)
      call rexit("Bailing out of dirseg.")
      endif
      call succ(l,i,j,nadj,madj,ntot)
      call circen(i,j,l,c,d,x,y,ntot,eps,collin)
      if(collin)then
      call intpr("Vertices of triangle are collinear.",-1,ndi,0)
      call rexit("Bailing out of dirseg.")
      endif
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
      call dldins(a,b,slope,rwu,ai,bi,rw,intfnd,bptab,nedgeab)
      if(.not.intfnd)then
      call intpr("Line from midpoint to circumcenter",-1,ndi,0)
      call intpr("does not intersect rectangle boundary!",-1,ndi,0)
      call intpr("But it HAS to!!!",-1,ndi,0)
      call rexit("Bailing out of dirseg.")
      endif
      call dldins(c,d,slope,rwu,ci,di,rw,intfnd,bptcd,nedgecd)
      if(.not.intfnd)then
      call intpr("Line from midpoint to circumcenter",-1,ndi,0)
      call intpr("does not intersect rectangle boundary!",-1,ndi,0)
      call intpr("But it HAS to!!!",-1,ndi,0)
      call rexit("Bailing out of dirseg.")
      endif
      goferit = .false.
      if(bptab .and. bptcd)then
      xm = 0.5*(ai+ci)
      ym = 0.5*(bi+di)
      if(xmin.lt.xm.and.xm.lt.xmax.and.ymin.lt.ym.and.ym.lt.ymax)then
      goferit = .true.
      endif
      endif
      if((.not.bptab).or.(.not.bptcd))then
      goferit = .true.
      endif
      if(goferit)then
      kseg = kseg + 1
      if(kseg .gt. ndir)then
      incseg = 1
      return
      endif
      dirsgs(1,kseg) = ai
      dirsgs(2,kseg) = bi
      dirsgs(3,kseg) = ci
      dirsgs(4,kseg) = di
      dirsgs(5,kseg) = i
      dirsgs(6,kseg) = j
      if(bptab)then
      dirsgs(7,kseg) = 1.d0
      else
      dirsgs(7,kseg) = 0.d0
      endif
      if(bptcd)then
      dirsgs(8,kseg) = 1.d0
      else
      dirsgs(8,kseg) = 0.d0
      endif
      if(bptab)then
      dirsgs(9,kseg) = -nedgeab
      else
      dirsgs(9,kseg) = k
      endif
      if(bptcd)then
      dirsgs(10,kseg) = -nedgecd
      else
      dirsgs(10,kseg) = l
      endif
      endif
      endif
23006 continue
23007 continue
23004 continue
23005 continue
      ndir = kseg
      return
      end
