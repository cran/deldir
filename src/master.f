      subroutine master(x,y,sort,rw,npd,ntot,nadj,madj,ind,tx,ty,ilst,
&     eps, delsgs,ndel,delsum,dirsgs,ndir,dirsum,nerror)
      implicit double precision(a-h,o-z)
      logical sort
      dimension x(-3:ntot), y(-3:ntot)
      dimension nadj(-3:ntot,0:madj)
      dimension ind(npd), tx(npd), ty(npd), ilst(npd), rw(4)
      dimension delsgs(6,ndel), dirsgs(8,ndir)
      dimension delsum(npd,4), dirsum(npd,3)
      one = 1.d0
      if(.not.(sort))goto 23000
      call binsrt(x,y,ntot,rw,npd,ind,tx,ty,ilst,nerror)
      if(.not.(nerror .gt. 0))goto 23002
      return
23002 continue
      goto 23001
23000 continue
      do 23004 i = 1,npd 
      ind(i) = i
23004 continue
23001 continue
      do 23006 i = -3,ntot 
      do 23008 j = 0,madj 
      nadj(i,j) = 0
23008 continue
23006 continue
      x(-3) = -one
      y(-3) = one
      x(-2) = one
      y(-2) = one
      x(-1) = one
      y(-1) = -one
      x(0) = -one
      y(0) = -one
      do 23010 i = 1,4 
      j = i-4
      k = j+1
      if(.not.(k.gt.0))goto 23012
      k = -3
23012 continue
      call insrt(j,k,nadj,madj,x,y,ntot,nerror,eps)
      if(.not.(nerror.gt.0))goto 23014
      return
23014 continue
23010 continue
      do 23016 i = 1,4 
      j = i-4
      call insrt(1,j,nadj,madj,x,y,ntot,nerror,eps)
      if(.not.(nerror.gt.0))goto 23018
      return
23018 continue
23016 continue
      do 23020 j = 2,npd 
      call addpt(j,nadj,madj,x,y,ntot,eps,nerror)
      if(.not.(nerror.gt.0))goto 23022
      return
23022 continue
23020 continue
      call delseg(delsgs,ndel,nadj,madj,x,y,ntot,ind,nerror)
      if(.not.(nerror.gt.0))goto 23024
      return
23024 continue
      call delout(delsum,nadj,madj,x,y,ntot,npd,ind,nerror)
      if(.not.(nerror.gt.0))goto 23026
      return
23026 continue
      call dirseg(dirsgs,ndir,nadj,madj,x,y,ntot,rw,eps,ind,nerror)
      if(.not.(nerror.gt.0))goto 23028
      return
23028 continue
      call dirout(dirsum,nadj,madj,x,y,ntot,npd,rw,ind,eps,nerror)
      return
      end
