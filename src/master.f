C Output from Public domain Ratfor, version 1.03
      subroutine master(x,y,rw,npd,ntot,nadj,madj,eps, delsgs,ndel,delsu
     *m,dirsgs,ndir,dirsum,nerror)
      implicit double precision(a-h,o-z)
      dimension x(-3:ntot), y(-3:ntot)
      dimension nadj(-3:ntot,0:madj)
      dimension rw(4)
      dimension delsgs(6,ndel), dirsgs(10,ndir)
      dimension delsum(npd,4), dirsum(npd,3)
      one = 1.d0
      do23000 i = -3,ntot 
      nadj(i,0) = 0
      do23002 j = 1,madj 
      nadj(i,j) = -99
23002 continue
23003 continue
23000 continue
23001 continue
      x(-3) = -one
      y(-3) = one
      x(-2) = one
      y(-2) = one
      x(-1) = one
      y(-1) = -one
      x(0) = -one
      y(0) = -one
      do23004 i = 1,4 
      j = i-4
      k = j+1
      if(k.gt.0)then
      k = -3
      endif
      call insrt(j,k,nadj,madj,x,y,ntot,nerror,eps)
      if(nerror.gt.0)then
      return
      endif
23004 continue
23005 continue
      do23010 i = 1,4 
      j = i-4
      call insrt(1,j,nadj,madj,x,y,ntot,nerror,eps)
      if(nerror.gt.0)then
      return
      endif
23010 continue
23011 continue
      ntri = 4
      do23014 j = 2,npd 
      call addpt(j,nadj,madj,x,y,ntot,eps,ntri,nerror)
      ntri = ntri + 3
23014 continue
23015 continue
      call delseg(delsgs,ndel,nadj,madj,npd,x,y,ntot,nerror)
      if(nerror.gt.0)then
      return
      endif
      call delout(delsum,nadj,madj,x,y,ntot,npd,nerror)
      if(nerror.gt.0)then
      return
      endif
      call dirseg(dirsgs,ndir,nadj,madj,npd,x,y,ntot,rw,eps,ntri,nerror)
      if(nerror.gt.0)then
      return
      endif
      call dirout(dirsum,nadj,madj,x,y,ntot,npd,rw,eps,nerror)
      return
      end
