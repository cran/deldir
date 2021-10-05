C Output from Public domain Ratfor, version 1.03
      subroutine master(x,y,rw,nn,ntot,nadj,madj,eps,delsgs,ndel,delsum,
     * dirsgs,ndir,dirsum,incadj,incseg)
      implicit double precision(a-h,o-z)
      dimension x(-3:ntot), y(-3:ntot)
      dimension nadj(-3:ntot,0:madj)
      dimension rw(4)
      dimension delsgs(6,ndel), dirsgs(10,ndir)
      dimension delsum(nn,4), dirsum(nn,3)
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
      call insrt(j,k,nadj,madj,x,y,ntot,eps,incadj)
      if(incadj.eq.1)then
      return
      endif
23004 continue
23005 continue
      do23010 i = 1,4 
      j = i-4
      call insrt(1,j,nadj,madj,x,y,ntot,eps,incadj)
      if(incadj.eq.1)then
      return
      endif
23010 continue
23011 continue
      ntri = 4
      do23014 j = 2,nn 
      call addpt(j,nadj,madj,x,y,ntot,eps,ntri,incadj)
      if(incadj.eq.1)then
      return
      endif
      ntri = ntri + 3
23014 continue
23015 continue
      call delseg(delsgs,ndel,nadj,madj,nn,x,y,ntot,incseg)
      if(incseg.eq.1)then
      return
      endif
      call delout(delsum,nadj,madj,x,y,ntot,nn)
      call dirseg(dirsgs,ndir,nadj,madj,nn,x,y,ntot,rw,eps,ntri,incadj,i
     *ncseg)
      if(incadj.eq.1 .or. incseg.eq.1)then
      return
      endif
      call dirout(dirsum,nadj,madj,x,y,ntot,nn,rw,eps)
      return
      end
