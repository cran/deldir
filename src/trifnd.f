C Output from Public domain Ratfor, version 1.03
      subroutine trifnd(j,tau,nedge,nadj,madj,x,y,ntot,eps,ntri)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot), xt(3), yt(
     *3)
      dimension ndi(1)
      integer tau(3)
      logical adjace, anticl
      if(j.eq.1)then
      call intpr("No triangles to find.",-1,ndi,0)
      call rexit("Bailing out of trifnd.")
      endif
      j1 = j-1
      tau(1) = j1
      tau(3) = nadj(j1,1)
      call pred(tau(2),j1,tau(3),nadj,madj,ntot)
      call adjchk(tau(2),tau(3),adjace,nadj,madj,ntot)
      if(.not.adjace)then
      tau(3) = tau(2)
      call pred(tau(2),j1,tau(3),nadj,madj,ntot)
      endif
      ktri = 0
1     continue
      call acchk(tau(1),tau(2),tau(3),anticl,x,y,ntot,eps)
      if(.not.anticl)then
      call acchk(tau(3),tau(2),tau(1),anticl,x,y,ntot,eps)
      if(.not.anticl)then
      ndi(1) = j
      call intpr("Point number =",-1,ndi,1)
      call intpr("Previous triangle:",-1,tau,3)
      call intpr("Both vertex orderings are clockwise.",-1,ndi,0)
      call intpr("See help for deldir.",-1,ndi,0)
      call rexit("Bailing out of trifnd.")
      else
      ivtmp = tau(3)
      tau(3) = tau(1)
      tau(1) = ivtmp
      endif
      endif
      ntau = 0
      nedge = 0
      do23008 i = 1,3 
      ip = i+1
      if(ip.eq.4)then
      ip = 1
      endif
      xt(1) = x(tau(i))
      yt(1) = y(tau(i))
      xt(2) = x(tau(ip))
      yt(2) = y(tau(ip))
      xt(3) = x(j)
      yt(3) = y(j)
      if(tau(i).le.0)then
      i1 = 1
      else
      i1 = 0
      endif
      if(tau(ip).le.0)then
      j1 = 1
      else
      j1 = 0
      endif
      k1 = 0
      ijk = i1*4+j1*2+k1
      call cross(xt,yt,ijk,cprd)
      if(cprd .ge. eps)then
      continue
      else
      if(cprd .gt. -eps)then
      nedge = ip
      else
      ntau = ip
      goto 23009
      endif
      endif
23008 continue
23009 continue
      if(ntau.eq.0)then
      return
      endif
      if(ntau.eq.1)then
      tau(2) = tau(3)
      call succ(tau(3),tau(1),tau(2),nadj,madj,ntot)
      endif
      if(ntau.eq.2)then
      tau(3) = tau(2)
      call pred(tau(2),tau(1),tau(3),nadj,madj,ntot)
      endif
      if(ntau.eq.3)then
      tau(1) = tau(3)
      call succ(tau(3),tau(1),tau(2),nadj,madj,ntot)
      endif
      ktri = ktri + 1
      if(ktri .gt. ntri)then
      ndi(1) = j
      call intpr("Point being added:",-1,ndi,1)
      call intpr("Cannot find an enclosing triangle.",-1,ndi,0)
      call intpr("See help for deldir.",-1,ndi,0)
      call rexit("Bailing out of trifnd.")
      endif
      go to 1
      end
