      subroutine trifnd(j,tau,nedge,nadj,madj,x,y,ntot,eps,nerror)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot), xt(3), yt(
&     3)
      integer tau(3)
      logical adjace
      nerror = -1
      if(.not.(j.eq.1))goto 23000
      nerror = 11
      return
23000 continue
      j1 = j-1
      tau(1) = j1
      tau(3) = nadj(j1,1)
      call pred(tau(2),j1,tau(3),nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23002
      return
23002 continue
      call adjchk(tau(2),tau(3),adjace,nadj,madj,ntot,nerror)
      if(.not.(nerror.gt.0))goto 23004
      return
23004 continue
      if(.not.(.not.adjace))goto 23006
      tau(3) = tau(2)
      call pred(tau(2),j1,tau(3),nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23008
      return
23008 continue
23006 continue
1     continue
      ntau = 0
      nedge = 0
      do 23010 i = 1,3 
      ip = i+1
      if(.not.(ip.eq.4))goto 23012
      ip = 1
23012 continue
      xt(1) = x(tau(i))
      yt(1) = y(tau(i))
      xt(2) = x(tau(ip))
      yt(2) = y(tau(ip))
      xt(3) = x(j)
      yt(3) = y(j)
      if(.not.(tau(i).le.0))goto 23014
      i1 = 1
      goto 23015
23014 continue
      i1 = 0
23015 continue
      if(.not.(tau(ip).le.0))goto 23016
      j1 = 1
      goto 23017
23016 continue
      j1 = 0
23017 continue
      k1 = 0
      ijk = i1*4+j1*2+k1
      call cross(xt,yt,ijk,cprd)
      if(.not.(cprd .ge. eps))goto 23018
      continue
      goto 23019
23018 continue
      if(.not.(cprd .gt. -eps))goto 23020
      nedge = ip
      goto 23021
23020 continue
      ntau = ip
      goto 23011
23021 continue
23019 continue
23010 continue
23011 continue
      I23022 = (ntau)
      if(.not.(I23022.eq.( 0)))goto 23023
      return
23023 continue
      if(.not.(I23022.eq.( 1)))goto 23024
      tau(2) = tau(3)
      call succ(tau(3),tau(1),tau(2),nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23025
      return
23025 continue
      goto 23022
23024 continue
      if(.not.(I23022.eq.( 2)))goto 23027
      tau(3) = tau(2)
      call pred(tau(2),tau(1),tau(3),nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23028
      return
23028 continue
      goto 23022
23027 continue
      if(.not.(I23022.eq.( 3)))goto 23030
      tau(1) = tau(3)
      call succ(tau(3),tau(1),tau(2),nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23031
      return
23031 continue
23030 continue
23022 continue
      go to 1
      end
