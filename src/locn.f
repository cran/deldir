      subroutine locn(i,j,kj,nadj,madj,x,y,ntot,eps)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      logical before
      n = nadj(i,0)
      if(.not.(n.eq.0))goto 23000
      kj = 1
      return
23000 continue
      do 23002 ks = 1,n 
      kj = ks
      k = nadj(i,kj)
      call acchk(i,j,k,before,x,y,ntot,eps)
      if(.not.(before))goto 23004
      km = kj-1
      if(.not.(km.eq.0))goto 23006
      km = n
23006 continue
      k = nadj(i,km)
      call acchk(i,j,k,before,x,y,ntot,eps)
      if(.not.(before))goto 23008
      goto 23002
23008 continue
      if(.not.(kj.eq.1))goto 23010
      kj = n+1
23010 continue
      return
23004 continue
23002 continue
      if(.not.(before))goto 23012
      kj = 1
      goto 23013
23012 continue
      kj = n+1
23013 continue
      return
      end
