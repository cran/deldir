      subroutine addpt(j,nadj,madj,x,y,ntot,eps,nerror)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      logical didswp
      call initad(j,nadj,madj,x,y,ntot,eps,nerror)
      if(.not.(nerror .gt. 0))goto 23000
      return
23000 continue
      now = nadj(j,1)
      nxt = nadj(j,2)
      ngap = 0
23002 continue
      call swap(j,now,nxt,didswp,nadj,madj,x,y,ntot,eps,nerror)
      if(.not.(nerror .gt. 0))goto 23005
      return
23005 continue
      n = nadj(j,0)
      if(.not.(.not.didswp))goto 23007
      now = nxt
      ngap = ngap+1
23007 continue
      call succ(nxt,j,now,nadj,madj,ntot,nerror)
      if(.not.(nerror .gt. 0))goto 23009
      return
23009 continue
23003 if(.not.(ngap.eq.n))goto 23002
      return
      end
