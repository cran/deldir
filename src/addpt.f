C Output from Public domain Ratfor, version 1.03
      subroutine addpt(j,nadj,madj,x,y,ntot,eps,ntri,incadj)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      logical didswp
      call initad(j,nadj,madj,x,y,ntot,eps,ntri,incadj)
      now = nadj(j,1)
      nxt = nadj(j,2)
      ngap = 0
23000 continue
      call swap(j,now,nxt,didswp,nadj,madj,x,y,ntot,eps,incadj)
      n = nadj(j,0)
      if(.not.didswp)then
      now = nxt
      ngap = ngap+1
      endif
      call succ(nxt,j,now,nadj,madj,ntot)
23001 if(.not.(ngap.eq.n))goto 23000
23002 continue
      return
      end
