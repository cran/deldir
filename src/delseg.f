C Output from Public domain Ratfor, version 1.03
      subroutine delseg(delsgs,ndel,nadj,madj,nn,x,y,ntot,incseg)
      implicit double precision(a-h,o-z)
      logical value
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      dimension delsgs(6,ndel)
      incseg = 0
      nn = ntot-4
      kseg = 0
      do23000 i = 2,nn 
      do23002 j = 1,i-1 
      call adjchk(i,j,value,nadj,madj,ntot)
      if(value)then
      kseg = kseg+1
      if(kseg .gt. ndel)then
      incseg = 1
      return
      endif
      delsgs(1,kseg) = x(i)
      delsgs(2,kseg) = y(i)
      delsgs(3,kseg) = x(j)
      delsgs(4,kseg) = y(j)
      delsgs(5,kseg) = i
      delsgs(6,kseg) = j
      endif
23002 continue
23003 continue
23000 continue
23001 continue
      ndel = kseg
      return
      end
