      subroutine delseg(delsgs,ndel,nadj,madj,x,y,ntot,ind,nerror)
      implicit double precision(a-h,o-z)
      dimension nadj(-3:ntot,0:madj), x(-3:ntot), y(-3:ntot)
      dimension delsgs(6,1), ind(1)
      logical value
      npd = ntot-4
      kseg = 0
      do 23000 i1 = 2,npd 
      i = ind(i1)
      do 23002 j1 = 1,i1-1 
      j = ind(j1)
      call adjchk(i,j,value,nadj,madj,ntot,nerror)
      if(.not.(nerror.gt.0))goto 23004
      return
23004 continue
      if(.not.(value))goto 23006
      kseg = kseg+1
      if(.not.(kseg .gt. ndel))goto 23008
      nerror = 14
      return
23008 continue
      delsgs(1,kseg) = x(i)
      delsgs(2,kseg) = y(i)
      delsgs(3,kseg) = x(j)
      delsgs(4,kseg) = y(j)
      delsgs(5,kseg) = i1
      delsgs(6,kseg) = j1
23006 continue
23002 continue
23000 continue
      ndel = kseg
      return
      end
