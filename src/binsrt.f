      subroutine binsrt(x,y,ntot,rw,npd,ind,tx,ty,ilst,nerror)
      implicit double precision(a-h,o-z)
      dimension x(-3:ntot), y(-3:ntot), tx(npd), ty(npd)
      dimension ind(npd), ilst(npd)
      dimension rw(4)
      nerror = -1
      kdiv = 1+dble(npd)**0.25
      xkdiv = dble(kdiv)
      xmin = rw(1)
      xmax = rw(2)
      ymin = rw(3)
      ymax = rw(4)
      w = xmax-xmin
      h = ymax-ymin
      dw = w/xkdiv
      dh = h/xkdiv
      kx = 1
      ky = 1
      ink = 1
      k = 0
      do 23000 i = 1,npd 
      ilst(i) = 0 
23000 continue
23002 if(.not.(ky.le.kdiv))goto 23003
      do 23004 i = 1,npd 
      if(.not.(ilst(i).eq.1))goto 23006
      goto 23004
23006 continue
      xt = x(i)
      yt = y(i)
      ix = 1+(xt-xmin)/dw
      if(.not.(ix.gt.kdiv))goto 23008
      ix = kdiv
23008 continue
      jy = 1+(yt-ymin)/dh
      if(.not.(jy.gt.kdiv))goto 23010
      jy = kdiv
23010 continue
      if(.not.(ix.eq.kx.and.jy.eq.ky))goto 23012
      k = k+1
      ind(i) = k
      tx(k) = xt
      ty(k) = yt
      ilst(i) = 1
23012 continue
23004 continue
      kc = kx+ink
      if(.not.((1.le.kc).and.(kc.le.kdiv)))goto 23014
      kx = kc
      goto 23015
23014 continue
      ky = ky+1
      ink = -ink
23015 continue
      goto 23002
23003 continue
      if(.not.(k.ne.npd))goto 23016
      nerror = 2
      return
23016 continue
      do 23018 i = 1,npd 
      x(i) = tx(i)
      y(i) = ty(i)
23018 continue
      return
      end
