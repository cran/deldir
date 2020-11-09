C Output from Public domain Ratfor, version 1.03
      subroutine binsrt(x,y,rw,npd,ind,rind,tx,ty,ilst)
      implicit double precision(a-h,o-z)
      dimension x(npd), y(npd), tx(npd), ty(npd)
      integer rind(npd)
      dimension ind(npd), ilst(npd)
      dimension rw(4)
      dimension ndi(1)
      ndi(1) = 0
      kdiv = int(1+dble(npd)**0.25)
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
      do23000 i = 1,npd 
      ilst(i) = 0 
23000 continue
23001 continue
23002 if(ky.le.kdiv)then
      do23004 i = 1,npd 
      if(ilst(i).eq.1)then
      goto 23004
      endif
      xt = x(i)
      yt = y(i)
      ix = int(1+(xt-xmin)/dw)
      if(ix.gt.kdiv)then
      ix = kdiv
      endif
      jy = int(1+(yt-ymin)/dh)
      if(jy.gt.kdiv)then
      jy = kdiv
      endif
      if(ix.eq.kx.and.jy.eq.ky)then
      k = k+1
      ind(i) = k
      rind(k) = i
      tx(k) = xt
      ty(k) = yt
      ilst(i) = 1
      endif
23004 continue
23005 continue
      kc = kx+ink
      if((1.le.kc).and.(kc.le.kdiv))then
      kx = kc
      else
      ky = ky+1
      ink = -ink
      endif
      goto 23002
      endif
23003 continue
      if(k.ne.npd)then
      call intpr("Mismatch between number of points",-1,ndi,0)
      call intpr("and number of sorted points.",-1,ndi,0)
      call rexit("Bailing out of binsrt.")
      endif
      do23018 i = 1,npd 
      x(i) = tx(i)
      y(i) = ty(i)
23018 continue
23019 continue
      return
      end
