C Output from Public domain Ratfor, version 1.0
      subroutine dldins(a,b,c,d,ai,bi,rw,intfnd,bpt)
      implicit double precision(a-h,o-z)
      dimension rw(4)
      logical intfnd, bpt
      intfnd = .true.
      bpt = .true.
      xmin = rw(1)
      xmax = rw(2)
      ymin = rw(3)
      ymax = rw(4)
      if(xmin.le.a.and.a.le.xmax.and.ymin.le.b.and.b.le.ymax)then
      ai = a
      bi = b
      bpt = .false.
      return
      endif
      if(a.lt.xmin)then
      ai = xmin
      s = (d-b)/(c-a)
      t = b-s*a
      bi = s*ai+t
      if(ymin.le.bi.and.bi.le.ymax)then
      return
      endif
      endif
      if(b.lt.ymin)then
      bi = ymin
      s = (c-a)/(d-b)
      t = a-s*b
      ai = s*bi+t
      if(xmin.le.ai.and.ai.le.xmax)then
      return
      endif
      endif
      if(a.gt.xmax)then
      ai = xmax
      s = (d-b)/(c-a)
      t = b-s*a
      bi = s*ai+t
      if(ymin.le.bi.and.bi.le.ymax)then
      return
      endif
      endif
      if(b.gt.ymax)then
      bi = ymax
      s = (c-a)/(d-b)
      t = a-s*b
      ai = s*bi+t
      if(xmin.le.ai.and.ai.le.xmax)then
      return
      endif
      endif
      intfnd = .false.
      return
      end
