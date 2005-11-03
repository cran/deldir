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
      if(.not.(xmin.le.a.and.a.le.xmax.and.ymin.le.b.and.b.le.ymax))
     *goto 23000
      ai = a
      bi = b
      bpt = .false.
      return
23000 continue
      if(.not.(a.lt.xmin))goto 23002
      ai = xmin
      s = (d-b)/(c-a)
      t = b-s*a
      bi = s*ai+t
      if(.not.(ymin.le.bi.and.bi.le.ymax))goto 23004
      return
23004 continue
23002 continue
      if(.not.(b.lt.ymin))goto 23006
      bi = ymin
      s = (c-a)/(d-b)
      t = a-s*b
      ai = s*bi+t
      if(.not.(xmin.le.ai.and.ai.le.xmax))goto 23008
      return
23008 continue
23006 continue
      if(.not.(a.gt.xmax))goto 23010
      ai = xmax
      s = (d-b)/(c-a)
      t = b-s*a
      bi = s*ai+t
      if(.not.(ymin.le.bi.and.bi.le.ymax))goto 23012
      return
23012 continue
23010 continue
      if(.not.(b.gt.ymax))goto 23014
      bi = ymax
      s = (c-a)/(d-b)
      t = a-s*b
      ai = s*bi+t
      if(.not.(xmin.le.ai.and.ai.le.xmax))goto 23016
      return
23016 continue
23014 continue
      intfnd = .false.
      return
      end
