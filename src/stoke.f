      subroutine stoke(x1,y1,x2,y2,rw,area,s1,eps,nerror)
      implicit double precision(a-h,o-z)
      dimension rw(4)
      logical value
      zero = 0.d0
      nerror = -1
      call testeq(x1,x2,eps,value)
      if(.not.(value))goto 23000
      area = 0.
      s1 = 0.
      return
23000 continue
      if(.not.(x1.lt.x2))goto 23002
      xl = x1
      yl = y1
      xr = x2
      yr = y2
      s1 = -1.
      goto 23003
23002 continue
      xl = x2
      yl = y2
      xr = x1
      yr = y1
      s1 = 1.
23003 continue
      xmin = rw(1)
      xmax = rw(2)
      ymin = rw(3)
      ymax = rw(4)
      slope = (yl-yr)/(xl-xr)
      x = max(xl,xmin)
      y = yl+slope*(x-xl)
      xl = x
      yl = y
      x = min(xr,xmax)
      y = yr+slope*(x-xr)
      xr = x
      yr = y
      if(.not.(xr.le.xmin.or.xl.ge.xmax))goto 23004
      area = 0.
      return
23004 continue
      ybot = min(yl,yr)
      ytop = max(yl,yr)
      if(.not.(ymax.le.ybot))goto 23006
      area = (xr-xl)*(ymax-ymin)
      return
23006 continue
      if(.not.(ymin.le.ybot.and.ymax.le.ytop))goto 23008
      call testeq(slope,zero,eps,value)
      if(.not.(value))goto 23010
      w1 = 0.
      w2 = xr-xl
      goto 23011
23010 continue
      xit = xl+(ymax-yl)/slope
      w1 = xit-xl
      w2 = xr-xit
      if(.not.(slope.lt.0.))goto 23012
      tmp = w1
      w1 = w2
      w2 = tmp
23012 continue
23011 continue
      area = 0.5*w1*((ybot-ymin)+(ymax-ymin))+w2*(ymax-ymin)
      return
23008 continue
      if(.not.(ybot.le.ymin.and.ymax.le.ytop))goto 23014
      xit = xl+(ymax-yl)/slope
      xib = xl+(ymin-yl)/slope
      if(.not.(slope.gt.0.))goto 23016
      w1 = xit-xib
      w2 = xr-xit
      goto 23017
23016 continue
      w1 = xib-xit
      w2 = xit-xl
23017 continue
      area = 0.5*w1*(ymax-ymin)+w2*(ymax-ymin)
      return
23014 continue
      if(.not.(ymin.le.ybot.and.ytop.le.ymax))goto 23018
      area = 0.5*(xr-xl)*((ytop-ymin)+(ybot-ymin))
      return
23018 continue
      if(.not.(ybot.le.ymin.and.ymin.le.ytop))goto 23020
      call testeq(slope,zero,eps,value)
      if(.not.(value))goto 23022
      area = 0.
      return
23022 continue
      xib = xl+(ymin-yl)/slope
      if(.not.(slope.gt.0.))goto 23024
      w = xr-xib
      goto 23025
23024 continue
      w = xib-xl
23025 continue
      area = 0.5*w*(ytop-ymin)
      return
23020 continue
      if(.not.(ytop.le.ymin))goto 23026
      area = 0.
      return
23026 continue
      nerror = 8
      return
      end
