      subroutine triar(x0,y0,x1,y1,x2,y2,area)
      implicit double precision(a-h,o-z)
      area = 0.5*((x1-x0)*(y2-y0)-(x2-x0)*(y1-y0))
      return
      end
