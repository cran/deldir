C Output from Public domain Ratfor, version 1.0
      subroutine trigraf(nv, ne, ie, je, nt, it, jt, kt)
      integer scratch(1)
      integer firstedge, lastedge
      dimension ie(1), je(1), it(1), jt(1), kt(1)
      nt = 1
      lastedge = 0
23000 if(lastedge .lt. ne)then
      firstedge = lastedge + 1
      i = ie(firstedge)
      do23002 m = firstedge+1,ne 
      if( ie(m) .ne. i )then
      goto 23003
      endif
23002 continue
23003 continue
      lastedge = m-1
      if(lastedge .gt. firstedge)then
      do23008 mj = firstedge,lastedge-1 
      j = je(mj)
      do23010 mk = firstedge+1,lastedge 
      k = je(mk)
      do23012 m = 1,ne 
      if(ie(m) .ge. j)then
      goto 23013
      endif
23012 continue
23013 continue
23016 if(m .le. ne .and. ie(m) .eq. j)then
      if(je(m) .eq. k)then
      it(nt) = i
      jt(nt) = j
      kt(nt) = k
      nt = nt+1
      endif
      m = m+1
      goto 23016
      endif
23017 continue
23010 continue
23011 continue
23008 continue
23009 continue
      endif
      goto 23000
      endif
23001 continue
      return
      end
