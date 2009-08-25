      subroutine mxint(ival,imin,imax)
c     find min and max of integer values
c
c
c     parameters
c
      integer ival, imin, imax
c
c     find min and max
c
      if (ival.lt.imin) imin = ival
      if (ival.gt.imax) imax = ival
      return
      end
