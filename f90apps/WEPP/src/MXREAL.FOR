      subroutine mxreal(rval,rmin,rmax)
c     find min and max of real values
c
c     parameters
c
      real rval, rmin, rmax
c
c     find min and max
c
      if (rval.lt.rmin) rmin = rval
      if (rval.gt.rmax) rmax = rval
      return
      end
