      subroutine root(a,b,c,x1,x2)
c
c     + + + PURPOSE + + +
c     Finds roots for the equation y=a*x**2+b*x+c
c
c     Called from subroutine XCRIT.
c     Author(s): G. Foster, M. Nearing
c     Reference in User Guide:
c
c     Changes:
c
c     Version: This module recoded from WEPP version 90.92.
c     Date recoded: 01/08/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real a, b
      double precision c, x1, x2
c
c     a    -
c     b    -
c     c    -
c     x1   -
c     x2   -
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + LOCAL VARIABLES + + +
      double precision cc, part
      real b1, tmpvr1
c     cc     -
c     part   -
c
c     + + + END SPECIFICATIONS + + +
c
      b1 = -b
      tmpvr1 = 2.0 * a
      cc = -c
      part = sqrt(b**2-4.0*a*cc)
      x1 = (b1-part) / tmpvr1
      x2 = (b1+part) / tmpvr1
c
      if (x1.gt.x2) then
        part = x2
        x2 = x1
        x1 = part
      end if
c
      return
      end
