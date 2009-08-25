      real function shear(a,b,c,x)
c
c     + + + PURPOSE + + +
c     Calculates nondimensional shear stress.
c
c     Called from subroutine XCRIT.
c     Author(s): G. Foster
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
c
      real a, b, c, x
c
c     + + + ARGUMENT DEFINITIONS + + +
c     a      - shear stress coefficient for current slope
c              section
c     b      - shear stress coefficient for current slope
c              section
c     c      - shear stress coefficient for current slope
c              section
c     x      - nondimensional distance at which shear stress
c              is to be calculated
c
c     + + + LOCAL VARIABLES + + +
c     value  -
      real value
c
c     + + + END SPECIFICATIONS + + +
c
      value = a * x ** 2 + b * x + c
      if (value.lt.0.0) value = 0.0
      shear = value ** 0.66666667
      if (shear.le.0.0) shear = 0.0001
c
      return
      end
