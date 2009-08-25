      real function depc(xu,a,b,phi,theta,du,ktrato,qostar)
c
c     + + + PURPOSE + + +
c     Computes a portion of the deposition equation.
c
c     Called (twice) from subroutine ROUTE.
c     Author(s): G. Foster, M. Nearing
c     Reference in User Guide:
c
c     Changes: 1) Added KTRATO & QOSTAR to argument list and de-referenced
c                 common blocks ENDS1 & INFCO1.
c              2) Since MXPLAN is never used, references to PROUTE1.INC
c                 were deleted.
c
c     Version: This module recoded from WEPP version 90.92.
c     Date recoded: 01/08/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real xu, a, b, phi, theta, du, ktrato, qostar
c
c     + + + ARGUMENT DEFINITIONS + + +
c     xu     - nondimensional distance on an OFE where current
c              deposition region begins
c     a      - "a" shear stress coefficient for current slope
c               section
c     b      - "b" shear stress coefficient for current slope
c              section
c     phi    - nondimensional deposition parameter for current OFE
c     theta  - nondimensional interrill detachment parameter for
c              current OFE
c     du     - deposition rate at xu
c     ktrato -
c     qostar -
c
c     + + + LOCAL VARIABLES + + +
c
c     + + + END SPECIFICATIONS + + +
c
      if (abs(qostar+xu).ge.10e-8) then
        depc = du - (a*ktrato*phi*2.0*(qostar+xu)/(phi+2.0)) - ((b*
     1      ktrato-2.0*a*ktrato*qostar-theta)*phi/(phi+1.0))
      else
        depc = 0.0
      end if
      return
      end
