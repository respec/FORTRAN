      real function cross(x1,y1,x2,y2)
c
c     + + + PURPOSE + + +
c
c     Determines the point where two lines cross
c
c     Called from subroutine EROD.
c     Author(s): M. Nearing
c     Reference in User Guide:
c
c     Changes:
c
c     Version: This module recoded from WEPP version 90.92.
c     Date recoded: 01/08/91.
c     Recoded by: Charles R. Meyer.
c
c
c     + + + KEYWORDS + + +
c
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real x1, y1, x2, y2
c
c     + + + ARGUMENT DEFINITIONS + + +
c     x1     -
c     y1     -
c     x2     -
c     y2     -
c
c     + + + LOCAL VARIABLES + + +
      real slope
c
c     slope  -   land slope (%)
c
c     + + + END SPECIFICATIONS + + +
c
c       new changes for overland flow elements
c
      if (x1.ne.x2) then
        slope = (y2-y1) / (x2-x1)
      else
        slope = 1.0e6
      end if
c
      cross = -y1 / slope + x1
c
      return
      end
