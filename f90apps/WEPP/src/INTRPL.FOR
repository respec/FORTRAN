      real function intrpl(x1,y1,x2,y2,x)
c
c     + + + PURPOSE + + +
c
c     Function INTRPL interpolates between columnar table entries.
c
c     Called from: SR TABLE
c     Author(s): Ascough II, R. van der Zweep, V. Lopes
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by: Jim Ascough II
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real x1, y1, x2, y2, x
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     x1 -
c     y1 -
c     x2 -
c     y2 -
c     x  -
c
c     + + + COMMON BLOCKS + + +
c
c     + + + LOCAL VARIABLES + + +
c
c     + + + LOCAL DEFINITIONS + + +
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
cd    Modified by S. Dun, May 24, 2005
cd    intrpl = ((y2-y1)*(x-x1)/(x2-x1)) + y1
      if(x2 .eq. x1) then
          intrpl = (y2-y1)/2
      else
          intrpl = ((y2-y1)*(x-x1)/(x2-x1)) + y1
      endif
cd   End modifying
      return
      end
