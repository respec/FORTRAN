      function shield(reyn)
c
c     + + + PURPOSE + + +
c
c     Function SHIELD generates parameters (shield parameters)
c     by interpolating values from a table (shield diagram)
c     for given Reynolds numbers.
c
c     Called from: SRS TRNCAP, YALIN
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
      real reyn
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     reyn -
c
c     + + + COMMON BLOCKS + + +
c
c     + + + LOCAL VARIABLES + + +
c
      real y(8), r(8), shield, slope, ycr
      integer i
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     y(8)   -
c     r(8)   -
c     shield -
c     slope  -
c     ycr    -
c
c     Integer Variables
c
c     i -
c
c     + + + SAVES + + +
c
      save
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
      data y /0.0772, 0.0579, 0.04, 0.035, 0.034, 0.045, 0.055, 0.057/
      data r /1.0, 2.0, 4.0, 8.0, 12.0, 100.0, 400.0, 1000.0/
c
c     + + + END SPECIFICATIONS + + +
c
c
      if (reyn.lt.r(1)) then
        i = 2
        slope = (alog(y(i))-alog(y(i-1))) / (alog(r(i))-alog(r(i-1)))
        ycr = alog(y(1)) - slope * (alog(r(1))-alog(reyn))
      else if (reyn.gt.r(8)) then
        i = 8
        slope = (alog(y(i))-alog(y(i-1))) / (alog(r(i))-alog(r(i-1)))
        ycr = y(8) + slope * (alog(reyn)-alog(r(8)))
c
      else
c
        do 10 i = 2, 8
c
          if (reyn.ge.r(i-1).and.reyn.le.r(i)) then
            slope = (alog(y(i))-alog(y(i-1))) / (alog(r(i))-
     1          alog(r(i-1)))
            ycr = alog(y(i-1)) + slope * (alog(reyn)-alog(r(i-1)))
            go to 20
          end if
c
   10   continue
c
      end if
c
   20 shield = exp(ycr)
c
      return
      end
