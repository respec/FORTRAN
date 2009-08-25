      real function psolR(solD,v,w,x,y)
c
c     + + + PURPOSE + + +
c     Computes potential solar radiation on a sloping surface, in cal/cm2/day
c
c     Called from sunmap.
c     Author(s): 
c     Reference: Swift, 1976 "Algorithm for solar radiation on Mountain slopes"
c
c     Date recoded: 11/07/2006.
c     Recoded by: S. Dun
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real solD,v,w,x,y
c
c     + + + ARGUMENT DEFINITIONS + + +
c     solD   - declination of sun
c     v - time offset in hour angle between actual annd equivalent slope
c     w - lattitude of eqivalent slope
c
c     + + + PARAMETERS + + +
c
c     + + + COMMON BLOCKS + + +
c
c
c     + + + LOCAL DEFINITIONS + + +
c
c     + + + FUNCTION DECLARATIONS + + +

c     + + + END SPECIFICATIONS + + +
c
      psolR = (sin(solD) * sin(w) * (x - y) *12.0 / 3.141593)
     1   + (cos(solD) * cos(w) * (sin(x + v) - sin(y + v))
     2   * 12. / 3.141593)
c
      return
      end
