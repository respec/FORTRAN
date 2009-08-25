      subroutine undflo(factor,expon)
c
c     + + + PURPOSE + + +
c
c     SR UNDFLO protects against numeric underflows and overflows.
c
c     Called from: Functions DEPEQS and DEPEND, SRS CASE12, CASE34,
c                  ENRICH and CONTIN
c
c     Author(s): G. Foster, M. Nearing
c     Reference in user guide:
c
c     Changes:
c
c     Version: SR UNDFLO recoded from Version 90.92
c     Date recoded: 01/08/91
c     Recoded by: Charles R. Meyer
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real factor, expon
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     factor -
c     expon  -
c
c     + + + COMMON BLOCKS + + +
c
c     + + + LOCAL VARIABLES + + +
c
      real exp10, power
c
c     + + + LOCAL DEFINITIONS + + +
c
c     exp10 -
c     power -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
      data power /30.0/
c
c     + + + END SPECIFICATIONS + + +
c
      if (factor.gt.0.0) then
c
        exp10 = expon * alog10(factor)
c
        if (abs(exp10).gt.power) then
          factor = 0.0
          expon = 1.0
        end if
c
      end if
c
      return
      end
