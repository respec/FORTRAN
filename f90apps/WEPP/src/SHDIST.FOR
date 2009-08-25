      real function shdist(x)
c
c     + + + PURPOSE + + +
c
c     Function SHDIST computes the shear stress distribution
c     if the depth to the non-erodible layer is zero.
c
c     Called from: SR DCAP
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
      real x
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     x -
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
      if (x.ge.0.02) then
        shdist = exp(0.12692-0.51634*alog(x)-0.40825*alog(x)**2-0.03442*
     1      alog(x)**3)
        return
      end if
c
      shdist = 0.13 * x / 0.02
c
      return
      end
