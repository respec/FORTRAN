      subroutine peak(peaksu,peaki,iel,ieltmp,icnt)
c
c     + + + PURPOSE + + +
c
c     SR PEAK determines what is feeding the channel (i.e.,
c     hillslope, channel, impoundment) and if peak flow is
c     significant (greater than 1.0e-05 m^3/s), then stores
c     the information in temporary arrays.
c
c     Called from: SR WSHPEK
c     Author(s): Jim Ascough II
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
c     + + + LOCAL VARIABLES + + +
c
      real peaki, peaksu(3)
      integer icnt, iel, ieltmp(3)
c
c     + + + LOCAL DEFINITIONS + + +
c
c     peaksu -
c     icnt   -
c     ieltmp -
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
      if (peaki.gt.1.0e-05) then
        icnt = icnt + 1
        peaksu(icnt) = peaki
        ieltmp(icnt) = iel
      end if
c
      return
      end
