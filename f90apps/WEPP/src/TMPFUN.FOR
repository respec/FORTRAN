      real function tmpfun(tpshf)
c
c     + + + PURPOSE + + +
c
c     Function TMPFUN computes mean square error of the temperature for a given phase shift value.
c
c     Called from: SRS TMPCFT
c     Author(s): Shuhui Dun, WSU
c     Verified by: Joan Wu, WSU
c     Reference in User Guide:
c
c     Version:
c     Date recoded: January 07, 2008
c     Recoded by: 
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real tpshf
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     tpshf - phase shift in the sin curve, day
c
c     + + + COMMON BLOCKS + + +
c
      include 'ctcurv.inc'
c     Read: YavgT,YampT,obavgT
c
c     + + + LOCAL VARIABLES + + +
c
      integer i
      real tday, esavgT,sqer
c
c     + + + LOCAL DEFINITIONS + + +
c
c      tday - day in a year
c     esavgT - estimated monthly average temperature
c     sqer - total square error
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
      sqer = 0.
c
      Do 10 i = 1, 12
         tday = 15 + (i-1)* 30.5         
         esavgT = YavgT + YampT * sin(2*3.14/365. *(tday-tpshf))
c
         sqer = sqer + (esavgT - obavgt(i))**2
10    continue
c
      tmpfun = (sqer/12.)**0.5        
c
      return
      end
