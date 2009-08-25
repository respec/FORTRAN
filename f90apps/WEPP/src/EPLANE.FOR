      subroutine eplane(ibpln,iepln,slplen,alpha,m,ealpha)
c
c     + + + PURPOSE + + +
c     Calculates the equivalent depth discharge coefficient for
c     using the equivalent plane method; this is for handling
c     multiple strips down the hillslope.
c
c     ====================================================================
c     *  Note: The only equation that Chezy was used for is:             *
c     *          alphq = (chezy(i)*sqrt(avgslp(i)))**power2              *
c     *                                                                  *
c     *        The equation for Chezy is:                                *
c     *          chezy(i) = alpha(i)/sqrt(avgslp(i))                     *
c     *                                                                  *
c     *        If the two equations are combined into:                   *
c     *          sdst  = sdst + (suml*alpha(i)**power2)/(tmpvr1-tmpvr2)  *
c     *                                                                  *
c     *        CHEZY(I) & AVGSLP(I) are not needed.  CRM 4/11/91         *
c     ====================================================================
c
c     Called from IRS
c     Author(s): Hernandez, Stone
c     Reference in User Guide:
c
c     Changes:
c         1) Local vars. AVS, SUMS, DSTT, CHEZY(20), DFL, SML &
c            POWER1 not used -- removed from code.
c         2) XNPLN, AVGSLP, & REMAX not needed -- removed from
c            parameter list. Corresponding changes made in call
c            from IRS.
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/09/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxpln.inc'
c       read: mxplan
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer ibpln, iepln
      real slplen(mxplan), alpha(mxplan), m, ealpha
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ibpln  - beginning segment (plane) number
c     iepln  - ending segment (plane) number
c     slplen - length of each plane (m)
c     alpha  - alpha for individual segment
c     m      - 3/2
c     ealpha - equivalent alpha for combined segments
c
c     + + + LOCAL VARIABLES + + +
      real suml, power2, power3, cml, tmpvr1, sdst, tmpvr2
      integer i
c
c     + + + LOCAL DEFINITIONS + + +
c     suml   - total length (m) of all the segments
c     power2 - 1./m
c     power3 - (m+1.)/m; ie, power2 + 1.
c     cml    - cumulative length (through current segment in loop)
c     tmpvr1 - temporary storage variable, changed each time through loop.
c     sdst   - WAS average storage at equilibrium (modified during recoding)
c     tmpvr2 - holds value of TMPVR1 previous time through the loop.
c
c     + + + END SPECIFICATIONS + + +
c
      suml = 0.
      do 10 i = ibpln, iepln
        suml = suml + slplen(i)
   10 continue
c
c     Computation of exponents
c
      power2 = 1. / m
      power3 = power2 + 1.
c
      cml = 0.
      sdst = 0.
      tmpvr2 = 0.
      do 20 i = ibpln, iepln
        cml = cml + slplen(i)
        tmpvr1 = cml ** power3
        sdst = sdst + (tmpvr1-tmpvr2) / (alpha(i)**power2)
        tmpvr2 = tmpvr1
   20 continue
c
c     Computation of equivalent alpha
c
      ealpha = (suml/sdst) ** m * suml
c
      return
      end
