      subroutine perc(vv,k1)
c
c     + + + PURPOSE + + +
c     Returns percolation; ie, seepage (SEP) from the bottom of the
c     current soil layer (into the layer below) when field capacity
c     in the current layer is exceeded.  Correction is made for
c     saturation of the layer below.
c
c  Note:  There are inconsistencies in the array subscripts.
c         Some include IPLANE (OFE number) and J1 (soil layer).
c         Others just include a dimension for J1.  Computationally
c         it makes no difference, since an entire OFE is processed
c         before another is started.
c
c
c     Called from PURK
c     Author(s): Williams, Savabi, Meyer
c     Reference in User Guide:
c
c     Changes:
c           1) Common block UPDATE not used.  Dereferenced.
c           2) Global SAVE of all local variables deleted.
c           3) The function:
c                 funzz=.92283-.4936*zz+.048187*zz**2
c              was put in place to approximate the function:
c                 funzz=exp(-zz)
c              since it executes quicker.  Unfortunately, it is
c              is not a very good approximation over the desired
c              range (0 < zz <= 10).  Therefore, the original
c              equation was put back.  (See note below.)
c              *** THIS SLIGHTLY CHANGES THE OUTPUT. ***
c           4) The line:
c                 sup=sup-sep(iplane)
c              was removed with no difference in output.  (It was the
c              last statement executed before the RETURN, and SUP is
c              an unsaved local variable.
c           5) PERC was changed to receive the parameter VV in lieu of
c              getting SU from a common block.  This is to make the
c              code easier to follow, etc.  J1 was changed into para-
c              meter K1 for the same reason.  SU, J1, & J2 SHOULD BE
c              REMOVED FROM COMMON BLOCK 'WATER'.
c           6) The percent saturation of the current layer (STZ)
c              was being calculated with a water content (ST) which
c              included the water excess (VV).  The percent saturation
c              for the layer below (STU) was being calculated with a
c              water content that had VV subtracted.  This was changed
c              so that STU was calculated like STZ (with VV included)
c              (Conversation with Savabi 9/17/91.)
c           7) Variable CR moved out of computation of ZZ, and into
c              computation of SEP(IPLANE).  (Conversation with
c              Savabi 9/18/91.)
c
c   Note:  Two approximations to the equation: Y = exp(-X) were
c          derived, one second order and one third order.  The
c          second order is a pretty good match to the original
c          for values of X between 0 and 2.  Its values overshoot,
c          so by the time X reaches 10, its y-value is 200X too
c          large. The third order equation is a reasonable match
c          for values of X from 0 to 3.  It overshoots much less.
c          At X=10, it is (only) 100X too large.  Both approxi-
c          mations are measurably faster than their exponential
c          counterpart.  The equations are:
c           Y = 1/(x**2 + 0.8*x + 1.0) and
c           Y = 1/(0.12*x**3 + x**2 + 0.7*x + 1.0)
c                 CRM -- 8/27/91
c
c     Version: This module originally recoded from WEPP version 91.20.
c     Date recoded: 08/22/91 - 09/18/91.
c     Version: This module updated from WEPP release version 91.50.
c     Date updated: 12/03/91.
c     Recoded by: Charles R. Meyer.
c
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxhil.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      real vv
      integer k1
c
c     + + + ARGUMENT DEFINITIONS + + +
c     vv     - Water excess in the current layer.
c     k1     - Index of current soil layer.
c
c     + + + COMMON BLOCKS + + +
      include 'cstruc.inc'
c       read: iplane
c
      include 'cwater.inc'
c       read: st(mxnsl,mxplan),ul(mxnsl,mxplan),ssc(mxnsl,mxplan),hk(mxnsl)
c      write: sep(mxplan)
c
      include 'ctemp.inc'
c       read: slflag,rockn
c
      include  'cwint.inc'
c       read:  snodpt(iplane),tfrdp(mxplan),tthawd(mxplan),frdp(mxplan),
c              thdp(mxplan),densg(mxplan)
c
c     + + + LOCAL VARIABLES + + +
      real stz, fx, stu, cr, zz, funzz, sscz,vartm1,vartm2
c
c     + + + LOCAL DEFINITIONS + + +
c     stz    - percent saturation (expressed as a fraction)
c     fx     - correction factor for sat. hyd. cond. for unsat. soil
c              (equation 7.4.3)
c     stu    - percent saturation (fraction) of lower layer
c     cr     - correction factor for lower layer saturation
c              (equation 7.4.5)
c     zz     - travel time of water through the layer (days)
c              (a part of equation 7.4.19 -delta t/ti)
c     funzz  - a part of equation 7.4.1, 1-exp(-delta t/ti) but
c              linear form
c
c     + + + END SPECIFICATIONS + + +
c
c
c      Compute percent saturation (fraction) in the current layer.
c
cd    Modified by S. Dun, March 04, 2008 for forzen soil
cd          stz = st(k1,iplane) / ul(k1,iplane)
      stz = (st(k1,iplane) + frzw(k1,iplane)) / ul(k1,iplane)
      if (stz.lt.0.95) then
        fx = stz ** hk(k1)
        if (fx.lt.0.002) fx = 0.002
      else
        fx = 1.
      end if
c
c     Adjust the percolation rate for the saturation of the soil
c     layer below the current one.  (Chapter 7, equation 7.4.3)
c
c     Compute percent saturation (fraction) in the layer below.
      if (k1.lt.nsl(iplane)) then
cd          stu = st(k1+1,iplane) / ul(k1+1,iplane)
          stu = (st(k1+1,iplane) + frzw(k1+1,iplane)) / ul(k1+1,iplane)
          if (stu.ge.0.95) stu = 0.95
      else
        stu = 0.
      end if
c
cx    Added by Arthur. Incorperated by S. Dun Dec. 03, 2003
cx     Modificaitons are made to simulate the bottom rock layer. 
cx     A specific K value is assigned to the bottom layer here.
cx     Arthur C.Xu Dec 12, 2000
cd    S. Dun changed K1 to K1+1
      if (k1.eq.nsl(iplane))then
        ssc(k1+1,iplane)= ssc(k1,iplane)
            if (slflag(iplane).eq.0)  ssc(k1+1,iplane)= ssc(k1,iplane)
            if (slflag(iplane).eq.1)  ssc(k1+1,iplane)= kslast(iplane)
      end if
cx     END OF ADDING
cx               
c     *** L0 IF ***     
      if (stu.lt.1.0) then
c       Correct for lower level saturation.  (Chapter 7, eq. 7.4.5)
                cr = sqrt(1.-stu)
c       Travel time of water (days) through the layer
cd    Modified by DSH 09/19/2002
        if(k1.eq.nsl(iplane)) then
           if(slflag(iplane).eq.0) ssc(k1+1,iplane)=ssc(k1,iplane)
            sscz = ssc(k1,iplane)*2/(ssc(k1,iplane)+ssc(k1+1,iplane))
     1                *ssc(k1+1,iplane)
        else
           sscz = ssc(k1,iplane)
cd         Added by S. Dun, Dec 01, 2007
c          for infiltration on frozen soil
           if ((frdp(iplane).gt.0.0).and.(sscv(k1,iplane).gt.0.0)) then
               sscz = sscv(k1,iplane)
           endif
cd         end adding            
        endif
        zz = 86400. * fx * sscz / vv
cd        zz = 86400. * fx * ssc(k1,iplane) / vv
cd    End Modifying.
c
        if (zz.le.10.) then
c         Note: For positive values of ZZ, FUNZZ starts at 1.0, and
c         approaches a lower limit of zero at positive infinity.
c
c         (Chapter 7, equation 7.4.1)
          funzz = exp(-zz)
c         funzz = 1.0/(zz**2 + 0.8*zz + 1.0)
cC        funzz = 1.0/(0.12*zz**3 + zz**2 + 0.7*zz + 1.0)
c
          sep(iplane) = vv * (1.0-funzz) * cr
        else
c         If time > 10 days, FUNZZ approaches zero.
          sep(iplane) = vv * cr
        end if
c
c     If lower level is saturated, there is no seepage
c     from the current level....
c     *** L0 ELSE ***
      else
        sep(iplane) = 0.0
c
c     *** L0 ENDIF ***
      end if
c
      return
      end
