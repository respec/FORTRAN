      subroutine rochek(xks,xsm,depr)
c
c
c     + + + PURPOSE + + +
c     This subroutine checks for runoff from a plane with runon
c     (iuprun(iplane)=1), but for which rainfall excess is zero.
c
c     Called from IRS
c     Author(s): Parker, Stone
c     Reference in User Guide:
c
c     Changes:
c         1) Order of parameters revised: QOUT (1st) deleted; IUPRUN &
c            EFFLEN (2nd & 3rd) moved to last positions.  Corresponding
c            change made in IRS.
c         2) SAVE of all local variables removed.
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 05/08/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxelm.inc'
      include 'pmxpln.inc'
      include 'pmxhil.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      real xks, xsm, depr
c
c     + + + ARGUMENT DEFINITIONS + + +
c     depr   - depression storage of current ofe
c     efflen - length of current OFE or equivalent plane
c     iuprun - flag denoting runon
c     xks    - saturated conductivity
c     xsm    - matric potential
c
c     + + + COMMON BLOCKS + + +
      include 'cefflen.inc'
c      modify: efflen(mxplan)
      include 'crozero.inc'
c     modify: timinf,potinf,fhat
c
      include 'chydrol.inc'
c     modify: remax(mxplan),runoff(maxplan)
c      write: peakro(mxplan)
c
      include 'cdiss11.inc'
c     include 'cdiss1.inc'
c       read: dur
c
      include 'cdiss3.inc'
c       read: p
c
      include 'cdist2.inc'
c       read: slplen(mxplan)
c
      include 'cstruc.inc'
c       read: iplane
c
      include 'cupsfl.inc'
      include 'pmxnsl.inc'
      include 'cwater.inc'
      
      real tstar
c
c
c     + + + END SPECIFICATIONS + + +
c
c**********************************************************************
c                                                                     *
c    TIMINF = maximum of storm duration or runoff duration            *
c    POTINF = potential infiltration (Solve the g-a equation for f)   *
c    FHAT = sum of runoff from upper plane and precip                 *
c                                                                     *
c**********************************************************************
c
      if (iuprun(iplane).ne.0) then
c
c       case 3 or 4 (Refer to subroutine IRS)
c
        timinf = dur
c
c       Calculate by volumes
c       (from stone et al., 1994, asce irrigation and drainage)
c
        tstar = timinf * xks / xsm
c
c       INCLUDE DEPRESSION STORAGE IN POTENTIAL INFILTRATION - JJS 9-94
c
        potinf = (xsm * (tstar+sqrt(2.0*tstar)-.02987*tstar**.7913) +
     1            depr) * slplen(iplane)
cd    Modified by S. Dun, June 04, 2004
cd        fhat   = runoff(iplane-1)*efflen(iplane-1) + p*slplen(iplane)
          fhat   = runoff(iplane-1)*efflen(iplane-1) + (p
     1             -(plaint(iplane)-pintlv(iplane)+resint(iplane)))
     1             *slplen(iplane)
cd    End modifying.
      end if
c
      if (fhat.lt.potinf) then
c
c       Case 4 - no runoff from end of plane.  Estimate how far down
c       the plane the surface water advances (EFFLEN).
c
        efflen(iplane) = slplen(iplane) * fhat / potinf
        if (iplane.ne.nplane) iuprun(iplane+1) = 0
        peakro(iplane) = 0.0
      else
c
c       Case 3 - Estimate peak flow as a ratio of the runoff volume and
c       peak rainfall excess of the upper OFE.
c
        runoff(iplane) = (fhat-potinf)
c
c       Believe the following line is incorrect.  MXPLAN needs to be
c       replaced by NPLANE(IHILL) so that watershed version of WEPP
c       will function properly.  dcf  1/19/93
c       IHILL REMOVED FROM NPLANE  JCA2   8/9/93
c       if (iplane.ne.mxplan) iuprun(iplane+1)=1
c
        if (iplane.ne.nplane) iuprun(iplane+1) = 1
        runoff(iplane) = runoff(iplane) / efflen(iplane)
        remax(iplane) = remax(iplane-1) / runoff(iplane-1) *
     1      runoff(iplane)
      end if
c
      return
      end
