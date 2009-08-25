      subroutine erod(xb,xe,a,b,c,atc,btc,ctc,eata,tauc,theta,phi,ilast,
     1    dl,ldlast,xdbeg,ndep)
c
c     + + + PURPOSE + + +
c     Calculates soil particle detachment in each slope segment
c     (to calculate detachment in each segment of the hillslope).
c
c     Called from subroutine ROUTE.
c     Author: G. Foster, M. Nearing, D. Flanagan
c     Reference in User Guide: Chapter 10
c
c     Changes:  1) Common block DETCOM had been left out of V-90.92.  It
c                  was put back in.
c               2) Common DETCOM should have been saved.  That has been
c                  corrected.
c               3) You can't initialize a common block with a DATA state-
c                  ment.  XX, which SHOULD have been in DETCOM was being
c                  initialized this way.  Initialization in BLKDATA will
c                  be used instead.
c               4) Some redundant ELSE-IF's changed to ELSE's.
c               5) The big "do 30" loop, followed by "50 continue" &
c                  "60 continue", were collectively changed to a set of
c                  "IF-ELSE-ENDIF's" with LOOPFG added to signal when to
c                  break out of the loop.
c               6) Some code above "M2 ELSE" changed, and some deleted,
c                  at Dennis's suggestion.  -- CRM -- 2/22/91
c
c     Version: This module recoded from WEPP version 90.92.
c     Date recoded: 12/21/90 - 1/3/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c
c     + + + ARGUMENT DECLARATIONS + + +
      real xb, xe, a, b, c, eata, tauc, theta, dl, xdbeg, ldlast
      real atc, btc, ctc, phi
      integer ilast, ndep
c
c     + + + ARGUMENT DEFINITIONS + + +
c     xb     - distance at beginning of detachment region
c     xe     - distance at end of detatchment region
c     a      - shear stress equation coefficient
c     b      - shear stress equation coefficient
c     c      - shear stress equation coefficient
c     atc    - transport eq. coef
c     btc    - transport eq. coef
c     ctc    - transport eq. coef
c     eata   - n.d. rill erodibility parameter
c     tauc   - n.d. critical shear stress parameter
c     theta  - n.d. interrill erodibility parameter
c     phi    - n.d. deposition parameter
c     ilast  - counter value for last point where load computed
c     dl     - n.d. deposition rate
c     ldlast - n.d. sediment load calculated at i=ilast
c     xdbeg  - n.d. distance where deposition begins
c     ndep   - flag indicating if deposition begins in
c              detachment segment
c
c     + + + PARAMETERS + + +
c
      include 'pmxpln.inc'
      include 'pmxhil.inc'
c
c     + + + COMMON BLOCKS + + +
      include 'cdetcom.inc'
c      modify: xx, eatax, taucx, shr, dcap
      include 'cdist1.inc'
c       read: xinput(101,mxplan)
      include 'cends1.inc'
c       read: ktrato
      include 'cerdva1.inc'
c     modify: load(100), tc(100)
      include 'cerdva2.inc'
c     modify: detach(100)
      include 'cinfco1.inc'
c       read: qostar
      include 'cstruc.inc'
c       read: iplane
c
c     + + + LOCAL VARIABLES + + +
      real ldtry, ldrat, dx, xlast, tcap, xtry, xfrt, detfrt, dettry,
     1    detlst, ldrat2, tclast, xterm, xtrmtc
      integer ibeg, kflag, i, loopfg, currpt
c
c     ldtry  - n.d. load returned from runge-kutta procedure - it
c              is used as long as load is less than tcap
c     ibeg   - beginning point counter
c     dx     - delta x value sent to Runge-Kutta procedure
c     xlast  - n.d. distance at last point where load was computed
c     dcap   - detachment capacity at a point
c     tcap   - transport capacity at a point
c     xtry   - iterative solution for distance to find point
c              where deposition begins in a detachment segment
c     xfrt   - n.d. distance at front point where deposition is
c              predicted to occur
c     detfrt - ratio at front point used to determine if detach.
c              or deposition conditions occur at x=xfrt
c     dettry - ratio relating sediment load and transport cap.
c              at x=xtry
c     detlst - ratio relating sediment load and transport cap.
c              at last point (x=xlast)
c     ldrat  - ratio relating sed. load and trans. cap. at point
c              used when tran. cap. > 0, and sed. load = 0
c     ldrat2 - ratio relating sed. load and trans. cap. at point
c              used when sediment load is greater than 0
c     tclast - transport capacity at x=xlast
c     kflag  - flag indicating type of hydrologic plane and
c              which ratio type to use
c     loopfg - flag. 1 = exit the L2 & N2 Loops.
c     currpt - number of the current point when L2 Loop exiteD.
c
c     + + + SAVES + + +
      save xfrt, detlst, detfrt
      save /detcom/
c
c     + + + SUBROUTINES CALLED + + +
c     runge
c
c     + + + FUNCTION DECLARATIONS + + +
      real cross
c
c     + + + END SPECIFICATIONS + + +
c
c
      ldrat = 0.0
      ldrat2 = 0.0
      ndep = 0
      ibeg = ilast + 1
c
c     Verify that the beginning point has not exceeded the end of
c     the slope segment.
c
c     *** L1 IF ***
      if (ibeg.lt.102) then
        if (xinput(ibeg,iplane).le.xe) then
c
c         *** Begin L2 Loop ***
c         Note: LOOPFG is set to 1 to force an exit from the loop.
          loopfg = 0
          i = ilast
   10     continue
          i = i + 1
c
c         *** L3 IF ***
          if (xinput(i,iplane).le.xe) then
c
c           Determine the nondimensional horizontal distance between
c           the beginning of the current slope segment and next of the
c           101 points of the OFE.  Also determine transport capacity
c           at the beginning of the slope segment.  [Equation 10.4.6]
            if (i.le.ibeg) then
              dx = xinput(i,iplane) - xb
              xlast = xb
              xterm = a * xb ** 2 + b * xb + c
              xtrmtc = atc * xb ** 2 + btc * xb + ctc
              tclast = xtrmtc * ktrato
              if (tclast.lt.0.0) tclast = 0.0
c
c           For all other points in the detachment segment, set the
c           nondimensional horizontal distance increment to the default
c           of 0.01.  Set distance, and obtain load and transport
c           capacity for the previous point.
            else
              dx = 0.01
              xlast = xinput(i-1,iplane)
              ldlast = load(i-1)
              tclast = tc(i-1)
            end if
c
c           Calculate dimensionless shear stress at the current
c           point.  [Equation 10.4.1]
            xterm = a * xinput(i,iplane) ** 2 + b * xinput(i,iplane) + c
            xtrmtc = atc * xinput(i,iplane) ** 2 + btc *
     1          xinput(i,iplane) + ctc
c
            if (xterm.ne.xx) then
              if (xterm.gt.0.0) then
                shr = exp(0.666667*log(xterm))
              else
                shr = 0.0
              end if
c
              xx = xterm
c
c             Calculate detachment capacity at the current point.
c             [Equations 10.2.3, 10.3.7, 10.3.8, and 10.4.1]
              dcap = eata * (shr-tauc)
              if (dcap.lt.0.0) dcap = 0.0
              eatax = eata
              taucx = tauc
            else if (eatax.ne.eata.or.taucx.ne.tauc) then
              dcap = eata * (shr-tauc)
              if (dcap.lt.0.0) dcap = 0.0
              eatax = eata
              taucx = tauc
            end if
c
c           Calculate dimensionless transport capacity at the current
c           point.  [Equation 10.4.6]
            tcap = xtrmtc * ktrato
            if (tcap.lt.0.0) tcap = 0.0
            tc(i) = tcap
c
c           Check whether on a Case 4 plane past where runoff ends.
c           If past point, set load equal to zero, and flag the plane
c           (kflag = 4).
            if (qostar.gt.-1.0.and.qostar.lt.0.0.and.xinput(i,iplane)
     1          .gt.-qostar) then
              load(i) = 0.0
              kflag = 4
              ndep = 0
c
c           Use Runge-Kutta numerical procedure to solve for sediment
c           load at the current point.  [Page 10.5; Equation 10.3.16]
            else
c
              call runge(a,b,c,atc,btc,ctc,eata,tauc,theta,dx,xlast,
     1            ldlast,load(i))
c
c
c             If transport capacity at the current point is greater
c             than zero, calculate ratio values used to test whether
c             current point is in deposition.
c             KFLG = 1  indicates  TC > 0.
c             KFLG = 2  indicates  LOAD > 0.
              if (tcap.gt.0.0) then
                ldrat = 1.0 - (load(i)/tcap)
                kflag = 1
                detach(i) = dcap * ldrat
c               dtot(i)=detach(i)+theta
                if (load(i).gt.0.0) then
                  ldrat2 = (tcap/load(i)) - 1.0
                  kflag = 2
                end if
c             When transport capacity at the current point <= zero,
c             if load exceeds zero, MUST use LDRAT2 ratio.
              else
                if (load(i).gt.0.0) then
                  ldrat2 = (tcap/load(i)) - 1.0
                  kflag = 2
c               When both load and transport capacity at point are
c               zero (KFLG = 3).
                else
                  load(i) = 0.0
                  kflag = 3
                end if
              end if
            end if
c
c
c           If deposition is predicted at current point, set flag
c           NDEP = 1, and exit detachment calculations.
            if ((kflag.eq.2.and.ldrat2.lt.0.0).or.(kflag.eq.1.and.ldrat
     1          .lt.0.0)) then
              ndep = 1
              loopfg = 1
            else
              ilast = i
            end if
c
            if (xinput(i,iplane).ge.1.0) loopfg = 1
c
c         *** L3 ELSE ***
          else
            loopfg = 1
c
c         *** L3 ENDIF ***
          end if
c
c         If end of segment has not been reached, and have not encountered
c         deposition, go back through the L2 Loop.
c         *** End L2 Loop ***
          if (loopfg.eq.0.and.i.lt.102) go to 10
c         Remember number of current point.
          currpt = i
c
c
c         On the last one of the 101 OFE points in this segment, if deposition
c         is not occurring, compute load at the end of the segment.
c         *** M2 IF ***
          if (ndep.eq.0) then
c
c           On a segment where water flow is present (not a Case 4),
c           use Runge-Kutta solution.  [Page 10.5, Section 10.3.5]
            if (kflag.ne.4) then
              if (xe.ne.xinput(ilast,iplane)) then
                dx = xe - xinput(ilast,iplane)
                call runge(a,b,c,atc,btc,ctc,eata,tauc,theta,dx,
     1              xinput(ilast,iplane),load(ilast),ldlast)
                xlast = xe
              else
                ldlast = load(ilast)
                xlast = xinput(ilast,iplane)
              end if
c
c           On a segment where water flow is not present (Case 4),
c           past where runoff ends, set load to zero and return to
c           ROUTE.
            else
              ldlast = 0.0
              xlast = xe
              dl = 0.0
              return
            end if
c
            xterm = a * xlast ** 2 + b * xlast + c
            xtrmtc = atc * xlast ** 2 + btc * xlast + ctc
c
            if (xterm.ne.xx) then
c
c             Calculate shear stress at end of segment ( X = XE ).
c             [Equation 10.4.1]
              if (xterm.gt.0.0) then
                shr = exp(0.666667*log(xterm))
              else
                shr = 0.0
              end if
c
              xx = xterm
c
c             Calculate detachment capacity at end of segment ( X = XE ).
c             [Equation 10.2.3, and others]
              dcap = eata * (shr-tauc)
              if (dcap.lt.0.0) dcap = 0.0
              eatax = eata
              taucx = tauc
            else if (eatax.ne.eata.or.taucx.ne.tauc) then
              dcap = eata * (shr-tauc)
              if (dcap.lt.0.0) dcap = 0.0
              eatax = eata
              taucx = tauc
            end if
c
c           Calculate transport capacity at end of segment ( X = XE ).
c           [Equation 10.4.6]
            tcap = xtrmtc * ktrato
            if (tcap.lt.0.0) tcap = 0.0
c
c           There is transport capacity.
            if (tcap.gt.0.0) then
              ldrat = 1.0 - (ldlast/tcap)
              dl = dcap * ldrat
              kflag = 1
c
c             If load is less than transport capacity at end of segment
c             (still in detachment condition), return to ROUTE.
              if (ldrat.ge.0.0) return
c
c           Transport capacity is zero.
            else
c
c             If load at end of segment is also zero, return to ROUTE.
              if (ldlast.le.0.0) then
                ldlast = 0.0
                dl = 0.0
                return
              end if
            end if
c
c           Set up the last point (XLAST) and front point (XFRT) ratios
c           used to determine where deposition begins (X = XDBEG).
            ldrat2 = (tcap/ldlast) - 1.0
            kflag = 2
            detfrt = ldrat2
            if (load(ilast).gt.0.0) detlst = (tc(ilast)/load(ilast)) -
     1          1.0
            ndep = 1
            xfrt = xlast
c
c           XXX -- Note from Dennis that this IF-ELSE-ENDIF could also be improved.
c
            if (xinput(ilast,iplane).eq.xfrt) then
              xlast = xinput(ilast-1,iplane)
              if (detfrt.eq.ldrat2) then
                if (load(ilast-1).gt.0.0) detlst = (tc(ilast-1)/
     1              load(ilast-1)) - 1.0
              else
                if (tc(ilast-1).gt.0.0) detlst = 1.0 - (load(ilast-1)/
     1              tc(ilast-1))
              end if
            else
              xlast = xinput(ilast,iplane)
            end if
c
c         On the last of the 101 OFE points on this segment, deposition is
c         occurring.  Compute where deposition begins (X = XDBEG).
c
c         *** M2 ELSE ***
          else
            xfrt = xinput(currpt,iplane)
            if (xlast.le.0.0.and.tclast.le.0.0.and.ldlast.le.0.0) then
              kflag = 5
              detlst = dl
              detfrt = (phi/(phi+1.0)) * (ktrato*(atc*xfrt*xfrt+btc*xfrt
     1            +ctc)-theta)
            end if
            if (kflag.eq.1) then
              detfrt = ldrat
              if (tclast.gt.0.0) then
                detlst = 1.0 - (ldlast/tclast)
              else
                detlst = 0.0
              end if
            else if (kflag.eq.2) then
              detfrt = ldrat2
              if (ldlast.gt.0.0) then
                detlst = (tclast/ldlast) - 1.0
              else
                detlst = 0.0
              end if
            end if
c
c           added 8/1/90 by dcf
c
c           If at top of OFE, and ratios are not positive at both the
c           beginning and end of the segment, say deposition begins at
c           top of OFE.
            if (detfrt.le.0.0.and.detlst.le.0.0.and.xlast.le.0.0) then
              xdbeg = 0.0
              return
            end if
c
c           Prevent a negative value for detachment at the last point.
            if (detlst.lt.0.0) detlst = 0.0
c
c         *** M2 ENDIF ***
          end if
c
c
c         Iterative proceedure to find point where deposition begins
c         (X = XDBEG).
c
c         *** Begin N2 Loop ***
          i = 0
   20     i = i + 1
c         if(xlast.eq.xfrt)xlast=xfrt-.001
c
c         Use CROSS function to solve for point (X = XTRY) where
c         TCAP = LDTRY, or where test ratios equal zero.
          xtry = cross(xlast,detlst,xfrt,detfrt)
          dx = xtry - xlast
c
c         Use Runge-Kutta procedure to estimate load at X = XTRY.
          call runge(a,b,c,atc,btc,ctc,eata,tauc,theta,dx,xlast,ldlast,
     1        ldtry)
          tcap = (atc*xtry**2+btc*xtry+ctc) * ktrato
          if (tcap.lt.0.0) tcap = 0.0
c
          loopfg = 0
          if (kflag.eq.2) then
            if (ldtry.le.0.0) ldtry = 0.00001
            if (abs((tcap-ldtry)/ldtry).lt.0.001) then
              loopfg = 1
            else
              dettry = (tcap/ldtry) - 1.0
            end if
          else if (kflag.eq.1) then
            if (tcap.le.0.0) tcap = 0.00001
            if (abs((ldtry-tcap)/tcap).lt.0.001) then
              loopfg = 1
            else
              dettry = 1.0 - (ldtry/tcap)
            end if
          else if (kflag.eq.5) then
            dettry = (phi/(phi+1.0)) * (tcap-theta)
          end if
c
          if (loopfg.eq.0) then
            if (dettry.le.0.0) then
              detfrt = dettry
              xfrt = xtry
            else
              xlast = xtry
              detlst = dettry
              ldlast = ldtry
            end if
          end if
c
c         *** End N2 Loop ***
          if (i.lt.10.and.loopfg.eq.0) go to 20
c
c
          xdbeg = xtry
          dl = 0.0
          ldlast = ldtry
c
c       *** L1 ENDIF ***
        end if
      end if
c
      return
      end
