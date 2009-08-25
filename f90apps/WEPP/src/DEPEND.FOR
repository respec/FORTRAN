      real function depend(xu,xl,a,b,cdep,phi,theta,ktrato,qostar)
c
c     + + + PURPOSE + + +
c     Calculates for a segment of an Overland Flow Element that
c     has deposition at the beginning of the segment - where that
c     deposition ends - using the analytic solution to the
c     deposition equation.
c
c     Called (once) from subroutine ROUTE
c     Author(s): G. Foster, M. Nearing, J. Ascough, D. Flanagan
c     Reference in User Guide:
c
c     Changes: 1) TMPVR1 added to eliminate redundant calculations.
c              2) ITMPV1 added and moved outside the do-loop to reduce
c                 the number of calls to ABS, and to substitute an
c                 integer comparison (.NE.) for a floating-point one.
c              3) KTRATO & QOSTAR added to argument list so that
c                 common blocks ENDS1 & INFCO1 do not need to be ref-
c                 erenced.
c              4) MXPLAN is never used.  Reference to PROUTE1.INC is
c                 deleted.
c
c     Version: This module recoded from WEPP version 90.92.
c     Date recoded: 01/08/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real xu, xl, a, b, cdep, phi, theta, ktrato, qostar
c
c     + + + ARGUMENT DEFINITIONS + + +
c     xu     - n.d. distance at top of slope segment
c     xl     - n.d. distance at end of slope segment
c     a      - shear stress equation coefficient
c     b      - shear stress equation coefficient
c     cdep   - portion of the solution to deposition equation
c     phi    - n.d. deposition parameter
c     theta  - n.d. interrill detachment parameter
c     ktrato -
c     qostar -
c
c     + + + LOCAL VARIABLES + + +
      real xdend, r1, r2, ratio, expon, f, df, xmin, tmpvr1
      integer j, kkkk, itmpv1, loopfg
c
c     xdend  - current trial value for n.d. distance where
c              deposition ends
c     r1     - portion of solution to deposition equation solved
c              for conditions of rate equal to 0 and beginning
c              point equal to x=xu
c     r2     -   "       "          "          "        "
c     ratio  -   "       "          "          "        "
c     expon  -   "       "          "          "        "
c     f      - current solution for deposition rate at x=xdend
c     df     - current solution for derivative of deposition rate
c     j      - counter variable - in loop to only allow a finite
c              number of iterations to determine where dep. ends
c     loopfg - flag. 1 = exit L1 Loop
c
c     + + + SUBROUTINES CALLED + + +
c     undflo
c
c     + + + END SPECIFICATIONS + + +
c
c       Solve for first term of deposition equation
      tmpvr1 = 2.0 * a * ktrato
c
      r1 = (phi/(1.0+phi)) * (b*ktrato-theta-(tmpvr1*qostar))
c
c     Solve for second term of deposition equation
c
      r2 = tmpvr1 * phi / (2.0+phi)
c
c     If flow is increasing down the OFE, check whether deposition
c     at the end of the current segment (uniform or convex shape)
c
      if (qostar.ge.0.0) then
        xdend = xl
        ratio = (xu+qostar) / (xdend+qostar)
        expon = 1.0 + phi
        call undflo(ratio,expon)
c
c       Determine deposition rate at x = xl (end of segment) -
c       if a negative value deposition occurs from xu to xl - RETURN
c
        f = r1 + r2 * (xdend+qostar) + cdep * ratio ** expon
        if (f.lt.0.0) then
          depend = xdend
          return
        end if
c
c       If deposition not occurring at x=xl, set beginning point for
c       iterative solution for xdent close to x=xu
c
        xdend = xu + 0.01
        if (xdend.gt.xl) xdend = (xu+xl) / 2.0
c
c     else - flow is decreasing down the OFE segment, first check
c     to determine that deposition does not immediately end
c     past the initial point ( x = xu ) - then set the
c     beginning point for the iterative solution for xdend
c     close to x = xu
c
      else

c new dcf
        if(abs(xu+qostar).le.0.0001)then
          depend=-qostar
          return
        endif
c new dcf end

        xdend = xu + 0.0001
        if (xdend.gt.xl) xdend = (xu+xl) / 2.0
        ratio = (xu+qostar) / (xdend+qostar)
        expon = 1.0 + phi
        call undflo(ratio,expon)
c
c       determine deposition at x = xdend, and if the value
c       is positive or zero - RETURN
c
        f = r1 + r2 * (xdend+qostar) + cdep * ratio ** expon
        if (f.ge.0.0) then
          depend = xdend
          return
        end if
c
      end if
c
c     *** Begin L1 Loop ***
c
      loopfg = 0
      j = 0
      xmin = xl
      kkkk = 0
   10 continue
      j = j + 1
c
c     ITMPV1 is a flag set if ABS(XDEND + QOSTAR) > 0.0 .  This
c     replacement code is put inside the loop, since XDEND, which
c     it depends on, is supposed to change value each time through
c     the loop. We still save time, since the line of original
c     code above is repeated below.
c
c
c
c     Iterative solution for XDEND
c
c
c     Solve for portions of the deposition equation
c     at the trial value of xdend
c
c     Original Code:
c     if(abs(xdend+qostar).gt.0.0)then
c
c     ITMPV1 is a flag set if ABS(XDEND + QOSTAR) > 0.0 this
c     replacement code is put inside the loop, since XDEND,
c     which it depends on, is supposed to change value each
c     time through the loop. We still save time, since the
c     line of orignial code above is repeated below
c
      tmpvr1 = xdend + qostar
      if (abs(tmpvr1).gt.0.0) then
        itmpv1 = 1
      else
        itmpv1 = 0
      end if
c
      if (itmpv1.ne.0) then
        ratio = (xu+qostar) / tmpvr1
      else
        ratio = 1.0
      end if
      if (ratio.lt.0.0) ratio = 1.0
      expon = 1.0 + phi
      call undflo(ratio,expon)
c
c     Solve the deposition equation at trial value of xdend.
c
      f = r1 + r2 * (xdend+qostar) + cdep * ratio ** expon
c
c     Check added 2/7/91 to flag if any positive values
c     of f have been calculated between xu and xl, and
c     then to record the point on slope where this was found.
c
      if (f.gt.0.0.and.qostar.lt.0.0) then
        kkkk = kkkk + 1
        if (xdend.lt.xmin) xmin = xdend
      end if
c
c     TEST - If the value of nondimensional deposition
c     rate is less than 0.0001, jump out of DO LOOP and
c     use the current value of XDEND as point where
c     deposition predicted to end.
c
      if (abs(f).gt.0.0001) then
c       Original Code:
c       if(abs(xdend+qostar).gt.0.0)then
        if (itmpv1.ne.0) then
c
c         Solve for the derivative of the deposition function
c
          df = r2 - (1.0+phi) * cdep * (ratio**expon) / tmpvr1
          if (abs(df).gt.0.0) then
c
c           Use the derivative to obtain a new trial value for XDEND
c
            xdend = xdend - f / df
c
            if (qostar.lt.0.0) then
              if (xdend.lt.xu) xdend = xu + 0.0001
              if (xdend.gt.-qostar) xdend = -qostar - 0.0001
              if (xdend.gt.xl) xdend = xl
            end if
          else
c
c           If the derivative is zero at the point,
c           restart the iterations with xdend close to xu
c
            xdend = xu + 0.0001
          end if
        end if
        if (xdend.lt.xu) xdend = xu + 0.0001
      else
        loopfg = 1
      end if
      if (j.lt.10.and.loopfg.eq.0) go to 10
c
c     Check if solution has not converged for a plane on which
c     flow is decreasing - if it has not and none of the trial
c     values of xdend between xu and xl (or -qostar) have produced
c     non-negative results - then set xdend equal to xl.
c
      if (loopfg.eq.0.and.qostar.lt.0.0) then
        if (kkkk.eq.0) then
          xdend = xl
        else
          xdend = xmin
        end if
      end if
c
      depend = xdend
c
      return
      end
