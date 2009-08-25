      subroutine runge(a,b,c,atc,btc,ctc,eata,tauc,theta,dx,x,ldold,
     1    ldnew)
c
c     + + + PURPOSE + + +
c     Performs the Runge-Kutta iteration.
c
c     Called from subroutine EROD.
c
c     Author(s): G. Foster, M. Nearing
c     Reference in User Guide:
c
c     Changes:  1) Common block DETCOM had been left out of V-90.92.  It
c                  was put back in.
c               2) DCAP was being "saved".  ALL of common block DETCOM
c                  SHOULD have been saved.  That has been corrected.
c               3) You can't initialize a common block with a DATA state-
c                  ment.  The variables that SHOULD have been in DETCOM
c                  were being initialized this way.  BLOCK DATA will be
c                  used instead.
c               4) LDOLD & LDNEW were declared to be "real" down with
c                  some of the local variables.  The remaining arguments
c                  were never explicitly declared.  Made that declara-
c                  tion "up" in the "argument declarations".
c               5) MXPLAN is not used.  PROUTE1.INC de-referenced.
c
c     Version: This module recoded from WEPP version 90.92.
c     Date recoded: 12/21/90 - 1/4/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c
c     + + + ARGUMENT DECLARATIONS + + +
      real atc, btc, ctc, a, b, c, eata, tauc, theta, dx, x, ldold,
     1     ldnew
c
c
c     + + + ARGUMENT DEFINITIONS + + +
c     a      - shear stress equation coefficient
c     b      - shear stress equation coefficient
c     c      - shear stress equation coefficient
c     eata   - n.d. rill erodibility parameter
c     tauc   - n.d. critical shear stress parameter
c     theta  - n.d. interrill erodibility parameter
c     dx     -
c     x      -
c     ldold  - n.d. sediment load calculated at i=ilast
c              detachment segment
c     ldnew  -
c
c     + + + COMMON BLOCKS + + +
      include 'cdetcom.inc'
c      modify: xx, eatax, taucx, shr, dcap
      include 'cends1.inc'
c       read: ktrato
c
c     + + + SAVES + + +
      save /detcom/
c
c     + + + LOCAL VARIABLES + + +
      real ldrk, k1, k2, k3, k4, tmpvr, tcap, xrk,
     1     xterm, xtrmtc,ldtest
c     xrk   -
c     ldrk  -
c     tcap  - uniform slope
c     k1    - \
c     k2    -  \
c     k3    -   > Constants used to compute LDNEW
c     k4    -  /
c
c     + + + END SPECIFICATIONS + + +
c
c        *** Compute K1 ***
      xrk = x
      ldrk = ldold
c
      xterm = a * xrk ** 2 + b * xrk + c
      xtrmtc = atc * xrk ** 2 + btc * xrk + ctc
c
      if (xterm.ne.xx) then
c       Update SHR and save XTERM.
        if (xterm.gt.0.0) then
          shr = exp(0.666667*log(xterm))
        else
          shr = 0.0
        end if
        xx = xterm
c       Update DCAP and save EATA & TAUC.
        dcap = eata * (shr-tauc)
        if (dcap.lt.0.0) dcap = 0.0
        eatax = eata
        taucx = tauc
c
      else if (eatax.ne.eata.or.taucx.ne.tauc) then
c       Update DCAP and save EATA & TAUC.
        dcap = eata * (shr-tauc)
        if (dcap.lt.0.0) dcap = 0.0
        eatax = eata
        taucx = tauc
      end if
c
      tcap = xtrmtc * ktrato
      if (tcap.lt.0.0) tcap = 0.0
c
c     if(tcap.ne.0.0) then
      if (tcap.gt.0.0) then
        tmpvr = dcap * ((tcap-ldrk)/tcap) + theta
      else
        tmpvr = theta
      end if
c
      k1 = dx * tmpvr
c
c     *** Compute K2 ***
      xrk = x + dx / 2.0
      ldrk = ldold + 0.5 * k1
c
      xterm = a * xrk ** 2 + b * xrk + c
      xtrmtc = atc * xrk ** 2 + btc * xrk + ctc
c
      if (xterm.ne.xx) then
c       Update SHR and save XTERM.
        if (xterm.gt.0.0) then
          shr = exp(0.666667*log(xterm))
        else
          shr = 0.0
        end if
        xx = xterm
c       Update DCAP and save EATA & TAUC.
        dcap = eata * (shr-tauc)
        if (dcap.lt.0.0) dcap = 0.0
        eatax = eata
        taucx = tauc
c
      else if (eatax.ne.eata.or.taucx.ne.tauc) then
c       Update DCAP and save EATA & TAUC.
        dcap = eata * (shr-tauc)
        if (dcap.lt.0.0) dcap = 0.0
        eatax = eata
        taucx = tauc
      end if
c
      tcap = xtrmtc * ktrato
      if (tcap.lt.0.0) tcap = 0.0
c
c     if(tcap.ne.0.0) then
      if (tcap.gt.0.0) then
        tmpvr = dcap * ((tcap-ldrk)/tcap) + theta
      else
        tmpvr = theta
      end if
c
      k2 = dx * tmpvr
c
c     *** Compute K3 ***
      ldrk = ldold + 0.5 * k2
c
c     if(tcap.ne.0.0) then
      if (tcap.gt.0.0) then
        tmpvr = dcap * ((tcap-ldrk)/tcap) + theta
      else
        tmpvr = theta
      end if
c
      k3 = dx * tmpvr
c
c     *** Compute K4 ***
      xrk = x + dx
      ldrk = ldold + k3
c
      xterm = a * xrk ** 2 + b * xrk + c
      xtrmtc = atc * xrk ** 2 + btc * xrk + ctc
c
      if (xterm.ne.xx) then
c       Update SHR and save XTERM.
        if (xterm.gt.0.0) then
          shr = exp(0.666667*log(xterm))
        else
          shr = 0.0
        end if
        xx = xterm
c       Update DCAP and save EATA & TAUC.
        dcap = eata * (shr-tauc)
        if (dcap.lt.0.0) dcap = 0.0
        eatax = eata
        taucx = tauc
c
      else if (eatax.ne.eata.or.taucx.ne.tauc) then
c       Update DCAP and save EATA & TAUC.
        dcap = eata * (shr-tauc)
        if (dcap.lt.0.0) dcap = 0.0
        eatax = eata
        taucx = tauc
      end if
c
      tcap = xtrmtc * ktrato
c
c     if(tcap.ne.0.0) then
      if (tcap.gt.0.0) then
        tmpvr = dcap * ((tcap-ldrk)/tcap) + theta
      else
        tmpvr = theta
      end if
c
      k4 = dx * tmpvr
      ldnew = ldold + (k1+2.0*k2+2.0*k3+k4) / 6.0
c
c     Add check to prevent use of negative sediment loads if
c     they are predicted by Runge-Kutta method because the
c     step size (x dimension) is too large.  A better solution
c     is needed in which the routine will check and redo the
c     computations using a smaller "dx" value, should negative
c     values of the "k1", "k2", "k3", and "k4" test loads be
c     computed.  For now, check to see if the new load predicted
c     is less than the old load plus the interrill contribution
c     across the "dx" distance.  If it is, set the new load equal
c     to this sum.      dcf  4/4/2000
c
      ldtest = ldold + theta*dx
      if(ldnew.lt.ldtest)then
        ldnew = ldtest
      endif

      return
      end
