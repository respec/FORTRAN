      real function psiinv(time,x)
c
c     + + + PURPOSE + + +
c     Computes the inverse of function PSI.  Since PSI is convex for
c     fixed time, a simple application of Newton's method will suffice
c     to invert PSI(TIME,U)=X. If time is changed by a small amount
c     between calls, the old PSIINV value provides a good starting
c     point for the next call.  Subroutine PSIS is used to compute
c     both PSI, and its derivative DPSI.  These are returned in common
c     block PSIS1.
c
c     Called from HDEPTH.
c     Author(s): Shirley, Stone
c     Reference in User Guide:
c
c     Changes:
c          1) Common block PASS not used.  It was de-referenced.
c          2) Double precison variables converted to single precision.
c          3) GOTO's changed to IF-THEN-ELSE's.
c          4) U now in common block cpsis1.inc jca2  8/31/93
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/25/91 - 04/26/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real time, x
c
c     + + + ARGUMENT DEFINITIONS + + +
c     time   - current time
c     x      - length of equivalent plane
c
c     + + + PARAMETERS + + +
      include 'pmxtim.inc'
      include 'pmxpln.inc'
c
c     + + + COMMON BLOCKS + + +
c
      include 'cpsis1.inc'
c       read: psi,dpsi
c
      include 'cprams1.inc'
c       read: tstar
c
c     + + + LOCAL VARIABLES + + +
      real to, teco, rnd
      double precision um, ub, ut
      integer loopfg
c
c     + + + LOCAL DEFINITIONS + + +
c --- XXX Next 2 definitions unclear -- CRM 6/3/91
c     to     - lower limit of integration
c     teco   - current time or end of rainfall, whichever is less
c              (upper limit of integration)
c     rnd    - returned random number
c     ub     - beginning of integration time step
c     ut     - end of integration time step
c     um     - used in bisection method
c     loopfg - Flag. If equal '1', exit loop.
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c     psis
c     rand
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
      to = 0.0
      ub = to
      teco = min(time,tstar)
      ut = teco
      if (u.le.to.or.teco.le.u) u = (to+teco) / 2.0
c
      loopfg = 0
   10 continue
      call psis(time,u)
      if (abs(psi-x).ge.0.005*x) then
        if (dpsi.eq.0.d0) then
          call rand(rnd)
          u = ub + (ut-ub) * rnd / 2.0
        else
          um = u - (psi-x) / dpsi
          if (um.lt.u) then
            ut = u
          else
            ub = u
          end if
          if ((ut-ub).ge.0.00001d0) then
c
            if (um.le.ub) then
              u = (ub+u) / 2.0
            else if (um.lt.ut) then
              u = um
            else
              u = (ut+u) / 2.0
            end if
          else
            loopfg = 1
          end if
c
        end if
      else
        loopfg = 1
      end if
      if (loopfg.eq.0) go to 10
c
      psiinv = u
cWarning from ftnchek
c     112       psiinv = u
c     ^
cWarning near line 112 col 14: dble truncated to real
cFunction PSIINV may modify argument TIME
cFunction PSIINV modifies common variable U
c
      return
      end
