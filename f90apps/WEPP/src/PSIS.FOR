      subroutine psis(time,uu)
c
c     + + + PURPOSE + + +
c     Computes function PSI, and its derivative DPSI.
c     Results are returned in common block PSIS.
c
c     Called from PSIINV.
c     Author(s): Shirley, Stone
c     Reference in User Guide:
c
c     Changes:
c         1) Local variable XU removed.
c         2) SAVE, which saves all applicable local variables, removed.
c         3) Argument UU changed from double precision to single.
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/25/91 - 05/08/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real time
      double precision uu
c
c     + + + ARGUMENT DEFINITIONS + + +
c     time   - current time
c     uu     - value returned from SINT
c
c     + + + PARAMETERS + + +
      include 'pmxtim.inc'
      include 'pmxpln.inc'
c
c     + + + COMMON BLOCKS + + +
      include 'cconsts.inc'
c       read: a2
c
      include 'cintgrl.inc'
c       read: si
c
      include 'cpass1.inc'
c       read: s(mxtime)
      include 'cpass3.inc'
c       read: ns
c
      include 'cprams1.inc'
c       read: tstar
c
      include 'cpsis1.inc'
c     modify: psi,dpsi
c
c     + + + LOCAL VARIABLES + + +
      double precision s1, s2, s1toa2, s2toa2, a, b, dptime
      integer iu, k, il
c
c     + + + LOCAL DEFINITIONS + + +
c     s1     - current value of cummulative rainfall excess
c     s2     - previous value of cummulative rainfall excess
c     s1toa2 - S1 to the A2 power
c     s2toa2 - S2 to the A2 power
c     a      - value returned from SINT
c     b      - rainfall excess of current time
c     iu     - index to track integration
c     k      - index to track integration
c     il     - "saved" value of K
c
c     + + + FUNCTION DECLARATIONS + + +
      double precision sintdp
c
c     + + + END SPECIFICATIONS + + +
c
      if (time.lt.tstar) then
        dptime=time
        b = sintdp(dptime)
        iu = ii + 1
      else
        b = si(ns+1)
        iu = ns + 1
      end if
      a = sintdp(uu)
      k = ii + 1
      il = k
      psi = 0.d0
      dpsi = 0.d0
      s1 = 0.d0
      s1toa2 = 0.d0
c
      if (k.ne.iu) then
   10   continue
c
c       Continue to integrate until the characteristic
c       distance equals the length of the OFE.
c
        s2 = max(si(k)-a,0.d0)
        s2toa2 = s2 ** a2
        psi = psi + (s2*s2toa2-s1*s1toa2) / s(k-1)
        dpsi = dpsi + (s2toa2-s1toa2) / s(k-1)
        k = k + 1
        s1 = s2
        s1toa2 = s2toa2
        if (k.ne.iu) go to 10
      end if
c
      s2 = max(b-a,0.d0)
      s2toa2 = s2 ** a2
      psi = (psi+(s2*s2toa2-s1*s1toa2)/s(k-1)) / m
      dpsi = -s(il-1) * (dpsi+(s2toa2-s1toa2)/s(k-1))
c
c     Continue to integrate if end of rainfall excess
c
      if (time.gt.tstar.and.s2.ne.0.) then
        psi = psi + s2toa2 * (time-tstar)
        dpsi = dpsi - s(il-1) * a2 * s2toa2 / s2 * (time-tstar)
      end if
c
      return
      end
