      real function phi(time)
c
c     + + + PURPOSE + + +
c     Computes PHI(TIME) values for function PSI. (DEFINED IN HDEPTH)
c     (Since phi(t)=psi(t,0), see subroutine PSIS for comments)
c
c     Called from HDEPTH.
c     Author(s): Shirley, Stone
c     Reference in User Guide:
c
c     Changes:
c         1) Common block STRUCT not used.  It was de-referenced.
c         2) SAVE of all local varaibles deleted.
c         3) Double precision vars. changed to single.
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/24/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real time
c
c     + + + ARGUMENT DEFINITIONS + + +
c     time   - CURRENT TIME
c
c     + + + PARAMETERS + + +
      include 'pmxtim.inc'
      include 'pmxpln.inc'
      include 'pmxhil.inc'
c
c     + + + COMMON BLOCKS + + +
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
c     + + + LOCAL VARIABLES + + +
c     real b
      double precision s1tom, s2tom, b
      integer iu, k
c
c     + + + LOCAL DEFINITIONS + + +
c     b      - cummulative rainfall excess
c     s1tom  - current value of SI**1.5
c     s2tom  - previous value of SI**1.5
c     iu     - index for integration of rainfall excess
c     k      - index to compare with IU
c
c     + + + FUNCTION DECLARATIONS + + +
c     real sint
      double precision sint
c
c     + + + END SPECIFICATIONS + + +
c
c
      if (time.lt.tstar) then
        b = sint(time)
        iu = ii + 1
      else
        b = si(ns+1)
        iu = ns + 1
      end if
c
      k = 2
      phi = 0.0
      s1tom = 0.0
c
      if (k.ne.iu) then
   10   continue
        s2tom = si(k) ** 1.5
        phi = phi + (s2tom-s1tom) / s(k-1)
cWarning from ftnchek
c       79         phi = phi + (s2tom-s1tom) / s(k-1)
c       ^
cWarning near line 79 col 13: dble truncated to real
        s1tom = s2tom
        k = k + 1
        if (k.ne.iu) go to 10
      end if
c
      phi = (phi+(b**1.5-s1tom)/s(k-1)) / m
cWarning from ftnchek
c     85       phi = (phi+(b**1.5-s1tom)/s(k-1)) / m
c     ^
cWarning near line 85 col 11: dble truncated to real
      if (time.gt.tstar) phi = phi + sqrt(b) * (time-tstar)
c
      return
      end
