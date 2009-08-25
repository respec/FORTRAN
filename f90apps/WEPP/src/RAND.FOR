      subroutine rand(rnd)
c
c     + + + PURPOSE + + +
c    This subroutine generates random number between 0 and 1 using
c    the multiplicative congruential method. It is called from FN
c    PSIINV to restart at a random value for variable U when function
c    PSI is zero.
c
c     Called from PSIINV.
c     Author(s): Shirley, Stone
c     Reference in User Guide:
c
c     Changes:
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/25/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real rnd
c
c     + + + ARGUMENT DEFINITIONS + + +
c     rnd    - random number between 0 and 1.
c
c     + + + PARAMETERS + + +
c
c     + + + COMMON BLOCKS + + +
      include 'crndm.inc'
c       read: aa,mrnd
c     modify: xrand
c
c     + + + LOCAL VARIABLES + + +
      double precision u
      real v
c
c     + + + LOCAL DEFINITIONS + + +
c     u      - xrand/MRND
c     v      - the whole-number part of U
c
c     + + + SAVES + + +
      save /rndm/
c
c     + + + END SPECIFICATIONS + + +
c
c
c***********************************************************************
c
c     This routine computes random numbers by a multiplicative
c     congruential method:
c
c        xrand = AA*(xrand mod MRND),  RND = xrand/MRND
c
c***********************************************************************
c Note: BGNRND should be used to initialize xrand.  The check made there
c       (that AA*MRND and AA can be represented exactly in xrand and U),
c       ensures that sucessive values of xrand can be calculated exactly.
c***********************************************************************
c
      xrand = aa * xrand
c
c------find xrand mod MRND
      u = xrand / mrnd
      v = aint(u)
cWarning from ftnchek
c     65       v = aint(u)
c     ^
cWarning near line 65 col 9: dble truncated to real
      xrand = xrand - v * mrnd
c
      if (xrand.lt.0.d0) then
        v = v - 1.
        xrand = xrand + mrnd
      end if
c
      rnd = u - v
cWarning from ftnchek
c     73       rnd = u - v
c     ^
cWarning near line 73 col 11: dble truncated to real
c
      return
      end
