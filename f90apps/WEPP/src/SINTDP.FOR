      double precision function sintdp(dptime)
c
c     + + + PURPOSE + + +
c     Computes accumulated rainfall to time T.
c
c     Called from PSIS.
c     Author(s): Shirley, Stone
c     Reference in User Guide:
c
c     Changes:
c         1) Messy GOTO's changed to nested IF's.
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/24/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
c     real time
      double precision dptime
c
c     + + + ARGUMENT DEFINITIONS + + +
c     dptime   - current time
c
c     + + + PARAMETERS + + +
      include 'pmxtim.inc'
      include 'pmxpln.inc'
c
c     + + + COMMON BLOCKS + + +
c
      include 'cintgrl.inc'
c       read: si
c
      include 'cpass1.inc'
c       read: s(mxtime)
      include 'cpass2.inc'
c       read: t(mxtime)
      include 'cpass3.inc'
c       read: ns
c
      include 'cprams1.inc'
c       read: tstar

      real sint
c
c     + + + END SPECIFICATIONS + + +
c
c***************************************************************
c    SINT = integral of S with respect to T, from 0 to TIME.   *
c***************************************************************
c
      if (dptime.ge.tstar) then
        sint = si(ns+1)
      else
        if (dptime.ge.t(ii+1)) then
c         comment from ftnchek 01-12-94 08:35am  sjl
c         Function SINT modifies common variable II
   10     continue
          ii = ii + 1
          if (dptime.ge.t(ii+1)) go to 10
        else if (dptime.lt.t(ii)) then
   20     continue
          ii = ii - 1
          if (dptime.lt.t(ii)) go to 20
        end if
        sintdp = si(ii) + s(ii) * (dptime-t(ii))
      end if
c
      return
      end
