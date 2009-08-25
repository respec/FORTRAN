      subroutine rdat(nowcrp)
c
c     + + + PURPOSE + + +
c     Sets up the input to HDRIVE.  It gets the distance and time
c     location for depth calculations.  Converts ALPHA and rainfall
c     excess rates to internal length, for T from 0 to T(N), for N
c     between 1 and NS+1.
c
c     Called from IRS.
c     Author(s): Stone
c     Reference in User Guide: NONE
c
c     Changes:
c         1) Neither parameter is used (XTIME & NF).  Deleted
c            from RDAT, and it's call in IRS.
c         2) "do 10" & "do 20" loops combined.
c         3) Local variable CHEZYC deleted.
c         4) Since S is a REAL (not double precision), the line:
c                 if (s(i1).eq.0.d0) s(i1) = 1.d-10
c              was changed to:
c                 if (s(i1).eq.0.0) s(i1) = 1.e-10
c              and
c                 si(i) = 0.d0
c              was changed to:
c                 si(i) = 0.0
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 05/08/91.
c     Recoded by: Charles R. Meyer.
c     Adapted to v91.39 by dcf - 9/13/91
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxcrp.inc'
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxslp.inc'
      include 'pmxtim.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nowcrp
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nowcrp - number of current crop
c
c     + + + COMMON BLOCKS + + +
      include 'ccntour.inc'
c       read: conseq(mxcrop,mxplan),cntslp(conseq),rowlen(conseq)
c
      include 'cconsta.inc'
c       read: accgav
c
      include 'cdist2.inc'
c       read: slplen(mxplan)
c
      include 'cffact.inc'
c       read: frcteq(iplane)
c
      include 'cintgrl.inc'
c      write: si(mxhtim+1)
c
      include 'cpass1.inc'
c       read: s(mxtime)
      include 'cpass2.inc'
c       read: t(mxtime)
      include 'cpass3.inc'
c     modify: ns
c      write: len
c
      include 'cprams.inc'
c     modify: alpha(mxplan)
c      write: m,tstar
c
      include 'cslope2.inc'
c       read: avgslp
c
      include 'cstruc.inc'
c       read: iplane
c
c     + + + LOCAL VARIABLES + + +
      integer i, i1, ns2
c
c     + + + LOCAL DEFINITIONS + + +
c     i1     - I minus 1
c     ns2    - NS minus 2
c
c     + + + END SPECIFICATIONS + + +
c
c
c     depth-discharge exponent
c
      m = 1.5
c
c     Get distance and time locations for depth calculations
c
c     use average OFE slope and OFE length if no contours
c
      if (conseq(nowcrp,iplane).eq.0) then
        alpha(iplane) = sqrt(avgslp(iplane)*8.0*accgav/frcteq(iplane))
        len = slplen(iplane)
c
c     else - use contour slope and length
c
      else
        alpha(iplane) = sqrt(cntslp(conseq(nowcrp,iplane))) * 8.0 *
     1      accgav / frcteq(iplane)
        len = rowlen(conseq(nowcrp,iplane))
      end if
c
c
c     XXX -- Is this really NECESSARY? -- CRM -- 5/8/91
      tstar = t(ns)
c
      ns = ns - 1
c
      ns2 = ns + 2
c     4) Since S is a REAL (not double precision), the line:
c     if (s(i1).eq.0.d0) s(i1) = 1.d-10
c     was changed to:
c     if (s(i1).eq.0.0) s(i1) = 1.e-10
c     and
c     si(i) = 0.d0
c     was changed to:
c     si(i) = 0.0
c
c     change #4 above was not made as Jeff Stone stated.  Warnings
c     cropped up because of that.
c     double precision settings for SI have been removed
c     12-16-93 09:18am  sjl
c
c
      si(1) = 0.d0
c     si(1) = 0.0
      do 10 i = 2, mxtime + 1
        i1 = i - 1
        if (i.lt.ns2) then
c
c         Compute SI(N) as the integral of S with respect to T,
c         from 0 to T(N) for N between 1 and NS+1.
c         XXX -- How is S related to T?  Ie, what does the equation
c         look like?  -- CRM 5/8/91
c
c         if (s(i1).eq.0.0) s(i1) = 1.e-10
          if (s(i1).eq.0.d0) s(i1) = 1.d-10
          si(i) = si(i1) + s(i1) * (t(i)-t(i1))
        else
c
c         Rainfall excess preparation for HDRIVE
c
c         4) Since S is a REAL (not double precision), the line:
c         if (s(i1).eq.0.d0) s(i1) = 1.d-10
c         was changed to:
c         if (s(i1).eq.0.0) s(i1) = 1.e-10
c         and
c         si(i) = 0.d0
c         was changed to:
c         si(i) = 0.0
c
c         change #4 above was not made as Jeff Stone stated.  Warnings
c         cropped up because of that.
c         double precision settings for SI have been removed
c         12-16-93 09:18am  sjl
c
          si(i) = 0.d0
c         si(i) = 0.0
        end if
c
   10 continue
c
      return
      end
