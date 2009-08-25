      subroutine rtpart(iplane,rtmasy)
c
c     + + + PURPOSE + + +
c     Partitions total root mass into layers.
c     Called from PTGRA & PTGRP.
c     Author(s):
c     Reference in User Guide:
c
c     Changes:
c
c     Version: This module recoded from WEPP version 91.50.
c     Date recoded: 12/05/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxpln.inc'
      include 'pmxnsl.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer iplane
      real rtmasy
c
c     + + + ARGUMENT DEFINITIONS + + +
c     iplane - current OFE
c     rtmasy - root mass yesterday
c
c     + + + COMMON BLOCKS + + +
      include 'ccrpout.inc'
c        read: rtd(mxplan),rtmass(mxplan)
c      modify: rtm15(mxplan),rtm30(mxplan),rtm60(mxplan)
c
c     + + + LOCAL VARIABLES + + +
      real delt
c
c     + + + LOCAL DEFINITIONS + + +
c     delt   - increase in rootmass from yesterday to today
c
c     + + + END SPECIFICATIONS + + +
c
c
      if (rtd(iplane).le.0.15) then
        rtm15(iplane) = rtmass(iplane)
        rtm30(iplane) = 0.0
        rtm60(iplane) = 0.0
c
      else if (rtd(iplane).gt.0.15.and.rtd(iplane).le.0.30) then
        delt = rtmass(iplane) - rtmasy
        rtm15(iplane) = rtm15(iplane) + delt * 0.6
        rtm30(iplane) = rtm30(iplane) + delt * 0.4
        rtm60(iplane) = 0.0
c
      else if (rtd(iplane).gt.0.30.and.rtd(iplane).le.0.60) then
        delt = rtmass(iplane) - rtmasy
        rtm15(iplane) = rtm15(iplane) + delt * 0.45
        rtm30(iplane) = rtm30(iplane) + delt * 0.30
        rtm60(iplane) = rtm60(iplane) + delt * 0.25
c
      else
        delt = rtmass(iplane) - rtmasy
        rtm15(iplane) = rtm15(iplane) + delt * 0.42
        rtm30(iplane) = rtm30(iplane) + delt * 0.28
        rtm60(iplane) = rtm60(iplane) + delt * 0.20
c
c     rtm60(iplane)=rtm60(iplane)+delt*0.20
c     -- XXX -- Shouldn't the above fractions add to '1.00' ? -- CRM -- 12/5/91
c     -  NOTE - Changed 0.20 to 0.30 for rtm60    dcf  5/25/93
c     Changed back - after discussions with Arnold and Alberts - the extra
c     10% is assumed to go to the root mass below 60 centimeters  dcf  11/18/93
c
      end if
c
      return
      end
