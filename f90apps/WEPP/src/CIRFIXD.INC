
c     begin include file cirfixd.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /irfixd/ are common to all irrigation
c     systems but specifically for fixed date irrigation scheduling.

      common /irfixd/   irday(mxplan),iryr(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           irday,iryr

c     + + + VARIABLE DEFINITIONS + + +

c     irday(mxplan) : julian date of the fixed date irrigation events
c     iryr(mxplan)  : year for which corresponding irday applies
c                     (relative value ... 1 for first simulation year,
c                     etc.)

c     end include file cirfixd.inc
