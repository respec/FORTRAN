
c     begin include file cimmon.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /immon/ are common to impoundment
c     element user interface and the impoundment element main
c     program and refer to variables needed to determine the
c     monthly sums and maximums.

      common /immon/ vim(mximp), qiinm(mximp), qomxm(mximp),
     1    hmaxm(mximp), totinm(mximp), clinm(mximp), slinm(mximp),
     1    sainm(mximp), sdinm(mximp), lainm(mximp), coutm(mximp),
     1    cloutm(mximp), sloutm(mximp), saoutm(mximp), sdoutm(mximp),
     1    laoutm(mximp), retm(mximp), clretm(mximp), slretm(mximp),
     1    saretm(mximp), sdretm(mximp), laretm(mximp), cpeakm(mximp),
     1    cpkim(mximp), cam(mximp), vom(mximp), tem(mximp),
     1    cainm(mximp), coutms(mximp), clotms(mximp), slotms(mximp),
     1    saotms(mximp), sdotms(mximp), laotms(mximp)

c     + + + VARIABLE DECLARATIONS + + +

      real vim, qiinm, qomxm, hmaxm, totinm, clinm, slinm,
     1    sainm, sdinm, lainm, coutm, cloutm, sloutm, saoutm, sdoutm,
     1    laoutm, retm, clretm, slretm, saretm, sdretm, laretm,
     1    cpeakm, cpkim, cam, vom, tem, cainm, coutms, clotms, slotms,
     1    saotms, sdotms, laotms

c     + + + VARIABLE DEFINITIONS + + +

c     vim(mximp)    : inflow volume for the month (m^3)
c     qiinm(mximp)  : peak inflow rate for the month (m^3/s)
c     qomxm(mximp)  : peak outflow rate for the month (m^3/s)
c     hmaxm(mximp)  : maximum stage for the month (m)
c     totinm(mximp) : total incoming sediment for the month (kgs)
c     clinm(mximp)  : total incoming clay for the month (kgs)
c     slinm(mximp)  : total incoming silt for the month (kgs)
c     sainm(mximp)  : total incoming small agg for the month (kgs)
c     sdinm(mximp)  : total incoming sand for the month (kgs)
c     lainm(mximp)  : total incoming lg agg for the month (kgs)
c     coutm(mximp)  : total outgoing sediment for the month (kgs)
c     coutms(msimp) : total outgoing sediment for the month (kgs)
c                     (not reset until written in monchn.for)
c     cloutm(mximp) : total outgoing clay for the month (kgs)
c     clotms(mximp): total outgoing clay for the month (kgs)
c                     (not reset until written in monchn.for)
c     sloutm(mximp) : total outgoing silt for the month (kgs)
c     slotms(mximp): total outgoing silt for the month (kgs)
c                     (not reset until written in monchn.for)
c     saoutm(mximp) : total outgoing sm agg for the month (kgs)
c     saotms(mximp): total outgoing sm agg for the month (kgs)
c                     (not reset until written in monchn.for)
c     sdoutm(mximp) : total outgoing sand for the month (kgs)
c     sdotms(mximp): total outgoing sand for the month (kgs)
c                     (not reset until written in monchn.for)
c     laoutm(mximp) : total outgoing lg agg for the month (kgs)
c     laotms(mximp): total outgoing lg agg for the month (kgs)
c                     (not reset until written in monchn.for)
c     retm(mximp)   : total sediment retained in the month (kgs)
c     clretm(mximp) : total clay retained in the month (kgs)
c     slretm(mximp) : total silt retained in the month (kgs)
c     saretm(mximp) : total sm agg retained in the month (kgs)
c     sdretm(mximp) : total sand retained in the month (kgs)
c     laretm(mximp) : total lg agg retained in the month (kgs)
c     cpeakm(mximp) : peak outflow concentration for the month (mg/l)
c     cpkim(mximp)  : peak inflow concentration for the month (mg/l)
c     cam(mximp)    : average outflow concentration for the month (mg/l)
c     vom(mximp)    : total outflow volume for the month (m^3)
c     tem(mximp)    : impoundment trapping efficiency for the month (%)
c     cainm(mximp)  : average inflow concentration for the month (mg/l)

c     end include file cimmon.inc



