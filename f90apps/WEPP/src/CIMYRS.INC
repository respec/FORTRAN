
c     begin include file cimyrs.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imyrs/ are common to impoundment
c     element user interface and the impoundment element main
c     program and refer to variables needed to determine the
c     yearly sums and maximums.

      common /imyrs/ viy(mximp), qiiny(mximp), qomxy(mximp),
     1    hmaxy(mximp), totiny(mximp), cliny(mximp), sliny(mximp),
     1    sainy(mximp), sdiny(mximp), lainy(mximp), couty(mximp),
     1    clouty(mximp), slouty(mximp), saouty(mximp), sdouty(mximp),
     1    laouty(mximp), rety(mximp), clrety(mximp), slrety(mximp),
     1    sarety(mximp), sdrety(mximp), larety(mximp), cpeaky(mximp),
     1    cpkiy(mximp), cay(mximp), voy(mximp), tey(mximp),
     1    cainy(mximp), coutys(mximp), clotys(mximp), slotys(mximp),
     1    saotys(mximp), sdotys(mximp), laotys(mximp)

c     + + + VARIABLE DECLARATIONS + + +

      real viy, qiiny, qomxy, hmaxy, totiny, cliny, sliny,
     1    sainy, sdiny, lainy, couty, clouty, slouty, saouty, sdouty,
     1    laouty, rety, clrety, slrety, sarety, sdrety, larety,
     1    cpeaky, cpkiy, cay, voy, tey, cainy, coutys, clotys,
     1    slotys, saotys, sdotys, laotys

c     + + + VARIABLE DEFINITIONS + + +

c     viy(mximp)    : inflow volume for the year (m^3)
c     qiiny(mximp)  : peak inflow rate for the year (m^3/s)
c     qomxy(mximp)  : peak outflow rate for the year (m^3/s)
c     hmaxy(mximp)  : maximum stage for the year (m)
c     totiny(mximp) : total incoming sediment for the year (kgs)
c     cliny(mximp)  : total incoming clay for the year (kgs)
c     sliny(mximp)  : total incoming silt for the year (kgs)
c     sainy(mximp)  : total incoming small agg for the year (kgs)
c     sdiny(mximp)  : total incoming sand for the year (kgs)
c     lainy(mximp)  : total incoming lg agg for the year (kgs)
c     couty(mximp)  : total outgoing sediment for the year (kgs)
c     coutys(mximp) : total outgoing sediment for the year (kgs)
c                     reset in annchn, not impyr
c     clouty(mximp) : total outgoing clay for the year (kgs)
c     clotys(mximp): total outgoing clay for the year (kgs)
c                     reset in annchn, not impyr
c     slouty(mximp) : total outgoing silt for the year (kgs)
c     slotys(mximp): total outgoing silt for the year (kgs)
c                     reset in annchn, not impyr
c     saouty(mximp) : total outgoing sm agg for the year (kgs)
c     saotys(mximp): total outgoing sm agg for the year (kgs)
c                     reset in annchn, not impyr
c     sdouty(mximp) : total outgoing sand for the year (kgs)
c     sdotys(mximp): total outgoing sand for the year (kgs)
c                     reset in annchn, not impyr
c     laouty(mximp) : total outgoing lg agg for the year (kgs)
c     laotys(mximp): total outgoing lg agg for the year (kgs)
c                     reset in annchn, not impyr
c     rety(mximp)   : total sediment retained in the year (kgs)
c     clrety(mximp) : total clay retained in the year (kgs)
c     slrety(mximp) : total silt retained in the year (kgs)
c     sarety(mximp) : total sm agg retained in the year (kgs)
c     sdrety(mximp) : total sand retained in the year (kgs)
c     larety(mximp) : total lg agg retained in the year (kgs)
c     cpeaky(mximp) : peak outflow concentration for the year (mg/l)
c     cpkiy(mximp)  : peak inflow concentration for the year (mg/l)
c     cay(mximp)    : average outflow concentration for the year (mg/l)
c     voy(mximp)    : total outflow volume for the year (m^3)
c     tey(mximp)    : impoundment trapping efficiency for the year (%)
c     cainy(mximp)  : average inflow concentration for the year (mg/l)

c     end include file cimyrs.inc
