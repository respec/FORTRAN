
c     begin include file cimeos.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imeos/ are common to impoundment
c     element user interface and the impoundment element main
c     program and refer to variables needed to determine the
c     end of simulation sums and maximums.

      common /imeos/ vie(mximp), qiine(mximp), qomxe(mximp),
     1    hmaxe(mximp), totine(mximp), cline(mximp), sline(mximp),
     1    saine(mximp), sdine(mximp), laine(mximp), coute(mximp),
     1    cloute(mximp), sloute(mximp), saoute(mximp), sdoute(mximp),
     1    laoute(mximp), rete(mximp), clrete(mximp), slrete(mximp),
     1    sarete(mximp), sdrete(mximp), larete(mximp), cpeake(mximp),
     1    cpkie(mximp), cae(mximp), voe(mximp), tee(mximp),
     1    caine(mximp), rett(mximp), coutt(mximp), totint(mximp)

c     + + + VARIABLE DECLARATIONS + + +

      real vie, qiine, qomxe, hmaxe, totine, cline, sline,
     1    saine, sdine, laine, coute, cloute, sloute, saoute, sdoute,
     1    laoute, rete, clrete, slrete, sarete, sdrete, larete,
     1    cpeake, cpkie, cae, voe, tee, caine, rett, coutt, totint

c     + + + VARIABLE DEFINITIONS + + +
c
c     vie(mximp)    : inflow volume for the simulation (m^3)
c     qiine(mximp)  : peak inflow rate for the simulation (m^3/s)
c     qomxe(mximp)  : peak outflow rate for the simulation (m^3/s)
c     hmaxe(mximp)  : maximum stage for the simulation (m)
c     totine(mximp) : total incoming sediment for the simulation (kgs)
c     cline(mximp)  : total incoming clay for the simulation (kgs)
c     sline(mximp)  : total incoming silt for the simulation (kgs)
c     saine(mximp)  : total incoming small agg for the simulation (kgs)
c     sdine(mximp)  : total incoming sand for the simulation (kgs)
c     laine(mximp)  : total incoming lg agg for the simulation (kgs)
c     coute(mximp)  : total outgoing sediment for the simulation (kgs)
c     cloute(mximp) : total outgoing clay for the simulation (kgs)
c     sloute(mximp) : total outgoing silt for the simulation (kgs)
c     saoute(mximp) : total outgoing sm agg for the simulation (kgs)
c     sdoute(mximp) : total outgoing sand for the simulation (kgs)
c     laoute(mximp) : total outgoing lg agg for the simulation (kgs)
c     rete(mximp)   : total sediment retained in the simulation (kgs)
c     clrete(mximp) : total clay retained in the simulation (kgs)
c     slrete(mximp) : total silt retained in the simulation (kgs)
c     sarete(mximp) : total sm agg retained in the simulation (kgs)
c     sdrete(mximp) : total sand retained in the simulation (kgs)
c     larete(mximp) : total lg agg retained in the simulation (kgs)
c     cpeake(mximp) : peak outflow concentration for the simulation (mg/l)
c     cpkie(mximp)  : peak inflow concentration for the simulation (mg/l)
c     cae(mximp)    : average outflow concentration for the simulation (mg/l)
c     voe(mximp)    : total outflow volume for the simulation (m^3)
c     tee(mximp)    : impoundment trapping efficiency for the simulation (%)
c     caine(mximp)  : average inflow concentration for the simulation (mg/l)
c     rett(mximp)   : total sediment retained for the day (kgs) and
c                     summed for simulation period
c     coutt(mximp)  : total outgoing sediment for the day (kgs) and
c                     summed for simulation period
c     totint(mximp) : total incoming sediment for the day (kgs) and
c                     summed for simulation period

c     end include file cimeos.inc



