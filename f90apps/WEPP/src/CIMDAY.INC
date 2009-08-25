
c     begin include file cimday.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imday/ are common to impoundment
c     element user interface and the impoundment element main
c     program and refer to daily variables.

      common /imday/ vi(mximp), qiin(mximp), qomx(mximp), hmax(mximp),
     1    totin(mximp), clin(mximp), slin(mximp), sain(mximp),
     1    sdin(mximp), lain(mximp), cout(mximp), clout(mximp),
     1    slout(mximp), saout(mximp), sdout(mximp), laout(mximp),
     1    ret(mximp), clret(mximp), slret(mximp), saret(mximp),
     1    sdret(mximp), laret(mximp), cpeak(mximp), ca(mximp),
     1    volo(mximp), ciin(mximp), pla(mximp), psa(mximp), psl(mximp),
     1    pcl(mximp), psd(mximp), cl50, sl50, sd50, sa50, la50,
     1    clot, slot, saot, laot, sdot, cl50o, sl50o, sa50o, la50o,
     1    sd50o, totco(mximp)

c     + + + VARIABLE DECLARATIONS + + +

      real vi, qiin, qomx, hmax, totin, clin, slin,
     1    sain, sdin, lain, cout, clout, slout, saout, sdout,
     1    laout, ret, clret, slret, saret, sdret, laret,
     1    cpeak, ca, volo, ciin, pla, psa, psl, pcl, psd,
     1    cl50, sl50, sd50, sa50, la50, clot, slot, saot, laot,
     1    sdot, cl50o, sl50o, sa50o, la50o, sd50o, totco

c     + + + VARIABLE DEFINITIONS + + +

c     vi(mximp)    : inflow volume for the day (m^3)
c     qiin(mximp)  : peak inflow rate for the day (m^3/s)
c     qomx(mximp)  : peak outflow rate for the day (m^3/s)
c     hmax(mximp)  : maximum stage for the day (m)
c     totin(mximp) : total incoming sediment for the day (kgs)
c     clin(mximp)  : total incoming clay for the day (kgs)
c     slin(mximp)  : total incoming silt for the day (kgs)
c     sain(mximp)  : total incoming small agg for the day (kgs)
c     sdin(mximp)  : total incoming sand for the day (kgs)
c     lain(mximp)  : total incoming lg agg for the day (kgs)
c     cout(mximp)  : total outgoing sediment for the day (kgs)
c     clout(mximp) : total outgoing clay for the day (kgs)
c     slout(mximp) : total outgoing silt for the day (kgs)
c     saout(mximp) : total outgoing sm agg for the day (kgs)
c     sdout(mximp) : total outgoing sand for the day (kgs)
c     laout(mximp) : total outgoing lg agg for the day (kgs)
c     ret(mximp)   : total sediment retained for the day (kgs)
c     clret(mximp) : total clay retained for the day (kgs)
c     slret(mximp) : total silt retained for the day (kgs)
c     saret(mximp) : total sm agg retained for the day (kgs)
c     sdret(mximp) : total sand retained for the day (kgs)
c     laret(mximp) : total lg agg retained for the day (kgs)
c     cpeak(mximp) : peak outflow concentration for the day (mg/l)
c     ca(mximp)    : average outflow concentration for the day (mg/l)
c     volo(mximp)  : total outflow volume for the day (m^3)
c     ciin(mximp)  : average inflow concentration for the day (mg/l)
c     pla(mximp)   : % of incoming sediment in the large aggregate size class
c     psa(mximp)   : % of incoming sediment in the small aggregate size class
c     psl(mximp)   : % of incoming sediment in the silt size class
c     pcl(mximp)   : % of incoming sediment in the clay size class
c     psd(mximp)   : % of incoming sediment in the sand size class
c     cl50         : 50% dia for clay particles entering the impoundment
c     sl50         : 50% dia for silt particles entering the impoundment
c     sd50         : 50% dia for sand particles entering the impoundment
c     sa50         : 50% dia for small aggregate particles entering the impoundment
c     la50         : 50% dia for large aggregate particles entering the impoundment
c     clot         : % of exiting sediment in the clay size class
c     slot         : % of exiting sediment in the silt size class
c     saot         : % of exiting sediment in the small aggregate size class
c     laot         : % of exiting sediment in the large aggregate size class
c     sdot         : % of exiting sediment in the sand size class
c     cl50o        : 50% dia for clay particles exiting the impoundment
c     sl50o        : 50% dia for silt particles exiting the impoundment
c     sa50o        : 50% dia for small aggregate particles exiting the impoundment
c     la50o        : 50% dia for large aggregate particles exiting the impoundment
c     sd50o        : 50% dia for sand particles exiting the impoundment
c     totco(mximp) : total outgoing sediment for the day (lbs)

c     end include file cimday.inc



