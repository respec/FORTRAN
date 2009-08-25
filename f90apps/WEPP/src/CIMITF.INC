
c     begin include file cimitf.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imitf/ are common to impoundment
c     element user interface and the impoundment element main
c     program and refer to hydraulic outflow function parameters,
c     stage-area-length parameters and impoundment size.

      common /imitf/ a(mxstc,mxrgm,mximp), b(mxstc,mxrgm,mximp),
     1    c(mxstc,mxrgm,mximp), d(mxstc,mxrgm,mximp),
     1    e(mxstc,mxrgm,mximp), ha(mxstc,mxrgm,mximp), ht(mxrgm,mximp),
     1    hlm(mxrgm,mximp), a0(mximp), a1(mximp), a2(mximp), l0(mximp),
     1    l1(mximp), l2(mximp), qinf(mximp), isize(mximp), hot(mximp),
     1    hfull(mximp), nt(mximp)

c     + + + VARIABLE DECLARATIONS + + +

      integer isize, nt
      real a, b, c, d, e, ha, ht, hlm, a0, a1, a2, l0, l1, l2,
     1    qinf, hot, hfull

c     + + + VARIABLE DEFINITIONS + + +

c     a(mxstc,mxrgm,mximp)  : used in final outflow functions -
c                             structure i, flow regime j
c     a0(mximp), a1(mximp),
c     a2(mximp)             : coefficients used in area power function
c     b(mxstc,mxrgm,mximp)  : used in final outflow functions -
c                             structure i, flow regime j
c     c(mxstc,mxrgm,mximp)  : used in final outflow functions -
c                             structure i, flow regime j
c     d(mxstc,mxrgm,mximp)  : used in final outflow functions -
c                             structure i, flow regime j
c     e(mxstc,mxrgm,mximp)  : used in final outflow functions -
c                             structure i, flow regime j
c     ha(mxstc,mxrgm,mximp) : adjustment stage used in final outflow
c                             function i, flow regime j
c     hfull(mximp)          : stage at which the impoundment is full of sediment
c     hlm(mxrgm,mximp)      : limiting lowest stage for flow regime j
c     hot(mximp)            : stage at which the impoundment is overtopped
c     ht(mxrgm,mximp)       : transition stage at which flow regime j begins
c     isize(mximp)          : structure size (1:small, 2:large)
c     l0(mximp), l1(mximp),
c     l2(mximp)             : coefficients of the length power function
c     qinf(mximp)           : infiltration rate
c     nt(mximp)             : number of flow regimes

c     end include file cimitf.inc



