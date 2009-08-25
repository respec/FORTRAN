
c     begin include file cimsed.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imsed/ are common to impoundment
c     element user interface and the impoundment element main
c     program and refer to variables needed to determine the
c     outgoing sediment concentration.

      common /imsed/ d0(mxsub,mximp), di(mxsub,mximp),
     1    d100(mxsub,mximp), sg(mxsub,mximp), ndiv(mximp),
     1    ctd(mxsub,mximp), cdp(mxsub,mximp), hmin(mximp),
     1    hset(mxsub,mximp), co(mxsub,mximp), vs(mxsub,mximp),
     1    cot(mximp)

c     + + + VARIABLE DECLARATIONS + + +

      integer ndiv
      real d0, di, d100, sg, ctd, cdp, hmin, hset, co, vs, cot

c     + + + VARIABLE DEFINITIONS + + +

c     cdp(mxsub,mximp)  : parameter to adjust deposition after inflow ceases
c     co(mxsub,mximp)   : outgoing sediment concentration for size subclass i
c                         at the beginning of a time step
c     cot(mximp)        : total outgoing sediment concentration at the
c                         beginning of a time step
c     ctd(mxsub,mximp)  : parameter to adjust the detention time
c     d0(mxsub,mximp)   : 0% diameter of particle size subclass i
c     d100(mxsub,mximp) : 100% diameter of particle size subclass i
c     di(mxsub,mximp)   : 50% diameter of particle size subclass i
c     hmin(mximp)       : the minimum stage in the impoundment
c     hset(mxsub,mximp) : settling depth for particle size class i
c     vs(mxsub,mximp)   : settling velocity for particle size class i
c     ndiv(mximp)       : number of particle size subclass divisions / 2
c     sg(mxsub,mximp)   : specific gravity for sediment size subclass i

c     end include file cimsed.inc
