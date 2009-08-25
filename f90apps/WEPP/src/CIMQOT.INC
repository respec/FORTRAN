
c     begin include file cimqot.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imqot/ are common to impoundment
c     element user interface, the impoundment element main
c     program, and the impoundment element numerical integration
c     routine and refer to variables needed to determine the change
c     stage and outflow for a particular time step

      common /imqot/ qo(mximp), qon(mximp), h(mximp), hn(mximp),
     1    t(mximp), tn(mximp), dttry(mximp), dtdid(mximp),
     1    dtnext(mximp), deltat(mximp)

c     + + + VARIABLE DECLARATIONS + + +

      real qo, qon, h, hn, t, tn, dttry, dtdid, dtnext, deltat

c     + + + VARIABLE DEFINITIONS + + +

c     deltat(mximp) : initial time step
c     dtdid(mximp)  : length of time step taken
c     dtnext(mximp) : recommended length to attempt the next time step
c     dttry(mximp)  : actual length to attempt the next time step
c     h(mximp)      : stage at the beginning of a time step
c     hn(mximp)     : stage at the end of a time step
c     qo(mximp)     : outflow at the beginning of a time step
c     qon(mximp)    : outflow at the end of a time step
c     t(mximp)      : time at the beginning of a time step
c     tn(mximp)     : time at the end of a time step

c     end include file cimqot.inc




