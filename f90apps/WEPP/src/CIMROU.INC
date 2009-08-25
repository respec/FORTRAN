
c     begin include file cimrou.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imrou/ are common to impoundment
c     element user interface and the water surface profile routine
c     and refer to parameters utilized in performing a water surface
c     profile.

      common /imrou/ bwes, sses, nes, pypos, zes, mdwit, dwt,
     1    rfrac, yc, qesr, depth, ncross

c     + + + VARIABLE DECLARATIONS + + +

      integer mdwit, ncross
      real bwes, sses, pypos(30), zes(30), dwt, rfrac, yc, qesr,
     1    depth(30), nes

c     + + + VARIABLE DEFINITIONS + + +

c     bwes      : bottom width of trapezoidal emergency spillway/open channel
c     dwt       : convergence tolerance for emergency spillway routing routine
c     depth(30) : flow depth for emergency spillway cross section i
c     mdwit     : maximum number of iterations used in subroutine route
c     ncross    : number of emergency spillway cross sections
c     nes       : manning's n for emergency spillway vegetation
c     pypos(30) : stage of emergency spillway cross section i
c     qesr     : emergency spillway flow entered into the routing subroutine
c     rfrac    : fraction of y1 that y2 is allowed to change (used
c                in routing subroutine)
c     sses     : side slopes of trapezoidal emergency spillway cross section
c     yc       : critical depth entered into routing subroutine
c     zes(30)  : distance from approach beginning of emergency
c                spillway cross section i

c     end include file cimrou.inc



