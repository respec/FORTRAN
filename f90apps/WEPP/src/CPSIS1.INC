
c     begin include file cpsis1.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /psis1/    psi,dpsi,u

c     + + + VARIABLE DECLARATIONS + + +

      double precision  psi,dpsi,u

c     + + + VARIABLE DEFINITIONS + + +

c     psi  : the position on the characteristics, starting
c            at "u", at time "time"
c     dpsi : the derivation on the characteristics, starting
c            at "u", at time "time"
c     u    : used in bisection method in psiinv

c     end include file cpsis1.inc

