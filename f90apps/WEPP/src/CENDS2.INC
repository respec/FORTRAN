
c     begin include file cends2.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c     NOTE : ENDS2 is used in SR ROUTE.

      common /ends2/    qout,qin

c     + + + VARIABLE DECLARATIONS + + +

      real              qout,qin

c     + + + VARIABLE DEFINITIONS + + +

c     qin  : flow discharge per unit width (m^3/m*s) at the top
c            (inlet) of the current OFE.
c     qout : flow discharge per unit width (m^3/m*s) at the bottom
c            (outlet) of the current OFE.

c     end include file cends2.inc

