
c     begin include file ciraflo.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /iraflo/ are used by the furrow irrigation
c     component to calculate area of flow.

      common /iraflo/   aqcnst,aqexp,botwid,ircon1,ircon2,sidslp

c     + + + VARIABLE DECLARATIONS + + +

      real              aqcnst,aqexp,botwid,ircon1,ircon2,sidslp

c     + + + VARIABLE DEFINITIONS + + +

c     aqcnst : coefficient of flow area in equation relating flow rate
c              to a power function of flow area (m^b/s where b depends
c              on the value of aqexp)
c     aqexp  : exponent of flow area in equation relating flow rate to a
c              power function of flow area (no units)
c     botwid : bottom width of irrigation furrow (m)
c     ircon1 : a combination of many variables which remain unchanged
c              for a large number of calculations (m^c where c depends
c              on the value of aqexp)
c     ircon2 : a combination of many variables which remain unchanged
c              for a large number of calculations (m^d where d depends
c              on the value of aqexp)
c     sidslp : side slope of irrigation furrow (m/m)

c     end include file ciraflo.inc

