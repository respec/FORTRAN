
c     begin include file cerdva1.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c     NOTE : ERDVA1 is used in SR ROUTE.

      common /erdva1/   load(101),tc(101)

c     + + + VARIABLE DECLARATIONS + + +

      real              load,tc

c     + + + VARIABLE DEFINITIONS + + +


c     load(101) : dimensionless sediment load at each point down an OFE
c     tc(101)   : sediment transport capacity at each point down an OFE
c                 (kg/s/m)

c     end include file cerdva1.inc

