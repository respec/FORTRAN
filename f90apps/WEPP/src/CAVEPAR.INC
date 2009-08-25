
c     begin include file cavepar.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /avepar/   aveks(mxplan),avesm(mxplan),avsat(mxplan),
     1                  avbd(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              aveks,avesm,avsat,avbd

c     + + + VARIABLE DEFINITIONS + + +

c     aveks(mxplan) : saturated hydraulic conductivity (m/sec)
c     avesm(mxplan) : effective matric potential (m)
c     avsat(mxplan) : volumetric water content (mm^3/mm^3)
c     avbd(mxplan)  : average bulk density for the infiltrated layer
c                     (top 20 cm) (kg/m^3)

c     end include file cavepar.inc

