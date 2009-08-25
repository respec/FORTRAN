
c     begin include file ccrpvr3.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /crpvr3/   gdd,sumgdd(mxplan),vdmmax(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              gdd,sumgdd,vdmmax

c     + + + VARIABLE DEFINITIONS + + +

c     gdd           : growing degree days - the number of heat units
c                     necessary to reach peak standing biomass
c     sumgdd(mxplan): cumulative growing degree days (degrees C)
c     vdmmax(mxplan): vegetative dry matter at maturity (kg/m^2)

      include 'ccrpvr3a.inc'

c     end include file ccrpvr3.inc

