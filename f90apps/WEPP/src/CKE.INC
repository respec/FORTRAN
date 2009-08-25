
c     begin include file cke.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /ke/ rkine, rkecum(mxplan), ksflag

c     + + + VARIABLE DECLARATIONS + + +

      real        rkine, rkecum
      integer     ksflag

c     + + + VARIABLE DEFINITIONS + + +

c     rkine  : kinetic energy of rainfall (J/m2)
c     rkecum : cumulative kinetic energy since last tillage (J/m2)
c     ksflag : user input flag (in SOIL file) to indicate whether
c              internal Ksat adjustments should be used in SR INFPAR
c              ksflag = 0    means  use no adjustments
c              ksflag = 1    means  use all adjustments

c     end include file cke.inc



