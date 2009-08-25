
c     begin include file cstore2.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /store2/ tmpvol(0:mxelem),tmprof(0:mxelem),
     1                tmppkr(0:mxelem),tmpsbv(0:mxelem),tmpsbf(0:mxelem)

c     + + + VARIABLE DECLARATIONS + + +

      real  tmpvol,tmprof,tmppkr,tmpsbv,tmpsbf

c     + + + VARIABLE DEFINITIONS + + +

c     tmpvol(0:mxelem): temporary variable for hillslope
c                       runoff volume (m^3)
c     tmprof(0:mxelem): temporary variable for hillslope
c                       runoff depth (m)
c     tmppkr(0:mxelem): temporary variable for hillslope
c                       peak runoff (m^3/s)
c     tmpsbv(0:mxelem): temporary variable for hillslope
c                        subsurface runoff volume (m^3)
c     tmpsbf(0:mxelem): temporary variable for hillslope
c                         subsurface runoff depth (m)

c     end include file cstore2.inc


