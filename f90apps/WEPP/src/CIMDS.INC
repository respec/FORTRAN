c     begin include file cimds.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imds/ are common to impoundment
c     element  impint and impreg subroutines to modify the 
c     rearrangment of the flow regimes.This part of variables
c     are used for dscribing the feature of drop spillway.

      common /imds/ dids(mximp),
     1    ddiars(mximp),dlenrs(mximp),dwidrs(mximp), 
     1    dhrs(mximp),dcoefw(mximp), dcoefo(mximp),
     1    ddiabl(mximp),dhitbl(mximp),dwdbl(mximp),
     1    dhrh(mximp),dlbl(mximp),dsbl(mximp),dhblot(mximp),
     1    dke(mximp),dkb(mximp),dkc(mximp)

c     + + + VARIABLE DECLARATIONS + + +
 
      integer dids
      real ddiars,dlenrs,dwidrs,dhrs,dcoefw,dcoefo,
     1     ddiabl,dhitbl,dwdbl,dhrh,dlbl,dsbl,dhblot,dke,dkb,dkc

c     + + + VARIABLE DEFINITIONS + + +


c 
c       dids(mximp)     - flag for judging whether drop spillway exists
c                         in an impoundment.
c       ddiars(mximp)   - diameter of a circular riser.
c       dlenrs(mximp)   - length of the box section of a rectangular
c                         riser.
c       dwidrs(mximp)   - width of the box section of a rectangular
c                         riser.
c       dhrs(mximp)     - Stage of the riser inlet.
c       dcoefw(mximp)   - weir coefficient.
c       dcoefo(mximp)   - orifice coefficient.
c       ddiabl(mximp)   - diameter of a circular barrel.
c       dhitbl(mximp)   - height of the box section of a rectangular
c                         barrel.
c       dwdbl(mximp)    - width of the box section of a rectangular
c                         barrel.
c       dhrh(mximp)     - height of riser inlet above barrel bottom.
c       dlbl(mximp)     - length of barrel.
c       dsbl(mximp)     - slope of barrel.
c       dhblot(mximp)   - height of barrel outlet above exit channel
c                         bottom.
c       dkb(mximp)      - bend loss coefficient
c       dkc(mximp)      - friction loss coefficient
c       dke(mximp)      - entrance loss coefficient
c
c     end include file cimds.inc
