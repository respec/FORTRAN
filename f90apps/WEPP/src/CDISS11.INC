
c     begin include file cdiss11.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /diss11/    sumint,dur,ninten(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer            ninten
      real               sumint,dur

c     + + + VARIABLE DEFINITIONS + + +

c     sumint   : time integral of rainfall intensity over non-zero
c                rainfall times
c     dur      : storm duration (s)
c     ninten   : number of rainfall/melt intensity values for each OFE

c     end include file cdiss11.inc

