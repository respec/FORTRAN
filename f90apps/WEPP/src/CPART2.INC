
c     begin include file cpart2.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c     NOTE : PART2 is used in SR ROUTE.

      common /part2/    fall(mxpart,mxelem),
     1                  frcly(mxpart,mxelem),frslt(mxpart,mxelem),
     1                  frsnd(mxpart,mxelem),frorg(mxpart,mxelem)

c     + + + VARIABLE DECLARATIONS + + +

      real              fall,frcly,frslt,frsnd,frorg

c     + + + VARIABLE DEFINITIONS + + +

c     fall(mxpart,mxelem)  : fall velocity of each
c                            particle class (m/s)
c     frcly(mxpart,mxelem) : fraction of clay (0-1)
c     frslt(mxpart,mxelem) : fraction of silt (0-1)
c     frsnd(mxpart,mxelem) : fraction of sand (0-1)
c     frorg(mxpart,mxelem) : fraction of organic matter (0-1)

c     end include file cpart2.inc

