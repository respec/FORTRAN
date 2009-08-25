
c     begin include file cpart3.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c     NOTE : PART33 is never used in SR ROUTE.

      common /part3/    dia(mxpart,mxelem),spg(mxpart),
     1                  eqsand(mxpart,mxelem)

c     + + + VARIABLE DECLARATIONS + + +

      real              dia,spg,eqsand

c     + + + VARIABLE DEFINITIONS + + +


c    dia(mxpart,mxelem)   : diameter of each particle class (m)
c    spg(mxpart)          : specific gravity of each particle class
c    eqsand(mxpart,mxelem): equivalent sand diameter of each particle
c                           class

c     end include file cpart3.inc

