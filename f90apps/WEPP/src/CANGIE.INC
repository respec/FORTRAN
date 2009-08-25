
c     begin include file cangie.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /angies/ radinc(mxelem),radlat,eqlat,delong,radpot,
     1 solef(mxelem),cloudC,datef

c     + + + VARIABLE DECLARATIONS + + +

      real      radinc,radlat,eqlat,delong,radpot,solef,cloudC
      integer   datef

c     + + + VARIABLE DEFINITIONS + + +

c     radinc :
c     radlat :
c     eqlat  :
c     delong :
c     radpot : potential radiation at a horizontal surface (cal/cm^2/day)
c              The meaning of radpot was changed by S. Dun Nov 10, 2006
c              in order to be consistant with the change in sunmap.for.
c     solef  : Adjustment factor for solar radation on a sloped surface.

c     end include file cangie.inc

