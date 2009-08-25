c     begin include file cimcv2.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imcv2/ are common to impoundment
c     element  impint and impreg subroutines to modify the 
c     rearrangment of the flow regimes.This part of variables
c     are used for dscribing the feature of culvert 2.

      common /imcv2/ c2icv(mximp),c2ncv(mximp),
     1    c2ar(mximp),c2hit(mximp),c2h(mximp), 
     1    c2l(mximp),c2s(mximp), c2hot(mximp),
     1    c2ke(mximp),c2kb(mximp),c2kc(mximp),
     1    c2kus(mximp),c2mus(mximp),c2cs(mximp),c2ys(mximp)

c     + + + VARIABLE DECLARATIONS + + +
  
      integer c2icv,c2ncv
      real c2ar,c2hit,c2h,c2l,c2s,c2hot,c2ke,c2kb,c2kc,
     1     c2kus,c2mus,c2cs,c2ys

c     + + + VARIABLE DEFINITIONS + + +


c 
c      c2icv(mximp)    - flag for judging whether culvert 2 exists 
c                        in an impoundment
c      c2ncv(mximp)    - number of culvert 2 in an impoundment
c      c2ar(mximp)     - flow area of  culvert 2
c      c2h(mximp)      - stage of the inlets' bottom of culvert 2
c      c2hit(mximp)    - height of culvert 2's inlets
c      c2l(mximp)      - length of culvert 2 
c      c2s(mximp)      - slope of culvert 2
c      c2hot(mximp)    - height of the culvert outlet above the  
c                        outlet channel bottom
c      c2kb(mximp)     - bend loss coefficient
c      c2kc(mximp)     - friction loss coefficient
c      c2ke(mximp)     - entrance loss coefficient
c      c2kus(mximp)    - constant used in culvert outflow function
c      c2mus(mximp)    - constant used in culvert outflow function
c      c2cs(mximp)     - constant used in culvert flow relationship
c      c2ys(mximp)     - constant used in culvert outflow function

c     end include file cimcv2.inc
