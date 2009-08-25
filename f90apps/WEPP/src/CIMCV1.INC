c     begin include file cimcv1.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imcv1/ are common to impoundment
c     element  impint and impreg subroutines to modify the 
c     rearrangment of the flow regimes.This part of variables
c     are used for dscribing the feature of culvert 1.

      common /imcv1/ c1icv(mximp),c1ncv(mximp),
     1    c1ar(mximp),c1hit(mximp),c1h(mximp), 
     1    c1l(mximp),c1s(mximp), c1hot(mximp),
     1    c1ke(mximp),c1kb(mximp),c1kc(mximp),
     1    c1kus(mximp),c1mus(mximp),c1cs(mximp),c1ys(mximp)

c     + + + VARIABLE DECLARATIONS + + +
 
      integer c1icv,c1ncv
      real c1ar,c1hit,c1h,c1l,c1s,c1hot,c1ke,c1kb,c1kc,
     1     c1kus,c1mus,c1cs,c1ys

c     + + + VARIABLE DEFINITIONS + + +


c 
c       c1icv(mximp)    - flag for judging whether culvert 1 exists 
c                         in an impoundment
c       c1ncv(mximp)    - number of culvert 1 in an impoundment
c       c1ar(mximp)     - flow area of  culvert 1
c       c1h(mximp)      - stage of the inlets' bottom of culvert 1
c       c1hit(mximp)    - height of culvert 1's inlets
c       c1l(mximp)      - length of culvert 1 
c       c1s(mximp)      - slope of culvert 1
c       c1hot(mximp)    - height of the culvert outlet above the  
c                         outlet channel bottom
c       c1kb(mximp)     - bend loss coefficient
c       c1kc(mximp)     - friction loss coefficient
c       c1ke(mximp)     - entrance loss coefficient
c       c1kus(mximp)    - constant used in culvert outflow function
c       c1mus(mximp)    - constant used in culvert outflow function
c       c1cs(mximp)     - constant used in culvert flow relationship
c       c1ys(mximp)     - constant used in culvert outflow function

c     end include file cimcv1.inc
