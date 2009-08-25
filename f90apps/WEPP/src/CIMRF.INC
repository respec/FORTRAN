c     begin include file cimrf.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imrf/ are common to impoundment
c     element  impint and impreg subroutines to modify the 
c     rearrangment of the flow regimes.This part of variables
c     are used for dscribing the feature of Rock-fill check dam.

      common /imrf/ rirf(mximp),
     1    rlnrf(mximp),rhrf(mximp),rhotrf(mximp),rwdrf(mximp), 
     1    rarf(mximp),rbrf(mximp)
c     + + + VARIABLE DECLARATIONS + + +
 
      integer rirf
      real rlnrf,rhrf,rhotrf,rwdrf,rarf,rbrf
    
c     + + + VARIABLE DEFINITIONS + + +


c 
c       rirf(mximp)     - flag for judging whether rock-fill check dam  
c                         exists in an impoundment
c       rlnrf(mximp)    - flow length of rock-fill check dam in an
c                         impoundment
c       rhrf(mximp)     - stage at which flow through the rock-fill
c                         check dam occurs
c       rhotrf(mximp)   - stage at which overtopping flow occurs
c       rwdrf(mximp)    - cross sectional width of the rock-fill
c                         check dam
c       rarf(mximp)     - coefficient given graphically in Haan
c                         et al(1994) determined by using the size
c                         of the rocks and the flow length. 
c       rbrf(mximp)     - coefficient given graphically in
c                         Haan et al(1994) determined
c                         by using the size of the rocks. 

c     end include file cimrf.inc
