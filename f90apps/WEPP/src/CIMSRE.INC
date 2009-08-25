c     begin include file cimsre.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imsre/ are common to impoundment
c     element  impint and impreg subroutines to modify the 
c     rearrangment of the flow regimes  when sediment stage
c     in the impoundment is heigher than the stage at which
c     a stucture's feature begin affected by the sediments. 

      common /imsre/ sstage

c     + + + VARIABLE DECLARATIONS + + +

      real sstage

c     + + + VARIABLE DEFINITIONS + + +

c     sstage: sediment stage of the last rearrangement in the 
c             impondment.
c
c     end include file cimsre.inc
