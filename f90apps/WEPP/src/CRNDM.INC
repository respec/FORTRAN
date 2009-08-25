
c     begin include file crndm.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /rndm/     xrand,aa,mrnd

c     + + + VARIABLE DECLARATIONS + + +

      double precision  xrand,aa,mrnd

c     + + + VARIABLE DEFINITIONS + + +

c     xrand : updated "seed" for SR RAND (called initially
c             from SR BGNRND)
c     aa    : a multiplier for the MCM used for random number
c             generation
c     mrnd  : a multiplier for the MCM used for random number
c             generation

c     end include file crndm.inc

