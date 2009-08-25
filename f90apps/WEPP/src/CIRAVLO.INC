
c     begin include file ciravlo.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /iravlo/ are used to maintain a record of soil
c     loss due to irrigation.

      common /iravlo/   irsold,irsolm,irsoly,irsolt

c     + + + VARIABLE DECLARATIONS + + +

      real              irsold,irsolm,irsoly,irsolt

c     + + + VARIABLE DEFINITIONS + + +

c     irsold : daily soil loss due to irrigation (kg/m)
c     irsolm : monthly soil loss due to irrigation (kg/m)
c     irsoly : yearly soil loss due to irrigation (kg/m)
c     irsolt : total soil loss due to irrigation (kg/m)

c     end include file ciravlo.inc

