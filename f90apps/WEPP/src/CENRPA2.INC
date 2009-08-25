
c     begin include file cenrpa2.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c     NOTE : ENRPA2 is used in SR ROUTE.

      common /enrpa2/   enrato(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              enrato

c     + + + VARIABLE DEFINITIONS + + +

c     enrato(mxplan) : enrichment ratio of the specific surface area
c                      of the sediment.  A value for enrato is computed
c                      at the end of each Overland Flow Element for each
c                      storm event.

c     end include file cenrpa2.inc

