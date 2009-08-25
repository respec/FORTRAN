
c     begin include file cgcovr.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /gcovr/    gcvplt(mxplan),mantyp(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              gcvplt
      integer           mantyp

c     + + + VARIABLE DEFINITIONS + + +

c     mantyp(mxplan) : current crop management type 1- annual
c                                                   2- perennial
c                                                   3- fallow
c     gcvplt(mxplan) : ground cover at planting for an annual crop

c     end include file cgcovr.inc

