
c     begin include file cdecvar.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /decvar/   senvin(mxplan),fenvin(mxres,mxplan),
     1                  benvin(mxres,mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              senvin,fenvin,benvin

c     + + + VARIABLE DEFINITIONS + + +

c     senvin(mxplan)       : standing environment coefficient
c     fenvin(mxres,mxplan) : flat environment coefficient
c     benvin(mxres,mxplan) : buried environment coefficient

      include 'cdecvar1.inc'

c     end include file cdecvar.inc

