
c     begin include file cinpsur.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /inpsur/ lantyp(ntype),op(ntype,ntype)

c     + + + VARIABLE DECLARATIONS + + +

      integer         lantyp, op

c     + + + VARIABLE DEFINITIONS + + +

c     lantyp(ntype)        : 1=cropland, 2=rangeland, 3=forest, 4=roads
c     op(ntype,ntype)      : index of operation used on this date

c     end include file cinpsur.inc

