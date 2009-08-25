
c     begin include file cinpcon.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /inpcon/  icont(ntype),cntsl1(ntype),rowsp1(ntype),
     1                 rowle1(ntype),rdghg1(ntype), ityp1(ntype)

c     + + + VARIABLE DECLARATIONS + + +

      real cntsl1,rowsp1,rowle1,rdghg1
      integer icont,ityp1

c     + + + VARIABLE DEFINITIONS + + +

c     ityp1(ntype)  :
c     icont(ntype)  : 1=cropland, 2=rangeland, 3=forest, 4=roads
c     cntsl1(ntype) : contour slope (m/m)
c     rowsp1(ntype) : contour row spacing (m)
c     rowle1(ntype) : contour row length (m)
c     rdghg1(ntype) : contour ridge height (m)

c     end include file cinpcon.inc

