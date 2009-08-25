
c     begin include file ccntour.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /cntour/   cntslp(mxplan),rowspc(mxplan),rowlen(mxplan),
     1                  rdghgt(mxplan),cntlen(mxplan),cnslp(mxplan),
     1                  conseq(mxcrop,mxplan),fail(3,mxplan),
     1                  totfal(mxplan),cntday(mxplan),cntend(mxplan),
     1                  contrs(mxcrop,mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              cntslp,rowspc,rowlen,rdghgt,cntlen,cnslp
      integer           conseq,fail,totfal,cntday,cntend,contrs

c     + + + VARIABLE DEFINITIONS + + +
c     cntday(mxplan)        : beginning day contours in effect (julian)
c     cntend(mxplan)        : ending day contours in effect (julian)
c     cntslp(mxplan)        : contour slope (m/m)
c     rowspc(mxplan)        : contour row spacing (m)
c     rowlen(mxplan)        : contour row length (m)
c     rdghgt(mxplan)        : contour ridge height (m)
c     cntlen(mxplan)        : contour length
c     cnslp(mxplan)         : contour slope
c     conseq(mxcrop,mxplan) : contour sequence from management input
c     fail(3,mxplan)        : counter for contour failures
c                             1 - contour slope is the same as average slope
c                             2 - slope angle exceeds row angle
c                             3 - not currently set
c     totfal(mxplan)        :

c     end include file ccntour.inc