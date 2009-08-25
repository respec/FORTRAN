
c     begin include file cslope1.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c     NOTE : SLOPE1 is used in SR ROUTE.

      common /slope1/  xu(mxslp,mxplan),xl(mxslp,mxplan),nslpts(mxplan),
     1                 a(mxslp,mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer          nslpts
      real             xu,xl,a

c     + + + VARIABLE DEFINITIONS + + +

c     xu(mxslp,mxplan): dimensionless upper end of section
c     xl(mxslp,mxplan): dimensionless lower end of section
c     nslpts(mxplan)  : number of input slope points for each OFE
c     a(mxslp,mxplan) : profile coefficient for curvature

c     end include file cslope1.inc

