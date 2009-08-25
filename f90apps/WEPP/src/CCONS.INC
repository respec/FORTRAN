
c     begin include file ccons.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /cons/     ck1(mxnsl,mxplan),ck2(mxnsl,mxplan),rre(mxplan),
     1                  bddry(mxnsl,mxplan),bdcons(mxnsl,mxplan),
     1                  cpm(mxnsl,mxplan),
     1                  coca(mxnsl,mxplan),avpor(mxplan),
     1                  thtdk1(mxnsl,mxplan),
     1                  thtdk2(mxnsl,mxplan),thetfk(mxnsl,mxplan),
     1                  wrdk(mxnsl,mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              ck1,ck2,rre,bddry,bdcons,cpm,
     1                  coca,avpor,thtdk1,thtdk2,
     1                  thetfk,wrdk

c     + + + VARIABLE DEFINITIONS + + +

c     ck1(mxnsl,mxplan)    : coefficient used to calculate Rawls
c                            coefficient
c     ck2(mxnsl,mxplan)    : coefficient used to calculate Rawls
c                            coefficient
c     rre(mxplan)          : random roughness parameter
c     bddry(mxnsl,mxplan)  : dry bulk density at 15 bars of tension
c                            (kg/m^3)
c     bdcons(mxnsl,mxplan) : consolidated bulk density (kg/m^3)
c     cpm(mxnsl,mxplan)    : rock fragment correction factor
c     coca(mxnsl,mxplan)   : entrapped air correction factor
c     avpor(mxplan)        : average porosity for infiltration zone
c     thtdk1(mxnsl,mxplan) : coefficient to calculate 15 bar water
c                            content
c     thtdk2(mxnsl,mxplan) : coefficient to calculate 15 bar water
c                            content
c     thetfk(mxnsl,mxplan) : 1/3 bar water content (m^3/m^3)
c     wrdk(mxnsl,mxplan)   : residual water content (m^3/m^3)

c     end include file ccons.inc

