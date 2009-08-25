
c     begin include file cinfco2.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c     NOTE : INFCO2 is never used in SR ROUTE.

      common /infco2/   ainf(mxslp),binf(mxslp),cinf(mxslp),
     1                  ainftc(mxslp),binftc(mxslp),cinftc(mxslp)

c     + + + VARIABLE DECLARATIONS + + +

      real              ainf,binf,cinf,ainftc,binftc,cinftc

c     + + + VARIABLE DEFINITIONS + + +

c     ainf(mxslp)   :  nondimensional shear stress coefficient
c     binf(mxslp)   :  nondimensional shear stress coefficient
c     cinf(mxslp)   :  nondimensional shear stress coefficient
c     ainftc(mxslp) :  nondimensional transport coefficient
c     binftc(mxslp) :  nondimensional transport coefficient
c     cinftc(mxslp) :  nondimensional transport coefficient

c     end include file cinfco2.inc

