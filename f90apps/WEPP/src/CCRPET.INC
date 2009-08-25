
c     begin include file ccrpet.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /crpet/kcb(mxcrop),rawp(mxcrop),kcbcon
    
c     + + + VARIABLE DECLARATIONS + + +

      real kcb,rawp,kcbcon

c     + + + VARIABLE DEFINITIONS + + +

c     kcb(mxcrop)  : mid-season crop coefficient for Penman-Monteith 
c                    dual coefficient method.
c     rawp(mxcrop) : coefficient p for readily available root zone
c                    soil water formula
c     kcbcon       : basal crop coefficient. 

c     end include file ccrpet.inc
