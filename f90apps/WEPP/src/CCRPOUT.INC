
c     begin include file ccrpout.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /crpout/   rescov(mxplan),rtd(mxplan),rtmass(mxplan),
     1                  rtm15(mxplan),rh(mxplan),
     1                  rtm30(mxplan),rtm60(mxplan),
     1                  bd(mxnsl,mxplan),lai(mxplan),rrc(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              rescov,rtd,rtmass,rtm30,rtm60,bd,lai,rrc,
     1                  rtm15,rh

c     + + + VARIABLE DEFINITIONS + + +

c     rescov(mxplan)   : residue cover (0-1)
c     bd(mxnsl,mxplan) : bulk density per soil layer
c     lai(mxplan)      : leaf area index
c     rrc(mxplan)      : random roughness coefficient
c     rtd(mxplan)      : root depth (m)
c     rtmass(mxplan)   : total root mass (kg/m^2)
c     rtm15(mxplan)    : root mass at 15 cm
c     rtm30(mxplan)    : root mass at 30 cm
c     rtm60(mxplan)    : root mass at 60 cm
c     rh(mxplan)       :

c     end include file ccrpout.inc

