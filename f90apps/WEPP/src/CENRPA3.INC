
c     begin include file cenrpa3.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c     NOTE : ENRPA3 is never used in SR ROUTE.

      common /enrpa3/   enrmm1,enrmm2,enryy1,enryy2,enrff1,enrff2,
     1                  enrmon,enryr,enravg,frcmm1(mxpart),
     1                  frcmm2(mxpart),frcyy1(mxpart),frcyy2(mxpart),
     1                  frcff1(mxpart),frcff2(mxpart),frcmon(mxpart),
     1                  frcyr(mxpart),frcavg(mxpart)

c     + + + VARIABLE DECLARATIONS + + +

      real              enrmm1,enrmm2,enryy1,enryy2,
     1                  enrff1,enrff2,enrmon,enryr,enravg,frcmm1,
     1                  frcmm2,frcyy1,frcyy2,frcff1,frcff2,frcmon,
     1                  frcyr,frcavg

c     + + + VARIABLE DEFINITIONS + + +

c     enrmm1         : used to calculate monthly weighted enrichment ratio
c     enrmm2         : used to calculate monthly weighted enrichment ratio
c     enryy1         : used to calculate annual weighted enrichment ratio
c     enryy2         : used to calculate annual weighted enrichment ratio
c     enrff1         : used to calculate total weighted enrichment ratio
c     enrff2         : used to calculate total weighted enrichment ratio
c     enrmon         : enrmm1/enrmm2 used for monthly
c                             weighted enrichment ratio
c     enryr          : enryy1/enryy2 used for annual
c                             weighted enrichment ratio
c     enravg         : enrff1/enrff2 used for total
c                             weighted enrichment ratio
c     frcmm1(mxpart) : used to calculated monthly weighted
c                             particle size fractions exiting
c     frcmm2(mxpart) : used to calculated monthly weighted
c                             particle size fractions exiting
c     frcyy1(mxpart) : used to calculated annual weighted
c                             particle size fractions exiting
c     frcyy2(mxpart) : used to calculated annual weighted
c                             particle size fractions exiting
c     frcff1(mxpart) : used to calculated total weighted
c                             particle size fractions exiting
c     frcff2(mxpart) : used to calculated total weighted
c                             particle size fractions exiting
c     frcmon(mxpart) : frcmm1/frcmm2 used for monthly weighted
c                             fraction of particle size classes exiting
c     frcyr(mxpart)  : frcyy1/frcyy2 used for annual weighted
c                             fraction of particle size classes exiting
c     frcavg(mxpart) : frcff1/frcff2 used for total weighted
c                             fraction of particle size classes exiting

c     end include file cenrpa3.inc

