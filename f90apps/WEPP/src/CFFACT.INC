
c     begin include file cffact.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /ffact/    frcsol(mxplan),frctrl(mxplan),frcteq(mxplan),
     1                  frccov(mxplan),frlive(mxplan),frrres(mxplan),
     1                  frican(mxplan),fribas(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              frcsol,frctrl,frcteq,frccov,frlive,frrres,
     1                  frican,fribas

c     + + + VARIABLE DEFINITIONS + + +

c     frcsol(mxplan) : soil grain friction factor
c     frctrl(mxplan) : total rill friction factor
c     frcteq(mxplan) : equivalent weighting friction factor for rill
c     frccov(mxplan) : rill cover friction factor under cropland
c     frlive(mxplan) : friction factor due to living plants under cropland
c     frrres(mxplan) : rill cover friction factor under rangeland
c     fribas(mxplan) : basal cover friction factor under rangeland
c     frican(mxplan) : friction due to canopy cover under rangeland
c     fritot(mxplan) : sum of fribas and frican under rangeland

c     end include file cffact.inc

