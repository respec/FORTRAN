
c     begin include file crinpt1a.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c**********************************************************************
c    NOTE: All references to the following variables were added by
c          Mary Kidwell on 5/9/94:
c          litcov, bascov, rokcov, crycov, fresr, fresi, frokr, froki,
c          fbasr, fbasi, fcryr, fcryi
c
c    NOTE: All references to the following variables were added by
c          Mary Kidwell on 5/26/95:
c          rescof, cancof
c
c    NOTE: All references to the following variables were added by
c          Mary Kidwell on 6/6/95:
c          resr, resi, rokr, roki, basr, basi, cryr, cryi
c**********************************************************************

      common /rinpt1a/  ffk(ntype),aleaf(ntype),plive(ntype,mxplan),
     1                  proot(ntype,mxplan),cold(ntype),bugs(ntype),
     1                  wood(ntype),
     1                  litcov(mxplan),bascov(mxplan),rokcov(mxplan),
     1                  crycov(mxplan),fresr(mxplan),fresi(mxplan),
     1                  frokr(mxplan),froki(mxplan),fbasr(mxplan),
     1                  fbasi(mxplan),fcryr(mxplan),fcryi(mxplan),
     1                  rescof(mxplan),cancof(mxplan),
     1                  resr(mxplan),resi(mxplan),rokr(mxplan),
     1                  roki(mxplan),basr(mxplan),basi(mxplan),
     1                  cryr(mxplan),cryi(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real ffk,aleaf,plive,proot,cold,bugs,wood,
     1     litcov,bascov,rokcov,crycov,
     1     fresr,fresi,frokr,froki,fbasr,
     1     fbasi,fcryr,fcryi,
     1     rescof,cancof,
     1     resr,resi,rokr,roki,basr,basi,cryr,cryi

c     + + + VARIABLE DEFINITIONS + + +

c     ffk(ntype)     : coefficient used to calculate foliar cover
c     aleaf(ntype)   : coefficient for leaf area (m^2/kg)
c     plive(ntype)   : maximum standing live plant biomass (kg/m^2)
c     proot(ntype)   : maximum peak root biomass (kg/m^2)
c     cold(ntype)    : a flag for plant defoliation
c     bugs(ntype)    : daily disappearance of surface organic residue
c                      (kg/m^2)
c     wood(ntype)    : fraction of standing biomass which is woody
c     litcov(mxplan) : fraction of ground surface covered with litter
c                      (0-1)
c     bascov(mxplan) : fraction of ground surface covered with basal
c                      vegetation (0-1)
c     rokcov(mxplan) : fraction of ground surface covered with rocks
c                      and gravel (0-1)
c     crycov(mxplan) : fraction of ground surface covered with
c                      cryptograms (0-1)
c     fresr(mxplan)  : fraction of total litter cover located in rills (0-1)
c     fresi(mxplan)  : fraction of total litter cover located in interrills
c                      (0-1)
c     frokr(mxplan)  : fraction of total rock cover located in rills (0-1)
c     froki(mxplan)  : fraction of total rock cover located in interrills
c                      (0-1)
c     fbasr(mxplan)  : fraction of total basal cover located in rills (0-1)
c     fbasi(mxplan)  : fraction of total basal cover located in interrills
c                      (0-1)
c     fcryr(mxplan)  : fraction of total cryptogamic cover located in rills
c                      (0-1)
c     fcryi(mxplan)  : fraction of total cryptogamic cover located in
c                      interrills (0-1)
c     resr(mxplan)   : fraction of area covered by litter that is
c                      outside plant canopy, i.e. in rill (0-1)
c     resi(mxplan)   : fraction of area covered by litter that is
c                      under plant canopy, i.e. in interrill (0-1)
c     rokr(mxplan)   : fraction of area covered by rock that is
c                      outside plant canoy, i.e. in rill (0-1)
c     roki(mxplan)   : fraction of area covered by rock that is
c                      under plant canoy, i.e. in interrill (0-1)
c     basr(mxplan)   : fraction of area covered by basal vegetation
c                      that is outside plant canopy, i.e. in rill (0-1)
c     basi(mxplan)   : fraction of area covered by basal vegetation
c                      that is under plant canopy, i.e. in rill (0-1)
c     cryr(mxplan)   : fraction of area covered by cryptogams
c                      that is outside plant canopy, i.e. in rill (0-1)
c     cryi(mxplan)   : fraction of area covered by cryptogams
c                      that is under plant canopy, i.e. in rill (0-1)
c     rescof(mxplan) : coefficient that relates litter cover to mass
c                      for continuous simulation
c     cancof(mxplan) : coefficient that relates canopy cover to standing
c                      biomass for continuous simulation
c
c     end include file crinpt1a.inc

