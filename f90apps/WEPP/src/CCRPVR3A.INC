
c     begin include file ccrpvr3a.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /crpvr3a/  hmax(ntype),decfct(ntype),mfocod(ntype),
     1                  crit(ntype),gddmax(ntype),bb(ntype),bbb(ntype),
     1                  rdmax(ntype),rsr(ntype),spriod(ntype),
     1                  xmxlai(ntype),dlai(ntype),dropfc(ntype),
     1                  gddmip(ntype)

c     + + + VARIABLE DECLARATIONS + + +

      real              hmax,crit,decfct,gddmax,bb,bbb,rdmax,
     1                  rsr,dlai,xmxlai,dropfc,gddmip
      integer           spriod,mfocod

c     + + + VARIABLE DEFINITIONS + + +

c     hmax(ntype)   : maximum plant height (m)
c     crit(ntype)   : growing degree days to emergence
c     bb(ntype)     : parameter value for canopy cover equation
c                     (unitless)
c     bbb(ntype)    : parameter value for canopy height equation
c                     (unitless)
c     rsr(ntype)    : root to shoot ratio, (unitless)
c     spriod(ntype) : period over which senescence occurs (days)
c     dlai(ntype)   : fraction of growing season when leaf area index
c                     starts to decline (0-1), unitless
c     xmxlai(ntype) : maximum leaf area index (unitless)
c     rdmax(ntype)  : maximum root depth (m)
c     gddmax(ntype) : growing degree days at maturity (degrees C)
c     decfct(ntype) : fraction of canopy cover remaining after
c                     senescence (0-1, unitless)
c     dropfc(ntype) : fraction of biomass remaining after
c                     senescence (0-1, unitless)
c     mfocod(ntype) : code for type of residue
c                     (1 - fragile, 2 - nonfragile)
c     gddmip(ntype) : Input file value for GDDMAX.  User may input a
c                     value of 0 for GDDMAX - but model will then
c                     calculate one.  This variable (GDDMIP) allows
c                     us to preserve the user input zero value when
c                     the initial conditions output file is written

c     end include file ccrpvr3a.inc

