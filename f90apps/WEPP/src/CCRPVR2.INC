
c     begin include file ccrpvr2.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /crpvr2/   cn(ntype),aca(ntype),cf(ntype),
     1                  ar(ntype),rminit,vdmt(mxplan),flivmx(ntype)

c     + + + VARIABLE DECLARATIONS + + +

      real              cn,aca,cf,ar,rminit,vdmt,flivmx

c     + + + VARIABLE DEFINITIONS + + +

c     cn(ntype)     : carbon-nitrogen ratio of residue and roots
c                     (unitless)
c     aca(ntype)    : decomposition constant for flat residue
c     cf(ntype)     : flat residue cover coefficient (ha/kg)
c     ar(ntype)     : decomposition constant for roots
c     rminit        : non-living root mass at beginning of simulation
c                     (kg/m^2)
c     vdmt(mxplan)  : vegetative dry matter (kg/m^2)
c     flivmx(ntype) : maximum friction factor due to living plant
c                     stems and leaves.

c     end include file ccrpvr2.inc
