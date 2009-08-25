
c     begin include file crinpt3a.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /rinpt3a/   shgt(ntype),spop(ntype),sdiam(ntype),
     1                   ghgt(ntype),scoeff(ntype),gpop(ntype),
     1                   gdiam(ntype),gcoeff(ntype)

c     + + + VARIABLE DECLARATIONS + + +

      real shgt,spop,sdiam,ghgt,scoeff,gpop,gdiam,gcoeff

c     + + + VARIABLE DEFINITIONS + + +

c     shgt(ntype)   : average plant height (m) of shrub plant component
c     spop(ntype)   : average number of plants along a 100 m
c                     transect, shrub plant component
c     sdiam(ntype)  : average canopy diameter for shrub plant component
c     ghgt(ntype)   : average plant height (m) of shrub plant component
c     scoeff(ntype) : projected plant area coefficient, for shrub
c                     plant component
c     gpop(ntype)   : average number of plants along a 100 m transect,
c                     plant component
c     gdiam(ntype)  : average canopy diameter for herb. plant coeff
c     gcoeff(ntype) : projected plant area coefficient, for herb.
c                     plant component

c     end include file crinpt3a.inc

