
c     begin include file crinpt5.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /rinpt5/   thgt(ntype),tpop(ntype),tdiam(ntype),
     1                  tcoeff(ntype)

c     + + + VARIABLE DECLARATIONS + + +

      real              thgt,tpop,tdiam,tcoeff

c     + + + VARIABLE DEFINITIONS + + +

c     thgt(ntype)   : average tree height, meters
c     tpop(ntype)   : average number of plants along a 100 m
c                     transect, shorter plant component
c     tdiam(ntype)  : canopy diameter, meters tree plant component
c     tcoeff(ntype) : projected plant area, plant coefficient

c     end include file crinpt5.inc

