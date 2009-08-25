
c     begin include file crinpt2.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c
      common /rinpt2/   pptg(ntype),rootf(ntype),pscday(ntype),
     1                  strrgc(ntype),cshape(ntype),dshape(ntype),
     1                  scday2(ntype),strgc2(ntype),eshape(ntype),
     1                  fshape(ntype),rgcmin(ntype),cf1(ntype),
     1                  cf2(ntype),gtemp(ntype),tempmn(ntype),
     1                  root10(ntype),ffp(ntype),pscdst(ntype),
     1                  pscd2s(ntype),shape2,prgc(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              pptg,rootf,pscday,strrgc,
     1                  cshape,dshape,scday2,strgc2,eshape,fshape,
     1                  rgcmin,cf1,cf2,gtemp,tempmn,root10,ffp,
     1                  pscdst,pscd2s,shape2,prgc

c     + + + VARIABLE DEFINITIONS + + +

c     pptg(ntype)   : precipitation during the growing season (m)
c     rootf(ntype)  : fraction of roots from maximum (both live and
c                     dead) at start of year (day 1)
c     rdf(ntype)    : root distribution coefficient for mass by depth
c                     (unitless)
c     pscday(ntype) : day peak standing crop is reached for first
c                     peak on the relative growth curve (1-366)
c     strrgc(ntype) : julian day growth begins (1-366)
c     cshape(ntype) : shaping parameter for the left side of growth
c                     curve
c     dshape(ntype) : shaping parameter for the right side of growth
c                     curve for the first peak
c     scday2(ntype) : day peak standing crop is reached for second
c                     peak on the relative growth curve
c     strgc2(ntype) : julian day second growth period begins (1-366)
c     eshape(ntype) : shaping parameter for the left side of growth
c                     peak for the second peak
c     fshape(ntype) : shaping parameter for the right side of growth
c                     curve for the second peak
c     rgcmin(ntype) : minimum fraction of live biomass at any point
c                     of a year (0-1)
c     cf1(ntype)    : fraction of maximum live standing forage for the
c                     first peak (0-1)
c     cf2(ntype)    : fraction of maximum live standing forage for the
c                     second peak (0-1)
c     gtemp(ntype)  : minimum temperature to start growth in the
c                     spring (degrees C)
c     tempmn(ntype) : minimum temperature for plant to stop growth
c                     in fall (degrees C)
c     root10(ntype) : root biomass in top 10 cm
c     ffp(ntype)    : frost free period
c     pscdst(ntype) :
c     pscd2s(ntype) :
c     shape2        : used to calculate EX for RGC2 in gcurve
c     prgc(mxplan)  : yesterdays's value (prior) for RGC in gcurve

c     end include file crinpt2.inc

