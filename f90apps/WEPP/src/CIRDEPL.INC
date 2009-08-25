
c     begin include file cirdepl.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c     Variables in common /irdepl/ are common to all irrigation systems
c     but specifically for depletion level irrigation scheduling.

      common /irdepl/   deplev(mxplan),depsev,iramt,irbeg(mxplan),
     1                  irdmin,irend(mxplan),yrbeg(mxplan),
     1                  yrend(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           irbeg,irend,yrbeg,yrend
      real              deplev,depsev,iramt,irdmin

c     + + + VARIABLE DEFINITIONS + + +

c     deplev(mxplan) : allowable available soil water depletion level
c                      for each overland flow element (m/m)
c     depsev         : variable for use with non-uniform hydrology, the
c                      severity of available soil water depletion for an
c                      overland flow element (m/m)
c     iramt          : depth of water to be applied by irrigation (m)
c     irbeg(mxplan)  : julian date of beginning of irrigation period
c     irdmin         : minimum irrigation depth (m)
c     irend(mxplan)  : julian date of end of irrigation period
c     yrbeg(mxplan)  : beginning year for irrigation period (relative
c                      value ... 1 for first simulation year, etc.)
c     yrend(mxplan)  : ending year for irrigation period (relative
c                      value)

c     end include file cirdepl.inc
