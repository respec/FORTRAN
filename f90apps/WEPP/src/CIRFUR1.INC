
c     begin include file cirfur1.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /irfur1/ are specifically for furrow
c     irrigation and must be initialized in INIDAT.

      common /irfur1/   aflow(0:xsteps,2),infltr(0:xsteps,2),
     1                  irapld(mxplan),irqin,nsurge,qflow(0:xsteps,2),
     1                  spavz,tstart(mxsrg),iraplo(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           nsurge
      real              aflow,infltr,irapld,irqin,spavz,tstart,qflow,
     1                  iraplo

c     + + + VARIABLE DEFINITIONS + + +

c     aflow ()       : flow area at time-distance nodes (m**2)
c     infltr()       : infiltration at time-distance nodes (m**2)
c     irapld(mxplan) : irrigation water volume supplied to an overland
c                      flow element per unit area (m)
c     irqin          : maximum inflow rate per unit width (m**2/s)
c     nsurge         : number of surges for next overland flow element
c     qflow()        : flow rate at time-distance nodes (m**3)
c     spavz          : space averaging coefficient for infiltration
c     tstart(mxsrg)  : begin time for irrigation inflow (s)
c
cd    Added by S. Dun 03/03/2004, irapld is use for irrigation depth on 
cd          effective length, iraplo is added only for current OFE
c 
c     iraplo(mxplan) : irrigation water volume supplied to an overland
c                      flow element per unit area (m)

c     end include file cirfur1.inc

