
c     begin include file crout.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /rout/     feed,unbio,utiliz,tfood(mxplan),tlive(mxplan),
     1                  slive,rroot(mxnsl,mxplan),lroot(mxnsl),
     1                  droot(mxnsl),rooty(mxnsl,mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              feed,unbio,utiliz,tfood,tlive,slive,
     1                  rroot,lroot,droot,rooty

c     + + + VARIABLE DEFINITIONS + + +

c     feed                : daily mass of forage consumed per animal
c     unbio               : forage unavailable for consumption (kg/m^2)
c     utiliz              : fraction of forage consumed during the grazing
c                           season (0-1)
c     tfood(mxplan)       : total forage consumed by grazing animals (kg/year)
c     slive               : new plant growth on day on simulation (kg/m^2)
c     tlive(mxplan)       : total live plant material on day of simulation
c                           (kg/m^2)
c     rroot(mxnsl,mxplan) : roots for each soil layer today (kg/m^2)
c     lroot(mxnsl)        : new roots added for each soil layer on day
c                           of simulation (kg/m^2)
c     droot(mxnsl)        : fraction of total roots in a soil layer (0-1)
c     rooty(mxnsl,mxplan) : total root mass in a soil layer on day of
c                           simulation

c     end include file crout.inc

