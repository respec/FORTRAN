
c     begin include file cdata2.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /data2/    rcum(mxtime),f(mxtime),ff(mxtime),recum(mxtime)

c     + + + VARIABLE DECLARATIONS + + +

      real              rcum,f,ff,recum

c     + + + VARIABLE DEFINITIONS + + +

c     f(mxtime)     : infiltration rate (m/s)
c     ff(mxtime)    : accumulated infiltration depth (m)
c     recum(mxtime) : accumulated rainfall excess depth (m)
c     rcum(mxtime)  : accumulated rainfall depth (m)

c     end include file cdata2.inc

