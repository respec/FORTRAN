
c     begin include file cparame.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /parame/   ks(mxplan),sm(mxplan),ksm,tt,tp(mxpond),
     1                  ts,cu,cp,pt(mxpond),
     1                  por(mxnsl,mxplan),sat(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              ks,sm,ksm,tt,tp,ts,cu,cp,pt,por,sat

c     + + + VARIABLE DEFINITIONS + + +

c     ks(mxplan)       : saturated hydraulic conductivity (m/s)
c     sm(mxplan)       : effective matric potential (m) for Green-Ampt
c     tp(mxpond)       : time to ponding (s)
c     ksm              : product Ks*sm
c     tt               : real infiltration time
c     ts               : pseudo time to adjust the real time for
c                        infiltration (s)
c     cu               : ponding indicator when no ponding at the
c                        beginning of interval (cu < 0 implies no
c                        ponding, cu > 0 implies ponding)
c     cp               : ponding indicator when ponded at the beginning
c                        of interval (cp < 0 implies ponding stops
c                        during interval, cp > 0 implies ponding)
c     pt(mxpond)       : accumulated rainfall at time of ponding (m)
c     por(mxplan,mxsl) : porosity for each soil
c     sat(mxplan)      : soil saturation index

c     end include file cparame.inc

