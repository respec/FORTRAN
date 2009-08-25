c     begin include file cirspri.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /irspri/ are specifically for sprinkler
c     irrigation.

      common /irspri/   aprati(mxplan),irdmax,irdur,irint(mxplan),
     1                  irrate(mxplan),noirr2(mxplan),nozzle(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           noirr2
      real              aprati,irdmax,irdur,irint,irrate,nozzle

c     + + + VARIABLE DEFINITIONS + + +

c     noirr2(mxplan) : flag indicating whether irrigation occurs today
c                      (0-no,1-yes)
c     aprati(mxplan) : user defined ratio of application depth to
c                      allowable irrigation depth (m/m)
c     irdmax         : maximum irrigation depth (m)
c     irdur          : duration of the irrigation (s)
c     irint(mxplan)  : rate of irrigation application (m/s)
c     irrate(mxplan) : rate of irrigation application (m/s)
c     nozzle(mxplan) : nozzle adjustment factor for interrill soil
c                      detachment

c     end include file cirspri.inc
