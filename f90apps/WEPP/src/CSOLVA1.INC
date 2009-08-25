
c     begin include file csolva1.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c     NOTE : SOLVA2 is used in SR ROUTE.

      common /solva1/   sand(mxnsl,mxelem), silt(mxnsl,mxelem),
     1                  clay(mxnsl,mxelem), orgmat(mxnsl,mxelem)

c     + + + VARIABLE DECLARATIONS + + +

      real              sand,silt,clay,orgmat

c     + + + VARIABLE DEFINITIONS + + +

c     sand(mxnsl,mxelem)   : fraction sand (0-1)
c     silt(mxnsl,mxelem)   : fraction silt (0-1)
c     clay(mxnsl,mxelem)   : fraction clay (0-1)
c     orgmat(mxnsl,mxelem) : fraction organic matter (0-1)

c     end include file csolva1.inc

