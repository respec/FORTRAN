
c     begin include file cridge.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /ridge/    iridge(mxtlsq),rilrm(mxres,mxplan),
     1                  rigrm(mxres,mxplan),rigcov(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           iridge
      real              rilrm,rigrm,rigcov

c     + + + VARIABLE DEFINITIONS + + +

c     iridge(mxtlsq)       : ridge tillage system flag
c     rilrm(mxres,mxplan)  : rill residue mass (kg/m^2)
c     rigrm(mxres,mxplan)  : ridge residue mass (kg/m^2)
c     rigcov(mxplan)       : ridge cover (kg/m^2)

c     end include file cridge.inc

