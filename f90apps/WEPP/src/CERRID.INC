
c     begin include file cerrid.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /errid/  ibomb,ifile,iost,iyfile,crpnam(ntype),scefil

c     + + + VARIABLE DECLARATIONS + + +

      integer         ibomb,ifile,iost,iyfile
      character*51    crpnam,scefil

c     + + + VARIABLE DEFINITIONS + + +

c     ibomb  : drop out upon invalid keyboard entry (y=1, n=2)
c     ifile  : flag for initial condition scenario  (y=1, n=2)
c     iost   : open file status to use based on ibomb
c     iyfile : flag for yearly summary file
c     crpnam : crop names
c     scefil : wepp initial condition scenario file name

c     end include file cerrid.inc


