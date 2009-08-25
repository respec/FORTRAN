
c     begin include file ccrpprm.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /crpprm/   itype(mxcrop,mxplan),jdplt(mxcrop,mxplan),
     1                  jdharv(mxcrop,mxplan),rw(mxcrop,mxplan),
     1                  yld(ntype),itill(mxtlsq,mxtill),
     1                  iresd(mxres,mxplan),grazig(mxplan),
     1                  resmgt(mxcrop,mxplan),
     1                  frmove(mxtill,mxtlsq),resad(mxtill,mxtlsq),
     1                  jdmove(mxtill,mxtlsq),iresad(mxtlsq,mxtill),
     1                  btemp(ntype),nycrop(mxplan),
     1                  iroot(mxres,mxplan),rngout,iplant(ntype),
     1                  jdsene(mxcrop,mxplan),rngplt,rnganm

c     + + + VARIABLE DECLARATIONS + + +

      integer           itype,jdplt,jdharv,itill,iresd,grazig,
     1                  resmgt,nycrop,iroot,rngout,iplant,jdsene,
     1                  rnganm, rngplt,jdmove,iresad
      real              rw,yld,btemp,frmove,resad

c     + + + VARIABLE DEFINITIONS + + +


c     itype(mxcrop, mxplan)  : plant type
c     jdplt(mxcrop, mxplan)  : planting date in Julian day (1-366)
c     jdharv(mxcrop, mxplan) : harvesting date in Julian day (1-366)
c     rw(mxcrop,mxplan)      : row width (m)
c     yld(ntype)             : crop yield (kg/m^2)
c     itill(mxtlsq, mxtill)  : type of tillage for current day
c     iresd(mxres,mxplan)    : residue type
c     grazig(mxplan)         : a flag for grazing occurrence
c     resmgt(mxcrop, mxplan) : residue management option
c                              1 = herbicide application
c                              2 = burning
c                              3 = silage
c                              4 = shredding
c                              6 = none
c     btemp(ntype)           : base daily air temperature (degrees C)
c     nycrop (mxplan)        : number of crops per year
c     iroot(mxres,mxplan)    :
c     rngout                 :
c     rnganm                 : rangeland animal output ,1=no, 2=yes
c     rngplt                 : rangeland plant output, 1=no, 2=yes
c     iplant(ntype)          :
c     jdsene(mxcrop,mxplan)  : user input value for approximate date to
c                              reach senescence for a perennial crop.
c                              A value of ZERO for this variable
c                              indicates to subroutine GROW that this
c                              particular perennial plant is not to
c                              senesce due to degree day accumulation.

c     end include file ccrpprm.inc