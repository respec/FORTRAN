
c     begin include file cwshed.inc

c     Miscellaneous variables needed for the hillslope/watershed
c     version or the watershed version

      common /wshed/ iwsbyr,maxyrs,wshcli(mxhill), pasfil(mxhill),
     1               watfil

c     + + + VARIABLE DECLARATIONS + + +

      integer         iwsbyr,maxyrs
      character*51    wshcli,pasfil,watfil

c     + + + VARIABLE DEFINITIONS + + +
c
c     iwsbyr         : beginning year check for hillslope climate consistency
c     maxyrs         : maximum simulation time for watershed version
c     wshcli(mxhill) : climate file name for each hillslope in watershed
c     pasfil(mxhill) : hillslope pass file names
c     watfil         : watershed master pass file name

c     end include file cwshed.inc

