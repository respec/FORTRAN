      subroutine winit
c
c     +++PURPOSE+++
c     This subroutine initializes those values required for
c     use in the winter routines.  Since initial frost depth
c     is allowed to be read from a file, we cannot sinmply set
c     to zero.  We must calculate on basis of initial frost depth
c     and soil water content.
c
c     Author(s):  John Witte, UofMn WCES @ USDA-ARS-NCSRL
c     Date: 01/26/94

c     Verified and tested by Reza Savabi, USDA-ARS, NSERL 317-494-5051
c                  August 1994
c
c     +++ARGUMENT DECLARATIONS+++
c
c     +++ARGUMENT DEFINITIONS+++
c
c     +++PARAMETERS+++
      include 'pmxelm.inc'
      include 'pmxcrp.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxslp.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'pmxres.inc'
c
c     +++COMMON BLOCKS+++
      include  'cclim.inc'
c       read:  tave,tmnavg,tmxavg
      include  'ccrpvr5.inc'
c       read:  diam
      include  'ccons.inc'
c       read:  bdcons
c      include  'ccrpprm.inc'
c       read:  iresd
      include  'ccover.inc'
c       read:  lanuse
      include  'ccrpout.inc'
c       read:  bd
      include  'chydrol.inc'
c       read:  rain
      include  'cstruc.inc'
c       read:  iplane
      include  'cupdate.inc'
c       read:  day,year,sdate
      include  'cwint.inc'
c       read:  wmelt(mxplan),drift,snodpt(mxplan),azm,deglat
      include  'cparame.inc'
c       read:  sm(mxplan)
      include  'cslope2.inc'
c       read:  avgslp(mxplan)
      include  'cwater.inc'
c       read:  solthk(mxplan,mxplan)
      include  'ccrpvr1.inc'
c       read:  rmogt
      include  'ctillge.inc'
c       read:  tildep(mxplan,mxplan)
c
c     +++LOCAL VARIABLES+++
c     save
      real    smtill,smutil
      integer i, j
c
c     +++LOCAL DEFINITIONS+++
c
c     +++END SPECIFICATIONS+++

c -- NO initial frost condition read from input file....................
      tilld(iplane) = 0.2
      do 3 j=1, 24
          hrmlt(j,iplane) = 0.0
   3  continue
c
      if (frdp(iplane) .lt. 0.0001) then
        tfrdp(iplane) = 0.0
        tthawd(iplane) = 0.0
        frdp(iplane)  = 0.0
        thdp(iplane) = 0.0


c -- Initial frost condition read from file.......................
c -- Here we assume frost exists from surface to bottom of frost
c -- depth layer.  No top frost or thaw exists.

      else
cweijun 4/15/94        thdp(iplane) = 0.0
        tfrdp(iplane) = 0.0
        tthawd(iplane) = 0.0
c
      endif
c
      snodpt(iplane) = snodpy(iplane)
c
      wdayct(iplane) = 0
      fsdfg(iplane) = 0
c
cd    Added by S. Dun, January 07, 2008
c     To fit air temperature curve for seasonal change in a year
c     The curve would be used in forst simulation
      call tmpCFT
      call saxpar
cd    End adding
      return
      end
