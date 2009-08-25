      subroutine wshrun
c
c     + + + PURPOSE + + +
c
c     SR WSHRUN routes the runoff from impoundments and
c     channels through the watershed.
c
c     Called from: SR WSHDRV
c     Author(s): Ascough II
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxcsg.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmximp.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cdiss11.inc'
      include 'cirspri.inc'
      include 'coutchn.inc'
      include 'cstore1.inc'
      include 'cstruct.inc'
      
      include 'chydrol.inc'
      include 'cchpek.inc'
      include 'cstruc.inc'
c
c     + + + LOCAL VARIABLES + + +
c
c     + + + LOCAL DEFINITIONS + + +
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     wshpek
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c     calculate the average runoff on the watershed at the outlet
c     of the current channel element
c
      rofave(ielmt) = runvol(ielmt) / wsarea(ielmt)
c
c     save monthly, annual, and total runoff from each channel
c     element
c
      trunm(ielmt) = trunm(ielmt) + runvol(ielmt)
      truny(ielmt) = truny(ielmt) + runvol(ielmt)
      trunt(ielmt) = trunt(ielmt) + runvol(ielmt)
cd    Added by S. Dun, Dec 20, 2007
c     for more output of annual summary of the chnnel
      tronvy(ielmt) = tronvy(ielmt) + rvolon(ielmt)
      tronvt(ielmt) = tronvt(ielmt) + rvolon(ielmt)
      sbrvty(ielmt) = sbrvty(ielmt) + sbrunf(ielmt-nhill)
     1               * charea(ielmt-nhill)
      sbrvtt(ielmt) = sbrvtt(ielmt) + sbrunf(ielmt-nhill)
     1               * charea(ielmt-nhill)
c     End adding      
c
c     the duration for the channel element is the maximum
c     duration of the watershed elements that may feed it
c
cd    Modified by S. Dun 1/08/2004
cd    This duration value has given in WSHCQI already.
cd    Only the dur for channel could be changed after WSHIRS.for
cd
cd      watdur(ielmt) = amax1(watdur(nhleft(ielmt)),
cd     1    watdur(nhrght(ielmt)),watdur(nhtop(ielmt)),
cd     1    watdur(ncleft(ielmt)),watdur(ncrght(ielmt)),
cd     1    watdur(nctop(ielmt)),watdur(nileft(ielmt)),
cd     1    watdur(nirght(ielmt)),watdur(nitop(ielmt)),dur,irdur)
      watdur(ielmt) = amax1(watdur(ielmt),dur)
cd    End modifying
c
c
c     calculate peak runoff using modified Rational method
c     (ipeak = 1) or modified CREAMS method (ipeak = 2)
c
      call wshpek
c
      return
      end
