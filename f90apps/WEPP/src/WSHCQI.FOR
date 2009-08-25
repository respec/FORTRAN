      subroutine wshcqi
c
c     + + + PURPOSE + + +
c
c     SR WSHCQI calculates the inflow volume into a channel and
c     the duration of the event for that channel.
c
c     Called from: SR WSHDRV
c     Author(s): Ascough II, Baffaut
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
      include 'cchpek.inc'
      include 'cdiss11.inc'
c
      include 'cdist2.inc'
      include 'cefflen.inc'
c
      include 'cirspri.inc'
      include 'cstore.inc'
      include 'cstruc.inc'
      include 'cstruct.inc'
c
c     + + + LOCAL VARIABLES + + +
c
c     + + + LOCAL DEFINITIONS + + +
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c     Compute the runoff volume coming onto the channel element and
c     convert it into runoff depth over the channel area
c
c     total runoff volume entering channel laterally (may come from
c     hillslopes or impoundments)
c
cx    Modified by Arthur Xu, Incoporated in by S. Dun 01/06/2004
cx    All subsurface flow from upsteam hillslopes is added into 
cx    channel directly.
cx      rvolat(ielmt) = tmpvol(nhleft(ielmt)) + tmpvol(nileft(ielmt)) +
cx     1    tmpvol(nhrght(ielmt)) + tmpvol(nirght(ielmt))
      rvolat(ielmt) = tmpvol(nhleft(ielmt)) + tmpvol(nileft(ielmt)) +
     1    tmpvol(nhrght(ielmt)) + tmpvol(nirght(ielmt))
     1    + tmpsbv(nhleft(ielmt)) + tmpsbv(nhrght(ielmt))
cx    End Modifying.
c
c
c     total runoff volume entering channel from top (may be fed
c     from hillslope, impoundment, or multiple channels)
c
c     even though channels are read in as feeding from the left,
c     right, or top, all channels really feed another channel from
c     the top only - for this reason ncleft(ielmt), ncrght(ielmt), and
c     nctop(ielmt) are summed under rvotop(ielmt)
c
cx    Modified by Arthur Xu, Incoporated in by S. Dun 01/06/2004
cx    All subsurface flow from upsteam hillslopes is added into 
cx    channal directly.
cx      rvotop(ielmt) = tmpvol(nhtop(ielmt)) + tmpvol(ncleft(ielmt)) +
cx     1    tmpvol(ncrght(ielmt)) + tmpvol(nctop(ielmt)) +
cx     1    tmpvol(nitop(ielmt))
      rvotop(ielmt) = tmpvol(nhtop(ielmt)) + tmpvol(ncleft(ielmt)) +
     1    tmpvol(ncrght(ielmt)) + tmpvol(nctop(ielmt)) +
     1    tmpvol(nitop(ielmt)) 
     1    + tmpsbv(nhtop(ielmt))
cx    End Modifying.
c
c     total runoff volume and depth added to channel through
c     lateral and top runon
c
      rvolon(ielmt) = rvolat(ielmt) + rvotop(ielmt)
      roffon(ielmt) = rvolon(ielmt) / charea(iplane)
c
cd    Added by S. Dun, April 10, 2008
c     To fix the problem of too large peakrunoff volume by traeting 
c     surface and subsurface from hillslopes differently. 
c     Subsurface flow has a duration of 24 hours
c
          sbrunv(0) = 0.
          tmpsbv(0) = 0.
          sbrunv(ielmt) = tmpsbv(nhleft(ielmt)) + tmpsbv(nhrght(ielmt))
     1                  + tmpsbv(nhtop(ielmt))
     1                  + sbrunv(ncleft(ielmt)) + sbrunv(ncrght(ielmt)) 
     2                  + sbrunv(nctop(ielmt)) + sbrunv(nitop(ielmt))
     3                  + sbrunv(nileft(ielmt)) + sbrunv(nirght(ielmt))                               
cd    End adding
c
c     the duration for the channel element is the maximum
c     duration of the watershed elements that may feed it
c     and is calculated in case there is no runoff produced
c     on the channel (in which case SR TRNLOS may be called
c     to estimate transmission losses)
c
      watdur(ielmt) = amax1(watdur(nhleft(ielmt)),
     1    watdur(nhrght(ielmt)),watdur(nhtop(ielmt)),
     1    watdur(ncleft(ielmt)),watdur(ncrght(ielmt)),
     1    watdur(nctop(ielmt)),watdur(nileft(ielmt)),
     1    watdur(nirght(ielmt)),watdur(nitop(ielmt)),dur,irdur)
cd    Added by S. Dun 1/08/2004
      if ((rvolon(ielmt).gt.0.0).and.(watdur(ielmt).le.0.0)) 
     1    watdur(ielmt) = 24.0*60.0*60.0 
cd    End adding    
c
      efflen(ichan) = amax1(efflen(idelmt(ncleft(ielmt))),
     1                efflen(idelmt(nctop(ielmt))),
     1                efflen(idelmt(ncrght(ielmt))),
     1                0.0)
      efflen(ichan) = efflen(ichan) + slplen(ichan)
c
      return
      end
