      subroutine wshiqi
c
c     + + + PURPOSE + + +
c
c     SR WSHIQI calculates the inflow volume and the peak inflow into
c     an impoundment.
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
      include 'cflags.inc'
      include 'chydrol.inc'
      include 'cimpnd.inc'
      include 'cstore.inc'
      include 'cstruct.inc'
c
c
c     + + + LOCAL VARIABLES + + +
c
      real peaksu(3), qpmax
      integer icnt, ieltmp(3), j
c
c     + + + LOCAL DEFINITIONS + + +
c
c     peaksu(3) -
c     qpmax -
c     icnt -
c     ieltmp(3) -
c     j - counter
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     peak
c     wshscs
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c     the total runoff volume history for the upstream elements  is
c     known at this point (runoff volume from hillslopes as read in
c     from the pass file and runoff volume on the channels as
c     calculated from the channel water balance)
c
      idflag = 0
c
      ipond = ipond + 1
c
c     the duration for the impoundment element is the maximum
c     duration of the watershed elements that may feed it
c
      watdur(ielmt) = amax1(watdur(nhleft(ielmt)),
     1    watdur(nhrght(ielmt)),watdur(nhtop(ielmt)),
     1    watdur(ncleft(ielmt)),watdur(ncrght(ielmt)),
     1    watdur(nctop(ielmt)))
c
c     impoundments can fed by either ONE hillslope or MULTIPLE
c     channels (up to three) but not by both
c
      if ((ncleft(ielmt).eq.0).and.(ncrght(ielmt).eq.0).and.(
     1    nctop(ielmt).eq.0)) then
c
c       no channels are feeding the impoundment
c
        idflag = 1
c
c       determine which hillslope is contributing runoff,
c       runoff volume and peak runoff to the impoundment
c
        if (nhleft(ielmt).gt.0) then
          runvol(ielmt) = tmpvol(nhleft(ielmt))
          runoff(ielmt) = tmprof(nhleft(ielmt))
          peakin(ielmt) = tmppkr(nhleft(ielmt))
c
        else if (nhrght(ielmt).gt.0) then
c
          runvol(ielmt) = tmpvol(nhrght(ielmt))
          runoff(ielmt) = tmprof(nhrght(ielmt))
          peakin(ielmt) = tmppkr(nhrght(ielmt))
c
        else if (nhtop(ielmt).gt.0) then
c
          runvol(ielmt) = tmpvol(nhtop(ielmt))
          runoff(ielmt) = tmprof(nhtop(ielmt))
          peakin(ielmt) = tmppkr(nhtop(ielmt))
        end if
c
c       calculate the average runoff entering the impoundment
c       from the hillslope
c
        rofave(ielmt) = runvol(ielmt) / wsarea(ielmt)
c
c       set variable to track runoff volume entering impoundment
c
        rvoimp(ielmt) = runvol(ielmt)
c
      else
c
c       one or more channels are feeding the impoundment
c
        idflag = 2
c
        runvol(ielmt) = tmpvol(ncleft(ielmt)) + tmpvol(ncrght(ielmt)) +
     1      tmpvol(nctop(ielmt))
c
c       calculate the average runoff entering the impoundment
c       from the channel(s)
c
        rofave(ielmt) = runvol(ielmt) / wsarea(ielmt)
c
c       set variable to track runoff volume entering impoundment
c
        rvoimp(ielmt) = runvol(ielmt)
c
        runoff(ielmt) = runvol(ielmt) / (charea(idelmt(ncleft(ielmt)))+
     1      charea(idelmt(ncrght(ielmt)))+charea(idelmt(nctop(ielmt))))
c
c       peak runoff into an impoundment depends on the contributing
c       hillslope or channel elements (one hillslope or up to
c       three channels may contribute to an impoundment)
c
c       initialize local variables
c
        do 10 j = 1, 3
          peaksu(j) = 0.0
          ieltmp(j) = 0
   10   continue
c
        icnt = 0
c
        call peak(peaksu,peakro(nhtop(ielmt)),nhtop(ielmt),ieltmp,icnt)
        call peak(peaksu,peakot(ncleft(ielmt)),ncleft(ielmt),ieltmp,icnt
     1      )
        call peak(peaksu,peakot(ncrght(ielmt)),ncrght(ielmt),ieltmp,icnt
     1      )
        call peak(peaksu,peakot(nctop(ielmt)),nctop(ielmt),ieltmp,icnt)
c
        if (icnt.eq.1) peakin(ielmt) = peaksu(1)
c
        if (icnt.gt.1) then
c
c         more than one channel element (only one hillslope element
c         can contribute to an impoundment so superposition of
c         hillslope runoff is not necessary) is contributing runoff
c         to the impoundment - calculate SCS Triangular Hydrographs
c         and superimpose to find a new peak runoff rate
c
          call wshscs(peaksu,ieltmp,icnt,qpmax)
c
          peakin(ielmt) = qpmax
c
        end if
c
      end if
c
      return
      end
