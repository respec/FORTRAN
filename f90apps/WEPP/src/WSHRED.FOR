      subroutine wshred
c
c     + + + PURPOSE + + +
c
c     SR WSHRED initializes remaining hydrologic variables
c     and reads in information from the hillslope to watershed
c     pass file.
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
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pmxcsg.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchprt.inc'
      include 'cenrpa1.inc'
      include 'chydrol.inc'
      include 'cpart1.inc'
      include 'cpart3.inc'
      include 'cstore.inc'
      include 'cstruc.inc'
      include 'cupdate.inc'
      include 'cseddet.inc'
      include 'cstruct.inc'
c
      include 'cflags.inc'
c     modify: chkflg.
c
      include 'coutchn.inc'
c
      include 'cslpopt.inc'
c       read: harea
c
c     + + + LOCAL VARIABLES + + +
c
      integer i, j
      character*8 event
c
c     + + + LOCAL DEFINITIONS + + +
c
c     i     -
c     j     -
c     event -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c     initialize runoff variables for hillslope slots
c
      do 20 i = 1, nhill
        peakro(i) = 0.0
        runoff(i) = 0.0
        runvol(i) = 0.0
        sbrunf(i) = 0.0
        sbrunv(i) = 0.0
c
c       initialize watershed version particle size
c       diameter variable and convert to feet
c
        do 10 j = 1, npart
          crdia(j,i) = dia(j,i) * 3.281
   10   continue
c
   20 continue
c
      do 40 i = 1, nelmt
c
        halpha(i) = 0.0
        htcs(i) = 0.0
c
        hildur(i) = 0.0
        rundur(i) = 0.0
        watdur(i) = 0.0
c
        do 30 j = 1, mxpart
          frcflw(j,i) = 0.0
          sedcon(j,i) = 0.0
   30   continue
c
   40 continue
c
c     read daily information in hillslope to watershed pass file
c
      read (49,1100) event
c
      if (event.eq.'EVENT   ') then
c
        write (6,1000) sdate, year
        chkflg = 1
c
        read (49,1200) (hildur(j),j = 1,nhill)
        read (49,1200) (htcs(j),j = 1,nhill)
        read (49,1200) (halpha(j),j = 1,nhill)
        read (49,1200) (runoff(j),j = 1,nhill)
        read (49,1200) (runvol(j),j = 1,nhill)
cx    Added by Arthur Xu, 06/19/2000
        read (49,1200) (sbrunf(j),j = 1,nhill)
        read (49,1200) (sbrunv(j),j = 1,nhill)
cx    End adding
        read (49,1200) (peakro(j),j = 1,nhill)
        read (49,1200) (tdet(j),j = 1,nhill)
        read (49,1200) (tdep(j),j = 1,nhill)
        read (49,1300) ((sedcon(i,j),i = 1,npart),j = 1,nhill)
        read (49,1300) ((frcflw(i,j),i = 1,npart),j = 1,nhill)
c
        do 60 i = 1, nhill
c
          rofave(i) = runvol(i) / wsarea(i)
          watdur(i) = hildur(i)
c
          detm(i) = detm(i) + tdet(i)
          dety(i) = dety(i) + tdet(i)
          dett(i) = dett(i) + tdet(i)
c
          depm(i) = depm(i) + tdep(i)
          depy(i) = depy(i) + tdep(i)
          dept(i) = dept(i) + tdep(i)
c
          hrom(i) = hrom(i) + runvol(i)
          hroy(i) = hroy(i) + runvol(i)
          hrot(i) = hrot(i) + runvol(i)
cx    Added by Arthur Xu, 06/19/2000
          sbrfty(i) = sbrfty(i) + sbrunv(i)
          sbrvty(i) = sbrvty(i) + sbrunv(i)
          sbrftm(i) = sbrftm(i) + sbrunv(i)
cx    End adding
c
          do 50 j = 1, npart
c
            hsedm(i) = hsedm(i) + sedcon(j,i) * runvol(i)
            hsedy(i) = hsedy(i) + sedcon(j,i) * runvol(i)
            hsedt(i) = hsedt(i) + sedcon(j,i) * runvol(i)
c
   50     continue
c
   60   continue
cx    Added by Arthur Xu, 06/19/2000
      else if (event.eq.'SUBEVENT') then
        chkflg = 2
c
      read (49,1200) (sbrunf(j),j = 1,nhill)
      read (49,1200) (sbrunv(j),j = 1,nhill)
c
      do 70 i = 1, nhill
        sbrfty(i) = sbrfty(i) + sbrunv(i)
        sbrvty(i) = sbrvty(i) + sbrunv(i)
        sbrftm(i) = sbrftm(i) + sbrunv(i)
70      continue
c
      else if (event.eq.'NO EVENT') then
        chkflg = 0
cx    End adding
c
      end if
c
      return
c
 1000 format (1x,'ROUTING  hillslope pass file event on day ',i4,
     1    ' of year ',i4)
 1100 format (a)
 1200 format (75(e11.5,1x))
 1300 format (75(5(e11.5,1x)))
      end
