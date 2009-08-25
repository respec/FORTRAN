      subroutine runout
c
c     + + + PURPOSE + + +
c
c     SR RUNOUT outputs the watershed runoff balance.
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
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
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
      include 'cstore.inc'
      include 'cstruc.inc'
      include 'cstruct.inc'
      include 'cupdate.inc'
      include 'cwint.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real runbef
c
      character*3 text(12)
      character*40 runsrc
c
c     + + + LOCAL DEFINITIONS + + +
c
c     runbef    -
c     runsrc    -
c     text(12)  - text for month of year
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
      data text /'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug',
     1    'sep', 'oct', 'nov', 'dec'/
c
c     + + + END SPECIFICATIONS + + +
c
c
      write (59,1000) text(mon), day, year, sdate, prcp * 1.0e3
c
      write (59,1200) nhill
c
      if (ipeak.eq.1) write (59,2600)
      if (ipeak.eq.2) write (59,2700)
c
      do 10 ielmt = 1, nhill
        if (ipeak.eq.1) write (59,2800) ielmt, htcs(ielmt),
     1      halpha(ielmt)
        if (ipeak.eq.2) write (59,2800) ielmt, htcs(ielmt)
   10 continue
c
      write (59,2900) nhill + 1, nelmt
c
      do 20 ielmt = nhill + 1, nelmt
c
        write (59,1100)
c
        if (elmt(ielmt).eq.2.and.runvol(ielmt).gt.0.0) then
          if (ipeak.eq.1) write (59,1300)
          if (ipeak.eq.2) write (59,1400)
        end if
c
        if (elmt(ielmt).eq.3.or.runvol(ielmt).eq.0.0) write (59,1500)
c
        if (runvol(ielmt).gt.0.0) then
c
          if (elmt(ielmt).eq.2) then
            if (ipeak.eq.1) write (59,1600) ielmt, ieltyp(ielmt),
     1          idelmt(ielmt), htcc(ielmt), walpha(ielmt)
            if (ipeak.eq.2) write (59,1700) ielmt, ieltyp(ielmt),
     1          idelmt(ielmt), htcc(ielmt)
          end if
c
          if (elmt(ielmt).eq.3) write (59,1800) ielmt, ieltyp(ielmt),
     1        idelmt(ielmt)
        else
          write (59,1900) ielmt, ieltyp(ielmt), idelmt(ielmt)
        end if
c
        if (elmt(ielmt).eq.2) then
c
          if (idflag.eq.2) then
            if (wmelt(idelmt(ielmt)).gt.0.0) then
              runsrc = 'channel runoff only with snowmelt'
            else
              runsrc = 'channel runoff only'
            end if
c
          else if (idflag.eq.3) then
            runsrc = 'channel runon only'
c
          else if (idflag.eq.4) then
c
            if (wmelt(idelmt(ielmt)).gt.0.0) then
              runsrc = 'channel runon and runoff with snowmelt'
            else
              runsrc = 'channel runon and runoff'
            end if
          end if
        end if
c
        if (elmt(ielmt).eq.3) then
c
          if (idflag.eq.1) then
            runsrc = 'hillslope runoff'
          else if (idflag.eq.2) then
            runsrc = 'channel runoff'
          end if
c
        end if
c
        if (runvol(ielmt).gt.0.0) write (59,2000) runsrc
c
        if ((elmt(ielmt).eq.2).and.(wmelt(idelmt(ielmt)).gt.0.0)) write
     1      (59,2100) wmelt(idelmt(ielmt)) * 1.0e3
c
        if (elmt(ielmt).eq.2) then
c
          write (59,2200)
c
          runbef = rvolon(ielmt) + chnvol(ielmt)
c
          write (59,2300) rvotop(ielmt), rvolat(ielmt), chnvol(ielmt),
     1        runbef, rtrans(ielmt), runvol(ielmt), peakin(ielmt),
     1        peakot(ielmt)
c
        end if
c
        if (elmt(ielmt).eq.3) then
          write (59,2400)
          write (59,2500) rvoimp(ielmt), runvol(ielmt), peakin(ielmt),
     1        peakot(ielmt)
        end if
c
   20 continue
c
      return
 1000 format (//9x,60('e'),//,18x,'Runoff Event Summary',/,18x,20('-'),
     1    //,18x,'Calendar Date',10x,a3,1x,i2,',',i4,/,18x,
     1    'Julian Date',12x,i3,/,18x,'Rainfall',12x,f6.2,2x,'(mm)')
 1100 format (/9x,60('-'))
 1200 format (//18x,'Hillslope elements: 1 - ',i2/)
 1300 format (/6x,'Element',5x,'Element',5x,'Channel/Impoundment',3x,
     1    'Time of',6x,'Channel',/,6x,'Number',7x,'Type',13x,'Number',10
     1    x,'Conc. (hr)',4x,'Alpha',/,6x,6('-'),7x,4('-'),13x,6('-'),10
     1    x,10('-'),4x,5('-'))
 1400 format (/11x,'Element',5x,'Element',5x,'Channel/Impoundment',3x,
     1    'Time of',/,11x,'Number',7x,'Type',13x,'Number',10x,
     1    'Conc. (hr)',/,11x,6('-'),7x,4('-'),13x,6('-'),10x,10('-'))
 1500 format (//18x,'Element',5x,'Element',5x,'Channel/Impoundment',/,18
     1    x,'Number',7x,'Type',13x,'Number',/,18x,6('-'),7x,4('-'),13x,6
     1    ('-'))
 1600 format (7x,i3,8x,a11,9x,i2,12x,f8.4,5x,f6.2)
 1700 format (12x,i3,8x,a11,9x,i2,12x,f8.4,5x,f6.2)
 1800 format (19x,i3,8x,a11,9x,i2)
 1900 format (19x,i3,8x,a11,9x,i2,//,18x,
     1    '*** NO RUNOFF VOLUME ON THIS ELEMENT ***')
 2000 format (//18x,'Runoff Source:',3x,a)
 2100 format (18x,'Snowmelt Amount:',4x,f8.3,' (mm)')
 2200 format (//21x,'Runoff Volumes (m^3)',17x,'Peak',7x,'Peak',/,1x,
     1    'Top',5x,'Lat.',5x,'Chan.',2x,'Before Trans.',2x,'Trans.',3x,
     1    'Final',4x,'Runoff',5x,'Runoff',/,1x,'Runon',3x,'Runon',4x,
     1    'Runoff',5x,'Losses',5x,'Losses',3x,'Runoff',3x,'Entering',3x,
     1    'Exiting',/,1x,'Volume',2x,'Volume',3x,'Volume',5x,'Volume',5
     1    x,'Volume',3x,'Volume',3x,'(m^3/s)',4x,'(m^3/s)',/,1x,6('-'),2
     1    x,6('-'),3x,6('-'),5x,6('-'),5x,6('-'),3x,6('-'),3x,7('-'),4x,
     1    7('-'))
 2300 format (f7.2,1x,f7.2,2x,f7.2,4x,f7.2,4x,f7.2,3x,f7.2,2x,f9.5,1x,f9
     1    .5)
 2400 format (//4x,'Impoundment Runoff Volume (m^3)',5x,
     1    'Impoundment Peak Runoff (m^3/s)',/,8x,'Entering',5x,
     1    'Exiting',17x,'Entering',7x,'Exiting',/,8x,8('-'),5x,7('-'),17
     1    x,8('-'),7x,7('-'))
 2500 format (8x,f8.3,5x,f8.3,16x,f8.3,7x,f8.3)
 2600 format (18x,'Hillslope',6x,'Time of',9x,'Hillslope',/,19x,
     1    'element',6x,'conc.(hr)',9x,'Alpha',/,19x,7('-'),6x,9('-'),9x,
     1    5('-'))
 2700 format (18x,'Hillslope',6x,'Time of',/,19x,'element',6x,
     1    'conc.(hr)',/,19x,7('-'),6x,9('-'))
 2800 format (21x,i2,8x,f8.2,10x,f6.2)
 2900 format (//18x,'Channel and Impoundment elements: ',i2,' - ',i2/)
      end
