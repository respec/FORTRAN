      subroutine impyr(iyr)
c
c     + + + PURPOSE + + +
c
c     SR IMPYR outputs and resets yearly impoundment variables.
c
c     Called from: SR WSHDRV
c     Author(s): Jim Ascough II
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
      include 'pmximp.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer iyr
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     iyr -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cflags.inc'
c     read: ipdout
c
      include 'cimday.inc'
      include 'cimitf.inc'
      include 'cimpnd.inc'
      include 'cimsed.inc'
      include 'cimyrs.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      character*4 otchar
      character*6 filchr
c
c     + + + LOCAL DEFINITIONS + + +
c
c     otchar - overtopping character string
c     filchr - sediment filling character string
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c     begin multiple impoundment loop
c
      do 10 ipond = 1, npond
c
        if ((npond.eq.1).and.(ipdout.eq.1)) then
          write (57,1100)
          write (56,1200)
        end if
c
        otchar = '    '
        filchr = '      '
c
        rety(ipond) = totiny(ipond) - couty(ipond)
        clrety(ipond) = cliny(ipond) - clouty(ipond)
        slrety(ipond) = sliny(ipond) - slouty(ipond)
        sarety(ipond) = sainy(ipond) - saouty(ipond)
        sdrety(ipond) = sdiny(ipond) - sdouty(ipond)
        larety(ipond) = lainy(ipond) - laouty(ipond)
c
c       calculate yearly trapping efficiency
c
        if (totiny(ipond).ne.0.0) tey(ipond) = ((totiny(ipond)-
     1      couty(ipond))/totiny(ipond)) * 100.0
c
c       convert yearly inflow and outflow average
c       concentrations from kg/m^3 to mg/l
c
        if (voy(ipond).ne.0.0) then
          cay(ipond) = couty(ipond) / voy(ipond) * 1000.0
        else
          cay(ipond) = 0.0
        end if
c
        if (viy(ipond).ne.0.0) then
          cainy(ipond) = totiny(ipond) / viy(ipond) * 1000.0
        else
          cainy(ipond) = 0.0
        end if
c
        hmin(ipond) = hmin(ipond) / 3.281
c
        if (hmaxy(ipond).gt.(hot(ipond)/3.281)) then
c
          if (hmin(ipond).gt.hfull(ipond)) then
c
c           case 1: impoundment overtopped and filled with sediment
c
            otchar = '>Hot'
            filchr = '>Hfull'
          else
c
c           case 2: impoundment overtopped but not filled with sediment
c
            otchar = '>Hot'
          end if
c
        else
c
          if (hmin(ipond).gt.hfull(ipond)) then
c
c           case 3: impoundment not overtopped but filled with sediment
c
            filchr = '>Hfull'
          else
c
c         case 4: impoundment not overtopped and not filled with sediment
c
c         otchar, filchr initialized above
c
          end if
        end if
c
        if ((npond.eq.1).and.(ipdout.eq.1)) then
c
          write (57,1000) iyr, qomxy(ipond), voy(ipond), qiiny(ipond),
     1        viy(ipond), hmaxy(ipond), otchar, cpeaky(ipond),
     1        cay(ipond), cainy(ipond), tey(ipond), hmin(ipond), filchr
c
          write (56,1300) iyr, totiny(ipond), couty(ipond),
     1        rety(ipond), cliny(ipond), clouty(ipond), clrety(ipond),
     1        sliny(ipond), slouty(ipond), slrety(ipond), sainy(ipond),
     1        saouty(ipond), sarety(ipond), sdiny(ipond),
     1        sdouty(ipond), sdrety(ipond), lainy(ipond),
     1        laouty(ipond), larety(ipond)
c
        end if
c
        if (ipdout.eq.1) then
c
          if (ipond.eq.1) write (51,1400) iyr
c
          write (51,1500) ipond, hmaxy(ipond), hmin(ipond), viy(ipond),
     1        voy(ipond), qiiny(ipond), qomxy(ipond), tey(ipond),
     1        cainy(ipond), cay(ipond), cpkiy(ipond), cpeaky(ipond),
     1        totiny(ipond), couty(ipond), rety(ipond), cliny(ipond),
     1        clouty(ipond), clrety(ipond), sliny(ipond),
     1        slouty(ipond), slrety(ipond), sainy(ipond),
     1        saouty(ipond), sarety(ipond), sdiny(ipond),
     1        sdouty(ipond), sdrety(ipond), lainy(ipond),
     1        laouty(ipond), larety(ipond)
        end if
c
        if (otflg(ipond).eq.1) then
          if (ipdout.eq.1) write (51,1600) otyr(ipond)
          otyr(ipond) = 0
        else
          if (ipdout.eq.1) write (51,1700)
        end if
c
        if (filflg(ipond).eq.1) then
          if (ipdout.eq.1) then
            if (filyr(ipond).eq.iyr) write (51,1800) filday(ipond)
            if (filyr(ipond).ne.iyr) write (51,1900)
          end if
        end if
c
        if ((filflg(ipond).ne.1).and.(ipdout.eq.1)) write (51,2000)
c
        hmin(ipond) = hmin(ipond) * 3.281
c
c       reset yearly variables
c
        viy(ipond) = 0.0
        voy(ipond) = 0.0
c
        qiiny(ipond) = 0.0
        qomxy(ipond) = 0.0
c
        hmaxy(ipond) = 0.0
        cpeaky(ipond) = 0.0
c
        totiny(ipond) = 0.0
c
        cliny(ipond) = 0.0
        sliny(ipond) = 0.0
        sainy(ipond) = 0.0
        sdiny(ipond) = 0.0
        lainy(ipond) = 0.0
c
        coutys(ipond) = couty(ipond)
        clotys(ipond) = clouty(ipond)
        slotys(ipond) = slouty(ipond)
        saotys(ipond) = saouty(ipond)
        sdotys(ipond) = sdouty(ipond)
        laotys(ipond) = laouty(ipond)
        couty(ipond) = 0.0
        clouty(ipond) = 0.0
        slouty(ipond) = 0.0
        saouty(ipond) = 0.0
        sdouty(ipond) = 0.0
        laouty(ipond) = 0.0
c
        if ((npond.eq.1).and.(ipdout.eq.1)) then
          write (57,1100)
          write (56,1200)
        end if
c
c     end multiple impoundment loop
c
   10 continue
c
      return
c
 1000 format ('YEAR = ',i2,1x,f10.6,f10.2,f10.6,f10.2,f6.2,2x,a4,3f8.0,
     1    f6.1,f6.2,2x,a6)
 1100 format (110('-'))
 1200 format (165('-'))
 1300 format ('YEAR = ',i2,4x,3(e12.6,1x),4x,'N/A',9x,15(e12.6,1x))
 1400 format (/5x,'IMPOUNDMENT HYDRAULIC AND SEDIMENT SUMMARY FOR ',
     1    'YEAR ',i4/)
 1500 format (/20x,'IMPOUNDMENT NUMBER ',i2,//,26x,'STAGE',/,26x,5('-'),
     1    //,5x,'Maximum impoundment stage during year: ',3x,f10.4,
     1    ' (m)',/,5x,'Deposition impoundment stage during year: ',f10
     1    .4,' (m)',//,24x,'HYDRAULICS',/,24x,10('-'),//,28x,
     1    'Impoundment Inflow',3x,'Impoundment Outflow',/,28x,18('-'),3
     1    x,19('-'),/,5x,'Total Volume (m^3):',6x,f12.4,9x,f12.4,/,5x,
     1    'Peak Rate (m^3/s) :',6x,f12.7,9x,f12.7,//,22x,
     1    'SEDIMENTATION',/,22x,13('-'),//,5x,
     1    'Impoundment Trapping Efficiency: ',f10.4,' (%)',//,36x,
     1    'Impoundment Inflow',3x,'Impoundment Outflow',/,36x,18('-'),3
     1    x,19('-'),/,5x,'Average Concentration (mg/l):',4x,f12.2,9x,f12
     1    .2,/,5x,'Peak Concentration (mg/l)   :',4x,f12.2,9x,f12.2,///,
     1    5x,'Breakdown by Particle Size Class:',//,25x,'Inflow',9x,
     1    'Outflow',7x,'Retained',/,25x,6('-'),9x,7('-'),7x,8('-'),//,5
     1    x,'Total',6x,'(kg):',1x,e12.4,3x,e12.4,2x,e12.4,/,5x,'Clay',7
     1    x,'(kg):',1x,e12.4,3x,e12.4,2x,e12.4,/,5x,'Silt',7x,'(kg):',1
     1    x,e12.4,3x,e12.4,2x,e12.4,/,5x,'Small Agg. (kg):',1x,e12.4,3x,
     1    e12.4,2x,e12.4,/,5x,'Sand',7x,'(kg):',1x,e12.4,3x,e12.4,2x,e12
     1    .4,/,5x,'Large Agg. (kg):',1x,e12.4,3x,e12.4,2x,e12.4//)
 1600 format (5x,'Impoundment overtopped during year?   : Yes',/,5x,
     1    'Number of days overtopped during year : ',i3/)
 1700 format (5x,'Impoundment overtopped during the year?: No'/)
 1800 format (5x,'Impoundment filled with sediment during ',
     1    ' year?   : Yes',/,5x,
     1    'Day impoundment filled with sediment during year :',1x,i3//)
 1900 format (5x,'Impoundment filled with sediment during year?: Yes'//)
 2000 format (5x,'Impoundment filled with sediment during year?: No'//)
      end
