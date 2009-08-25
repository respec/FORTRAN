      subroutine impeos
c
c     + + + PURPOSE + + +
c
c     SR IMPEOS outputs end of simulation impoundment variables.
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
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cflags.inc'
c     read: ipdout
c
      include 'cimday.inc'
      include 'cimeos.inc'
      include 'cimitf.inc'
      include 'cimpnd.inc'
      include 'cimsed.inc'
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
        rete(ipond) = totine(ipond) - coute(ipond)
        clrete(ipond) = cline(ipond) - cloute(ipond)
        slrete(ipond) = sline(ipond) - sloute(ipond)
        sarete(ipond) = saine(ipond) - saoute(ipond)
        sdrete(ipond) = sdine(ipond) - sdoute(ipond)
        larete(ipond) = laine(ipond) - laoute(ipond)
c
c       calculate simulation trapping efficiency
c
        if (totine(ipond).ne.0.0) tee(ipond) = ((totine(ipond)-
     1      coute(ipond))/totine(ipond)) * 100.0
c
c       convert simulation inflow and outflow average
c       concentrations from kg/m^3 to mg/l
c
        if (voe(ipond).ne.0.0) then
          cae(ipond) = coute(ipond) / voe(ipond) * 1000.0
        else
          cae(ipond) = 0.0
        end if
c
        if (vie(ipond).ne.0.0) then
          caine(ipond) = totine(ipond) / vie(ipond) * 1000.0
        else
          caine(ipond) = 0.0
        end if
c
        hmin(ipond) = hmin(ipond) / 3.281
c
        if (hmaxe(ipond).gt.(hot(ipond)/3.281)) then
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
          write (57,1000) qomxe(ipond), voe(ipond), qiine(ipond),
     1        vie(ipond), hmaxe(ipond), otchar, cpeake(ipond),
     1        cae(ipond), caine(ipond), tee(ipond), hmin(ipond), filchr
c
          write (56,1300) totine(ipond), coute(ipond), rete(ipond),
     1        cline(ipond), cloute(ipond), clrete(ipond), sline(ipond),
     1        sloute(ipond), slrete(ipond), saine(ipond),
     1        saoute(ipond), sarete(ipond), sdine(ipond),
     1        sdoute(ipond), sdrete(ipond), laine(ipond),
     1        laoute(ipond), larete(ipond)
c
          write (56,1200)
          write (57,1100)
c
        end if
c
        if (ipdout.eq.1) then
c
          if (ipond.eq.1) write (51,1400)
c
          write (51,1500) ipond, hmaxe(ipond), hmin(ipond), vie(ipond),
     1        voe(ipond), qiine(ipond), qomxe(ipond), tee(ipond),
     1        caine(ipond), cae(ipond), cpkie(ipond), cpeake(ipond),
     1        totine(ipond), coute(ipond), rete(ipond), cline(ipond),
     1        cloute(ipond), clrete(ipond), sline(ipond),
     1        sloute(ipond), slrete(ipond), saine(ipond),
     1        saoute(ipond), sarete(ipond), sdine(ipond),
     1        sdoute(ipond), sdrete(ipond), laine(ipond),
     1        laoute(ipond), larete(ipond)
        end if
c
        if (ipdout.eq.1) then
          if (otflg(ipond).eq.1) write (51,1600) oteos(ipond)
          if (otflg(ipond).ne.1) write (51,1700)
        end if
c
        if (ipdout.eq.1) then
          if (filflg(ipond).eq.1) write (51,1800) filday(ipond),
     1        filyr(ipond)
          if (filflg(ipond).ne.1) write (51,1900)
        end if
c
   10 continue
c
      return
c
 1000 format ('SUMMARY',3x,f10.6,f10.2,f10.6,f10.2,f6.2,2x,a4,3f8.0,f6
     1    .1,f6.2,2x,a6)
 1100 format (110('-'))
 1200 format (165('-'))
 1300 format ('SUMMARY',6x,3(e12.6,1x),4x,'N/A',9x,15(e12.6,1x))
 1400 format (/5x,'IMPOUNDMENT HYDRAULIC AND SEDIMENT END OF ',
     1    'SIMULATION SUMMARY'/)
 1500 format (/20x,'IMPOUNDMENT NUMBER ',i2,//,26x,'STAGE',/,26x,5('-'),
     1    //,5x,'Maximum impoundment stage during simulation:',4x,f10.4,
     1    ' (m)',/,5x,
     1    'Deposition impoundment stage during simulation: ',f10.4,
     1    ' (m)',//,24x,'HYDRAULICS',/,24x,10('-'),//,28x,
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
 1600 format (5x,'Impoundment overtopped during simulation?   : Yes',/,5
     1    x,'Number of days overtopped during simulation :',1x,i4/)
 1700 format (5x,'Impoundment overtopped during simulation?: No'/)
 1800 format (5x,'Impoundment filled with sediment during simulation: ',
     1    ' Yes',//,5x,'Day impoundment filled with sediment  : ',i3,/,5
     1    x,'Year impoundment filled with sediment : ',i4//)
 1900 format (5x,'Impoundment filled with sediment during year?: No'//)
      end
