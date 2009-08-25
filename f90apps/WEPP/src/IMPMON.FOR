      subroutine impmon(nmon)
c
c     + + + PURPOSE + + +
c
c     SR IMPMON outputs and resets monthly impoundment variables.
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
      integer nmon
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     nmon -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cflags.inc'
c     read: ipdout
c
      include 'cimday.inc'
      include 'cimitf.inc'
      include 'cimmon.inc'
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
        retm(ipond) = totinm(ipond) - coutm(ipond)
        clretm(ipond) = clinm(ipond) - cloutm(ipond)
        slretm(ipond) = slinm(ipond) - sloutm(ipond)
        saretm(ipond) = sainm(ipond) - saoutm(ipond)
        sdretm(ipond) = sdinm(ipond) - sdoutm(ipond)
        laretm(ipond) = lainm(ipond) - laoutm(ipond)
c
c       calculate monthly trapping efficiency
c
        if (totinm(ipond).ne.0.0) tem(ipond) = ((totinm(ipond)-
     1      coutm(ipond))/totinm(ipond)) * 100.0
c
c       convert monthly inflow and outflow average
c       concentrations from kg/m^3 to mg/l
c
        if (vom(ipond).ne.0.0) then
          cam(ipond) = coutm(ipond) / vom(ipond) * 1000.0
        else
          cam(ipond) = 0.0
        end if
c
        if (vim(ipond).ne.0.0) then
          cainm(ipond) = totinm(ipond) / vim(ipond) * 1000.0
        else
          cainm(ipond) = 0.0
        end if
c
        hmin(ipond) = hmin(ipond) / 3.281
c
        if (hmaxm(ipond).gt.(hot(ipond)/3.281)) then
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
          write (57,1000) nmon, qomxm(ipond), vom(ipond), qiinm(ipond),
     1        vim(ipond), hmaxm(ipond), otchar, cpeakm(ipond),
     1        cam(ipond), cainm(ipond), tem(ipond), hmin(ipond), filchr
c
          write (56,1300) nmon, totinm(ipond), coutm(ipond),
     1        retm(ipond), clinm(ipond), cloutm(ipond), clretm(ipond),
     1        slinm(ipond), sloutm(ipond), slretm(ipond), sainm(ipond),
     1        saoutm(ipond), saretm(ipond), sdinm(ipond),
     1        sdoutm(ipond), sdretm(ipond), lainm(ipond),
     1        laoutm(ipond), laretm(ipond)
c
        end if
c
        hmin(ipond) = hmin(ipond) * 3.281
c
c       reset monthly variables
c
        vim(ipond) = 0.0
        vom(ipond) = 0.0
c
        qiinm(ipond) = 0.0
        qomxm(ipond) = 0.0
c
        hmaxm(ipond) = 0.0
        cpeakm(ipond) = 0.0
c
        totinm(ipond) = 0.0
c
        clinm(ipond) = 0.0
        slinm(ipond) = 0.0
        sainm(ipond) = 0.0
        sdinm(ipond) = 0.0
        lainm(ipond) = 0.0
c
        coutms(ipond) = coutm(ipond)
        clotms(ipond) = cloutm(ipond)
        slotms(ipond) = sloutm(ipond)
        saotms(ipond) = saoutm(ipond)
        sdotms(ipond) = sdoutm(ipond)
        laotms(ipond) = laoutm(ipond)
        coutm(ipond) = 0.0
        cloutm(ipond) = 0.0
        sloutm(ipond) = 0.0
        saoutm(ipond) = 0.0
        sdoutm(ipond) = 0.0
        laoutm(ipond) = 0.0
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
 1000 format ('MON = ',i2,2x,f10.6,f10.2,f10.6,f10.2,f6.2,2x,a4,3f8.0,f6
     1    .1,f6.2,2x,a6)
 1100 format (110('-'))
 1200 format (165('-'))
 1300 format ('MON = ',i2,5x,3(e12.6,1x),4x,'N/A',9x,15(e12.6,1x))
      end
