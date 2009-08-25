      subroutine impday(iyr,sdate)
c
c     + + + PURPOSE + + +
c
c     SR IMPDAY outputs and sums daily impoundment variables.
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
      integer iyr, sdate
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     iyr   -
c     sdate -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cflags.inc'
c     read: ipdout
c
      include 'cimday.inc'
      include 'cimeos.inc'
      include 'cimitf.inc'
      include 'cimmon.inc'
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
c
c     begin multiple impoundment loop
c
      do 10 ipond = 1, npond
c
        otchar = '    '
        filchr = '      '
c
c       calculate daily total sediment
c
        totin(ipond) = ciin(ipond) * vi(ipond) * 0.4536
        ret(ipond) = totin(ipond) - cout(ipond)
c
        if (ret(ipond).lt.0.0) ret(ipond) = 0.0
c
        clin(ipond) = pcl(ipond) * totin(ipond)
        slin(ipond) = psl(ipond) * totin(ipond)
        sain(ipond) = psa(ipond) * totin(ipond)
        sdin(ipond) = psd(ipond) * totin(ipond)
        lain(ipond) = pla(ipond) * totin(ipond)
c
        clret(ipond) = clin(ipond) - clout(ipond)
        slret(ipond) = slin(ipond) - slout(ipond)
        saret(ipond) = sain(ipond) - saout(ipond)
        sdret(ipond) = sdin(ipond) - sdout(ipond)
        laret(ipond) = lain(ipond) - laout(ipond)
c
c       calculate sediment balance remaining in impoundment
c
        totint(ipond) = totint(ipond) + totin(ipond)
        coutt(ipond) = coutt(ipond) + cout(ipond)
        rett(ipond) = totint(ipond) - coutt(ipond)
c
c       convert variables for printout
c
c       inflow volume from ft^3 to m^3
c
        vi(ipond) = vi(ipond) / 3.281 ** 3
c
c       inflow and outflow rates from ft^3/s to m^3/s
c
        qiin(ipond) = qiin(ipond) / 3.281 ** 3
        qomx(ipond) = qomx(ipond) / 3.281 ** 3
c
c       stages from ft to m
c
        hmax(ipond) = hmax(ipond) / 3.281
        hmin(ipond) = hmin(ipond) / 3.281
c
c       concentration from lbs/ft^3 to mg/l
c
        ciin(ipond) = ciin(ipond) / 0.00006241778465
c
        if (hmax(ipond).gt.(hot(ipond)/3.281)) then
c
          otflg(ipond) = 1
          otyr(ipond) = otyr(ipond) + 1
          oteos(ipond) = oteos(ipond) + 1
c
          if (hmin(ipond).gt.hfull(ipond)) then
c
            if (filflg(ipond).ne.1) then
              filflg(ipond) = 1
              filday(ipond) = sdate
              filyr(ipond) = iyr
            end if
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
            if (filflg(ipond).ne.1) then
              filflg(ipond) = 1
              filday(ipond) = sdate
              filyr(ipond) = iyr
            end if
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
          write (57,1000) iyr, sdate, qomx(ipond), volo(ipond),
     1        qiin(ipond), vi(ipond), hmax(ipond), otchar,
     1        cpeak(ipond), ca(ipond), ciin(ipond), hmin(ipond), filchr
c
          write (56,1100) iyr, sdate, totin(ipond), cout(ipond),
     1        ret(ipond), rett(ipond), clin(ipond), clout(ipond),
     1        clret(ipond), slin(ipond), slout(ipond), slret(ipond),
     1        sain(ipond), saout(ipond), saret(ipond), sdin(ipond),
     1        sdout(ipond), sdret(ipond), lain(ipond), laout(ipond),
     1        laret(ipond)
c
        end if
c
        hmin(ipond) = hmin(ipond) * 3.281
c
c       monthly variables
c
        vom(ipond) = vom(ipond) + volo(ipond)
        vim(ipond) = vim(ipond) + vi(ipond)
c
        if (qiin(ipond).gt.qiinm(ipond)) qiinm(ipond) = qiin(ipond)
        if (qomx(ipond).gt.qomxm(ipond)) qomxm(ipond) = qomx(ipond)
        if (hmax(ipond).gt.hmaxm(ipond)) hmaxm(ipond) = hmax(ipond)
c
        totinm(ipond) = totinm(ipond) + totin(ipond)
c
        clinm(ipond) = clinm(ipond) + clin(ipond)
        slinm(ipond) = slinm(ipond) + slin(ipond)
        sainm(ipond) = sainm(ipond) + sain(ipond)
        sdinm(ipond) = sdinm(ipond) + sdin(ipond)
        lainm(ipond) = lainm(ipond) + lain(ipond)
c
        coutm(ipond) = coutm(ipond) + cout(ipond)
        cloutm(ipond) = cloutm(ipond) + clout(ipond)
        sloutm(ipond) = sloutm(ipond) + slout(ipond)
        saoutm(ipond) = saoutm(ipond) + saout(ipond)
        sdoutm(ipond) = sdoutm(ipond) + sdout(ipond)
        laoutm(ipond) = laoutm(ipond) + laout(ipond)
c
c       yearly variables
c
        voy(ipond) = voy(ipond) + volo(ipond)
        viy(ipond) = viy(ipond) + vi(ipond)
c
        if (qiin(ipond).gt.qiiny(ipond)) qiiny(ipond) = qiin(ipond)
        if (qomx(ipond).gt.qomxy(ipond)) qomxy(ipond) = qomx(ipond)
        if (hmax(ipond).gt.hmaxy(ipond)) hmaxy(ipond) = hmax(ipond)
c
        totiny(ipond) = totiny(ipond) + totin(ipond)
c
        cliny(ipond) = cliny(ipond) + clin(ipond)
        sliny(ipond) = sliny(ipond) + slin(ipond)
        sainy(ipond) = sainy(ipond) + sain(ipond)
        sdiny(ipond) = sdiny(ipond) + sdin(ipond)
        lainy(ipond) = lainy(ipond) + lain(ipond)
c
        couty(ipond) = couty(ipond) + cout(ipond)
        clouty(ipond) = clouty(ipond) + clout(ipond)
        slouty(ipond) = slouty(ipond) + slout(ipond)
        saouty(ipond) = saouty(ipond) + saout(ipond)
        sdouty(ipond) = sdouty(ipond) + sdout(ipond)
        laouty(ipond) = laouty(ipond) + laout(ipond)
c
c       end of simulation variables
c
        voe(ipond) = voe(ipond) + volo(ipond)
        vie(ipond) = vie(ipond) + vi(ipond)
c
        if (qiin(ipond).gt.qiine(ipond)) qiine(ipond) = qiin(ipond)
        if (qomx(ipond).gt.qomxe(ipond)) qomxe(ipond) = qomx(ipond)
        if (hmax(ipond).gt.hmaxe(ipond)) hmaxe(ipond) = hmax(ipond)
c
        totine(ipond) = totine(ipond) + totin(ipond)
c
        cline(ipond) = cline(ipond) + clin(ipond)
        sline(ipond) = sline(ipond) + slin(ipond)
        saine(ipond) = saine(ipond) + sain(ipond)
        sdine(ipond) = sdine(ipond) + sdin(ipond)
        laine(ipond) = laine(ipond) + lain(ipond)
c
        coute(ipond) = coute(ipond) + cout(ipond)
        cloute(ipond) = cloute(ipond) + clout(ipond)
        sloute(ipond) = sloute(ipond) + slout(ipond)
        saoute(ipond) = saoute(ipond) + saout(ipond)
        sdoute(ipond) = sdoute(ipond) + sdout(ipond)
        laoute(ipond) = laoute(ipond) + laout(ipond)
c
   10 continue
c
      return
c
 1000 format (i4,2x,i3,1x,f10.6,f10.2,f10.6,f10.2,f6.2,2x,a4,3f8.0,2x,
     1    'N/A',1x,f6.2,2x,a6)
 1100 format (i4,2x,i3,1x,19(f12.2,1x))
      end
