      subroutine annchn(npart,lyear,nrainy,trainy,nptsc,toplen,ichplt)
c
c     + + + PURPOSE + + +
c
c     SR ANNCHN writes the annual summary for the net erosion
c     occurring within the channel and impoundment elements.
c
c     Called from: SR WSHDRV
c     Author(s): Ascough II, S. Livingston, R. van der Zweep
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by: Jim Ascough II
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxcsg.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
      include 'pmximp.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real trainy, toplen
      integer npart, lyear, nrainy, nptsc, ichplt
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     npart  -
c     lyear  -
c     nrainy -
c     trainy -
c     nptsc  -
c     toplen -
c     ichplt -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchcon.inc'
      include 'cchpar.inc'
      include 'cchpek.inc'
      include 'cchprt.inc'
      include 'cchsed.inc'
      include 'cenrpa1.inc'
      include 'cimeos.inc'
      include 'coutchn.inc'
      include 'cpart2.inc'
      include 'cimyrs.inc'
      include 'cseddet.inc'
      include 'cslpopt.inc'
      include 'cstore.inc'
      include 'cstruc.inc'
      include 'cstruct.inc'
      include 'csumirr.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real y(mxcseg), concpc, dx, frac, tconc, tcncpc, tppm,
     1    tgs(mxelem), trunsi, ppm, xchn, conv, sdr, wdet
      integer i, k, iseg, l, ipond
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     y(mxcseg) -
c     concpc    -
c     dx        -
c     frac      -
c     tconc     -
c     tcncpc    -
c     tppm      -
c     tgs       -
c     trunsi    -
c     ppm       -
c     xchn      -
c     conv      -
c     sdr       -
c     wdet      -
c
c     Integer Variables
c
c     i      -
c     k      -
c     iseg   -
c     l      -
c     ipond  -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     enrcmp
c
c     + + + DATA INITIALIZATIONS + + +
c
      wdet = 0
      sdr = 0.0
      ipond = 0
c
      ichan = 0
c
c     convert lbs to total kg soil loss or deposition on channel
c
      conv = 0.4536
c
c     + + + END SPECIFICATIONS + + +
c
c
c     override ichplt so that plotting sections of code not
c     entered
c
      ichplt = 0
c
      if (watsum.ge.1) then
c
        write (38,1000) lyear
c
        write (38,1100)
c
        do 10 l = 1, nhill
c         Modified - WSU
          write (38,1300) l, hroy(l), sbrfty(l),dety(l),
     1        depy(l) * (-1),hsedy(l)
          sbrfty(l) = 0.0
          wdet = wdet + dety(l)
          dety(l) = 0.0
          depy(l) = 0.0
          hroy(l) = 0.0
          hsedy(l) = 0.0
   10   continue
c
        write (38,1200)
      end if
c
c     get total sediment delivery for each channel
c
      do 30 ielmt = nhill + 1, nelmt
c
        tgs(ielmt) = 0.0
c
        if (elmt(ielmt).eq.2) then
          ichan = ichan + 1
        else
          ipond = ipond + 1
        end if
c
        if (elmt(ielmt).eq.3) then
          tgs(ielmt) = coutys(ipond) / conv
        else
c
          do 20 k = 1, npart
            tgs(ielmt) = tgs(ielmt) + tgsy(k,ielmt)
   20     continue
c
        end if
c
c       write annual precipitation summary to output file (unit 38)
c       for watershed yearly summary
c
        if (watsum.gt.0) then
c
c         convert lbs to total kg soil loss or deposition on channel
c
          wdet = wdet + (cdety(ielmt)*conv)
c
          if (elmt(ielmt).eq.2) write (38,1400) ichan, truny(ielmt),
     1        tgs(ielmt) * conv / 1000.0,tronvy(ielmt),sbrvty(ielmt)
c
          if (elmt(ielmt).eq.3) write (38,1500) ipond, truny(ielmt),
     1        tgs(ielmt) * conv / 1000.0
c
          cdety(ielmt) = 0.0
          cdepy(ielmt) = 0.0
        end if
   30 continue
c
      if (watsum.gt.0) then
c
        do 40 ielmt = nhill + 1, nelmt
c
c         convert total runoff volume at the channel outlet from
c         m^3 to mm using the local variable trunsi (si units)
c
          trunsi = (truny(ielmt)/wsarea(ielmt)) * 1000.0
c
          if (ielmt.eq.nelmt) then
c
c           write to summary output
c
            write (38,1600) nrainy, trainy
            write (38,1700) nruny, trunsi
c
            if (wdet.gt.0) sdr = tgs(nelmt) * conv / wdet
            if (wdet.le.0) sdr = 0.0
c
            write (38,2300) wsarea(nelmt) / 10000, wsarea(nelmt) * (
     1        trainy/1000), tirry, truny(nelmt), tgs(nelmt)*conv/1000.0,
     1        ((tgs(nelmt)*conv)/1000) / (wsarea(nelmt)/10000), sdr
          end if
c
   40   continue
      end if
c
      if (nruny.gt.0) then
c
        ichan = 0
        ipond = 0
c
        do 60 ielmt = nhill + 1, nelmt
c
          if (elmt(ielmt).eq.2) then
            ichan = ichan + 1
          else
            ipond = ipond + 1
          end if
c
c         write output if soil loss for the year is > 1 kg
c
          if (tgs(ielmt).gt.2.205) then
c
            if (elmt(ielmt).eq.2) then
c             if (watsum.ge.3) write (38,1700)
              if (watsum.gt.0.and.ielmt.eq.nelmt) write (38,2400)
            else
c             if (watsum.ge.3) write (38,1750)
              if (watsum.gt.0.and.ielmt.eq.nelmt) write (38,2500)
            end if
c
            tconc = 0.0
            tcncpc = 0.0
            tppm = 0.0
c
            do 50 k = 1, npart
c
              if (elmt(ielmt).eq.2) then
                conc(k,ielmt) = (tgsy(k,ielmt)*0.4536) / truny(ielmt)
              else
                conc(1,ielmt) = clotys(ipond) / truny(ielmt)
                conc(2,ielmt) = slotys(ipond) / truny(ielmt)
                conc(3,ielmt) = saotys(ipond) / truny(ielmt)
                conc(4,ielmt) = laotys(ipond) / truny(ielmt)
                conc(5,ielmt) = sdotys(ipond) / truny(ielmt)
              end if
c
              concpc = conc(k,ielmt) / (wtdh2o*16.0211)
              ppm = concpc * 1.0e+06
c
              if (elmt(ielmt).eq.2) then
                frac = tgsy(k,ielmt) / tgs(ielmt)
              else
                frac = conc(k,ielmt) * truny(ielmt) / coutys(ipond)
              end if
c
              if (elmt(ielmt).eq.2) then
c
                if (watsum.ge.1.and.ielmt.eq.nelmt) write (38,1800) k,
     1              crdia(k,ielmt) * 1000 / 3.281, crspg(k),
     1              frsnd(k,ielmt) * 100, frslt(k,ielmt) * 100, 
     1              frcly(k,ielmt) * 100, frorg(k,ielmt) * 100, frac
c
              else
c
                if (watsum.ge.1.and.ielmt.eq.nelmt) write (38,1900) k,
     1              frac
c
              end if
c
              tconc = tconc + conc(k,ielmt)
              tcncpc = tcncpc + concpc
              tppm = tppm + ppm
c
   50       continue
c
            if (ielmt.eq.nelmt.and.watsum.ge.1) call
     1          enrcmp(2,ielmt,nelmt,tgs,elmt(ielmt))
c
          else
c
            if (watsum.ge.1.and.ielmt.eq.nelmt) write (38,2000)
c
          end if
c
   60   continue
c
      else
c
      end if
c
      do 70 ielmt = 1, nhill
        dety(ielmt) = 0.0
        depy(ielmt) = 0.0
   70 continue
c
      do 90 ielmt = nhill + 1, nelmt
c
        truny(ielmt) = 0.0
cd      Added by S. Dun, Dec 20, 2007 
c       for more outputs in annual summary of the cahnnel
        sbrvty(ielmt) = 0.0
        tronvy(ielmt) = 0.0
c             End adding
        if (elmt(ielmt).eq.3) coutys(ielmt) = 0.0
c
        do 80 k = 1, npart
          tgsy(k,ielmt) = 0.0
   80   continue
c
   90 continue
c
c     reset annual summary values
c
      trainy = 0.0
      nrainy = 0
      nruny = 0
      tirry = 0.0
c
      if (ichplt.eq.1) then
c
c       write out channel profile plotting data
c
        write (37,2100) lyear
c
        iseg = nptsc
        y(iseg) = 0.0
c
        do 100 i = 1, nptsc - 1
          dx = (chnx(ichan,iseg)-chnx(ichan,iseg-1)) * 3.281
          y(iseg-1) = y(iseg) + dx * chnslp(ichan,iseg)
          iseg = iseg - 1
  100   continue
c
        do 110 iseg = 1, nptsc
          xchn = (chnx(ichan,iseg)-toplen) * 3.281
          write (37,2200) xchn, y(iseg), nsedy(iseg), -csedy(iseg)
          nsedy(iseg) = 0.0
          csedy(iseg) = 0.0
  110   continue
c
      end if
c
      return
c
 1000 format (////18x,'ANNUAL SUMMARY FOR WATERSHED IN YEAR ',i5,/,18x,
     1    42('-')/)
c    Modified WSU
 1100 format (/17x,'Runoff',8x,'Subrunoff',5x,'Soil',10x,'Sediment',6x,
     1    'Sediment',/,17x,
     1    'Volume',8x,'Volume',8x,'Loss',10x,'Deposition',4x,'Yield',/,
     1    'Hillslopes',7x,'(m^3)',9x,'(m^3)',9x,'(kg)',10x,'(kg)',10x,
     1    '(kg)',/,10('-'),7x,5(10('-'),4x))
c    End Modifying.
cd 1200 format (///'Channels',9x,'Discharge',5x,'Sediment',6x,/,'and',14x,
cd     1    'Volume',8x,'Yield',/,'Impoundments',5x,'(m^3)',9x,'(tonne)'/,
cd     1    12('-'),5x,2(10('-'),4x),/)
 1200 format (///'Channels',9x,'Discharge',5x,'Sediment',6x,'Upland',
     1        6x,'Subsuface Flow'/,
     1        'and',14x,'Volume',8x,'Yield',9x,'Charge',8x,'Volume' /,
     1        'Impoundments',5x,'(m^3)',9x,'(tonne)',7x,'(m^3)',10x,
     1         '(m^3)'/,12('-'),5x,2(10('-'),4x),2(10('-'),3x),/)
 1300 format ('Hill    ',i3,5(2x,f12.1))
 1400 format ('Channel    ',1x,i3,1x,4(f9.1,5x))
 1500 format ('Impoundment',1x,i2,1x,2(f9.1,5x))
 1600 format (/i4,' storms produced ',f8.2,' mm. of rainfall')
 1700 format (/i4,' events produced ',f8.2,' mm. of runoff  ',/5x,
     1    'passing through the watershed outlet',/)
 1800 format (1x,i2,4x,f6.3,6x,f4.2,4x,f5.1,4x,f5.1,4x,f5.1,4x,f5.1,5x,
     1    f5.3,4x,f5.3)
 1900 format (1x,i2,10x,f5.3)
 2000 format (/19x,'*** total soil loss < 1 kg ***')
 2100 format (//10x,'Total Soil Loss For Year 19',i2,//,10x,
     1    '     x       y        sed yld      chan',/,10x,
     1    '   (ft)    (ft)       (t/ac)     (lbs/ft ch)',/,10x,
     1    '   ----    ----       -------     -------')
 2200 format (12x,f5.1,3x,f5.1,6x,f9.2,3x,f9.2)
 2300 format (/'Delivery From Channel Outlet:',/,
     1    '-------- ---- ------- ------',//,
     1    'Total contributing area to outlet               = ',f10.2,
     1    ' ha',/,'Total precipitation volume in contributing area = ',
     1    f10.0,' m^3',/,
     1    'Total irrigation volume in contributing area    = ',f10.0,
     1    ' m^3',/,'Total water discharge from outlet               = ',
     1    f10.0,' m^3',/,
     1    'Total sediment discharge from outlet         = ',f13.1,
     1    ' tonnes',/,'Sed. delivery per unit area of watershed        =
     1    ',f10.1,' T/ha',/,
     1    'Sediment Delivery Ratio for Watershed           = ',f10.3,//)
 2400 format ('Sediment Particle Information Leaving Channel',//,
     1    '-----------------------------------------------',
     1    '------------------------',/,
     1    '                             Particle Composition',
     1    '              Fraction',/,'Class  Diameter  Specific  ',
     1    '------------------------            In Flow',/,9x,
     1    '(mm)    Gravity   % Sand   % Silt   % Clay   % O.M.',
     1    '   Exiting',/,
     1    '-------------------------------------------------------',
     1    '------------------------')
 2500 format ('Sediment Particle Information Leaving Impoundment:',//,
     1    '-------------------------------------------------------',/,
     1    '             Fraction',/,
     1    'Class        In Flow',/,
     1    '             Exiting',/,
     1    '-------------------------------------------------------')
      end