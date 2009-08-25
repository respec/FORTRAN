      subroutine monchn(npart,lmonth,lyear,nrainm,trainm,nptsc,toplen,
     1    ichplt)
c
c     + + + PURPOSE + + +
c
c     SR MONCHN prints hydrologic and erosion monthly output
c     summaries for channels.
c
c     Called from: SR WSHDRV
c     Author(s): Ascough II, Livingston, R. van der Zweep
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
      real trainm, toplen
      integer npart, lmonth, lyear, nrainm, nptsc, ichplt
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     trainm -
c     toplen -
c     npart  -
c     lmonth -
c     lyear  -
c     nrainm -
c     nptsc  -
c     ichplt -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchcon.inc'
      include 'cchpar.inc'
      include 'cchpek.inc'
      include 'cchprt.inc'
      include 'cchsed.inc'
      include 'cdist2.inc'
      include 'cenrpa1.inc'
      include 'cimmon.inc'
      include 'cimeos.inc'
      include 'coutchn.inc'
      include 'cpart2.inc'
      include 'cslpopt.inc'
      include 'cstore.inc'
      include 'cstruc.inc'
      include 'cstruct.inc'
      include 'csumirr.inc'
      include 'cseddet.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real concpc, dx, frac, ppm, tcncpc, tconc, tgs(mxelem), tppm,
     1    trunsi, xchn, y(mxcseg), conv, sdr, wdet
      integer i, k, iseg, ipond, l
      character*10 months(12)
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     concpc -
c     dx     -
c     frac   -
c     ppm    -
c     tcncpc -
c     tconc  -
c     tgs    -
c     tppm   -
c     trunsi -
c     xchn   -
c     y      -
c     conv   -
c     sdr    -
c     wdet   -
c
c     Integer Variables
c
c     i      -
c     k      -
c     iseg   -
c     ipond  -
c     l      -
c
c     Character Variables
c
c     months(12) -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     enrcmp
c
c     + + + DATA INITIALIZATIONS + + +
c
      data months /'January   ', 'February  ', 'March     ',
     1    'April     ', 'May       ', 'June      ', 'July      ',
     1    'August    ', 'September ', 'October   ', 'November  ',
     1    'December  '/
c
c
      conv = 0.4536
      wdet = 0.0
      sdr = 0.00
      ichan = 0
      ipond = 0
c
c     + + + END SPECIFICATIONS + + +
c
c     override ichplt so that plotting sections of code not
c     entered
c
      ichplt = 0
c
      if (months(lmonth).eq.'January   '.and.watsum.gt.1) write (38,1000
     1    ) lyear
c
      if (watsum.gt.1) then
c
        write (38,1300) months(lmonth), lyear
        write (38,1100)
c
        do 10 l = 1, nhill
c
          write (38,1400) l, hrom(l),sbrftm(l), detm(l), depm(l) * (-1),
     1        hsedm(l)
c
          wdet = wdet + detm(l)
          detm(l) = 0.0
          depm(l) = 0.0
          hrom(l) = 0.0
          hsedm(l) = 0.0
          sbrftm(l) = 0.0
   10   continue
c
        write (38,1200)
      end if
c
c     get total sediment delivery each channel
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
          tgs(ielmt) = coutms(ipond) / conv
        else
c
          do 20 k = 1, npart
            tgs(ielmt) = tgs(ielmt) + tgsm(k,ielmt)
   20     continue
c
        end if
c
c       write annual precipitation summary to output file (unit 38)
c       for watershed yearly summary
c
        if (watsum.gt.1) then
c
c         convert lbs to total kg soil loss or deposition on channel
c
          wdet = wdet + (cdetm(ielmt)*conv)
c
          if (elmt(ielmt).eq.2) write (38,1500) ichan, trunm(ielmt),
     1        tgs(ielmt) * conv/1000.00
          if (elmt(ielmt).eq.3) write (38,1600) ipond, trunm(ielmt),
     1        tgs(ielmt) * conv/1000.00
c
          cdetm(ielmt) = 0.0
          cdepm(ielmt) = 0.0
        end if
c
   30 continue
c
      if (watsum.gt.0) then
c
        do 40 ielmt = nhill + 1, nelmt
c
c         convert total runoff volume at the channel outlet from
c         m^3 to mm using the local variable trunsi (si units)
c
          trunsi = (trunm(ielmt)/wsarea(ielmt)) * 1000.0
c
          if (ielmt.eq.nelmt.and.watsum.gt.1) then
c
c           write to summary output
c
            write (38,1700) nrainm, trainm
c
            write (38,1800) nruny, trunsi
c
            if (wdet.gt.0) sdr = tgs(nelmt) * conv / wdet
            if (wdet.le.0) sdr = 0.0
c
            write (38,2100) wsarea(nelmt) / 10000, wsarea(nelmt) * (
     1          trainm/1000), tirrm, trunm(nelmt), tgs(nelmt) * conv /
     1          1000.0,((tgs(nelmt)*conv)/1000) / (wsarea(nelmt)/10000)
     1          , sdr
          end if
c
   40   continue
      end if
c
      if (nrunm.gt.0) then
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
c         write output if soil loss for the year is > than 1 kg
c
          if (tgs(ielmt).gt.2.205) then
c
            if (elmt(ielmt).eq.2) then
              if (watsum.gt.1.and.ielmt.eq.nelmt) write (38,2500)
            else
              if (watsum.gt.1.and.ielmt.eq.nelmt) write (38,2600)
            end if
c
            tconc = 0.0
            tcncpc = 0.0
            tppm = 0.0
c
            do 50 k = 1, npart
c
              if (elmt(ielmt).eq.2) then
                conc(k,ielmt) = (tgsm(k,ielmt)*0.4536) / trunm(ielmt)
              else
                conc(1,ielmt) = clotms(ipond) / trunm(ielmt)
                conc(2,ielmt) = slotms(ipond) / trunm(ielmt)
                conc(3,ielmt) = saotms(ipond) / trunm(ielmt)
                conc(4,ielmt) = laotms(ipond) / trunm(ielmt)
                conc(5,ielmt) = sdotms(ipond) / trunm(ielmt)
              end if
c
              concpc = conc(k,ielmt) / (wtdh2o*16.0211)
              ppm = concpc * 1.0e+06
c
              if (elmt(ielmt).eq.2) then
                frac = tgsm(k,ielmt) / tgs(ielmt)
              else
                frac = conc(k,ielmt) * trunm(ielmt) / coutms(ipond)
              end if
c
              if (elmt(ielmt).eq.2) then
c
                if (watsum.gt.1.and.ielmt.eq.nelmt) write (38,1900) k,
     1              crdia(k,ielmt) * 1000 / 3.281, crspg(k),
     1              frsnd(k,ielmt) * 100, frslt(k,ielmt) * 100, 
     1              frcly(k,ielmt) * 100, frorg(k,ielmt) * 100, frac
  
c
              else
                if (watsum.gt.1.and.ielmt.eq.nelmt) write (38,2000) k,
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
            if (ielmt.eq.nelmt.and.watsum.gt.1) call
     1          enrcmp(2,ielmt,nelmt,tgs,elmt(ielmt))
c
          else
c
            if (watsum.gt.1.and.ielmt.eq.nelmt) write (38,2200)
c
          end if
c
   60   continue
c
      else
c
      end if
c
      do 80 ielmt = nhill + 1, nelmt
c
        trunm(ielmt) = 0.0
        if (elmt(ielmt).eq.3) coutms(ielmt) = 0.0
c
        do 70 k = 1, npart
          tgsm(k,ielmt) = 0.0
   70   continue
c
   80 continue
c
c     reset monthly summary values
c
      trainm = 0.0
      nrainm = 0
      nrunm = 0
      tirrm = 0.0
c
      if (ichplt.eq.1) then
c
c       write out channel profile plotting data
c
        write (37,2300) months(lmonth)
c
        iseg = nptsc
        y(iseg) = 0.0
c
        do 90 i = 1, nptsc - 1
          dx = (chnx(ichan,iseg)-chnx(ichan,iseg-1)) * 3.281
          y(iseg-1) = y(iseg) + dx * chnslp(ichan,iseg)
          iseg = iseg - 1
   90   continue
c
        do 100 iseg = 1, nptsc
          xchn = (chnx(ichan,iseg)-toplen) * 3.281
          write (37,2400) xchn, y(iseg), nsedm(iseg), -csedm(iseg)
          nsedm(iseg) = 0.0
          csedm(iseg) = 0.0
  100   continue
c
      end if
c
      return
 1000 format (////13x,'MONTHLY SUMMARY FOR WATERSHED IN YEAR ',i4,/,13x,
     1    42('-')/)
 1100 format (/17x,'Runoff',8x,'Subrunoff',5x,'Soil',10x,'Sediment',6x,
     1    'Sediment',/,17x,
     1    'Volume',8x,'Volume',8x,'Loss',10x,'Deposition',4x,'Yield',9x,
     1    /,'Hillslopes',7x,'(m^3)',9x,'(m^3)',9x,'(kg)',10x,'(kg)',10x,
     1    '(kg)',/,10('-'),7x,5(10('-'),4x))
 1200 format (///'Channels',9x,'Discharge',5x,'Sediment',6x,/,'and',14x,
     1    'Volume',8x,'Yield',/,'Impoundments',5x,'(m^3)',9x,'(tonne)'/,
     1    12('-'),5x,2(10('-'),4x),/)
 1300 format (//a10,' Summary for Watershed in Year ',i4,/,49('-'))
 1400 format ('Hill    ',i3,5(2x,f12.1))
 1500 format ('Channel    ',1x,i3,1x,2(f9.1,5x))
 1600 format ('Impoundment',1x,i2,1x,2(f9.1,5x))
 1700 format (/i4,' storms produced ',f8.2,' mm. of ','rainfall'/)
 1800 format (/i4,' events produced ',f8.2,' mm. of runoff  ',/5x,
     1    'passing through the watershed outlet',/)
 1900 format (1x,i2,4x,f6.3,6x,f4.2,4x,f5.1,4x,f5.1,4x,f5.1,4x,f5.1,5x,
     1    f5.3,4x,f5.3)
 2000 format (1x,i2,10x,f5.3)
 2100 format (/'Delivery From Channel Outlet:',/,
     1    '-------- ---- ------- ------',//,
     1    'Total contributing area to outlet               = ',f10.2,
     1    ' ha',/,'Total precipitation volume in contributing area = ',
     1    f10.0,' m^3',/,
     1    'Total irrigation volume in contributing area    = ',f10.0,
     1    ' m^3',/,'Total water discharge from outlet               = ',
     1    f10.0,' m^3',/,
     1    'Total sediment discharge from outlet         = ',f13.1,
     1    ' tonnes',/,'Sed. delivery per unit area of watershed       ='
     1    ,f10.1,' T/ha',/,
     1    'Sediment Delivery Ratio for Watershed           = ',f10.3,//)
 2200 format (/19x,'*** total soil loss < 1 kg ***')
 2300 format (//19x,'soil loss for the month of ',a10,//,19x,
     1    'dist. from',/,19x,
     1    'channel top    net soil loss    chan soil loss',/,19x,
     1    '   (ft)         (lbs/ft of channel segment)',/,19x,
     1    '   ----          -------           -------')
 2400 format (20x,f5.1,8x,f9.2,9x,f9.2)
 2500 format ('Sediment Particle Information Leaving Channel',//,
     1    '-----------------------------------------------',
     1    '------------------------',/,
     1    '                             Particle Composition',
     1    '              Fraction',/,'Class  Diameter  Specific  ',
     1    '------------------------            In Flow',/,9x,
     1    '(mm)    Gravity   % Sand   % Silt   % Clay   % O.M.',
     1    '   Exiting',/,
     1    '-------------------------------------------------------',
     1    '------------------------')
 2600 format ('Sediment Particle Information Leaving Impoundment:',//,
     1    '-------------------------------------------------------',/,
     1    '             Fraction',/,
     1    'Class        In Flow',/,
     1    '             Exiting',/,
     1    '-------------------------------------------------------')
      end
