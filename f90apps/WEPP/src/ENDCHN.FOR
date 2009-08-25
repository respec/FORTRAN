      subroutine endchn(npart,nraint,traint,nptsc,toplen,nyear,ichplt)
c
c     + + + PURPOSE + + +
c
c     SR ENDCHN writes the hydrologic and erosion summary
c     for the entire simulation period.
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
      include 'pmximp.inc'
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real traint, toplen
      integer npart, nptsc, nraint, nyear, ichplt
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     traint -
c     toplen -
c     npart  -
c     nptsc  -
c     nraint -
c     nyear  -
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
      include 'cseddet.inc'
      include 'cslpopt.inc'
      include 'cstore.inc'
      include 'cstruc.inc'
      include 'cstruct.inc'
      include 'csumirr.inc'
      include 'cimacl.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real y(mxcseg), concpc, dx, frac, ppm, tcncpc, tconc,
     1    tgs(mxelem), tppm, trunsi, xchn, conv, sdr, wdet
c
      integer i, k, iseg, ipond,ni
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     y(mxcseg) -
c     concpc    -
c     dx        -
c     frac      -
c     ni        - impoundment clean times(<100)
c     ppm       -
c     tcncpc    -
c     tconc     -
c     tgs       -
c     tppm      -
c     trunsi    -
c     xchn      -
c     conv      -  conversion factor from lbs to kg (0.4536)
c     sdr       -
c     wdet      -
c
c     Integer Variables
c
c     i     -
c     k     -
c     iseg  -
c     ipond -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     enrcmp
c
c     + + + DATA INITIALIZATIONS + + +
c
c
      ichan = 0
      ipond = 0
      sdr = 0.00
      wdet = 0.0
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
c
      write (38,1000)nyear
      write (38,1100)
c
      do 10 k = 1, nhill
        write (38,1300) k,hrot(k)/nyear,sbrvty(k)/nyear,dett(k)/nyear,
     1    (dept(k)/nyear) * (-1), hsedt(k)/nyear
        wdet = wdet + dett(k)
   10 continue
c
      write (38,1200)
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
c       get total sediment delivery for each channel
c
        if (elmt(ielmt).eq.3) then
          tgs(ielmt) = coute(ipond) / conv
        else
c
          do 20 k = 1, npart
            tgs(ielmt) = tgs(ielmt) + tgst(k,ielmt)
   20     continue
c
        end if
c
c       convert lbs to total kg soil loss or deposition on channel
c
        wdet = wdet + (cdett(ielmt)*conv)
c
        if (elmt(ielmt).eq.2) write (38,1400) ichan, trunt(ielmt)/nyear,
     1      (tgs(ielmt)/nyear) * conv / 1000.0,
     1      tronvt(ielmt)/nyear, sbrvtt(ielmt)/nyear
        if (elmt(ielmt).eq.3) write (38,1500) ipond, trunt(ielmt)/nyear,
     1      (tgs(ielmt)/nyear) * conv / 1000.0
c
        cdett(ielmt) = 0.0
        cdept(ielmt) = 0.0
c
   30 continue
c
      do 40 ielmt = nhill + 1, nelmt
c
c       convert total runoff volume at the watershed outlet from
c       m^3 to mm using the local variable trunsi (si units)
c
        trunsi = (trunt(ielmt)/wsarea(ielmt)) * 1000.0
c
        if (ielmt.eq.nelmt) then
c
c         write to summary output
c
          write (38,1600) nraint/nyear, traint/nyear
c
          write (38,1700) nrunt/nyear, trunsi/nyear
c
          if (wdet.gt.0) sdr = tgs(nelmt) * conv / wdet
          if (wdet.le.0) sdr = 0.0
c
          write (38,2500) wsarea(nelmt) / 10000,
     1        (wsarea(nelmt) * (traint/1000))/nyear, tirrt/nyear,
     1        trunt(nelmt)/nyear, (tgs(nelmt) * conv/1000.)/nyear,
     1        (((tgs(nelmt)*conv)/1000) / (wsarea(nelmt)/10000))/nyear
     1        , sdr
        end if
c
   40 continue
c
      if (nrunt.gt.0) then
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
c         write output if total soil loss for the simulation is > 1 kg
c
          if (tgs(ielmt).gt.2.205) then
c
            if (elmt(ielmt).eq.2) then
              if (watsum.le.2.and.ielmt.eq.nelmt) write (38,1800)
            else
              if (watsum.le.2.and.ielmt.eq.nelmt) write (38,1900)
            end if
c
            tconc = 0.0
            tcncpc = 0.0
            tppm = 0.0
c
            do 50 k = 1, npart
c
              if (elmt(ielmt).eq.2) then
                conc(k,ielmt) = (tgst(k,ielmt)*0.4536) / trunt(ielmt)
              else
                conc(1,ielmt) = cloute(ipond) / trunt(ielmt)
                conc(2,ielmt) = sloute(ipond) / trunt(ielmt)
                conc(3,ielmt) = saoute(ipond) / trunt(ielmt)
                conc(4,ielmt) = laoute(ipond) / trunt(ielmt)
                conc(5,ielmt) = sdoute(ipond) / trunt(ielmt)
              end if
c
              concpc = conc(k,ielmt) / (wtdh2o*16.0211)
              ppm = concpc * 1.0e+06
c
              if (elmt(ielmt).eq.2) then
                frac = tgst(k,ielmt) / tgs(ielmt)
              else
                frac = conc(k,ielmt) * trunt(ielmt) / coute(ipond)
              end if
c
              if (elmt(ielmt).eq.2) then
c
                if (watsum.le.2.and.ielmt.eq.nelmt) write (38,2000) k,
     1              crdia(k,ielmt) * 1000 / 3.281 , crspg(k), 
     1              frsnd(k,ielmt) * 100, frslt(k,ielmt) * 100, 
     1              frcly(k,ielmt) * 100, frorg(k,ielmt) * 100, frac
c
              else
c
                if (watsum.le.2.and.ielmt.eq.nelmt) write (38,2100) k,
     1              frac
c
                if (watsum.ge.3) write (38,2100) k, frac
c
              end if
c
              tconc = tconc + conc(k,ielmt)
              tcncpc = tcncpc + concpc
              tppm = tppm + ppm
c
   50       continue
c
            if (ielmt.eq.nelmt) call enrcmp(2,ielmt,nelmt,tgs,
     1          elmt(ielmt))
c
          else
c
            if (watsum.le.2.and.ielmt.eq.nelmt) write (38,2200)
c
          end if
c
   60   continue
c
      else
c
      end if
c
      if (ichplt.eq.1) then
c
c       write out channel profile plotting data
c
        write (37,2300)
c
        iseg = nptsc
        y(iseg) = 0.0
c
        do 70 i = 1, nptsc - 1
          dx = (chnx(ichan,iseg)-chnx(ichan,iseg-1)) * 3.281
          y(iseg-1) = y(iseg) + dx * chnslp(ichan,iseg)
          iseg = iseg - 1
   70   continue
c
        do 80 iseg = 1, nptsc
          xchn = (chnx(ichan,iseg)-toplen) * 3.281
          write (37,2400) xchn, y(iseg), nsedt(iseg) / nyear, -
     1        csedt(iseg) / nyear
   80   continue
c
      end if
c
c      Added by S.Dun
        ipond=0
        do 100 ielmt = nhill + 1, nelmt
c
          if (elmt(ielmt).eq.2) goto 100
          ipond = ipond + 1
          write(38,3000) ipond,alnum(ipond)
                 ni=alnum(ipond)
                if (ni.gt.100) ni=100
                do 90 i=1,ni
                  write( 38,3100) i,alvol(ipond,i)/3.28**3,
     1               aldate(ipond,i),alyear(ipond,i),alhs(ipond,i)/3.281
   90           continue
  100    continue
c        end add
      return
c
c
 1000 format (////,16x,i5,' YEAR AVERAGE ANNUAL VALUES FOR WATERSHED',/,
     1    15x,    '------ ---- ------- ------ ------ --- ---------',//)
 1100 format (/17x,'Runoff',8x,'Subrunoff',5x,'Soil',10x,'Sediment',6x,
     1    'Sediment',/,17x,
     1    'Volume',8x,'Volume',8x,'Loss',10x,'Deposition',4x,'Yield',/,
     1    'Hillslopes',7x,'(m^3)',9x,'(m^3)',9x,'(kg)',10x,'(kg)',10x,
     1    '(kg)',/,10('-'),7x,5(10('-'),4x))
cd 1200 format (///,'Channels',9x,'Discharge',5x,'Sediment',6x,/,'and',
cd     1    14x,'Volume',8x,'Yield',/,'Impoundments',5x,'(m^3/yr)',6x,
cd     1    '(tonne/yr)',/,12('-'),5x,2(10('-'),4x),/)
 1200 format (///'Channels',9x,'Discharge',5x,'Sediment',6x,'Upland',
     1        6x,'Subsuface Flow'/,
     1        'and',14x,'Volume',8x,'Yield',9x,'Charge',8x,'Volume' /,
     1        'Impoundments',5x,'(m^3)',9x,'(tonne)',7x,'(m^3)',10x,
     1         '(m^3)'/,12('-'),5x,2(10('-'),4x),2(10('-'),3x),/)
 1300 format ('Hill    ',i4,5(2x,f12.2))
 1400 format ('Channel    ',1x,i3,1x,4(f9.1,5x))
 1500 format ('Impoundment',1x,i2,1x,2(f9.1,5x))
 1600 format (/,i4,' storms produced ',f8.2,' mm. of rainfall on',
     1    ' an AVERAGE ANNUAL basis')
 1700 format (/,i4,' events produced ',f8.2,' mm. of runoff',/,5x,
     1    'passing through the watershed outlet on',
     1    ' an AVERAGE ANNUAL basis',/)
 1800 format ('Sediment Particle Information Leaving Channel:',/,/,
     1    '-----------------------------------------------',
     1    '------------------------',/,
     1    '                             Particle Composition',
     1    '              Fraction',/,'Class  Diameter  Specific  ',
     1    '------------------------            In Flow',/,9x,
     1    '(mm)    Gravity   % Sand   % Silt   % Clay   % O.M.',
     1    '   Exiting',/,
     1    '-------------------------------------------------------',
     1    '------------------------')
 1900 format ('Sediment Particle Information Leaving Impoundment:',//,
     1    '-------------------------------------------------------',/,
     1    '             Fraction',/,'Class        In Flow',/,
     1    '             Exiting',/,
     1    '-------------------------------------------------------')
 2000 format (1x,i2,4x,f6.3,6x,f4.2,4x,f5.1,4x,f5.1,4x,f5.1,4x,f5.1,5x,
     1    f5.3,4x,f5.3)
 2100 format (1x,i2,10x,f5.3)
 2200 format (/,19x,'*** total soil loss < 1 kg ***')
 2300 format (//,10x,'Annual Average Sediment Yield',//,10x,
     1    '     x       y        sed yld      chan',/,10x,
     1    '   (ft)    (ft)       (t/ac)     (lbs/ft ch)',/,10x,
     1    '   ----    ----       -------     -------')
 2400 format (12x,f5.1,3x,f5.1,6x,f9.2,3x,f9.2)
 2500 format (/,' Average Annual Delivery From Channel Outlet:',/,
     1   ' ------- ------ -------- ---- ------- ------',/,/,
     1   'Total contributing area to outlet                   =   ',
     1   f10.2,
     1   ' ha',/,'Avg. Ann. Precipitation volume in contributing',
     1   ' area = ',f10.0,'   m^3/yr',/,
     1   'Avg. Ann. irrigation volume in contributing area    = ',
     1   f10.0,
     1   '   m^3/yr',/,'Avg. Ann. water discharge from outlet         ',
     1   '      = ',f10.0,'   m^3/yr',/,
     1   'Avg. Ann. sediment discharge from outlet            ='
     1   ,f12.1,1x,' tonnes/yr',/,
     1    'Avg. Ann. Sed. delivery per unit area of watershed  ',
     1    '= ',f11.1,'  T/ha/yr',/,
     1    'Sediment Delivery Ratio for Watershed               =    ',
     1    f10.3,//)
3000   format('Impoundment',1x,i3,1x,'automatically cleaned',
     1       1x,i3,1x,'times',/,     
     1       '--------------------------------------------')
3100  format('No.',1x,i3,1x,'cleaned out',1x,f10.2,1x,'m^3 sediment at',
     1 ' Day',1x,i3,1x,'of Year',1x,i3,1x,'sed-stage=',f8.2,' m')    
c        end add     
      end
