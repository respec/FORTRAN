      subroutine print(effdrr)
c*********************************************************************
c                                                                    *
c     called from subroutine contin.                                 *
c     prints out the detailed hydrology output for every event       *
c     that is routed (i.e., those producing rainfall excess)         *
c                                                                    *
c*********************************************************************
c
      include 'pmxelm.inc'
      include 'pmxnsl.inc'
      include 'pmxtim.inc'
      include 'pmxpnd.inc'
      include 'pmxpln.inc'
      include 'pmxslp.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxhil.inc'
c
c*********************************************************************
c                                                                    *
c    common blocks                                                   *
c                                                                    *
c*********************************************************************
c
      include 'cavepar.inc'
c
      include 'cclim.inc'
c
      include 'ccons.inc'
c
      include 'ccover.inc'
c
      include 'cdata.inc'
c
      include 'cdiss1.inc'
c
      include 'cdiss3.inc'
c
      include 'cdist.inc'
c
      include 'cefflen.inc'
c     read efflen(mxplan)
c
      include 'chydrol.inc'
c
      include 'cirriga.inc'
c
      include 'cirspri.inc'
c
      include 'cparame.inc'
c
      include 'cpass.inc'
c
      include 'cprams.inc'
c
      include 'cslope.inc'
c
      include 'cstruc.inc'
c
      include 'cupdate.inc'
c
      include 'cwater.inc'
c
c reza added 4/7/94
      include 'cwint.inc'
c
c*********************************************************************
c                                                                    *
c   local variables                                                  *
c     chezy  : equivalent chezy coefficient                          *
c     hvc    : a constant used for unit conversions                  *
c     it     :                                                       *
c     i      :                                                       *
c     volinf :                                                       *
c                                                                    *
c*********************************************************************
c
c
      real chezy(mxplan), pkint, effdrr, ropeak, volinf
      integer i
      save pkint
c
      if (iplane.eq.1) then
        write (32,1100)
        write (32,1200) day, mon, year
c
c       ... check minimum temp - if less than or equal to zero all the
c       rainfall is snow
c
c       reza 4/7/94 next line is new   if (tave.gt.0.0) then
c
c       following line should be "tmin", not "tmax"    dcf 6/3/94
c       if(tmax.gt.0.000) then
c       if(tmin.gt.0.000) then
c XXX   Don't need to use temperature at all here.  Since we have the
c       the actual rainfall value (IF it truly is rain then we have
c       a positive value for "rain(iplane)" - then use it.  dcf 8/26/94
c
        if(rain(iplane).gt.0.0001 .or. wmelt(iplane).gt.0.0001
     1    .or. (irsyst.eq.1 .and. irdept(iplane).gt.0.0001))then
          if (rain(iplane).gt..0001)then
            if(ibrkpt.eq.0)then
              write (32,2000) rain(iplane) * 1000., stmdur / 60.,
     1          ip, timep
            else
              write (32,2050) rain(iplane) * 1000., stmdur / 60.
            endif
          endif
          if (irsyst.eq.1) write (32,2100) irofe, irdept(iplane) *
     1        1000., irdur / 60., irint(iplane) * 3.6e06
c         next line is new reza 4/7/94
          if(wmelt(iplane).gt.0.0001)then
            if(ibrkpt.eq.0)then
              write (32,2150) wmelt(iplane)*1000.,stmdur/60.,ip,timep
            else
              write (32,2170) wmelt(iplane)*1000.,stmdur/60.
            endif
          endif
          if(irsyst.eq.1)then
            if(wmelt(iplane).le.0.0001)then
              write (32,1800)
            else
              write (32,1820)
            endif
          else
            if(wmelt(iplane).le.0.0001)then
              write (32,1850)
            else
              write (32,1870)
            endif
          endif
c
c XXX     NOTE - Since each OFE can now have a different value for
c         "ninten", the print out is incomplete - as it contains
c         only the rainfall hyetograph for the first OFE. dcf 6/6/94
c
          do 10 i = 1, ninten(iplane)
            write (32,1900) timem(i) / 60., intsty(i) * 3.6e6
            if (i.eq.1) then
              pkint = intsty(1)
            else
              if (intsty(i).gt.pkint) pkint = intsty(i)
            end if
   10     continue
          write (32,2900)
        else
          write (32,*)
          write (32,*)
     1        ' No liquid precipitation; no melt or irrigation'
c    1        ' Temperature less than zero - all precipitation ',
c    1        'is snow'
        endif
      end if
c
      write (32,1300) ihill
      write (32,1400)
      write (32,1500) iplane
      write (32,1600)
      write (32,1700) ks(iplane) * 3.6e6, sm(iplane) * 1000.,
     1    por(nsl(iplane),iplane), sat(iplane) * 100., cancov(iplane) *
     1    100., gcover(iplane) * 100.
      if (norun(iplane).eq.0) then
        write (32,1000)
      else
c       hydrograph
c
        chezy(iplane) = alpha(iplane) / sqrt(avgslp(iplane))
        write (32,2200) slplen(iplane), m, avgslp(iplane),
     1      chezy(iplane)
c
        write (32,2300) aveks(iplane) * 3.6e6, avesm(iplane) * 1000.,
     1      avpor(iplane), avsat(iplane)
c
        write (32,2400) runoff(iplane) * 1000., peakro(iplane) * 3.6e6,
     1      effdrn(iplane) / 60., efflen(iplane)
c
c XXX   PROBLEMS with multiple OFE hydrology output.  Jeff Stone is
c       correcting and revising hydrograph and runoff outputs.  For
c       now only output values for single OFE hillslopes - and
c       notify user for multiple OFE's that revised output is
c       forthcoming.   dcf  8/29/94
c       if (iplane.eq.nplane) then
        if (iplane.eq.nplane .and. nplane.eq.1) then
          write (32,2500) ihill
          write (32,2600)
          ropeak = 0.0
          do 20 i = 1, nqt
            write (32,2700) i, tq1(i) / 60., q(i) / efflen(nplane) *
     1          3.6e6, qtot(i) / efflen(iplane) / 0.001
            if (q(i)/efflen(nplane)*3.6e6.gt.ropeak) ropeak = q(i) /
     1          efflen(nplane) * 3.6e6
   20     continue
          write (32,2800)
c
c XXX - Changed from using (ns+1) - which is the index for the number
c       of rainfall excess time steps to (nf+1) - which is the index
c       for the number of infiltration time steps.  Is this correct??
c       Need to check this out with Jeff Stone.  dcf  5/27/94
c         volinf = rcum(ns+1) * 1000. - runoff(nplane) * 1000.
c XXX - Problems still with the following line.  Data sets from
c       Andreas Klik in Austria - single storm with breakpoint data,
c       by changing only effective conductivity in soil file, the
c       nf+1 index is messed up.  In one case "rcum(nf+1)=0.0"
c       when it should have been 0.0587 meters.  This needs to
c       be fixed by Jeff Stone.    dcf  8/26/94
c         volinf = rcum(nf+1) * 1000. - runoff(nplane) * 1000.
c XXX   TEMPORARY FIX - compute VOLINF and print out following
c       information ONLY for the case of 1 OFE hillslopes.
c       Jeff Stone is looking into movement of call to PRINT and
c       how these variables are computed and should be printed -
c       in order for them to work properly for multiple OFE
c       hillslopes.   dcf   8/29/94
c         if(nplane.eq.1)then
            volinf = rr(nf) * 1000.  -  runoff(nplane) * 1000.
c
          if(irsyst.eq.1)then
            if(wmelt(nplane).gt.0.0001)then
c XXX - Changed from using (ns+1) - which is the index for the number
c       of rainfall excess time steps to (nf+1) - which is the index
c       for the number of infiltration time steps.  Is this correct??
c       Need to check this out with Jeff Stone.  dcf  5/27/94
c             write (32,3000) ihill, rcum(ns+1) * 1000., volinf,
c XXX - Problems still with the following line.  Data sets from
c       Andreas Klik in Austria - single storm with breakpoint data,
c       by changing only effective conductivity in soil file, the
c       nf+1 index is messed up.  In one case "rcum(nf+1)=0.0"
c       when it should have been 0.0587 meters.  This needs to
c       be fixed by Jeff Stone.    dcf  8/26/94
c             write (32,3000) ihill, rcum(nf+1) * 1000., volinf,
c XXX - Changed from RCUM(NF+1) to RR(NF) re: Jeff Stone  8/29/94   dcf
              write (32,3000) ihill, rr(nf) * 1000., volinf,
     1          runoff(nplane) * 1000., pkint * 3.6e6, effint(nplane) *
     1          3.6e6, effdrr / 60., f(nf) * 3.6e6, ropeak, stmdur / 60,
     1          tp(1) / 60., effdrn(iplane) / 60., efflen(iplane)
            else
c XXX - Changed from using (ns+1) - which is the index for the number
c       of rainfall excess time steps to (nf+1) - which is the index
c       for the number of infiltration time steps.  Is this correct??
c       Need to check this out with Jeff Stone.  dcf  5/27/94
c             write (32,3100) ihill, rcum(ns+1) * 1000., volinf,
c XXX - Problems still with the following line.  Data sets from
c       Andreas Klik in Austria - single storm with breakpoint data,
c       by changing only effective conductivity in soil file, the
c       nf+1 index is messed up.  In one case "rcum(nf+1)=0.0"
c       when it should have been 0.0587 meters.  This needs to
c       be fixed by Jeff Stone.    dcf  8/26/94
c             write (32,3100) ihill, rcum(nf+1) * 1000., volinf,
c XXX - Changed from RCUM(NF+1) to RR(NF) re: Jeff Stone  8/29/94   dcf
              write (32,3100) ihill, rr(nf) * 1000., volinf,
     1          runoff(nplane) * 1000., pkint * 3.6e6, effint(nplane) *
     1          3.6e6, effdrr / 60., f(nf) * 3.6e6, ropeak, stmdur / 60,
     1          tp(1) / 60., effdrn(iplane) / 60., efflen(iplane)
            endif
          elseif(wmelt(nplane).gt.0.0001)then
c XXX - Changed from using (ns+1) - which is the index for the number
c       of rainfall excess time steps to (nf+1) - which is the index
c       for the number of infiltration time steps.  Is this correct??
c       Need to check this out with Jeff Stone.  dcf  5/27/94
c           write (32,3200) ihill, rcum(ns+1) * 1000., volinf,
c XXX - Problems still with the following line.  Data sets from
c       Andreas Klik in Austria - single storm with breakpoint data,
c       by changing only effective conductivity in soil file, the
c       nf+1 index is messed up.  In one case "rcum(nf+1)=0.0"
c       when it should have been 0.0587 meters.  This needs to
c       be fixed by Jeff Stone.    dcf  8/26/94
c           write (32,3200) ihill, rcum(nf+1) * 1000., volinf,
c XXX - Changed from RCUM(NF+1) to RR(NF) re: Jeff Stone  8/29/94   dcf
            write (32,3200) ihill, rr(nf) * 1000., volinf,
     1        runoff(nplane) * 1000., pkint * 3.6e6, effint(nplane) *
     1        3.6e6, effdrr / 60., f(nf) * 3.6e6, ropeak, stmdur / 60,
     1        tp(1) / 60., effdrn(iplane) / 60., efflen(iplane)
          else
c XXX - Changed from using (ns+1) - which is the index for the number
c       of rainfall excess time steps to (nf+1) - which is the index
c       for the number of infiltration time steps.  Is this correct??
c       Need to check this out with Jeff Stone.  dcf  5/27/94
c           write (32,3300) ihill, rcum(ns+1) * 1000., volinf,
c XXX - Problems still with the following line.  Data sets from
c       Andreas Klik in Austria - single storm with breakpoint data,
c       by changing only effective conductivity in soil file, the
c       nf+1 index is messed up.  In one case "rcum(nf+1)=0.0"
c       when it should have been 0.0587 meters.  This needs to
c       be fixed by Jeff Stone.    dcf  8/26/94
c           write (32,3300) ihill, rcum(nf+1) * 1000., volinf,
c XXX - Changed from RCUM(NF+1) to RR(NF) re: Jeff Stone  8/29/94   dcf
            write (32,3300) ihill, rr(nf) * 1000., volinf,
     1        runoff(nplane) * 1000., pkint * 3.6e6, effint(nplane) *
     1        3.6e6, effdrr / 60., f(nf) * 3.6e6, ropeak, stmdur / 60,
     1        tp(1) / 60., effdrn(iplane) / 60., efflen(iplane)
          end if
        else
c         write a message to users that multiple OFE hydrology output
c         is being corrected.
c         if(iplane.eq.nplane)write (32,3400)
        end if
      end if
c
      return
c
 1000 format (//30x,'***************************'/30x,
     1    '*                         *'/30x,
     1    '*    no rainfall excess   *'/30x,
     1    '*                         *'/30x,'**************************'
     1    /)
 1100 format (//'I.   SINGLE STORM HYDROLOGY',/,2x,9('-'),1x,5('-'),1x,
     1    36('-'))
 1200 format (//'  infiltration, rainfall excess, and runoff',
     1    ' hydrograph for event of',3(1x,i2)//2x,'hydrology summary'/2
     1    x,52('-')/)
 1300 format (//2x,18('*'),/4x,'hillslope ',i2,/,2x,18('*'))
 1400 format (//6x,27('*'))
 1500 format (7x,'overland flow element ',i2)
 1600 format (6x,27('*'))
 1700 format (/' infiltration input parameters'/2x,52('-')//
     1    '      effective saturated conductivity ',f8.2,' (mm/h)'/
     1    '      effective matric potential       ',f8.2,' (mm)'/
     1    '      effective porosity               ',f8.2,' (mm/mm)'/
     1    '      saturation                       ',f8.2,' (%)'/
     1    '      canopy cover                     ',f8.2,' (%)'/
     1    '      surface cover                    ',f8.2,' (%)'/)
 1800 format (//2x,'rainfall + sprinkle irrigation',/,
     1    8x,'time   intensity',/,
     1    8x,'(min)   (mm/hr)',/,2x,52('-'))
 1820 format (//2x,'rainfall + snow melt + sprinkle irrigation',/,
     1    8x,'time   intensity',/,
     1    8x,'(min)   (mm/hr)',/,2x,52('-'))
 1850 format (//2x,'rainfall',/,
     1    8x,'time   intensity',/,
     1    8x,'(min)   (mm/hr)',/,2x,52('-'))
 1870 format (//2x,'rainfall + snow melt',/,
     1    8x,'time   intensity',/,
     1    8x,'(min)   (mm/hr)',/,2x,52('-'))
 1900 format (4x,2f10.2)
 2000 format ('    rainfall amount           ',f8.2,' (mm)'/
     1    '    rainfall duration         ',f8.2,' (min)'/
     1    '    normalized peak intensity ',f8.2,/
     1    '    normalized time to peak   ',f8.2,/)
 2050 format ('    rainfall amount           ',f8.2,' (mm)'/
     1    '    rainfall duration         ',f8.2,' (min)'/
     1    '    INPUT BREAKPOINT PRECIPITATION USED',/)
 2100 format ('    plane irrigated           ',i2/
     1    '    sprinkle irrig amount     ',f8.2,' (mm)'/
     1    '    sprinkle irrig duration   ',f8.2,' (min)'/
     1    '    sprinkle irrig intensity  ',f8.2,' (mm/hr)'/)
 2150 format ('    snowmelt amount           ',f8.2,' (mm)'/
     1    '    snowmelt duration         ',f8.2,' (min)'/
     1    '    normalized peak intensity ',f8.2,/
     1    '    normalized time to peak   ',f8.2,/)
 2170 format ('    snowmelt amount           ',f8.2,' (mm)'/
     1    '    snowmelt duration         ',f8.2,' (min)'/)
 2200 format (/'  input runoff parameters'/2x,52('-')/
     1    '      plane length               ',f8.2,' (m)'/
     1    '      discharge exponent         ',f8.2/
     1    '      average slope of profile   ',f8.2/
     1    '      chezy coefficient          ',f8.2,' (m**0.5/s)'/)
 2300 format (/'  output runoff parameters'/2x,52('-')/
     1    '      equivalent sat. hydr. cond.',f8.2,' (mm/hr)'/
     1    '      equivalent matr. potential ',f8.2,' (mm)'/
     1    '      average pore fraction      ',f8.2,' (m/m)'/
     1    '      average saturation fraction',f8.2,' (m/m)'/)
 2400 format (/'  runoff output'/2x,52('-')/
     1    '      runoff volume              ',f8.2,' (mm)'/
     1    '      peak runoff rate           ',f8.2,' (mm/hr)'/
     1    '      effective runoff duration  ',f8.2,' (min)'/
     1    '      effective length           ',f8.2,' (meters)'/)
 2500 format (/'  output runoff hydrograph for hillslope ',i2,/2x,52('-'
     1    )/)
 2600 format ('                                  cumul.'/
     1    '      index    time      rate     depth  '/
     1    '               (min)    (mm/h)     (mm)'/5x,49('-'))
 2700 format (5x,i5,3f10.2)
 2800 format (2x,52('-'))
 2900 format (/2x,52('*'))
 3000 format (//2x,'runoff hydrograph summary for hillslope ',i2,
     1         /2x,'------ ---------- ------- --- --------- --',/,
     1    '  rainfall, snow melt + sprinkle volume   ',f8.2,' (mm)'/
     1    '  infiltration volume                     ',f8.2,' (mm)'/
     1    '  runoff volume                           ',f8.2,' (mm)'//
     1    '  peak rainfall intensity                 ',f8.2,' (mm/h)'/
     1    '  effective rainfall intensity            ',f8.2,' (mm/h)'/
     1    '  effective rainfall duration             ',f8.2,' (min)'/
     1    '  final infiltration rate                 ',f8.2,' (mm/h)'/
     1    '  peak runoff rate                        ',f8.2,' (mm/h)'//
     1    '  duration of rain/irrigation             ',f8.2,' (min)'/
     1    '  time to first ponding                   ',f8.2,' (min)'/
     1    '  effective runoff duration               ',f8.2,' (min)'/
     1    '  effective length                        ',f8.2,' (meters)',
     1       /)
 3100 format (//2x,'runoff hydrograph summary for hillslope ',i2,
     1         /2x,'------ ---------- ------- --- --------- --',/,
     1    '  rainfall + sprinkle irrigation volume   ',f8.2,' (mm)'/
     1    '  infiltration volume                     ',f8.2,' (mm)'/
     1    '  runoff volume                           ',f8.2,' (mm)'//
     1    '  peak rainfall intensity                 ',f8.2,' (mm/h)'/
     1    '  effective rainfall intensity            ',f8.2,' (mm/h)'/
     1    '  effective rainfall duration             ',f8.2,' (min)'/
     1    '  final infiltration rate                 ',f8.2,' (mm/h)'/
     1    '  peak runoff rate                        ',f8.2,' (mm/h)'//
     1    '  duration of rain/irrigation             ',f8.2,' (min)'/
     1    '  time to first ponding                   ',f8.2,' (min)'/
     1    '  effective runoff duration               ',f8.2,' (min)'/
     1    '  effective length                        ',f8.2,' (meters)',
     1       /)
 3200 format (//2x,'runoff hydrograph summary for hillslope ',i2,
     1         /2x,'------ ---------- ------- --- --------- --',/,
     1    '  rainfall + snow melt volume  ',f8.2,' (mm)'/
     1    '  infiltration volume          ',f8.2,' (mm)'/
     1    '  runoff volume                ',f8.2,' (mm)'//
     1    '  peak rainfall intensity      ',f8.2,' (mm/h)'/
     1    '  effective rainfall intensity ',f8.2,' (mm/h)'/
     1    '  effective rainfall duration  ',f8.2,' (min)'/
     1    '  final infiltration rate      ',f8.2,' (mm/h)'/
     1    '  peak runoff rate             ',f8.2,' (mm/h)'//
     1    '  duration of rainfall         ',f8.2,' (min)'/
     1    '  time to first ponding        ',f8.2,' (min)'/
     1    '  effective runoff duration    ',f8.2,' (min)'/
     1    '  effective length             ',f8.2,' (meters)'/)
 3300 format (//2x,'runoff hydrograph summary for hillslope ',i2,
     1         /2x,'------ ---------- ------- --- --------- --',/,
     1    '  rainfall volume              ',f8.2,' (mm)'/
     1    '  infiltration volume          ',f8.2,' (mm)'/
     1    '  runoff volume                ',f8.2,' (mm)'//
     1    '  peak rainfall intensity      ',f8.2,' (mm/h)'/
     1    '  effective rainfall intensity ',f8.2,' (mm/h)'/
     1    '  effective rainfall duration  ',f8.2,' (min)'/
     1    '  final infiltration rate      ',f8.2,' (mm/h)'/
     1    '  peak runoff rate             ',f8.2,' (mm/h)'//
     1    '  duration of rainfall         ',f8.2,' (min)'/
     1    '  time to first ponding        ',f8.2,' (min)'/
     1    '  effective runoff duration    ',f8.2,' (min)'/
     1    '  effective length             ',f8.2,' (meters)'/)
c XXX - MESSAGE TO USERS THAT MULTIPLE OFE HYDROLOGY OUTPUT NOT
c       AVAILABLE CURRENTLY.
c3400 format (//2x, 'NOTE - Additional multiple OFE hydrology output',
c    1  /,'  (hydrograph, hillslope infiltration & runoff summaries)',
c    1  /,'  is currently undergoing revision and correction.',/,
c    1    '  This information will be included in a future WEPP',/,
c    1    '  model release.',/,
c    1    '  Note Date:  8/29/94       Authors:  Flanagan & Stone',//)
c3400 format (//2x,'runoff hydrograph summary for MULTIPLE OFE',
c    1    ' hillslope ',i2,
c    1         /2x,'------ ---------- ------- --- --------- --',/,
c    1    '  water inputs volume          ',f8.2,' (mm)'/
c    1    '  infiltration volume          ',f8.2,' (mm)'/
c    1    '  runoff volume                ',f8.2,' (mm)'//
c    1    '  peak rainfall intensity      ',f8.2,' (mm/h)'/
c    1    '  effective rainfall intensity ',f8.2,' (mm/h)'/
c    1    '  effective rainfall duration  ',f8.2,' (min)'/
c    1    '  final infiltration rate      ',f8.2,' (mm/h)'/
c    1    '  peak runoff rate             ',f8.2,' (mm/h)'//
c    1    '  duration of rainfall         ',f8.2,' (min)'/
c    1    '  time to first ponding        ',f8.2,' (min)'/
c    1    '  effective runoff duration    ',f8.2,' (min)'/
c    1    '  effective length             ',f8.2,' (meters)'//,
c    1    '  NOTE - Runoff outputs for multiple OFE hillslopes',/,
c    1    '         are currently under revision.',/,
c    1    '  ***WARNING*** Values reported here may be incorrect.')
      end
