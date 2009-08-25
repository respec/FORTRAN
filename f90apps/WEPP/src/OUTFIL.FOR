      subroutine outfil(lunp,luns,lunw,lun1)
c
c     + + + PURPOSE + + +
c
c     Opens output files (if requested) for slope plotting,
c     erosion plotting, and hydrology output.
c
c     Called from: SR CONTIN
c     Author(s): Livingston, Ascough II, Others
c     Reference in User Guide:
c
c     Version: This module not yet recoded.
c     Date recoded:
c     Recoded by:
c
c     Output Files:
c
c      Unit #      Output file description
c      ------      -----------------------
c        30        Event by event summary
c        31        Ofe soil loss
c        32        Single event output file
c        33        Event by event summary for each ofe (hillslope only)
c        34        Ofe abbreviate raw summary (ABBREV.RAW)
c        35        Water balance daily output
c        36        Plant and residue daily output
c        37        Channel profile erosion output file
c        38        Watershed erosion output file
c        39        Soil properties daily output
c        40        Hillslope large graphics output file (PLOTFILE.WGR)
c        41        Watershed large graphics output file (PLOTFILE.WGR)
c        42        Winter component output
c        43        Hillslope profile erosion output file
c        44        Rangeland range output file
c        45        Rangeland animal output file
c        46        Crop yield output file
c        47        Initial condition warmup results
c        48        Hillslope hydrology/sediment pass files
c        49        Watershed hydrology/sediment master pass file
c        50        Impoundment unformatted hydraulic pass file
c        51        Impoundment output file
c        52        Temporary summary file
c        53        Final summary file
c        54        Impoundment sediment graph output
c        55        Impoundment hydrograph output
c        56        Impoundment detailed sediment output
c        57        Impoundment detailed hydraulic output
c        59        Watershed runoff detailed output
c
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxhil.inc'
      include 'pmximp.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pntype.inc'
      include 'pmxcsg.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer lunp, luns, lunw, lun1
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     lunp,luns,lunw - flags for plant, soil, and water output respectively
c     lun1   - flag for plotting output
c
c     + + + COMMON BLOCKS + + +
c
      include 'cavloss.inc'
c     modify: ioutpt,ioutss
c
      include 'cerrid.inc'
c     read: ibomb,iost
c
      include 'cflags.inc'
c     modify: bigflg,ichplt,ipdout,snoflg,wflag,wgrflg,yldflg
c
      include 'cimpnd.inc'
c     read: impond
c
      include 'ciplot.inc'
      include 'cprams.inc'
c
      include 'cstruc.inc'
c
      include 'cchpar.inc'
c
      include 'cunicon.inc'
c     read outopt
c
c     + + + LOCAL VARIABLES + + +
c
      character*51 filen
      character*65 ostrng
      character*72 header
      integer useout
c
c     + + + LOCAL DEFINITIONS + + +
c
c     filen          - local file name to be opened
c     ostrng         - string passed to subroutine open
c     outopt         - flag for output option
c     useout         - integer function for yes/no answers
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     open
c     useout (function)
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c     ... open output files if hillslope or hillslope/watershed version
c
      outopt = 0

      if (ivers.ne.3) then
c
        if (imodel.eq.1) then
c
c         ... continuous version output files
c
          outopt = 1
          write (6,1100)
c
          read (5,2000,err=10) outopt
c
   10     if ((outopt.lt.1).or.(outopt.gt.5)) then
            write (6,1200)
            outopt = 1
          end if
c
          if (outopt.eq.1) then
            header = ' Annual; abbreviated (English Units)'
            if(units.eq.0)header = ' Annual; abbreviated (Metric Units)'
            ioutpt = 3
            ioutas = 2
c
          else if (outopt.eq.2) then
            header = ' Annual; detailed (Metric Units)'
            ioutpt = 3
            ioutas = 1
          else if (outopt.eq.3) then
            header = ' Event-by-event; abbreviated (Metric Units)'
            ioutpt = 1
            ioutss = 2
          else if (outopt.eq.4) then
            header = ' Event-by-event; detailed (Metric Units)'
            ioutpt = 1
            ioutss = 1
          else if (outopt.eq.5) then
            header = ' Monthly (Metric Units)'
            ioutpt = 2
            ioutas = 1
          end if
c
c         ... open abbrev.raw output file
c
c         open (unit=34,file='abbrev.raw',status='unknown')
c
c         ... open initial condition WARMUP output file
c
          if (useout('initial condition scenario').eq.1) then
            ostrng =
     1          'Enter initial condition scenario output file name -->'
            call open(47,iost,1,ostrng,scefil)
            ifile = 2
          end if
c
c         ... open file containing soil loss output
c         ... unit = 31, iost depends on ibomb, 1 if ibomb=2, 3 if ibomb=1
c         ... loop if ibomb=2, write over if ibomb=1
c
          ostrng = 'Enter file name for soil loss output -->'
          call open(31,iost,1,ostrng,filen)
c
          write (31,2800) header
c         detailed output for water data
c
          if (useout('water balance').eq.1) then
c
c           ... open file for water balance output
c
            ostrng = 'Enter file name for water balance output -->'
            call open(35,iost,1,ostrng,filen)
c
c           write headers for the water
c
            write (35,1400)
            write (35,1500)
            lunw = 1
          end if
c
          if (useout('crop').eq.1) then
c
c           ... open file for plant output
c           ... unit = 36, status = iost
c
            ostrng = 'Enter file name for plant output -->'
            call open(36,iost,1,ostrng,filen)
c
            write (36,1600)
            write (36,1700)
            write (36,1800)
            lunp = 1
          end if
c
          if (useout('soil').eq.1) then
c
c           ... open file for soil output
c           ... unit = 39, status = iost
c
            ostrng = 'Enter file name for soil output -->'
            call open(39,iost,1,ostrng,filen)
c
            write (39,1900)
            luns = 1
          end if
c
        else
c
          header = ' Single storm'
c
c         single event version output file
c
          ostrng = 'Enter file name for single event output -->'
          call open(32,iost,1,ostrng,filen)
          write (32,2800) header
c
        end if
c
c       .. end of single event output
c
c       ... if desired, open file for plotting output
c       ... unit = 43, status = iost
c
        if (useout('distance and sediment loss').eq.1) then
          ostrng = 'Enter file name for plotting output -->'
          call open(43,iost,1,ostrng,filen)
          write (43,2500)
          lun1 = 1
        end if
c
c       ... only print out large graphics file for
c           continuous simulations
c
        if (imodel.eq.1) then
c
          if (useout('large graphics').eq.1) then
            bigflg = 1
            ostrng = 'Enter file name for large graphics output -->'
            call open(40,iost,1,ostrng,filen)
c
            if (lun1.eq.1) then
              lun1 = 2
            else
              lun1 = 3
            end if
c
          end if
        end if
c
        ievt = 0
        ifofe = 0
        isum = 0
c
        if (useout('event by event').eq.1) then
c
          ostrng = 'Enter file name for event by event summary -->'
          call open(30,iost,1,ostrng,filen)
          write (30,2100)
c
          ievt = 1
        end if
c
        if (useout('element').eq.1) then
c
          ostrng = 'Enter file name for variable OFE line summary -->'
          call open(33,iost,1,ostrng,filen)
          write (33,2300)
          ifofe = 1
        end if
c
        if (useout('final summary').eq.1) then
c
          ostrng = 'Enter file name for final summary file -->'
          call open(53,iost,1,ostrng,filen)
c
          isum = 1
c
          if (ioutpt.eq.1) then
            write (53,2700)
          else
            write (53,2600)
          end if
c
c         ... open yearly summary temp file
c         ... unit = 52, status = 'unknown'
c
          open (unit=52,file='delete.me',status='unknown')
c
        end if
c
        if (useout('daily winter').eq.1) then
          snoflg = 1
          ostrng = 'Enter file name for daily winter output -->'
          call open(42,iost,1,ostrng,filen)
        end if
c
c       ... open crop yield output file
c
        if (imodel.eq.1) then
c
          if (useout('plant yield').eq.1) then
            yldflg = 1
            ostrng = 'Enter file name for plant yield outputs -->'
            call open(46,iost,1,ostrng,filen)
            write (46,1300) filen
          end if
c
        end if
c
c     ... end of output files open for hillslope and
c         watershed/hillslope versions
c
      end if
c
c     ...open files if watershed version only
c
      if (ivers.eq.3) then
c
        if (imodel.eq.1) then
c
c         ... open initial condition warmup output file
c
          if (useout('initial condition scenario').eq.1) then
            ostrng =
     1          'Enter initial condition scenario output file name -->'
            call open(47,iost,1,ostrng,scefil)
            ifile = 2
          end if
c
c         ... open file for watershed erosion output
c         ... unit = 38, status = iost ...
c
          watsum = 0
          write (6,3000)
c
          read (5,2000,err=20) watsum
c
   20     if ((watsum.lt.0).or.(watsum.gt.5)) then
            write (6,3100)
            watsum = 0
          end if
c
          if (watsum.eq.0) then
            header = ' Annual  average watershed    values;Abbreviated'
c            wattyp = 0
          else if (watsum.eq.1) then
            header = ' Yearly  average watershed    values;Abbreviated'
c            wattyp = 1
          else if (watsum.eq.2) then
            header = ' Monthly average watershed    values;Abbreviated'
c            wattyp = 2
c          else if (watsum.eq.3) then
c            header = ' Annual  average subwatershed values;Abbreviated'
c            wattyp = 0
c          else if (watsum.eq.4) then
c            header = ' Yearly  average subwatershed values;Abbreviated'
c            wattyp = 1
c          else if (watsum.eq.5) then
c            header = ' Monthly average subwatershed values;Abbreviated'
c            wattyp = 2
          end if
c
        else
c
          watsum = 2
          header = ' Single storm '
c          wattyp = 2
c
        end if
c
        ostrng = 'Enter file name for watershed erosion output -->'
        call open(38,iost,1,ostrng,filen)
c
        write (38,2800) header
c
        if (imodel.eq.1) then
c         ... detailed output for water data
c
          if (useout('water balance').eq.1) then
c
c           ... open file for water balance output
c
            ostrng = 'Enter file name for water balance output -->'
            call open(35,iost,1,ostrng,filen)
c
c           ... write headers for the water balance output
c
            write (35,1400)
            write (35,1500)
            lunw = 1
          end if
c
          if (useout('crop').eq.1) then
c
c           ... open file for plant output
c           ... unit = 36, status = iost
c
            ostrng = 'Enter file name for plant output -->'
            call open(36,iost,1,ostrng,filen)
c
            write (36,1600)
            write (36,1700)
            write (36,1800)
            lunp = 1
          end if
c
c         ... open file for soil output
c
          if (useout('soil').eq.1) then
c
c           ... open file for soil output
c           ... unit = 39, status = iost
c
            ostrng = 'Enter file name for soil output -->'
            call open(39,iost,1,ostrng,filen)
c
            luns = 1
c
            write (39,1900)
          end if
c
        end if
c
c         ... if desired, open file for channel plotting output
c         ... unit = 43, status = iost
c
        if (useout('channel erosion plotting').eq.1) then
          ostrng = 'Enter file name for channel plotting output -->'
          call open(43,iost,1,ostrng,filen)
          write (6,1000)
          write (43,2500)
          write (43,2900)
          ichplt = 1
          lun1 = 1
        end if
c
c       ... if desired, open file for watershed large graphics output
c       ... only if continuous simulation
c       ... unit = 40, status = iost
c
        if (imodel.eq.1) then
          if (useout('watershed large graphics').eq.1) then
            ostrng = 'Enter file name for watershed graphics output -->'
            call open(40,iost,1,ostrng,filen)
c           wgrflg = 1
            bigflg = 1
c
            if (lun1.eq.1) then
              lun1 = 2
            else
              lun1 = 3
            end if
c
          end if
c
        end if
c
c       ... if desired, open files for event, OFE and final summary files
c
c
        ievt = 0
        isum = 0
        ifofe = 0
c
        if (useout('event by event').eq.1) then
c
          ostrng = 'Enter file name for event by event summary -->'
          call open(30,iost,1,ostrng,filen)
          write (30,3200)
c
          ievt = 1
        end if
c
c       if (useout('element').eq.1)then
c
c       ostrng = 'Enter file name for variable OFE line summary -->'
c       call open(33,iost,1,ostrng,filen)
c       write (33,2200)
c       write (33,2300)
c       iofe = 1
c       end if
c
        if (useout('final summary').eq.1) then
c
          ostrng = 'Enter file name for final summary file -->'
          call open(53,iost,1,ostrng,filen)
c
          isum = 1
c
          if (ioutpt.eq.1) then
            write (53,2700)
          else
            write (53,2600)
          end if
c
c         ... open yearly summary temp file
c         ... unit = 52, status = 'unknown'
c
          open (unit=52,file='delete.me',status='unknown')
c
        end if
c
c       ... if desired open winter output file
c
        if (useout('daily winter').eq.1) then
          snoflg = 1
          ostrng = 'Enter file name for daily winter output -->'
          call open(42,iost,1,ostrng,filen)
        end if
c
c       ... if desired open crop yield output file
c
        if (imodel.eq.1) then
c
          if (useout('plant yield').eq.1) then
            yldflg = 1
            ostrng = 'Enter file name for plant yield outputs -->'
            call open(46,iost,1,ostrng,filen)
            write (46,1300) filen
          end if
c
        end if
c
c       ... if impoundment(s) open file for impoundment output
c       ... unit = 51, status = iost
c
        if (impond.ne.0) then
          if (useout('impoundment').eq.1) then
            ostrng = 'Enter file name for impoundment output -->'
            call open(51,iost,1,ostrng,filen)
            ipdout = 1
          end if
        end if
c
c       ... open file for detailed runoff output
c
        open (unit=59,file='runout',status='unknown')
c
      end if
c
      return
 1000 format (20x,' *** NOTE ***',/,
     1    ' Channel erosion plotting disabled at this time')
c
 1100 format (/' Soil loss output options for continuous simulation',/,
     1    ' ---- ---- ------ ------- --- ---------- ----------',/,
     1    ' [1] - Abbreviated annual',/,
     1    '  2  - Detailed annual',/,
     1    '  3  - Abbreviated event by event',/,
     1    '  4  - Detailed event by event',/,
     1    '  5  - Monthly',/,
     1    ' --------------------------------------------------',/,
     1    ' Enter Soil Loss output option [1]',/)
 1200 format (/,' *** WARNING ***',/,' output options are 1-5',/,
     1    ' Abbreviated annual output assumed',/,' *** WARNING ***',/)
 1300 format (' WEPP CROP YIELD OUTPUT FILE -  YIELDS BY CROP',
     1    ' TYPE AND OFE AT EACH HARVEST',/,
     1    '        ************'/,
     1    ' SILAGE VALUES NOT SUMMARIZED',/,
     1    '        ************',//'OUTPUT FILE NAME:',a51)
 1400 format (' DAILY WATER BALANCE',/)
 1500 format (' J=julian day, Y=simulation year',/1x,
     1    'P= precipitation       ',/1x,
     1    'RM=rainfall+irrigation+snowmelt',/1x,
     1    'Q=daily runoff, Ep=plant transpiration',/1x,
     1    'Es=soil evaporation, Dp=deep percolation',/1x,
     1    'watstr=water stress for plant growth  ',
     1    'latqcc=lateral subsurface flow',//1x,78('-'),/2x,'OFE ',
     1    ' J    Y      P      RM     Q      Ep    Es     Dp  UpStrmQ',
     1    '  latqcc Total-Soil  frozwt',/,2x,'#   ',
     1    ' -    -      mm     mm     mm     mm     mm     mm   ',
     1    '  -      mm    Water(mm)   mm'/1x,78('-'),/)
 1600 format (26x,'PLANT AND RESIDUE OUTPUT',/,26x,24('-')/)
 1700 format (26x,'All residue mass units are kg/m**2',/,26x,
     1    'NOTE : output is by Overland Flow Element',/)
 1800 format (' OFE jday  year  canopy -  LAI  cover cover - live',
     1 ' standing',/,'  -    -     - height cover  -  rill',
     1 '  inter -  bio',
     1 ' residue  * * flat residue mass *   buried residue mass',
     1 '  *    dead root mass * * average',/, 
     1 '  -    -     -  (m)   (%)    -   ',
     1 '(%)  (%) type mass  mass  - resd#1 - resd#2 - resd#3     #1',
     1 '     #2     #3',
     1 ' - #1---  - #2---  - #3--- temp(C)')
 1900 format (' Soil properties, daily output'/,78('-'),/,
     1    ' OFE Day   Y   Poros   Keff  Suct',
     1    '    FC     WP    Rough    Ki     Kr    Tauc',/,
     1    '                 %    mm/hr   mm ',
     1    '   mm/mm  mm/mm    mm   adjsmt adjsmt adjsmt',/,78('-')/)
c
 2000 format (i8)
 2100 format(' EVENT OUTPUT',/,
     1'day mo  year Precp  Runoff  IR-det Av-det Mx-det  Point',
     1'  Av-dep Max-dep  Point Sed.Del    ER',/,
     1'--- --  ----  (mm)    (mm)  kg/m^2 kg/m^2 kg/m^2    (m)',
     1'  kg/m^2  kg/m^2    (m)  (kg/m)  ----')
 2300 format(
     1' OFE DD MM YYYY  Precip   Runoff   EffInt PeakRO  EffDur Enrich',
     1'    Keff   Sm  LeafArea  CanHgt  Cancov IntCov  RilCov  LivBio',
     1' DeadBio  Ki    Kr     Tcrit RilWid   SedLeave',/,
     1' na  na na  na     mm       mm     mm/h    mm/h      h    Ratio',
     1'    mm/h   mm    Index    m       %       %       %     Kg/m^2',
     1'  Kg/m^2  na    na      na     m       kg/m')
 2500 format (1x,'Dist. Downslope',1x,'Elevation',1x,'Soil Loss'/,/,
     1    '   (meters) ',1x,' (meters) ',1x,'(kg/m**2)'/)
 2600 format (/,/,/,
     1' IV.  EROSION OUTPUT SUMMARIES FOR SIMULATION',/,
     1'      ------- ------ --------- --- ----------',/,/,
     1'                1         2         3        4        5     ',
     1'      6         7         8         9         10',/,
     1'Year Month     Tot.      Tot.   ---------- Runoff ---------- ',
     1'   Total   Interrill   Total    Sediment   Enrich.',/,
     1'              Precip.   Irrig.    Rain   Snowmelt   Irrig.   ',
     1'   Detach.  Detach.    Depos.     Yield     Ratio',/,
     1'           --------------------(mm)------------------------- ',
     1' ---------(kg/m^2)----------    (kg/m)   (m^2/m^2)',/,110('-'))
 2700 format (5x,
     1    '******************************************************',
     1    '*****',/,5x,
     1    '*                                                     ',
     1    '    *',/,5x,
     1    '* SUMMARY OUTPUT NOT AVAILABLE WHEN EVENT OUTPUT SELEC',
     1    'TED *',/,5x,
     1    '*                                                     ',
     1    '    *',/,5x,
     1    '* SELECT EITHER DETAILED ANNUAL, ABBREVIATED ANNUAL OR',
     1    '    *',/,5x,
     1    '* MONTHLY SOIL LOSS OUTPUT                            ',
     1    '    *',/,5x,
     1    '*                                                     ',
     1    '    *',/,5x,
     1    '******************************************************',
     1    '*****')
 2800 format (a72)
 2900 format (10x,//,
     1    '*******************************************************',/,
     1    '*                                                     *',/,
     1    '* WATERSHED PROFILE OUTPUT NOT AVAILABLE AT THIS TIME *',/,
     1    '*                                                     *',/,
     1    '*******************************************************')
 3000 format (/' Soil loss output options for watershed simulation',/,
     1    ' ---- ---- ------ ------- --- ---------- ----------',/,
     1    ' [0] - Abbreviated annual  average for watershed',/,
     1    '  1  - Abbreviated yearly  average for watershed',/,
     1    '  2  - Abbreviated monthly average for watershed',/,
     1    '  3  - Abbreviated annual  average for SUB-watersheds',/,
     1    '  4  - Abbreviated yearly  average for SUB-watersheds',/,
     1    '  5  - Abbreviated monthly average for SUB-watersheds',/,
     1    ' --------------------------------------------------',/,
     1    ' Enter watershed soil loss output option [0]',/)
 3100 format (/' *** WARNING ***',/,' output options are 0-5',/,
     1    ' Abbreviated annual average output for watershed assumed',
     1    /,' *** WARNING ***',/)
 3200 format (/,
     1    'WATERSHED OUTPUT: DISCHARGE FROM WATERSHED OUTLET',/,
     1    '(Results listed for Runoff Volume > 0.005m^3)',//,
     1    'Day          Precip.    Runoff      Peak       Sediment',/,
     1    '   Month     Depth      Volume      Runoff     Yield',/,
     1    '       Year  (mm)       (m^3)       (m^3/s)    (kg)',/,
     1    '-------------------------------------------------------',/)
      end
