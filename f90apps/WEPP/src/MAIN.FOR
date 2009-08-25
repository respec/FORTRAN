      program main
c
c     + + + PURPOSE + + +
c
c     Program main coordinates, through calls to the corresponding
c     subroutines, the writing of the program's heading (SR HEADER),
c     the opening of the output data files (SR FILES), the reading
c     and calculation of hillslope characteristics (SR PROFILE),
c     the initialization of variables, and hillslope and watershed
c     simulation runs (SRS CONTIN and WSHDRV).
c
c     Called from: Program Driver
c     Author(s): Livingston, Ascough II
c     Reference in User Guide:
c
c     Version: This module not yet recoded.
c     Date recoded:
c     Recoded by:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxhil.inc'
      include 'pmximp.inc'
      include 'pmxpln.inc'
      include 'pntype.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cerrid.inc'
c
      include 'cimpnd.inc'
c     modify: impond
c
      include 'cstruc.inc'
c     read: ivers,nhill
c     modify: imodel
c
      include 'cunicon.inc'
c     modify units
c
      include 'cver.inc'
c
      include 'cwshed.inc'
c     modify: pasfil(mxhill),watfil
c
c     + + + LOCAL VARIABLES + + +
c
      integer dum1, dum2, iniflg, irun, iwpass, useout, wflag,
     1    fexist
c
      character*1 ans
      character*51 filen
      character*65 ostrng
c
c     + + + LOCAL DEFINITIONS + + +
c
c     ans    - character variable to store user responses
c     dum1   - dummy year variable (used in SR MAIN for call
c              to SR WSHPAS)
c     dum2   - dummy day of year variable (used in SR MAIN for
c              call to SR WSHPAS)
c     filen  - local file name to be opened
c     fexist - flag for opening hillslope pass file for hillslope/
c              watershed version
c     iniflg - SR INIDAT flag for accessing version information
c     irun   - runtime error flag
c     iwpass - hillslope pass file creation flag
c     ostrng - string passed to subroutine open
c     useout - integer function for yes/no answers
c     wflag  - version dependent flag for opening watershed pass file
c     units  - 1=english (default)
c              0=metric
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     contin
c     inidat
c     open
c     useout
c     wshdrv
c     wshpas
c
c     + + + DATA INITIALIZATIONS + + +
c
      data dum1, dum2, wflag /3 * 0/
c
c     + + + END SPECIFICATIONS + + +
c
      iniflg = 1
      call inidat(iniflg)
      iniflg = 2
c
      write (6,2600) mxplan, ver, vermon, veryr
c
c     metric or english units for abbreviated output file
c     m = M ==>   0=metric
c     e = E ==>   1=english
c     default to metric units
c
      write (6,2700)

      read (5,1000,err=9) ans
      if (ans.eq.'M') ans = 'm'
      if (ans.eq.'E') ans = 'e'

 9    if (ans.eq.'m')then
        units=0
        write(6,2725)
      elseif (ans.eq.'e') then
        units=1
        write(6,2750)
      else
        write (6,2775)
        units=0
      end if
c
      ibomb = 1
c
c     ... irun=0 for successful wepp run without warnings
c     ... irun=1 for successful wepp run with warnings
c     ... irun=2 for fatal error program stopped
c
      irun = 0
c
c     ... drop out of wepp upon keyboard entry error or loop
c
      write (6,1100)
      read (5,1000,err=10) ans
c
      if (ans.eq.'n') ans = 'N'
      if (ans.eq.'y') ans = 'Y'
c
   10 if (ans.ne.'N'.and.ans.ne.'Y') then
        write (6,1200)
        irun = 1
        ans = 'Y'
        iost = 3
        go to 20
      end if
c
      if (ans.ne.'Y') then
c
c       ... do not exit upon invalid input or write over output files
c
        ibomb = 2
        iost = 1
      else
c
c       ... exit upon invalid input and write over output files
c
        iost = 3
      end if
c
c     ... choose continuous simulation or single event option
c
   20 write (6,1300)
c
      read (5,1500,err=30) imodel
c
   30 if (imodel.lt.1.or.imodel.gt.2) then
        write (6,1600)
        imodel = 1
        irun = 1
      end if
c
c     ... choose version option
c
      write (6,1400)
      read (5,1500,err=40) ivers
c
   40 if (ivers.lt.1.or.ivers.gt.3) then
        write (6,1700)
        ivers = 1
        irun = 1
      end if
c
      iwpass = 0
c
      if (ivers.eq.1) then
c
c       ... hillslope profile version
c
        nhill = 1
c
        iwpass = 0
c
        if (useout('hillslope pass file').eq.1) then
          ostrng = 'Enter name of hillslope pass file -->'
          call open(48,3,1,ostrng,filen)
          iwpass = 1
        end if
c
      else if (ivers.gt.1) then
c
c       ... open master watershed pass file
c
        if (ivers.eq.2) then
          ostrng = 'Enter name of master watershed pass file -->'
          call open(49,3,1,ostrng,filen)
          wflag = 1
          watfil = filen
c
c         ... blank line for shell interface
c
        end if
c
c       ... read the number of hillslopes for
c       ... hillslope/watershed or watershed versions
c
   50   write (6,1800)
        read (5,1500,err=60) nhill
c
   60   if (nhill.lt.1.or.nhill.gt.mxhill) then
          write (6,1900) mxhill
          irun = 1
          go to 50
        end if
c
        if (ivers.eq.2) then
          write (6,2700)
          read (5,*)
        end if
c
c       ... commented out so WEPP can write over output files
c       ... if necessary
c
c       if (iost.eq.3) then
c         iost = 1
c         ibomb = 2
c       end if
c
      end if
c
c     ... if hillslope or hillslope/watershed version call SR CONTIN
c
      if (ivers.ne.3) then
        do 90 ihill = 1, nhill
c
          if (ivers.eq.2) then
c
            write (6,3300)
            read (5,1000,err=70) ans
c
            if (ans.eq.'n') ans = 'N'
            if (ans.eq.'y') ans = 'Y'
c
   70       if (ans.ne.'N'.and.ans.ne.'Y') then
              write (6,3400)
              irun = 1
              ans = 'N'
            end if
c
            if (ans.eq.'N') fexist = 0
            if (ans.eq.'Y') fexist = 1
c
c           ... open hillslope pass file
c
c           ... if opening existing pass file then do not perform the
c           ... hillslope simulation; if creating  new pass file then
c           ... open and perform hillslope simulation
c
            if (fexist.eq.0) then
              ostrng = 'Enter name of new hillslope pass file -->'
              call open(48,3,1,ostrng,filen)
            end if
c
            if (fexist.eq.1) then
              ostrng = 'Enter name of existing hillslope pass file -->'
              call open(dum1,dum1,3,ostrng,filen)
			  close(unit=dum1)
            end if
c
            pasfil(ihill) = filen
            if (fexist.eq.1) go to 80
c
          end if
c
c         ... initialize common block variables (either for
c         ... hillslope version or for each hillslope in the
c         ... hillslope/watershed version)
c
          call inidat(iniflg)
c
c         ... begin simulation
c
          call contin(iwpass)
c
   80     if (ivers.eq.1) then
            write (6,3000)
c
          else
c
            if (ihill.ne.nhill) then
              write (6,2800) ihill
c
c             ... return if keyboard entry or space in data file
c             ... blank line used by shell interface
c
              write (6,2700)
              read (5,*)
            else
c
              write (6,2900)
c
c             ... write out pass file information for watershed version
c
              call wshpas(dum1,dum2,2)
              read (5,*)
c
c             ... reset ivers for watershed simulation and print
c             ... out run status
c
              ivers = 3
              if (irun.eq.1) write (6,2100)
              if (irun.ne.1) write (6,2400)
            end if
          end if
c
   90   continue
      end if
c
      if (ivers.eq.3) then
c
c       ... open master watershed pass file
c
        if (wflag.eq.0) then
          ostrng = 'Enter name of master watershed pass file -->'
          call open(49,2,1,ostrng,filen)
          watfil = filen
        else
          open (unit=49,file=watfil,status='old')
        end if
c
c       ... re-initialize data
c
        call inidat(iniflg)
c
c       ... ask if impoundments will be modeled
c
        write (6,3100)
        read (5,1000,err=100) ans
c
        if (ans.eq.'n') ans = 'N'
        if (ans.eq.'y') ans = 'Y'
c
  100   if (ans.ne.'N'.and.ans.ne.'Y') then
          write (6,3200)
          irun = 1
          ans = 'N'
        end if
c
        if (ans.eq.'N') impond = 0
        if (ans.eq.'Y') impond = 1
c
c       call watershed driver
c
        call wshdrv
c
      end if
c
      if (irun.eq.1) then
        if (ivers.eq.1) write (6,2000)
        if (ivers.eq.3) write (6,2200)
      else
        if (ivers.eq.1) write (6,2300)
        if (ivers.eq.3) write (6,2500)
      end if
c
 1000 format (a1)
 1100 format (' Do you wish to drop out of the model upon invalid',
     1    ' input and',/,
     1    ' write over identical output file names? (batch mode)',//,
     1    ' Enter N to run watershed option (not a SHELL option).',//,
     1    ' Enter Y or N --> [Y]')
 1200 format (' *** NOTE ***',/,' Y OR N FOR WEPP EXIT MODE',/,
     1    ' Y assumed',/,' *** NOTE ***',/)
 1300 format (/,' Continuous or single event option',/,
     1    ' ---------------------------------',/,
     1    ' 1 - continuous simulation',/,' 2 - single storm',//,
     1    ' Enter option (1 or 2) --> [1]')
 1400 format (//,' Model version option',/,' --------------------'/,
     1    ' 1 - hillslope version (single hillslope only)',/,
     1    ' 2 - hillslope/watershed version',/,
     1    '     (multiple hillslopes, channels, and impoundments)',/,
     1    ' 3 - watershed version (channels and impoundments)',/
     1    '     (option 2 or 3 must be previously selected)',//,
     1    ' Enter option (1,2 or 3) --> [1] ')
 1500 format (i8)
 1600 format (//,' *** NOTE ***',/,' EVENT OPTIONS ARE 1 OR 2***',/,
     1    ' Continuous simulation assumed',/,' *** NOTE ***',/)
 1700 format (//,' *** NOTE ***',/,' VERSION OPTIONS ARE 1,2 OR 3***',
     1    /,' Option 1 assumed',/,' *** NOTE ***',/)
 1800 format (/,' Multiple hillslopes or hillslope/watershed',
     1    ' version',/,1x,50('-'),/,
     1    ' Enter the number of hillslopes --> ')
 1900 format (//,' *** NOTE ***',/,' Number of hillslopes must',/,
     1    ' be between one and ', i4, ' - please enter again'/
     1    ,' *** NOTE ***',/)
 2000 format (/' WEPP COMPLETED HILLSLOPE SIMULATION WITH NOTES,',
     1    ' CAUTIONS, OR WARNINGS',/)
 2100 format (/' WEPP COMPLETED HILLSLOPE/WATERSHED SIMULATION',
     1    ' WITH NOTES, CAUTIONS OR WARNINGS',/)
 2200 format (/' WEPP COMPLETED WATERSHED SIMULATION WITH NOTES,',
     1       ' CAUTIONS OR WARNINGS',/)
 2300 format (/' WEPP COMPLETED HILLSLOPE SIMULATION SUCCESSFULLY')
 2400 format (/' WEPP COMPLETED HILLSLOPE/WATERSHED SIMULATION',
     1    ' SUCCESSFULLY'/)
 2500 format (/' WEPP COMPLETED WATERSHED SIMULATION SUCCESSFULLY')
 2600 format (////,2x,68('*'),/,2x,'*',66x,'*',/,2x,'*',66x,'*',/,2x,
     1    '*',14x,'USDA - WATER EROSION PREDICTION',' PROJECT',13x,'*',
     1    /,2x,'*',66x,'*',/,2x,'*',20x,'HILLSLOPE PROFILE - WATERSHED',
     1    17x,'*'/2x,'*',27x,'EROSION MODEL',26x,'*'/2x,'*',66x,'*',/,2
     1    x,'*',20x,'CONTINUOUS SIMULATION',' AND',21x,'*',/,2x,'*',23x,
     1    'SINGLE STORM OPTIONS',23x,'*',/,2x,'*',
     1    17x,'MAXIMUM WATERSHED CHANNELS: ',i4,17x,'*',/,2x,'*',
     1    66x,'*',/,2x,'*',66x,
     1    '*',/,2x,'*',25x,' VERSION',f9.3,24x,'*',/,2x,'*',
     1    66x,'*',/,2x,'*',23x,a15,1x,i4,23x,'*',/,2x,'*',66x,'*',/,2x,
     1    '*',66x,'*',/,2x,'*',66x,'*',/,2x,68('*'),/)
 2700 format (//' Do you wish to output [e]nglish or [m]etric units for'
     1    ,/,' the average annual summary output file?',//,
     1    ' Enter [e] or [m] --> [m]etric default  ')
 2725 format(/,' Metric units selected',//)
 2750 format(/,' English units selected',//)
 2775 format (//,' *** NOTE ***',/,' SUMMARY UNIT OPTIONS ARE e OR m ',
     1    '***',/,' Metric units assumed',/,' *** NOTE ***',/)
 2800 format (/,' Hillslope ',i2,' SUCCESSFUL simulation'/)
 2900 format (/' WRITING HILLSLOPE --> WATERSHED PASS FILE',
     1    ' INFORMATION'/)
 3000 format (/,' Hillslope SUCCESSFUL simulation - ','returning to DOS'
     1    /)
 3100 format (//' Do you wish to model impoundments on the ',
     1    'watershed?',//,' Enter Y or N --> [N]')
 3200 format (' *** NOTE ***',/,' Y OR N FOR IMPOUNDMENT MODELING',/,
     1    ' N assumed',/,' *** NOTE ***',/)
 3300 format (//' Do you wish to use an existing hillslope pass ',
     1    'file?',//,' Enter Y or N --> [N]',/,' *** NOTE ***',/)
 3400 format (' *** NOTE ***',/,' Y OR N FOR EXISTING PASS FILE',/,
     1    ' N assumed',/,' *** NOTE ***',/)
      end
