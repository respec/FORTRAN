      subroutine wshpas(year,day,istat)
c
c     + + + PURPOSE + + +
c
c     Controls the reading and writing of hydrologic information
c     from the hillslope (hillslope/watershed version) to the
c     channels and impoundments (watershed version).
c
c     Called from: MAIN, SR CONTIN
c     Author(s): Ascough II, C. Baffaut
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
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
      include 'pmxpts.inc'
      include 'pmxslp.inc'
      include 'pmxtim.inc'
      include 'pmxseg.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer day, istat, year
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     day   - julian date
c     istat - flag for read/write status
c     year  - current climate file simulation year
c
c     + + + COMMON BLOCKS + + +
c
      include 'ccliyr.inc'
      include 'cdata1.inc'
      include 'cdata3.inc'
      include 'cdiss11.inc'
      include 'cdist.inc'
      include 'cefflen.inc'
      include 'cenrpas.inc'
      include 'chydrol.inc'
      include 'cpart.inc'
      include 'csedld.inc'
      include 'cslope.inc'
      include 'cslpopt.inc'
      include 'cstore.inc'
      include 'cstruc.inc'
      include 'cver.inc'
      include 'cwater.inc'
      include 'cwshed.inc'
      include 'cseddet.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real tmpdur(mxhill), tmptcs(mxhill), tmpalp(mxhill),
     1    tmpsed(mxpart,mxhill), tmpdia(mxpart,mxhill),
     1    tmpflw(mxpart,mxhill), oalpha, tcs, tcstmp, tmpdet(mxhill),
     1    tmpdep(mxhill)
c
      integer i, iflag, j, k, l, m, ndays, tmpbyr, tmpnyr
c
      character*8 event(mxhill)
c
c     + + + LOCAL DEFINITIONS + + +
c
c     tmpdur(mxhill)  - storage variable for storm duration
c     tmptcs(mxhill)  - storage variable for overland flow time
c                       of concentration
c     tmpalp(mxhill)  - storage variable for overland flow alpha
c     tmprof(mxhill)  - storage variable for runoff depth |
c     tmpvol(mxhill)  - storage variable for runoff volume| Those are now in
c     tmppkr(mxhill)  - storage variable for peak runoff  | cstore.inc
c     tmpsed(mxpart,mxhill)  - storage variable for sediment
c                              concentration
c     tmpdia(mxpart,mxhill)  - storage variable for particle
c                              size diameter
c     tmpflw(mxpart,mxhill)  - storage variable for fraction of sediment
c                              in each particle class
c     oalpha  -
c     tcs     -
c     tcstmp  -
c     i -
c     iflag   - flag set if runoff event occurs on any hillslope
c     j -
c     k -
c     l -
c     m -
c     ndays   - number of days in a year
c     event(mxhill)   - describes runoff or no runoff event
c     tmpbyr - beginning year in climate file
c     tmpnyr - maximum years value read in for each hillslope
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     table
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      if (istat.eq.1) then
c
c       storing information from hillslope(s) to pass files
c       for later writing to a master watershed pass file
c       (only if there is runoff from the last hillslope ofe
c       for current day)
c
c       write out variables that change with hillslopes but not with
c       time (climate file, maximum simulation years, beginnning year
c       of climate file, hillshillslope area, and particle size
c       diameter) to the top of the hillslope pass file(s)
c
        if ((year.eq.ibyear).and.(day.eq.1)) write (48,1100)
     1      wshcli(ihill), nyear, ibyear, harea(ihill), npart, (
     1      dia(i,nplane),i = 1,npart)
c
        if (runoff(nplane).ge.0.001) then
c
          event(1) = 'EVENT   '
c
CX    Added by Arthur Xu, 05/2000
         if(sbrunf(nplane).gt.0.00)then
           sbrunv(nplane) = sbrunf(nplane) * slplen(nplane)
     1        * fwidth(nplane) 
         else
            sbrunf(nplane) = 0.0
            sbrunv(nplane) = 0.0
          end if
cx      The T1-IF-THEN-ELSE-ENDIF block is added to take lateral 
cx      flow into account    ------ Arthur May,00
cx    End adding
c
c         calculate overland flow time of concentration (hr)
c         and alpha value for use in the watershed version
c
          tcs = (0.0216*((hleng*hmann)**0.75)) / ((((runoff(nplane)/dur)
     1        *3.6e06)**0.25)*(hslop**0.375))
c
c         calculation of overland flow alpha
c
          tcstmp = tcs * 3600.0
c
          oalpha = tcstmp * peakro(nplane) * harea(ihill) /
     1        runvol(nplane)
c
c         check for minimum bounds on oalpha
c
          if (oalpha.lt.(tcs/24.0)) oalpha = tcs / 24.0
c
c         check for maximum bounds on oalpha
c
c         alpha temporarily allowed to be greater than 1.0
c
c         if (oalpha.gt.1.0) oalpha = 1.0
c
cx    Modified by Arthur Xu, 05/2000 Incorporated in by S. Dun 12/2003
        write (48,1000) event(1), year, day, dur, tcs, oalpha,
     1        runoff(nplane), runvol(nplane),sbrunf(nplane), 
     1        sbrunv(nplane), peakro(nplane) *
     1        harea(ihill), tdet(ihill), tdep(ihill), (
     1        sedcon(i,nplane),i = 1,npart), (frcflw(i,nplane),i = 1,
     1        npart)
cx
        else if (sbrunf(nplane).gt.0.00)then        
          sbrunv(nplane)=sbrunf(nplane) * slplen(nplane)
     1        * fwidth(nplane)
          if (sbrunv(nplane).gt.0.01) then
             event(1)= 'SUBEVENT' 
             write(48,1000) event(1), year, day, sbrunf(nplane), 
     1            sbrunv(nplane)
          else
             event(1) = 'NO EVENT'
             write (48,1000) event(1), year, day
          endif
cx    End Modifying.
c
        else
c
          event(1) = 'NO EVENT'
          write (48,1000) event(1), year, day
        end if
c
      end if
c
      if (istat.eq.2) then
c
        do 10 i = 1, nhill
c
c       Bug Fix: Mitch Nelson (KSU) 11-14-2001
c        - If nhill = 47, then j becomes 49, and an error may occur.
c        - Fix: replace next two lines with j = i + 70.
c          if (i.lt.5) j = i
c          if (i.ge.5) j = i + 2
            j = i + 70
c
c         open hillslope pass files
c
c         LAHEY compiler needs status = 'old'
c
         open (unit=j,file=pasfil(i),status='old')
c
c         SALFORD compiler needs status = 'readonly'
c
c          open (unit=j,file=pasfil(i),status='readonly')
c
c         read in stored information from initial lines of hillslope
c         pass files
c
          read (j,1100) wshcli(i), tmpnyr, tmpbyr, harea(i), npart, (
     1        tmpdia(l,i),l = 1,npart)
c
c         check hillslopes to determine maximum simulation years and
c         for consistency in beginning year of climate files
c
          if (i.eq.1) then
            maxyrs = tmpnyr
            iwsbyr = tmpbyr
          end if
c
          if (i.gt.1) then
c
            if (iwsbyr.ne.tmpbyr) then
              write (6,1900) iwsbyr, ibyear
              stop
            end if
c
            if (tmpnyr.lt.maxyrs) maxyrs = tmpnyr
c
          end if
c
   10   continue
c
        write (49,1600) ver, nhill, maxyrs, iwsbyr
c
        do 20 i = 1, nhill
c
c         write out hillslope climate file, particle size
c         diameter (m), and drainage area information (m^2) to
c         permanent watershed pass file
c
          write (49,1700) i, wshcli(i), (tmpdia(l,i),l = 1,npart),
     1        harea(i)
c
   20   continue
c
        write (49,1800)
c
        year = iwsbyr
c
        do 60 i = 1, maxyrs
c
c         check for leap year
c
          if (mod(year,4).ne.0) ndays = 365
          if (mod(year,4).eq.0) ndays = 366
c
c         check for simulation mode (single storm or continuous)
c
          if (imodel.eq.2) ndays = 1
c
          do 50 j = 1, ndays
c
            iflag = 0
c
            do 40 k = 1, nhill
c
c             Bug Fix: Mitch Nelson (KSU) 11-14-2001
c                 adjust as above, replace next two lines with m=k+70
c              if (k.lt.5) m = k
c              if (k.ge.5) m = k + 2
                m = k+70
c
c             read remainder of information in hillslope pass files
c
              read (m,1000) event(k)
c
              if (event(k).eq.'EVENT   ') then
                backspace (m)
cx    Modified by Arthur Xu, 05/2000
                read (m,1000) event(k), year, day, tmpdur(k),
     1              tmptcs(k), tmpalp(k), tmprof(k), tmpvol(k),
     1              tmpsbf(k),tmpsbv(k),
     1              tmppkr(k), tmpdet(k), tmpdep(k), (tmpsed(l,k),l = 1,
     1              npart), (tmpflw(l,k),l = 1,npart)
cx    End Modifying.
                iflag = 1
cx    Added by Arthur Xu, 05/2000
            else if(event(k).eq.'SUBEVENT')then
              backspace(m)
              read(m,1000) event(k), year, day, tmpsbf(k),
     1            tmpsbv(k)
              if (iflag.NE.1) iflag = 2
c
              tmpdur(k) = 0.0
              tmptcs(k) = 0.0
              tmpalp(k) = 0.0
              tmprof(k) = 0.0
              tmpvol(k) = 0.0
              tmppkr(k) = 0.0
              tmpdet(k) = 0.0
              tmpdep(k) = 0.0
c
              do 25 l = 1, npart
                tmpsed(l,k) = 0.0
                tmpflw(l,k) = 0.0
   25         continue
cx    End adding.
              else
                tmpdur(k) = 0.0
                tmptcs(k) = 0.0
                tmpalp(k) = 0.0
                tmprof(k) = 0.0
                tmpvol(k) = 0.0
                tmppkr(k) = 0.0
                tmpdet(k) = 0.0
                tmpdep(k) = 0.0
cx    Added by Arthur Xu, 05/2000
                tmpsbf(k) = 0.0
                tmpsbv(k) = 0.0
cx    End adding
c
                do 30 l = 1, npart
                  tmpsed(l,k) = 0.0
                  tmpflw(l,k) = 0.0
   30           continue
c
              end if
c
   40       continue
c
c           write out hillslope hydrology, sediment concentration,
c           and particle size information to permanent pass file
c
c           no runoff event on current day for ANY HILLSLOPE
c           in the watershed
c
            if (iflag.eq.0) write (49,1200) year, j
c
c           runoff event on current day for ONE OR MORE hillslopes
c           in the watershed
c
c           writing out hillslope storm duration (s), overland flow
c           time of concentration (hr), overland flow alpha
c           parameter (unitless), runoff (m), runoff volume (m^3),
c           peak runoff (m^3/s), total detachment (kg), total
c           deposition (kg), sediment concentration (kg/m^3),
c           and fraction of each particle type (dimensionless)
c
            if (iflag.eq.1) then
              write (49,1300) year, day
              write (49,1400) (tmpdur(k),k = 1,nhill)
              write (49,1400) (tmptcs(k),k = 1,nhill)
              write (49,1400) (tmpalp(k),k = 1,nhill)
              write (49,1400) (tmprof(k),k = 1,nhill)
              write (49,1400) (tmpvol(k),k = 1,nhill)
cx    Added by Arthur Xu, 05/2000
              write (49,1400) (tmpsbf(k), k = 1, nhill)
              write (49,1400) (tmpsbv(k), k = 1, nhill)
cx    End adding
              write (49,1400) (tmppkr(k),k = 1,nhill)
              write (49,1400) (tmpdet(k),k = 1,nhill)
              write (49,1400) (tmpdep(k),k = 1,nhill)
              write (49,1500) ((tmpsed(l,k),l = 1,npart),k = 1,nhill)
              write (49,1500) ((tmpflw(l,k),l = 1,npart),k = 1,nhill)
cx    Added by Arthur Xu, 05/2000
            else if (iflag.eq.2)then
              write(49,1350)year,day
              write(49,1400)(tmpsbf(k),k=1, nhill)
              write(49,1400)(tmpsbv(k),k=1, nhill)
cx    End adding
            end if
c
   50     continue
c
          year = year + 1
c
   60   continue
c
        close (unit=49)
c
c       Bug Fix: Mitch Neilsen (KSU)  11-14-2001 
c                  -- close descriptors 71 to 70+nhill
c        do 70 i = 1, nhill + 2
          do 70 i = 71, 70+nhill
c          if (.not.((i.eq.5).or.(i.eq.6))) then
            close (unit=i)
c          end if
   70   continue
c
      end if
c
      return
c
cx Modified by Arthur Xu 05/2000

 1000 format (a8,2x,2(i5,1x),5x,10(e11.5,1x),2(5x,5(e11.5,1x)))
cx End Modifying.
 1100 format (a,/,i5,5x,i5,/,e10.5,/,i3,4x,5(e11.5,1x))
 1200 format ('NO EVENT',2x,2(i5,1x))
 1300 format ('EVENT   ',2x,2(i5,1x))
cx Added by Arthur Xu 05/2000
 1350 format ('SUBEVENT',2x,2(i5,1x))
cx End adding
 1400 format (75(e11.5,1x))
 1500 format (75(5(e11.5,1x)))
 1600 format ('GENERAL SIMULATION INFORMATION',//,f9.3,
     1    '   --> VERSION NUMBER',/,i6,
     1    '   --> NUMBER OF UNIQUE HILLSLOPES IN WATERSHED',/,i6,
     1    '   --> WATERSHED MAXIMUM SIMULATION TIME (YEARS)',/,i6,
     1    '   --> BEGINNING YEAR OF WATERSHED CLIMATE FILE',//,
     1    'SPECIFIC SIMULATION INFORMATION',//,'HILLSLOPE #',6x,
     1    'CLIMATE FILE',39x,'PARTICLE DIAMETER (M)',44x,'AREA'/)
 1700 format ('HILLSLOPE ',i2,5x,a,5(e11.5,1x),5x,e10.5)
 1800 format (/'BEGIN HILLSLOPE HYDROLOGY AND SEDIMENT INFORMATION'/)
 1900 format (//'*** BEGINNING YEARS OF HILLSLOPE CLIMATE FILES ',
     1    ' ARE NOT THE SAME ***',//,'** SIMULATION STOPPED ***'//)
      end
