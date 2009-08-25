      subroutine stmget(mxint)
c******************************************************************
c                                                                 *
c  This subroutine is called from SR CONTIN to read in storm data *
c  on a storm-by-storm basis. It calls SR SUMRNF.                 *
c                                                                 *
c******************************************************************
c******************************************************************
c                                                                 *
c  Arguments                                                      *
c    ibrkpt - flag for breakpoint rainfall inputs 0 - no brkpts   *
c    mxint - maximum rainfall intensity based on average          *
c            intensity value and Ip value                         *
c                                                                 *
c******************************************************************
c                                                                 *
c  Parameters                                                     *
c      mxplan : maximum number of over land flow elements         *
c      mxtlsq : maximum number of tillage sequences               *
c      mxtill : maximum number of tillage operations per tillage  *
c                                                                 *
c******************************************************************
c
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
c
c******************************************************************
c
c  Common Blocks                                                  *
c                                                                 *
c******************************************************************
c                                                                 *
      include 'cclig.inc'
c        read: iclig
c
      include 'cclim.inc'
c******************************************************************
c
c  clim variables updated                                         *
c     tave,tmnavg,tmxavg                                          *
c                                                                 *
c******************************************************************
c
      include 'cdiss1.inc'
      include 'cdiss3.inc'
c
      include 'chydrol.inc'
c
c******************************************************************
c                                                                 *
c  hydrol variables updated                                       *
c    stmdur,avrint                                                *
c                                                                 *
c******************************************************************
c
      include 'cstmflg.inc'
c
c******************************************************************
c                                                                 *
c   stmflg variables updated                                      *
c      norain                                                     *
c                                                                 *
c******************************************************************
c
      include 'cupdate.inc'
c
      include 'cstmgt.inc'
c
      include 'cstruc.inc'
c
c******************************************************************
c                                                                 *
c   local variables                                               *
c   mxint                                                         *
c                                                                 *
c******************************************************************
c
c     real mxint,totrn
      real mxint
      integer xday(10), xmon(10), xyear(10), nbrkpt, i, jkjk, jkl, kjl
      real xrain(10), xstmdr(10), xtimep(10), xip(10), xtmax(10),
     1    xtmin(10), xradly(10), xvwind(10), xwind(10), xtdpt(10)
c    1    xnint(10)
c
c XXX NOTE - use of global save does not follow WEPP coding
c            conventions.  Only local variables whose values need
c            to be retained should be saved.   dcf  5/20/94
      save
c      data totrn /0.0/
c
c  Variables needed to "lump" disk reads.
c
      mxint = 0.
      stmstr = 0.
c
c     Added if-else to prevent numeric overflow of integer variable
c     IDTOT in very long simulations (model bombed in year 90). dcf 2/94
c
      if(jyear.le.1 .and. sdate.le.1)then
        idtot = 0
      elseif(idtot.lt.6)then
        idtot = idtot + 1
      endif
c
      do 10 kjl =1,nplane
        norain(kjl) = 1
 10   continue
c
      if (ibrkpt.eq.0) then
c       read(13,*) day,mon,year,rain,stmdur,timep,ip,
c       1             tmax,tmin,radly,vwind,wind,tdpt
c
c       Try to speed up disk I/O by doing it in larger chunks.
        nrec = nrec + 1
        if (nrec.ge.11) then
          nrec = 1
          read (13,*,end=20) (xday(i),xmon(i),xyear(i),xrain(i),
     1        xstmdr(i),xtimep(i),xip(i),xtmax(i),xtmin(i),xradly(i),
     1        xvwind(i),xwind(i),xtdpt(i),i = 1,10)
        end if
c
   20   continue
c
c       Most of the variables below are never used in STMGET (except RAIN).
c       They are just stuffed into common.
c
        day = xday(nrec)
        mon = xmon(nrec)
        year = xyear(nrec)
c
c       If a leap year check to see if 29 days in Feb
c
        if (mod(year,4).eq.0) then
          if (mon.eq.3.and.day.eq.1.and.sdate.lt.61) then
            write (6,1000)
            write (6,1100) year
          end if
        end if
c
        do 30 jkjk=1,nplane
          rain(jkjk) = xrain(nrec)
 30     continue
        prcp = xrain(nrec)
        stmdur = xstmdr(nrec)
c
c       CLIGEN versions 2.3 to 3.1 correction factor for storm duration
c       from Risse  7/1/94  dcf
c
c       STILL NEED THIS IF USER IS RUNNING OLD (pre-CLIGEN 4) FILES
        if (imodel.eq.1 .and. iclig.eq.2) stmdur = stmdur * 2.06
c
        if (stmdur .gt. 23.999) stmdur = 23.999
c
        timep = xtimep(nrec)
        ip = xip(nrec)
c
        if (imodel.eq.1 .and. iclig.eq.2)then
c
c         correction factor for CLIGEN versions 2.3 to 3.1
c         From Risse and Nearing.   7/1/94.   dcf
c
          ip=ip*1.44
c
        elseif(iclig.eq.1)then
c
c         correction factor for CLIGEN version 4.0+ to
c         account for effects of steady-state erosion equation
c         assumption.   mn/dcf  3/95
c
          ip=ip*0.70
c
        endif
c
        tmax = xtmax(nrec)
        tmin = xtmin(nrec)
        radly = xradly(nrec)
        vwind = xvwind(nrec)
        wind = xwind(nrec)
        tdpt = xtdpt(nrec)
c
c       ...rain is changed from  mm to meters
c
c       CONVERT RAIN FROM MM TO METERS AND STMDUR FROM HR TO SEC
c
        if (stmdur.gt.0.0) then
c
          do 40 jkjk=1,nplane
            rain(jkjk) = rain(jkjk) * 0.001
 40       continue
c
          prcp = prcp * 0.001
          stmdur = stmdur * 3600.0
          avrint = rain(1) / stmdur
          mxint = ip * avrint
        else
          if(prcp.gt.0.0)then
            write (6,1300)mon,day,year
            prcp = 0.0
            do 45 jkjk=1,nplane
               rain(jkjk) = 0.0
 45         continue
          end if
        end if
c
      else
c       READ IN BREAKPOINT DATA
        read (13,*) day, mon, year, nbrkpt, tmax, tmin, radly, vwind,
     1              wind, tdpt
        if (mod(year,4).eq.0) then
          if (mon.eq.3.and.day.eq.1.and.sdate.lt.61) then
            write (6,1000)
            write (6,1200) year
          end if
        end if
c
c       NINT IS THE NUMBER OF BREAKPOINTS OCCURRING ON THE GIVEN DAY.
c       READ IN BREAKPOINT DATA IF RAINFALL OCCURS
c
        if (nbrkpt.gt.0) then
          call brkpt(nbrkpt,mxint)
c
          do 50 jkjk=1,nplane
            rain(jkjk) = prcp
            ninten(jkjk) = nbrkpt
 50       continue
c
        else
c
          do 60 jkjk=1,nplane
            rain(jkjk) = 0.0
            ninten(jkjk) = 0
 60       continue
c
          prcp = 0.0
        end if
      end if
c
c     ...if continuous simulation then get five day antecedent
c     moisture for plant growth
c******************************************************************
c
      if (imodel.eq.1) then
        if (idtot.lt.6) then
          tmp(sdate) = tmin
          tmpx(sdate) = tmax
          tmnavg = (tmnavg*(sdate-1)+tmp(sdate)) / float(sdate)
          tmxavg = (tmxavg*(sdate-1)+tmpx(sdate)) / float(sdate)
        else
          tmp(6) = tmin
          tmpx(6) = tmax
          tmnavg = 0.
          tmxavg = 0.
          do 70 i = 1, 5
            tmp(i) = tmp(i+1)
            tmpx(i) = tmpx(i+1)
            tmnavg = tmnavg + tmp(i)
            tmxavg = tmxavg + tmpx(i)
   70     continue
          tmnavg = tmnavg / 5.
          tmxavg = tmxavg / 5.
        end if
c
      end if
c
      tave = (tmin+tmax) / 2.
c
      if (prcp.le.0.0) then
        do 80 jkl=1,nplane
          norain(jkl) = 0
 80     continue
        return
      end if
c
c
      if (imodel.eq.1) call sumrnf
c
      return
 1000 format ('')
 1100 format ('********************************************************'
     1    /' *** WARNING ***'/
     1    '*Leap year detected with 28 days in February           *'/
     1    '*please modify your climate file by adding 1 day       *'/
     1    '*to February in year ',i4,'.  Leap year annual values    *'/
     1    '*will not be output.                                   *'/
     1    ,' *** WARNING ***',/
     1    '*                                                      *'/
     1    '********************************************************')
 1200 format ('********************************************************'
     1    /'*                       WARNING!!                      *'/
     1    '*Leap year detected with 28 days in February           *'/
     1    '*please modify your climate file by adding 1 day       *'/
     1    '*to February in year ',i4,'.  Simulation will not be     *'/
     1    '*completed.                                            *'/
     1    ,' *** WARNING ***',/
     1    '*                                                      *'/
     1    '********************************************************')
 1300 format ('********************************************************'
     1    /'*                       WARNING!!                      *'/
     1    '* Input precipitation event detected which has a       *'/
     1    '* positive value for precipitation depth and a ZERO    *'/
     1    '* value for precipitation duration.  WEPP has changed  *'/
     1    '* the value for precipitation depth to 0.0.            *'/
     1    '*                                                      *'/
     1    '* PLEASE CHECK YOUR CLIMATE INPUT FILE FOR THE DATE:   *'/
     1    '*        ',i2,'/',i2,'/',i4,'                           ',
     1    '         *'/
     1    ,' *** WARNING ***',/
     1    '*                                                      *'/
     1    '********************************************************')
      end
