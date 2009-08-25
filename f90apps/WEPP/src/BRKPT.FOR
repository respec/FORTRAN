      subroutine brkpt(nbrkpt,mxint)
c******************************************************************
c                                                                 *
c  This subroutine is called from SR STMGET to read in rainfall   *
c  breakpoint data.  The first column of data read in is time     *
c  after midnight in hours(f4.2) and the second column is the     *
c  accumulated rainfall in mm(f6.2).  Irrigation is not an option *
c  when breakpoint input is used.                                 *
c                                                                 *
c******************************************************************
c                                                                 *
c  Arguments                                                      *
c    nbrkpt - number of rainfall breakpoints                      *
c    mxint  - maximum rainfall intensity (m/s)                    *
c                                                                 *
c******************************************************************
c                                                                 *
c  Argument Declarations                                          *
c                                                                 *
      integer nbrkpt
      real mxint
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
      include 'pmxpln.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pmxhil.inc'
c
c******************************************************************
c                                                                 *
c  Common Blocks                                                  *
c                                                                 *
c******************************************************************
c
      include 'cdiss1.inc'
      include 'cdiss3.inc'
      include 'chydrol.inc'
      include 'cupdate.inc'
c     Added by S. Dun, Feb 20, 2008 
c     for record storm start time for breakpoint data
      include 'cclim.inc'
c     End adding
c
c******************************************************************
c
c
c     local variables
c
      real pptcum(100), drain, dtime,pktime
      integer i
c
c     read in break points and convert into seconds and meters
      do 10 i = 1, nbrkpt
        read (13,*,end=20) timem(i), pptcum(i)
        if (i.eq.1) stmstr = timem(1)
        timem(i) = (timem(i)-stmstr) * 3600.
        pptcum(i) = pptcum(i) / 1000.
   10 continue
   20 continue
c
      stmdur = 0.0
      mxint = 0.0
      pktime = 0.0
c     compute rainfall intensity in m/s
      do 30 i = 1, nbrkpt - 1
c
        drain = pptcum(i+1) - pptcum(i)
        if (drain.eq.0.0) then
c
c XXX     Commented out following line - The time value should not be
c         zero according to Jeff Stone - even for periods of zero
c         intensity within a storm.
c         dtime = 0.0
          dtime = timem(i+1) - timem(i)
          intsty(i) = 0.0
        else if (drain.lt.0.0) then
          write (6,1000) day, mon, year
          stop
        else
          dtime = timem(i+1) - timem(i)
          if (dtime.le.0.0) then
            write (6,1100) day, mon, year
            stop
          end if
          intsty(i) = drain / dtime
        end if
c
c       compute storm duration
        stmdur = stmdur + dtime
        dur = stmdur
        if (intsty(i).gt.mxint) then
          mxint = intsty(i)
          pktime = stmdur - dtime / 2.
        end if
c     write (6,*)timem(i)/3600.,intsty(i)*3.6e+06
   30 continue
c
c     compute normalized peak intensity and time to peak
c XXX Commented out following 2 lines because these parameter values
c     are not needed in breakpoint simulations - and now their values
c     will be incorrectly estimated since STMDUR computation above
c     has been corrected.    dcf  9/19/94
c     timep = pktime / stmdur
c     ip = mxint / (pptcum(nbrkpt)/stmdur)
c
c     set rainfall intensity of last interval equal to zero
      intsty(nbrkpt) = 0.0
      prcp = pptcum(nbrkpt)
      p = prcp
c
      return
 1000 format (/1x,'*** Error in breakpoint input data. ***',//1x,
     1    'Date of precipitation : ',i2,1x,i2,1x,i4,/,1x,
     1    'The cumulative precipitation volume decreased. Check',/1x,
     1    'climate input file and restart simulation.')
 1100 format (/1x,'*** Error in breakpoint input data. ***',//1x,
     1    'Date of precipitation : ',i2,1x,i2,1x,i4,/,1x,
     1    'The breakpoint time is less than the preceeding time.',/,
     1    1x,'Check climate input file and restart simulation.')
      end
