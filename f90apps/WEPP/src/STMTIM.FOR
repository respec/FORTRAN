      subroutine stmtim(rain,stmdur,hour,wnttim,rans,
     1                  snodpt,rnhrs,snohrs, daytmin)
c     +++ PURPOSE +++
c     This subroutine handles winter storm time and duration calculations.
c     It was originally incorporated into WINTER, but was split out
c     by Charles R. Meyer 6/6/96 to reduce code complexity.
c
c     +++ ARGUMENT DECLARATIONS +++
      integer hour,rnhrs,snohrs
      real    rain,stmdur,wnttim,snodpt
      real    rans, daytmin
c
c     +++ ARGUMENT DEFINITIONS +++
c
c     +++PARAMETERS+++
      include 'pmxpln.inc'
      include 'pmxhil.inc'
c
c     +++COMMON BLOCKS+++
      include  'cclim.inc'
c
c     +++LOCAL VARIABLES+++
      real denwat,tmpvr3
      integer wntdur
c
c     +++LOCAL DEFINITIONS+++
c     denwat - density of water (kg/m^3).
c
c     +++ END SPECIFICATIONS +++
c
c
c -- Now, we calculate hourly precipitation amount and type.
c -- We start with rain.
c
      denwat = 1000.0
      hrrain(hour) = 0.0
      hrsnow(hour) = 0.0
c
c --------------------------
c -- Winter storm time and duration calculations...
c --------------------------
c
      if (rain .gt. 0.0001) then
c
c -- First, we need to round the duration to the nearest hour.
c
c
C       wntdur = aint(stmdur/3600)
        tmpvr3 = stmdur * 0.00027778
        wntdur = aint(tmpvr3)
c
C       if (((stmdur/3600) - wntdur) .ge. 0.5) wntdur = wntdur + 1.0
        if ((tmpvr3 - wntdur) .ge. 0.5) wntdur = wntdur + 1.0
c
c -- What if the storm is only 1/2 hour long?
c
        if (wntdur .lt. 0.0001) wntdur = 1.0
        if ((wnttim + wntdur) .gt. 24.0) wnttim = 24.0 - wntdur
c
c       condtion L2
        if ((hour.ge.wnttim) .and. (hour.lt.(wnttim+wntdur))) then
c
c       condition L3
        if (vercli .ne. 0) then
c       No observed dew point temperature
c
c         density of new falling snow
          densnf = 100.0
c       
          if (hrtemp .gt. 0.0) then
c -- In this case, we convert units of precip to meters.
c
            hrrain(hour) = rain / wntdur
            hrsnow(hour) = 0.0
c           if (snodpt .gt. 0.001)then
c            krs=krs+1
c            ras=ras+hrrain
c           else
c            krns=krns+1
c            rans=rans+hrrain
c           endif
            if (snodpt .le. 0.001) rans = rans + hrrain(hour)
          else
c
c -- Otherwise, we're talking snowfall...
c -- Note that 10.0 is a conversion factor from rain to snow.
            hrsnow(hour) = rain / wntdur * 10.0
            hrrain(hour) = 0.0
c           if (snodpt .gt. 0.001)then
c            kss=kss+1
c            snsn=snsn+hrsnow
c           else
c            ksns=ksns+1
c            sns=sns+hrsnow
c           endif
          endif
c
c       condtion L3
        else
c       observed dew point temperature
c       Snow or rain partition fallows Link and Marks (1999) using dew point temperature
c       Point simulation of seasonal snow cover dynamics beneath boreal forest canopies
c       J. Geophysical Reserch, Vol. 104, 27841-27857
c
           if(tdpt .gt. 0.5) then
c          warm rain
               hrrain(hour) = rain / wntdur
               hrsnow(hour) = 0.0
               if (snodpt .le. 0.001) rans = rans + hrrain(hour)
c
            else
c           snow
               if(tdpt .gt. 0.0) then
                  densnf = 200.
               elseif (tdpt .gt. -5.0) then
                  densnf = 100.
               elseif (tdpt .gt. -10.0) then
                  densnf = 70.
               else
                  densnf = 60.
               endif
               hrsnow(hour) = rain / wntdur * denwat/densnf
               hrrain(hour) = 0.0
            endif    
c       condition L3
        endif        
        endif
c       condition L2
c
        if (hrrain(hour) .gt. 0.00001) rnhrs = rnhrs + 1
        if (hrsnow(hour) .gt. 0.00001) snohrs = snohrs + 1
c
      endif
c
      return
      end
