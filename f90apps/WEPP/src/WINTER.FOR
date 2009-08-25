      subroutine winter(wrain,snoflg)
c
c     +++PURPOSE+++
c     This subroutine calls the snowmelt, snowdrift, and frost
c     models from wepp, it will perform the waterbalance until
c     the frost is out and computes a new depth and density of
c     of snow on an hourly basis.  The equations found in this
c     program are by Bob Young and George Benoit, etal.
c
c     Authors(s): Cully Hession and Bruce Lucord, USDA-ARS-NCSRL
c                 Revised by John Witte, UofMn WCES @ USDA-ARS-NCSRL
c     Date: 04/01/93
c
c     Verified and tested by Reza Savabi, USDA-ARS, NSERL 317-494-5051
c                  August 1994
c
c     Modified by Dennis Flanagan, 11/26/96 with Savabi changes.
c     Modified by Dennis Flanagan, 1/31/97 with Meyer changes.
c
c     Changes:
c        1) Made major changes to streamline logic -- primarily that
c           used to determine WCASE -- also "*1000's" in write statements
c           -- additionally, various block-if's converted to simple if's,
c           and divides converted to equivalent multiplys.  As a result
c           Winter went from gobbling 53.6% of CPU time, to 36.7.
c        2) Added embedded calling map.
c
c
c     Recoded from V-95.1 by Charles R. Meyer
c     5/2x/96 - 6/04/96
c
c     Changes:
c        3) The cyclomatic complexity v(g) of Winter was 81.  Split out
c           W_CASE as a function and SNOWD, HR_TMP, STMTIM, RES_DP
c           as separate subroutines
c           to give v(g) of 33 (Winter), 11, 17, 12, & 11 respectively.
c        4) Eliminated "goto 111" structure.
c        5) Removed subroutine W_CASE, as it did not affect any other
c           part of the code.  dcf  2/6/97
c
c
c                                     WINTER
c                                       |
c       -----------------------------------------------------------
c       |        |           |             |      |       |       |
c     SET_HC  SUNMAP   The Killer Loop   SNOWD  HR_TMP  STMTIM  RES_DP
c       |                 (do 500)       (add)  (add)   (add)   (add)
c     CALCHC                 |
c                            |
c                      ----------------------------------
c                      |          |           |         |
c                   RADCUR      TMPADJ      FROST      MELT
c                                 |           |
c                               HRTMP         |
c                                             |
c         --------------------------------------------------------------
c         |        |       |       |        |        |        |        |
c       CAQDRY   CAQWET   WMLT   TFWMLT   CAQOUT   AVOID2   CLDFRZ   MLTFDP
c       (gone)   (gone)                            (gone)
c
c
c     +++ARGUMENT DECLARATIONS+++
      real wrain
      integer snoflg
c
c     +++ARGUMENT DEFINITIONS+++
c     wrain  - Daily rainfall amount (m).
c     snoflg - Flag used for winter output.
c
c     +++PARAMETERS+++
      include 'pmxelm.inc'
      include 'pmxcrp.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxslp.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'pmxres.inc'
c
c     +++COMMON BLOCKS+++
      include  'cangie.inc'
c       read:  radlat
      include  'cclim.inc'
c       read:  tave,tmnavg,tmxavg
      include  'ccliyr.inc'
c       read:  ibyear
      include  'ccrpvr5.inc'
c       read:  diam
      include  'ccons.inc'
c       read:  bdcons
      include  'ccrpprm.inc'
c       read:  iresd
      include  'ccover.inc'
c       read:  lanuse
      include  'ccrpout.inc'
c       read:  bd
      include  'chydrol.inc'
c       read:  rain(mxplan)
      include  'cdiss11.inc'
c       read:  dur
      include  'cstruc.inc'
c       read:  iplane
      include  'cupdate.inc'
c       read:  day,year,sdate
      include  'cwint.inc'
c       read:  wmelt(mxplan),drift,snodpt(mxplan),azm,deglat
      include  'cparame.inc'
c       read:  sm(mxplan)
      include  'cslope2.inc'
c       read:  avgslp(mxplan)
      include  'cwater.inc'
c       read:  solthk(mxplan,mxplan)
      include  'ccrpvr1.inc'
c       read:  rmogt
      include  'ctillge.inc'
c       read:  tildep(mxplan,mxplan)
      include  'cstmflg.inc'
c       read:  jyear
c
      include 'cflgfs.inc'
c     fine layer for frost simulation
      include 'cincon.inc'
c
c     +++LOCAL VARIABLES+++
      integer itflag,isflag,temphr
      integer hour,numres,snohrs,rnhrs,i,j
      real    tmpvr1,tmpvr2,tmpvr3,tmpvr4,tmpvr5
      real    tmpvr6,tmpvr7,tmpvr8,tmpvr9,tmpv10
      real    densgy,denh2o,snodep,densgt,halfdy,langmj,
     1        durtn,k(4),randn,wbdtil(mxplan),bdutil(mxplan),
     1        smtill(mxplan),smutil(mxplan)
      real    dgutil,porutl(mxplan),portil(mxplan),estrad
      real    driftf,driftg,fdrft,gdrft,smelt,temp24,totmel
      real    temp,dsunmp,cratio
cd    Added in by S. Dun, July 12, 2007       
      real    pstvML, ngtvML
      integer pstvhr
cd    End adding
c
      save wbdtil,bdutil,porutl
      save temp24
c
c     +++LOCAL DEFINITIONS+++
c     densgy - Temporary variable representing snow density (Kg/m^3).
c     denh2o - Density of water (Kg/m^3).
c     snodep - Temporary variable representing snow depth (m).
c     snodep - Temporary variable representing snow depth after
c              adjustments have been made (m).
c     densgt - Temporty variable representing snow density after
c              adjustments have been made (Kg/m^3).
c     halfdy - Length of day from sunrise until noon (hrs).
c     hour   - Hour counter...used in the looping structure (hr).
c     langmj - Conversion factor used to convert from Langly's to MJ.
c     temp   - Temporary variable.
c     numres - Number of residue layers.
c     snohrs - Number of hours which there is snowfall throughout day.
c     rnhrs  - Number of hours which there is rainfall throughout day.
cc    schrs  - Number of hours which there is snow cover during the day.
c     durtn  - Local variable keeping track of storm duration (h).
c     k      - Variables used in random number generator.
c     randn  - Random number used to calculate time of storm.
c     i      - Seed used for random number generator.
c     hourx  - Previous value of hour
c     smtilx - Previous value of smtill(iplane)
c     smutix - Previous value of smutil(iplane)
c     wbdtix - Previous value of wbdtil(iplane)
c     bdutix - Previous value of bdutil(iplane)
c
c     + + + DATA INITIALIZATIONS + + +
C     data   hourx,smtilx,smutix,wbdtix,bdutix /5 * -1/
c
c     +++END SPECIFICATIONS+++
c
c -- First, we do a little initializing...
c
      denh2o = 1000.0
      langmj = 0.04184
      halfdy = 0.0
      snow   = 0.0
      snohrs = 0
      rnhrs  = 0
      durtn  = 0.0
      i      = sdate
      rans=0.0
      snodpt(iplane)=snodpy(iplane)
      do 5 j=1, 24
        hrmlt(j,iplane) = 0.0
 5    continue
c
c     added following line because variable "temp24" was undefined
c     dcf  8/16/94
c
      if(wntflg(iplane) .eq. 0)temp24 = tmin
c
c     cycle(iplane) = 1
c
c -- We borrowed CLIGEN'S random number gen. for storm time...
      if (ibrkpt.eq.0) then
      k(1)=i
      k(2)=i
      k(3)=i
      k(4)=i
      k(4)=3*k(4)+k(2)
      k(3)=3*k(3)+k(1)
      k(2)=3*k(2)
      k(1)=3*k(1)
      i=k(1)/1000
      k(1)=k(1)-i*1000
      k(2)=k(2)+i
      i=k(2)/100
      k(2)=k(2)-100*i
      k(3)=k(3)+i
      i=k(3)/1000
      k(3)=k(3)-i*1000
      k(4)=k(4)+i
      i=k(4)/100
      k(4)=k(4)-100*i
      randn=((((k(1)*.001+k(2))*.01+k(3))*.001+k(4))*.01) * 24
      wnttim = aint(randn)
      else
cd    Added by S.Dun, Feb 20, 2008
c     to use the storm start time in the beark point data
          wnttim = stmstr
cd    End adding
      endif
      if (wnttim .lt. 1.0) then
        wnttim = 1.0
      endif
c
      smelt = 0.0
c
      if (frdp(iplane) .lt. 0.0001) then
        tfrdp(iplane) = 0.0
        tthawd(iplane) = 0.0
        frdp(iplane)  = 0.0
        thdp(iplane) = 0.0
      endif

c -- Now we must calculate the total residue depth (m).
c
      call res_dp(lanuse(iplane),numres,iplane,resdep(iplane))
c
c
c -- The following converts rad to MJoules from Langley's
c  This will cause problem for ET calculation during
c winter, because rad is converted to mJ in evap subroutine
c
c Note; radmj= radiation in mj/day, radly is ly/day Reza 3/9/94
      temp = radly * langmj
      radmj = temp
c
c -- We now begin our 24 hour loop for each day...
c
      hour = 1
      totmel=0.0
      etm(iplane) = 0.0

      itflag=0
      if((tmax-tmin).le.1.0) itflag=1
 
c
c -- Sunmap calculates the radiation on a sloping surface.
      call sunmap(estrad,halfdy,dsunmp)

  
      hour=0
c
c ------------------------------------------------------------
c     The 500 hourly loop follows
  500 continue
        hour=hour+1
c
c      -- Now we call the RADCUR routine to calc hourly radiation.
c
       call radcur(sdate,hour,radlat,dsunmp,cratio)
c
c      -- Now we know the ratio of daily radiation to hourly radiation.
c      reza, in the next line hradmj is in MJ
c
        call hr_tmp(itflag,jyear,sdate,hour,temp24,radmj,halfdy,
     1              estrad,cratio,rpoth,tmin,tmax,hradmj,hrtemp)
c

        call stmtim(rain(iplane),stmdur,hour,wnttim,rans,
c     1              hrrain(hour),hrsnow(hour),hrtemp,
     2              snodpt(iplane),rnhrs,snohrs,tmin)
c
        snow = snow + hrsnow(hour)

c --------------------------------------------------------------
c    SNOW DRIFTING ROUTINES ARE CURRENTLY NOT ACTIVE - dcf
c
c -- SNOW DRIFT calculations are found in the sndrft.for file.
c -- It's purpose is to calculate the amount of drifting and/or
c -- scouring of new and old snow.
c
c     Set residue type index to be used to determine height of
c     residue for drifting computations.
c
       irtype = iresd(1,iplane)
c reza we are not currently simulating snow drift 8/1/94
c       if((snodpt(iplane).gt.0.0) .or. (hrsnow(hour).gt.0.0)) then
c         call sndrft(irtype,hour,driftf,driftg)
c reza, test for water balance cal. during winter
c         fdrft = driftf
c         gdrft = driftg
c         schrs = schrs + 1
c       else
c         fdrft = 0.0
c         gdrft = 0.0
c       endif
c
        driftf = 0.0
        driftg = 0.0
        fdrft = 0.0
        gdrft = 0.0
c --------------------------------------------------------------
c
c -- Next, we move on to the FROST subroutine.  This routine is
c -- called for every hour that the winter simulation is run.
c -- The main frost driver is located in the Sfrost.for file.
c
        if ((frdp(iplane).ge.0.001) .or. (hrtemp.lt.0.0) .or.
     1                   (slsic(1,1,iplane).ge.0.00001)) then
             call frostN(hour)
         endif
c
c -- CHECK... Still have to move infiltration capacity calculations
c --          out of frost to this point.
c
c --------------------------
c
        call snowd(iresd(1,iplane),denh2o,iplane,driftf,driftg,
     1             snodep,densgy,densgt,smelt,hour)
c
c -- Set up the hourly arrays for thaw and melt.  These will be
c -- passed back to the main WEPP model to account for erosion
c -- possibly taking place.
c
         hrmlt(hour,iplane)  = wmelt(iplane)
cd        wmelt(iplane) = wmelt(iplane) + hrrain(hour)
       
c
c     Write hourly winter output
        if(snoflg.eq.1) then
          tmpvr2=hrsnow(hour)*1000.0
          tmpvr3=hrrain(hour)*1000.0
          tmpvr4=gdrft*1000.0
          tmpvr5=fdrft*1000.0
          tmpvr6=wmelt(iplane)*1000.0
          tmpvr7=snodpt(iplane)*1000.0
          tmpvr8=frdp(iplane)*1000.0
          tmpvr9=thdp(iplane)*1000.0
          tmpv10 = frsttk(iplane)*1000.0
          write(42,'(1x,2(i3,1x),i5,1x,11(f6.1,1x),i3,1x,i3)')
cd     1      sdate,hour,year-ibyear+1,tmpvr2,tmpvr3,tmpvr4,tmpvr5,tmpvr6,
     1      sdate,hour,year,tmpvr2,tmpvr3,tmpvr4,tmpvr5,tmpvr6,
     2      tmpvr7,densgt,tmpvr8,tmpvr9,tmpv10,resdep(iplane)*1000,
     3      fcycle(iplane),iplane
        endif

cd        totmel = totmel + wmelt(iplane)

        if(hour.eq.24) temp24=hrtemp

c
      if(hour.lt.24) goto 500
c
c     END OF 500 HOURLY LOOP
c ------------------------------------------------------------
cd       Added by S. Dun, July 12, 2007
c       We should have some negative melt vlaues form the hourly melt calculation
c       due to we are using a daily model in an hourly basis. 
c       For snow depth calculation, whever positive melt happens, snow depth is adjusted.
c       The added codes is to correct these problems.
c
       pstvML = 0.0
       pstvhr = 0
       ngtvML = 0.0
       Do 20 hour = 1, 24
c
           if(hrmlt(hour,iplane).gt.0) then
             pstvML = pstvML + hrmlt(hour,iplane)
                 pstvhr = pstvhr + 1
           else if(hrmlt(hour,iplane).lt.0) then
                 ngtvML = ngtvML + hrmlt(hour,iplane)
           endif
c
20     continue
c
       if (pstvML .le. ngtvML) then
              wmelt(iplane) = 0
           do 30 hour = 1, 24
              hrmlt(hour,iplane) = 0.0
30         continue
c             
c        Prevent crash with divide by 0 5-5-2008 jrf
         if (densgt.ne.0.0) then 
            snodpt(iplane) = snodpt(iplane) + pstvML*1000/densgt
         endif
       else
c
           do 40 hour = 1, 24
              hrmlt(hour,iplane) = hrmlt(hour,iplane)*(1-ngtvML/pstvML)
40         continue
c        Prevent crash with divide by 0 5-5-2008 jrf
         if (densgt.ne.0.0) then 
           snodpt(iplane) = snodpt(iplane) + ngtvML*1000/densgt
         endif
       endif
c
       do 50 hour = 1, 24
           if(hrrain(hour).gt.0) then
               hrmlt(hour,iplane)= hrmlt(hour,iplane) + hrrain(hour)              
           endif
           totmel = totmel + hrmlt(hour,iplane)
50     continue

cd    End adding

      wmelt(iplane)=totmel
c
      return
      end
