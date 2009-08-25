      subroutine wshdrv
c
c     + + + PURPOSE + + +
c
c     Contains the main simulation subroutines for the continuous
c     water balance WATERSHED model.
c
c     Initializes and reads the input through calls to subroutines
c     INPUT, INIT1, SOIL, AND WATBAL.
c
c     Controls the simulation and programs output through
c     calls to subroutines STMGET, MONOUT, ANNOUT, UPDPAR, IRS,
c     SUMRNF, WATBAL, ROUTE, SEGOUT, STMOUT, HYDOUT, SLOSS, SUMRUN,
c     and ENDOUT.
c
c     Called from:  MAIN
c     Author(s): Jim Ascough II, C. Baffaut
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
      include 'pmxcrp.inc'
      include 'pmxcsg.inc'
      include 'pmxcut.inc'
      include 'pmxelm.inc'
      include 'pmxgrz.inc'
      include 'pmxhil.inc'
      include 'pmximp.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxprt.inc'
      include 'pmxpts.inc'
      include 'pmxres.inc'
      include 'pmxseg.inc'
      include 'pmxslp.inc'
      include 'pmxsrg.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'ptilty.inc'
      include 'pxstep.inc'
      include 'ctemp.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cavloss.inc'
c
c     modify: ioutpt,dsavg(mxplan,100),avsoly,dsmon(mxplan,100),
c             avsolm,iroute,avsolf,dsyear(mxplan,100),avsole
c
      include 'cchpar.inc'
c
      include 'cchpek.inc'
c     read:   ipeak
c
      include 'cchvar.inc'
      include 'cclim.inc'
c
      include 'ccliyr.inc'
c     modify: nyear, ibyear, numyr
c
      include 'ccons.inc'
c
      include 'cconsta.inc'
c     read:   accgav
c
      include 'ccntour.inc'
c     modify: cnfail(mxplan)
c
      include 'ccrpout.inc'
      include 'ccrpvr1.inc'
      include 'ccrpvr2.inc'
      include 'ccrpvr3.inc'
c
      include 'ccrpvr5.inc'
c     modify: isimyr, ncount(mxplan)
c
      include 'ccrpprm.inc'
      include 'ccontcv.inc'
c
      include 'ccover.inc'
c     modify: daydis(mxplan), ntill(mxtlsq)
c
      include 'cdecvar1.inc'
      include 'cdist.inc'
c
      include 'cdiss11.inc'
c     modify: dur,ninten(mxplan)
c
      include 'cefflen.inc'
c     read:   efflen(mxplan)
c
      include 'cends.inc'
c     modify: width(mxplan), rspace(mxplan), qsout, qout
c
      include 'cenrpas.inc'
c     modify: enryy1,enryy2, frcyy1(10), frcyy2(10), enrmm1, enrmm2,
c             frcmm1(10),frcmm2(10), enrmon, frcmon(10), enryr,
c             frcyr(10), enravg, frcavg(10), enrato
c
      include 'cerrid.inc'
c     read: ifile
c
      include 'cffact.inc'
c     read:   frctrl(mxplan)
c
      include 'cflags.inc'
c     read:   snoflg,yldflg
c     modify: bigflg,iflag,idflag
c
      include 'cgully.inc'
c     modify: depa,depb,wida,widb
c
      include 'chydrol.inc'
c     read:   prcp
c     modify: rain(mxplan),runoff(mxelem),peakro(mxelem)
c
      include 'cimpnd.inc'
c     modify: ipond
c
      include 'cinpman1.inc'
      include 'cincon.inc'
      include 'cirfurr.inc'
      include 'cirriga.inc'
c
      include 'cke.inc'
c     modify: rkecum(mxplan), rkine
c
      include 'cnew.inc'
      include 'cnew1.inc'
      include 'coutchn.inc'
      include 'crinpt1.inc'
      include 'crinpt2.inc'
      include 'crinpt3.inc'
      include 'crinpt5.inc'
      include 'crinpt6.inc'
c
      include 'cperen.inc'
c     modify: nnc(mxplan)
c
      include 'cparame.inc'
      include 'cpart.inc'
      include 'ciplot.inc'
      include 'cparva2.inc'
c
      include 'ccrpgro.inc'
c     read:   be,otemp,hi,hia,vdmx,beinp,daymin,daylen,ytn,y4
c
      include 'cprams.inc'
c     modify: norun(mxplan)
c
      include 'cseddet.inc'
      include 'cslinit.inc'
      include 'cslope2.inc'
c
      include 'cslpopt.inc'
c     read:   fwidth(mxplan)
c
      include 'csolva2.inc'
c
      include 'cstore.inc'
c     modify: peakin(mxelem),peakot(mxelem),runvol(mxelem)
c
      include 'cstmflg.inc'
c     modify: jyear, nmon
c
      include 'cstruc.inc'
c     modify: iplane
c
      include 'cstruct.inc'
c     modify: ichan, ipond
c
      include 'csedld.inc'
      include 'csumout.inc'
      include 'csumirr.inc'
      include 'ctillge.inc'
c
      include 'cupdate.inc'
c     modify: indxy(mxplan), sdate, year
c       added by S.Dun
        include 'cimdate.inc'
c       impoundment simulation date
c     end add
c
      include 'cver.inc'
      include 'cwater.inc'
c
      include 'cwint.inc'
c     read:   azm(mxplan)
c
      include 'cwshed.inc'
c     modify: iwsbyr,maxyrs
c
      include 'cyield.inc'
c     read:   sumyld(ntype,mxplan), iyldct(ntype,mxplan), yldflg
c     modify: yldflg
c
cd    Added by S. Dun Feb. 04, 2004
      include 'cxmxint.inc'
      include 'cupsfl.inc'
cd    End adding
c
c     + + + LOCAL VARIABLES + + +
c
      integer nday(12,2), nowcrp(mxplan), ncrop, nmonth,
     1    i, j, lp, ijk, i9, jj, k, switch(mxplan), iiyear, oldind,
     1    iofe, itemp, limyr, lyear, myear, nomelt, nrots,
     1    nsurf, nyears, jstruc, froday(mxplan), l, norflg, nptsc,
     1    minyr, lun1, luns, lunp, lunw, idout, bigcrp(mxplan), mm, nn,
     1    ipart,i1,i2
c
cd      real dslost(mxplan,100), xmxint(mxplan), mxint, effdrr(mxplan),
      real dslost(mxplan,100),mxint, effdrr(mxplan),
     1    rcalsl, avdatm, hday, sumrtm, sumsrm, warain, dsunmp, toplen
c
      character*8 inifil
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Integer Variables:
c
c     nday(12,2)     - a day for a specific month in a specific year
c     nowcrp(mxplan) - current crop number for the current year
c     iuprun(mxplan) - flag to indicate flow onto an OFE from above
c                      ( 1 - yes ;  0  - no )
c     ncrop   - number of different crops in simulation
c     nmonth  -
c     i       -
c     j       -
c     lp      -
c     ijk     -
c     i9      -
c     jj      -
c     k       -
c     switch(mxplan) -
c     iiyear  -
c     oldind  -
c     iofe    -
c     ibrkpt  -
c     itemp   -
c     limyr   -
c     lyear   -
c     myear   -
c     nomelt  -
c     nrots   -
c     nsurf   -
c     nyears  -
c     jstruc  -
c     froday(mxplan) -
c     l       -
c     norflg  -
c     nptsc   -
c     minyr   - minimum of maxyrs and limyr and nyear
c     lun1    - flag indicating that user wants plotting
c               output (1 - yes ;  0 - no )
c     luns    - flag indicating that user wants soil output
c               (1 - yes ;  0 - no )
c     lunp    - flag indicating that user wants plant output
c               (1 - yes ;  0 - no )
c     lunw    - flag indicating that user wants water output
c               (1 - yes ;  0 - no )
c     idout   -
c     bigcrp(mxplan) -
c     mm -
c     nn -
c
c     Real Variables:
c
c
c     dslost(mxplan,100) - net soil loss/gain for each point on a channel
c                          for a storm event
c     xmxint(mxplan) -
c     mxint          - maximum rainfall intensity
c     effdrr(mxplan) -
c     avdatm         -
c     hday           -
c     sumrtm         -
c     sumsrm         -
c     warain         -
c     dsunmp         -
c     toplen         -
c
c     Character Variables:
c
c     inifil -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     annchn     aspect     bighdr     chnero     close
c     cutgrz     decomp     endchn     impday     impeos
c     impmon     impyr      infile     init1      initd
c     input      irrig      monchn     newtil     nowup
c     outfil     prtcmp     rngint     runout     scon
c     soil       stmget     strip      sunmap     tilage
c     watbal     winit      winter     winthd     wshcqi
c     wshimp     wshini     wshinp     wshiqi     wshirs
c     wshred     wshrun
c
c     + + + FUNCTION DECLARATIONS + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
      data nday /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 29,
     1    31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
c
c     + + + END SPECIFICATIONS + + +
c
c
c     initialize plotting variables
c
      lun1 = 0
      luns = 0
      lunp = 0
      lunw = 0
c
c     initialize other local variables
c
      jyear = 0
      lyear = 0
      oldind = 0
c
c     open output files
c
      call outfil(lunp,luns,lunw,lun1)
c
c     open input files
c
      call infile(ncrop,jstruc,nsurf,nrots,nyears,iofe,0)
c
c     convert index values from infile to input parameters
c
      call input(ncrop,jstruc,nsurf,0)
c
c     assign plant/management input parameters
c
      call tilage(nowcrp)
c
c     initialize effective duration of rainfall parameter for
c     furrow irrigation
c
      do 10 i = 1, mxplan
        effdrr(i) = 0.0
        froday(i) = 0
   10 continue
c
c     print heading for winter output file
c
      if (snoflg.eq.1) then
        call winthd(snodpy,frdp,thdp,nplane,2)
      end if
c
      do 20 iplane = 1, nplane
c
c       generate default set of particles for soil if not input
c
        call prtcmp(iplane)
c
        iflag = 0
c
c       initialize slope, water balance and crop/range routines
c
        if (lanuse(iplane).eq.1) then
c
c         initialize residue mass and decomposition parameters
c
          call init1(nowcrp(iplane))
c
        else
c
c         initialize range plant growth parameters
c
          call rngint(ncrop,nowcrp(iplane))
c
        end if
c
        iflag = 0
c
c       SR SCON computes constants for bulk density (bd)
c       and surface roughness (rro)
c
c       compute soil constants
c
        call scon(lanuse(iplane))
c
c       initialize soil parameters
c
        call soil(nowcrp(iplane))
c
c       initialize soil water content
c
        call watbal(lunp,luns,lunw,nowcrp(iplane),elev)
c
c       initialize decomposition indices
c
        call initd(iplane)
c
c       initialize winter parameters
c
        call winit
c
   20 continue
c
c     read in watershed structure and channel parameters
c
      call wshinp(jstruc)
c
c     initialize miscellaneous watershed variables and
c     impoundment routines (if impoundments exsit on watershed)
c
      call wshini
c
      iflag = 1
      isimyr = 1
c
c     start loop calls
c
c     nday  = a day for a specific month in a specific year
c     nyear = number of years in simulation
c
      if (imodel.eq.1) then
c
c       determine maximum number of years available in management file
c
        myear = nyears * nrots
        limyr = min0(numyr,myear)
c
        write (6,1200)
c
c       read in number of years to run the simulation
c
        nyear = 0
        read (5,*,err=30) nyear
c
   30   if (nyear.lt.1) then
          nyear = 1
          write (6,2600)
        end if
c
c       determine limiting datafile, either limyr (minimum of
c       management and climate years), nyear (user requested number
c       of years), or maxyrs (hillslope pass file number of years)
c
        minyr = min0(nyear,limyr,maxyrs)
c
        if ((nyear.gt.limyr).or.(nyear.gt.maxyrs)) then
          nyear = minyr
          write (6,1000) nyear, nyear
        end if
c
c       check channel climate file beginning year with
c       hillslope\watershed pass file beginning year -
c       if different then stop the simulation
c
        if (iwsbyr.ne.ibyear) then
          write (6,1100) ibyear, iwsbyr
          stop
        end if
c
        year = ibyear - 1
        nmonth = 12
c
        if (bigflg.eq.1) call bighdr(nyear,iofe,ver)
c
      else
c
c       single event simulation
c
        nyear = 1
        nmonth = 1
        j = 1
        lp = 1
        nday(j,1) = 1
        ioutpt = 1
      end if
c
c     ******************** yearly loop ********************
c
      do 230 i = 1, nyear
c
        iiyear = i
c
        if (imodel.eq.1) then
          jyear = jyear + 1
          lyear = ibyear + jyear - 1
c
c         check to see if year is a leap year
c
          lp = 1
          if (mod(year+1,4).eq.0) lp = 2
        else
          lyear = ibyear
        end if
c
        sdate = 0
c
        write (6,*) '    '
        if (imodel.eq.1) write (6,*) 'SIMULATION YEAR =', jyear
c
        do 40 iplane = 1, nplane
c
          if (tilseq(nowcrp(iplane),iplane).gt.0) then
            call newtil(tilseq(nowcrp(iplane),iplane),
     1          ntill(tilseq(nowcrp(iplane),iplane)),iplane,oldind)
          end if
c
          if (nycrop(iplane).gt.1) then
c
            if (tilseq(nowcrp(iplane)+1,iplane).gt.0) then
              call nowup(tilseq(nowcrp(iplane)+1,iplane),
     1            ntill(tilseq(nowcrp(iplane)+1,iplane)),
     1            jdplt(nowcrp(iplane)+1,iplane),
     1            jdharv(nowcrp(iplane)+1,iplane),
     1            jdharv(nowcrp(iplane),iplane),
     1            jdstop(nowcrp(iplane),iplane),switch(iplane))
            else
              call nowup(0,0,jdplt(nowcrp(iplane)+1,iplane),
     1            jdharv(nowcrp(iplane)+1,iplane),
     1            jdharv(nowcrp(iplane),iplane),
     1            jdstop(nowcrp(iplane),iplane),switch(iplane))
            end if
c
          else
            switch(iplane) = 0
          end if
   40   continue
c
c       ******************** monthly loop ************************
c
        do 210 j = 1, nmonth
c
          nmon = j
c
c         ******************** daily loop **************************
c
          do 200 k = 1, nday(j,lp)
c
            dur = 0.0
c
c           initialize channel runoff variables
c
            tmpvol(0) = 0.0
            tmprof(0) = 0.0
            tmppkr(0) = 0.0
cx    Added by Arthur. Icorperated in by S. Dun  Dec 05, 2003
            tmpsbv(0) = 0.0
            tmpsbf(0) = 0.0
cx    End adding
c
            do 50 l = 1, nelmt
c
              tmpvol(l) = 0.0
              tmprof(l) = 0.0
              tmppkr(l) = 0.0
cx    Added by Arthur. Icorperated in by S. Dun  Dec 05, 2003
              tmpsbv(l) = 0.0
              tmpsbf(l) = 0.0
              sbrunv(l) = 0.0
              sbrunf(l) = 0.0
cx    End adding
c
              runvol(l) = 0.0
              sbrunv(l) = 0.0
              chnvol(l) = 0.0
              runoff(l) = 0.0
              chnrun(l) = 0.0
              rvolon(l) = 0.0
              roffon(l) = 0.0
              rofave(l) = 0.0
c
              peakin(l) = 0.0
              peakot(l) = 0.0
              peakro(l) = 0.0
c
              rtrans(l) = 0.0
              htcc(l) = 0.0
c
   50       continue
c
            sdate = sdate + 1
c
c           mxint is equal to the maximum rainfall intensity
c           before irs is called there is a test for mxint > ks
c
            call stmget(mxint)
c
            do 70 mm = 1, nplane
c
              do 60 nn = 1, 100
                dslost(mm,nn) = 0.0
   60         continue
c
   70       continue
c
            do 80 l = 1, nplane
              norun(l) = 0
              iuprun(l) = 0
              wmelt(l) = 0
              dpress(l) = 0.0
              bigcrp(l) = itype(nowcrp(l),l)
   80       continue
c
c           do parameter updating and rainfall runoff computations
c
            qsout = 0.0
            qout = 0.0
            idout = 0
c
c           if temperature less than zero then precipitation
c           for this day is snowfall
c
            snow = 0.0
            nomelt = 0
            warain = 0.0
c
            do 90 iplane = 1, nplane
c
              rain(iplane) = prcp
c
              if (snodpy(iplane).le.0.0.and.tmin.ge.0.0.and.
     1            rain(iplane).gt.0.0) warain = rain(iplane)
c
c             set maximum water input rate of each plane/channel
c
              if (norain(iplane).eq.1) then
                xmxint(iplane) = mxint
              else
                xmxint(iplane) = 0.0
              end if
c
c             perform tillage on cropland - update soil and residue
c
              if (imodel.eq.1) then
                if (lanuse(iplane).eq.1) call decomp(nowcrp(iplane))
                  call soil(nowcrp(iplane))
              end if
c
c             calculate aspect of the channel
c
              call aspect(deglat,azm(1),avgslp(iplane))
c
c             reset frost cycle variable when 50 days have passed
c             and no frost has been present in the soil
c
              if (frdp(iplane).le.0.0) then
                if (froday(iplane).lt.51) froday(iplane) =
     1              froday(iplane) + 1
              else
                froday(iplane) = 0
              end if
c
              if (froday(iplane).gt.50) fcycle(iplane) = 0
c
              wmelt(iplane) = 0.0
              avdatm = tave
c
c             call SR WINTER - if snowdepth > 0.0 or minimum
c             temperature < 0.0 or the frost depth > 0.0
c
              if ((snodpy(iplane).gt.0.0).or.(tmin.le.0.0).or.(
     1            frdp(iplane).gt.0.0)) then
                call winter(rain(iplane),snoflg)
                rain(iplane) = 0.0
                norain(iplane) = 0
                wntflg(iplane) = 1
                if (wmelt(iplane).gt.0.0001) nomelt = 1
c               next line has been added so that grna would not be called
c               when it is winter and there is no snowmelt on that plane.
c               (CB/23OCT,95)
                if (wmelt(iplane).le.0.0001) xmxint(iplane) = 0.0
              else
                wntflg(iplane) = 0
c
c               call SR SUNMAP to get the amount of heat unit
c               calories available for melting snow
c
                call sunmap(rcalsl,hday,dsunmp)
              end if
c
              if (norain(iplane).eq.0.and.nomelt.eq.0) ninten(iplane) =
     1            0
c
              tave = avdatm
c
              if (tmin.ge.0.0.and.warain.gt.0.0) then
                rain(iplane) = warain
                norain(iplane) = 1
                wmelt(iplane) = 0.0
                nomelt = 0
              end if
c
   90       continue
c
            if (irsyst.ne.0) call irrig(iiyear,nowcrp,0.0,0.0)
c
            if (lanuse(1).eq.2.and.imodel.eq.1) then
c
              if (sdate.lt.6.and.jyear.eq.1) then
c
                do 100 iplane = 1, nplane
                  r5(iplane,sdate) = rain(iplane) + wmelt(iplane) +
     1                irdept(iplane)
                  am(iplane) = am(iplane) + r5(iplane,sdate) /
     1                rx(sdate)
                  am2(iplane) = am(iplane)
                  if (am(iplane).gt.0.01) am(iplane) = 0.01
  100           continue
c
              else
c
                do 120 iplane = 1, nplane
c
                  r5(iplane,6) = rain(iplane) + wmelt(iplane) +
     1                irdept(iplane)
                  am(iplane) = 0.0
c
                  do 110 i9 = 1, 5
                    r5(iplane,i9) = r5(iplane,i9+1)
                    am(iplane) = am(iplane) + r5(iplane,i9) / rx(i9)
  110             continue
c
                  am2(iplane) = am(iplane)
                  if (am(iplane).gt.0.01) am(iplane) = 0.01
  120           continue
c
              end if
c
              am2(iplane) = am(iplane)
              if (am(iplane).gt.0.01) am(iplane) = 0.01
c
            end if
c
c           check for occurrence of rainfall and/or irrigation
c           (if rainfall norain = 1, if stationary sprinkler irrigation
c           noirr > 0 and irsyst = 1) or for snow melt nomelt > 0
c
            norflg = 0
c
            do 130 l = 1, nplane
              if (norain(l).gt.norflg) norflg = norain(l)
  130       continue
c
c           read hillslope hydrology and erosion information from the
c           watershed master pass file
c
            call wshred
c
c           save hillslope variables in slots to recover them after
c           runoff calculations on each channel element (plane)
c
            do 140 l = 1, nhill
              tmpvol(l) = runvol(l)
              tmprof(l) = runoff(l)
              tmppkr(l) = peakro(l)
cx    Added by Arthur. Icorperated in by S. Dun Dec 05, 2003
              tmpsbf(l) = sbrunf(l)
              tmpsbv(l) = sbrunv(l)
cx    End adding
  140       continue
c
c           reinitialize the runoff volume, runoff depth, and
c           peak runoff variables
c
            do 150 l = 1, nelmt
              runvol(l) = 0.0
              sbrunv(l) = 0.0
              runoff(l) = 0.0
              peakro(l) = 0.0
cx    Added by Arthur. Icorperated in by S. Dun Dec 05, 2003
              sbrunf(l) = 0.0
              sbrunv(l) = 0.0
cx    End adding
  150       continue
c
c           ****** loop through channel flow elements ******
c
            iplane = 0
            ichan = 0
            ipond = 0
c
            do 170 ielmt = nhill + 1, nelmt
c
              if (elmt(ielmt).eq.3) then
c
c               if impoundment element call SR WSHIQI to calculate
c               inflow and SR WSHIMP to control hydraulic and
c               sediment routing
c
c               SR WSHIMP is called even if there is no inflow
c               for infiltration and evaporation calculations as
c               well as sedimentation volume
c
                call wshiqi
c
                if (runvol(ielmt).lt.0.001) idflag = 0
c
c       added by S.Dun
                ddate=sdate
                dyear=year
c       end add
                call wshimp(elev)
c
              end if
c
              if (elmt(ielmt).eq.2) then
c
                iplane = iplane + 1
                ichan = ichan + 1
c
c               if channel element call SR WSHCQI to calculate inflow
c
                call wshcqi
c
c               calculate runoff on the channel element if there is
c               rainfall, snow melt or sprinkler irrigation (furrow
c               irrigation runoff calculations are in SR FURROW)
c
                if ((norflg.eq.1).or.((noirr.ne.0).and.(irsyst.eq.1))
     1              .or.(nomelt.ne.0)) then
c
cd    Modified by S. Dun 03/18/2004
cd                  call wshirs(xmxint,nowcrp,wmelt,ibrkpt,
cd                     iuprun(iplane),effdrr)
                  call wshirs(nowcrp,wmelt,ibrkpt,
     1                effdrr)
cd    End Modifying
                end if
c
c
c               update cumulative kinetic energy
c
                if (wmelt(iplane).gt.0.0) rkine = 0.0
                rkecum(iplane) = rkecum(iplane) + rkine
c
c               update water balance and plant growth
c
                if (imodel.eq.1) then 
                  call watbal(lunp,luns,lunw,nowcrp(iplane),elev)
                  if ((runoff(iplane).gt.0.0).and.(watdur(ielmt).le.0.0)
     1                .and.(ivers.eq.3).and.(dur.le.0.0)) then
                      watdur(ielmt) = 24.0*60.0*60.0
                   endif
                endif
cx    Added by Arthur. Modified by S. Dun Dec 05, 2003
                tmpsbf(ielmt) = sbrunf(iplane)
cx    End adding
c
                if (lanuse(iplane).eq.1) then
c
c                 update tillage index to next tillage date on day of
c                 tillage
c
                  if (tilseq(nowcrp(iplane),iplane).gt.0.and.
     1                indxy(iplane).gt.0) then
                    if (sdate.eq.
     1                  mdate(indxy(iplane),
     1                  tilseq(nowcrp(iplane),iplane))) call newtil(
     1                  tilseq(nowcrp(iplane),iplane),
     1                  ntill(tilseq(nowcrp(iplane),iplane)),iplane,
     1                  oldind)
                  end if
                end if
c
c               check for occurrence of runoff (if runoff norun = 1)
c
              if (norun(iplane).eq.1) then
c
c                 write routing information to screen
c
c                 continuous simulation - write event date to screen
c
                  if (imodel.eq.1) then
                    write (6,1300) iplane, sdate, lyear
                  else
                    write (6,1400) iplane, sdate, lyear
                  end if
c
c                 calculate runoff volume and depth on channel
c                 and put variables in appropriate watershed
c                 element slots
c
                  chnvol(ielmt) = chnrun(iplane) * charea(iplane)
                  chnrun(ielmt) = chnrun(iplane)
c
                  runvol(ielmt) = runoff(iplane) * charea(iplane)
cd    Added by S. Dun 02/08/2004
cd     1                *efflen(iplane)/slplen(iplane)
cd    End adding
                  runoff(ielmt) = runoff(iplane)
cd     Added by S. Dun 02/08/2004 for debug
cd    write (60,*) sdate,runoff(iplane),charea(iplane)
cd    End adding
c
                  tmpvol(ielmt) = runvol(ielmt)
                  tmprof(ielmt) = runoff(ielmt)
c
                  rtrans(ielmt) = rvolon(ielmt) + chnvol(ielmt) -
     1                runvol(ielmt)
c
                  idout = 1
c
                else
c
                  qsout = 0.0
                  qout = 0.0
                  runvol(ielmt) = 0.0
                  sbrunv(ielmt) = 0.0
                  tmpvol(ielmt) = 0.0
                  tmprof(ielmt) = 0.0
                  chnvol(ielmt) = 0.0
c
                  effint(iplane) = 0.0
                  effdrn(iplane) = 0.0
                  peakro(ielmt) = 0.0
                  irdgdx(iplane) = 0.0
                  rtrans(ielmt) = rvolon(ielmt) + chnvol(ielmt) -
     1                runvol(ielmt)
c
                end if
c
                if (imodel.eq.1.and.lanuse(iplane).eq.1) call
     1              cutgrz(nowcrp(iplane),sdate,iplane)
c
                if (sdate.eq.switch(iplane)) then
                  nowcrp(iplane) = nowcrp(iplane) + 1
                  write (6,*) 'NEW CROP #', nowcrp(iplane), ' ON DATE',
     1                sdate
                  nnc(iplane) = 1
                  indxy(iplane) = 0
c
                  if (tilseq(nowcrp(iplane),iplane).gt.0) call newtil(
     1                tilseq(nowcrp(iplane),iplane),
     1                ntill(tilseq(nowcrp(iplane),iplane)),iplane,oldind
     1                )
c
                  if (nowcrp(iplane).lt.nycrop(iplane)) then
c
                    if (tilseq(nowcrp(iplane)+1,iplane).gt.0) then
                      call nowup(tilseq(nowcrp(iplane)+1,iplane),
     1                    ntill(tilseq(nowcrp(iplane)+1,iplane)),
     1                    jdplt(nowcrp(iplane)+1,iplane),
     1                    jdharv(nowcrp(iplane)+1,iplane),
     1                    jdharv(nowcrp(iplane),iplane),
     1                    jdstop(nowcrp(iplane),iplane),switch(iplane))
                    else
                      call nowup(0,0,jdplt(nowcrp(iplane)+1,iplane),
     1                    jdharv(nowcrp(iplane)+1,iplane),
     1                    jdharv(nowcrp(iplane),iplane),
     1                    jdstop(nowcrp(iplane),iplane),switch(iplane))
                    end if
                  end if
                end if
c
c               end of plane (channel) hydrology routines
c
c               calculate runoff and peak runoff on the watershed only
c               if a channel runoff event OR a hillslope runoff event
c               OR both events occur
c
c               calculate peak runoff and call erosion routines
c
                if (runvol(ielmt).gt.0.001) then
                  call wshrun
cd    Following line modified by S. Dun Temperarily 01/14/2004
                  do 155 ipart = 1, npart
                        tgsd(ipart,ielmt) = 0.0
155               continue
c
c     8-8-2007 statement commented out because it prevents subsurface and
c     runoff events from existing on the same day
c                  if ((solwpv.eq.2006).or.(chkflg.ne.2)) then
                    call chnero(ichplt,sdate,nptsc,toplen)
c                  end if
                end if
c
c               when primary tillage occurs on a naturally eroded
c               channel (e.g., ephemeral gully) then reset the
c               channel depth and width to initial values
c
                if (lanuse(iplane).eq.1) then
c
c                 tillage variables indexed to iplane, channel
c                 variables indexed to ichan, however, both
c                 are the same within the channel if statement
c
                  i1 = oldind
                  i2 = tilseq(nowcrp(iplane),iplane)
c
                  if ((i1.gt.0).and.(i2.gt.0)) then
                    if (sdate.eq.mdate(i1,i2)) then
c
c                     reset if primary tillage and naturally eroded
c                     channel
c
                      if ((typtil(i1,i2).eq.1).and.(ishape(ichan).eq.3))
     1                    then
c
                        do 160 l = 1, 11
                          depa(ichan,l) = chnedm(ichan)
                          depb(ichan,l) = chnedm(ichan)
                          wida(ichan,l) = chnwid(ichan)
                          widb(ichan,l) = chnwid(ichan)
  160                   continue
c
                      end if
                    end if
                  end if
                end if
c
              end if
c
  170       continue
c
            do 180 l = 1, nelmt
              runvol(l) = tmpvol(l)
              if (elmt(l).ne.3) runoff(l) = tmprof(l)
              if (l.le.nhill) peakro(l) = tmppkr(l)
  180       continue
c
c           generate detailed runoff output
c
            if (idout.eq.1) call runout
c
            if (imodel.eq.2) then
              call endchn(npart,nraint,traint,nptsc,toplen,nyear,ichplt)
c
c             event summary output for watershed
c
              if (ievt.eq.1) then
                call sedout(iiyear,dslost,isum,ievt,ifofe,lun1,0,
     1            2,nowcrp(nplane))
              end if
              return
            end if
c
c           call daily output impoundment routine
c
            if (npond.gt.0) call impday(iiyear,sdate)
c
c           event summary output for watershed
c
            if (ievt.eq.1) then
              call sedout(iiyear,dslost,isum,ievt,ifofe,lun1,0,2,
     1            nowcrp(nplane))
            end if
c
c           write daily output for large graphical output file
c
            if (lun1.gt.1) then
c
              do 190 iplane = 1, nplane
                bigflg = 0
                call wshout(bigcrp(iplane),iiyear)
  190         continue
c
            end if
c
  200     continue
c
c         monthly impoundment output
c
          if (npond.gt.0) call impmon(nmon)
c
c         monthly erosion output
c
          call monchn(npart,nmon,lyear,nrainm,trainm,nptsc,toplen,ichplt
     1        )
c
  210   continue
c
        do 220 jj = 1, nplane
          indxy(jj) = 0
          switch(jj) = 0
          ncount(jj) = 0
          nnc(jj) = 0
  220   continue
c
c       get next years tillage information
c
        isimyr = 2
c
        if (i.ne.nyear) call tilage(nowcrp)
        if (yldflg.eq.1) write (46,*) '  '
c
c       yearly impoundment output
c
        if (npond.gt.0) call impyr(iiyear)
c
c       yearly erosion output
c
        call annchn(npart,lyear,nrainy,trainy,nptsc,toplen,ichplt)
c
  230 continue
c
c     write minimum and maximum values for large graphical output file
c
      if (lun1.gt.1) then
        bigflg = 1
        call wshout(1,iiyear)
      end if
c
c     end of simulation impoundment output
c
      if (npond.gt.0) call impeos
c
c     end of simulation erosion output
c
      call endchn(npart,nraint,traint,nptsc,toplen,nyear,ichplt)
c
c     write note to unit 45 if no grazing occured
c
      if (rngout.eq.2) then
c
        if (ianflg.lt.1) then
          if (rnganm.eq.2) write (45,1600)
        end if
c
      end if
c
c     final values for creating initial condition scenario
c
      if (ifile.eq.2) then
c
c       write initial condition scenario output file
c
        idshar = sdate - jdharv(nowcrp(1),1)
        itill1 = 0.1
        itill2 = 0.2
        irspac = 1
        itemp = 1
        icanco = cancov(1)
        idaydi = daydis(1)
        ifrdp = frdp(1)
        iinrco = inrcov(1)
        iiresd = itype(nowcrp(1),1)
        irilco = rilcov(1)
        irrini = rrc(1)
        irhini = rh(1)
        isnodp = snodpy(1)
        ithdp = thdp(1)
        iwidth = width(1)
        irmagt = rmagt(1)
        irmogt = rmogt(1,1) + rmogt(2,1) + rmogt(3,1)
        icrypt = cancov(1)
        j = itype(nowcrp(1),1)
c
        write (47,2200)
c
c       write plant name from management file
c
        write (47,2400) crpnam(j)
        write (47,2500) (mancom(i),i = 1,3)
c
c       write plant associated with ofe 1 at end of simulation for
c       cropland residue parameters
c
        write (47,1700) lanuse(1)
c
        if (lanuse(1).eq.1) then
c
          write (47,*) 'WeppWillSet'
          write (47,*) bb(j), bbb(j), beinp(j), btemp(j), cf(j),
     1        crit(j), critvm(j), cuthgt(j), decfct(j), diam(j)
          write (47,*) dlai(j), dropfc(j), extnct(j), fact(j),
     1        flivmx(j), gddmip(j), hi(j), hmax(j)
c
          if (mfocod(j).eq.1) then
            write (47,*) mfocod(j), '  # mfo - <Fragile>'
          else
            write (47,*) mfocod(j), '  # mfo - <NonFragile>'
          end if
c
          write (47,*) oratea(j), orater(j), otemp(j), pltol(j),
     1        pltsp(j), rdmax(j), rsr(j), rtmmax(j), spriod(j),
     1        tmpmax(j)
          write (47,*) tmpmin(j), xmxlai(j), yld(j)
c
        else if (lanuse(1).eq.2) then
c
          write (47,*) aca(j), aleaf(j), ar(j), bbb(j), bugs(j),
     1        cf1(j), cf2(j), cn(j), cold(j), ffp(j)
          write (47,*) gcoeff(j), gdiam(j), ghgt(j), gpop(j), gtemp(j),
     1        hmax(j), plive(j,1), pltol(j), pscday(j), rgcmin(j)
c
          if (scday2(j).gt.365) then
            write (6,*) 'WARNING - calculated value for SCDAY2 = ',
     1          scday2(j)
            write (6,*) ' resetting value for SCDAY2 to 365 in initial',
     1          ' condition scenario file'
            scday2(j) = 365
          end if
c
          write (47,*) root10(j), rootf(j), scday2(j), scoeff(j),
     1        sdiam(j), shgt(j), spop(j), tcoeff(j), tdiam(j),
     1        tempmn(j)
          write (47,*) thgt(j), tpop(j), wood(j)
c
        else
c
c       no other plant types supported at this time
c
c       initial condition creation section
c
        end if
c
        write (47,2300)
        call strip(scefil,inifil)
c
        write (47,2400) inifil
        if (inifil.eq.'        ') inifil = crpnam(j)
c
        write (47,2500) (mancom(i),i = 1,3)
c
        write (47,1700) lanuse(1)
c
        if (lanuse(1).eq.1) then
          write (47,1800) ibd / 1000, icanco, idaydi, idshar, ifrdp,
     1        iinrco
          write (47,1500) itemp, '# iresd'
          write (47,1500) imngm1(j), '# mgmt'
          write (47,1900) irfcum * 1000, irhini, irilco, irrini, irspac
          write (47,1500) rwflag(1), '# rtyp'
          write (47,1900) isnodp, ithdp, itill1, itill2, iwidth
c
c         initial condition values for dead root mass and
c         submerged residue mass
c
          sumrtm = 0.0
          sumsrm = 0.0
c
          do 240 ijk = 1, 3
            sumrtm = sumrtm + rtm(ijk,1)
            sumsrm = sumsrm + smrm(ijk,1)
  240     continue
c
          write (47,2000) sumrtm, sumsrm
c
        else if (lanuse(1).eq.2) then
c
          write (47,2500) frdp(1), pptg(1), rmagt(1), irmogt,
     1        rrough(1), snodpy(1), thdp(1), tillay(1,1), tillay(2,1)
          write (47,2500) rescov(1), bascov(1), rokcov(1), crycov(1),
     1        fresr(1), frokr(1), fbasr(1), fcryr(1), cancov(1)
c
        end if
c
        write (47,2100) nyear
c
      end if
c
c     close all open files
c
      call close(lunp,luns,lunw,lun1)
      close (59)
c
      return
c
 1000 format (' ***WARNING***'/
     1    ' Number of years to simulate can"t be larger than ',i3,/,i3,
     1    ' years used ')
 1100 format (' *** WARNING ***',/,
     1    ' Channel climate file beginning year ',i4,' is not equal',/,
     1    ' to Hillslope\watershed pass file beginning year ',i4,//,
     1    ' *** SIMULATION STOPPED ***')
 1200 format (/,'  Enter number of years to simulate --> ')
 1300 format (1x,'ROUTING runoff event on channel ',i3,' on day ',i4,
     1    ' of year ',i4)
 1400 format (12x,'ROUTING channel ',i3,' on day ',i4,' of ',i4)
 1500 format (i3,a20)
 1600 format (//,'    NO GRAZING OCCURED DURING THIS SIMULATION')
 1700 format (i1,10x,'# land use class')
 1800 format (2(f10.5,1x),f10.1,1x,i10,2(1x,f10.5))
 1900 format (5(f10.5,1x))
 2000 format (2(f10.5,1x))
 2100 format ('#',/,
     1    '####################################################',/,
     1    '# Number of years simulated to create this initial #',/,
     1    '# condition scenarios: ',i3,'                         #',/,
     1    '####################################################',/,'#')
 2200 format (/,'#################',/,'# Plant Section #',/,
     1    '#################',/,'#',/,
     1    '1         # looper; number of plant scenerios')
 2300 format (/,'##############################',/,
     1    '# Initial Conditions Section #',/,
     1    '##############################',/,'#',/,
     1    '1         # looper;',
     1    ' number of initial conditions scenerios')
 2400 format (a)
 2500 format (a60,/,a60,/,a60)
 2600 format ('*** WARNING *** Assuming 1 year simulation')
      end
