      subroutine contin(iwpass)
c
c     + + + PURPOSE + + +
c
c     Contains the main simulation subroutines for the continuous
c     water balance model.
c
c     Initializes and reads the input through calls to subroutines
c     INPUT, INIT1, SOIL, AND WATBAL.
c
c     Controls the simulation and programs output through
c     calls to subroutines STMGET, MONOUT, ANNOUT, UPDPAR, IRS,
c     SUMRNF, WATBAL, ROUTE, SEGOUT, STMOUT, HYDOUT, SLOSS, SUMRUN,
c     and ENDOUT.
c
c     Called from: PROGRAM MAIN
c     Author(s): Flanagan, Ferris, Stone, Ascough, Livingston, Others
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
      include 'pmxgrz.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxprt.inc'
      include 'pmxpts.inc'
      include 'pmxelm.inc'
      include 'pmxres.inc'
      include 'pmxseg.inc'
      include 'pmxslp.inc'
      include 'pmxsrg.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'ptilty.inc'
      include 'pxstep.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer iwpass
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     iwpass - hillslope pass file creation flag
c
c     + + + COMMON BLOCKS + + +
c
      include 'cavloss.inc'
c
c     modify: ioutpt,dsavg(mxplan,100),avsoly,dsmon(mxplan,100),
c             avsolm,iroute,avsolf,dsyear(mxplan,100),avsole
c
      include 'cchpar.inc'
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
c     modify: ninten(mxplan)
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
c     modify: bigflg,iflag
c
      include 'cgully.inc'
c     modify: depa,depb,wida,widb
c
      include 'chydrol.inc'
c     read:   prcp
c     modify: runoff(mxelem),rain(mxplan)
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
c     modify: hmann
c
      include 'csolva2.inc'
c
      include 'cstore.inc'
c     modify: runvol(mxelem)
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
c
      include 'cunicon.inc'
c     read units flag
c     pass outopt flag
      include 'cver.inc'
      include 'cwater.inc'
c
      include 'cwint.inc'
c     read:   azm(mxplan)
c
      include 'cyield.inc'
c     read:   sumyld(ntype,mxplan), iyldct(ntype,mxplan), yldflg
c     modify: yldflg
c
cd    Added by S. Dun June 07,2003
      include 'coutfg.inc'
c    read: lunp,luns,lunw
c
      include 'cxmxint.inc'
c    maximum rainfall intensity.
      include 'cupsfl.inc'
      include 'ccntfg.inc'
cd    End adding
c
c     + + + LOCAL VARIABLES + + +
c
      integer lun1,nday(12,2),nowcrp(mxplan),
     1    ncrop,nmonth,i,j,lp,mm,nn,iyear,kk,idout,ijk,i9,iirout,
     1    jj,k,ipass,passby,switch(mxplan),noout,iiyear,oldind,iofe,
     1    bigcrp(mxplan),iii,ilay,ioutfl,itemp,jjj,limyr,lyear,
     1    myear,nomelt,nowres,nrots,nsurf,nyears,jstruc,froday(mxplan),
     1    l,jjkkll,norflg
      real dslost(mxplan,100),mxint,runmax,pkrmax,pkefdn,
     1    effdrr(mxplan),aveyld,rcalsl,tmpvar,watcon,yrsed,yrir,yrrain,
     1    hilir,mirrig,mirrro,hilsed,hilenr,morait,moirt(12),momrot,
     1    tora(12),toirr(12),toraro(12),tomero(12),toirro(12),todet(12),
     1    toirde(12),todep(12),toseye(12),toenra(12),frara(mxplan),
     1    hfric,hyrad,avdatm,sumsrm,sumrtm,totsed,warain,hday,totenr,
     1    mondet,mondep,det,dep,dsunmp,runom,tirig,tiro,tmelt,totir,
     1    train,yirrig,yirrro,yrmro,yrrro,fuzzr,powerz,hyradz
c
      character text2(12)*3
      character*8 inifil
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Integer Variables:
c
c     lun1 - flag indicating that user wants plotting
c            output    (1 - yes ;  0 - no )
c     nday(12,2) - a day for a specific month in a specific year
c     nowcrp(mxplan) - current crop number for the current year
c     iuprun(mxplan) - flag to indicate flow onto an OFE from above
c                      (1 - yes ; 0 - no)
c     ncrop - number of different crops in simulation
c     nmonth -
c     i -
c     j -
c     lp -
c     mm -
c     nn -
c     iyear -
c     kk -
c     idout -
c     ijk -
c     i9 -
c     iirout - flag to indicate routing message printed
c     jj -
c     k -
c     ipass -
c     passby -
c     switch(mxplan) -
c     noout -
c     iiyear -
c     oldind -
c     iofe -
c     bigcrp(mxplan) -
c     iii -
c     ilay -
c     ioutfl -
c     itemp -
c     jjj -
c     limyr -
c     lyear -
c     myear -
c     nomelt -
c     nowres -
c     nrots -
c     nsurf -
c     nyears
c     jstruc -
c     froday(mxplan) -
c     l -
c     jjkkll -
c     norflg -
c
c     Real Variables:
c
c     dslost(mxplan,100) - net soil loss/gain for each point on an OFE
c                          for a storm event
c     xmxint(mxplan) -
c     mxint - maximum rainfall intensity
c     runmax -
c     pkrmax -
c     pkefdn -
c     effdrr(mxplan) -
c     aveyld -
c     tmpvar -
c     watcon -
c     yrsed - sediment delivered off end of hillslope for year
c     yrrain - total rainfall for year
c     yrir - interrill detatchment from hillslope for year
c     hilir -
c     mirrig -
c     mirrro -
c     hilsed -
c     hilenr -
c     morait -
c     moirt(12) -
c     momrot -
c     tora(j) - total rain for all months with index j
c     toirr(j) - total irrigation for all months with index j
c     toraro(j) - total rainfall runoff for all months with index j
c     tomero(j) - total melt runoff for all months with index j
c     toirro(j) - total irrigation runoff for all months with index j
c     todet(j) - total detachment for all months with index j
c     toirde(j) - total interrill detachment for all months with index j
c     todep(j) - total deposition for all months with index j
c     toseye(j) - total sediment yield for all months with index j
c     toenra(j) - total enrichment ratio for all months with index j
c     frara(mxplan) -
c     hfric -
c     hyrad -
c     avdatm -
c     sumsrm -
c     sumrtm -
c     totsed -
c     warain -
c     hday -
c     totenr -
c     mondet -
c     mondep -
c     det -
c     dep -
c     dsunmp -
c     runom -
c     tirig -
c     tiro -
c     tmelt -
c     totir -
c     train -
c     yirrig -
c     yirrro -
c     yrmro -
c     yrrro -
c
c     Character Variables:
c
c     text2(12)*3  - month of year text for writing routing information
c                    to the screen
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     outfil    infile     input    tilage    winthead    prtcmp
c     init1     rngint     scon     soil      watbal      initd
c     bighdr    newtil     nowup    stmget    decomp      aspect
c     sunmap    winter     irrig    irs       frcfac      xinflo
c     param     route      sloss    sumrun    print       cutgrz
c     sedout    bigout     sumfrc   close     hdreng      outeng
c
c     + + + FUNCTION DECLARATIONS + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
      data nday /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 29,
     1    31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
c
      data text2 /'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul',
     1    'aug', 'sep', 'oct', 'nov', 'dec'/
c
c     + + + END SPECIFICATIONS + + +
c
c     initialize local variables
c
      cntflg = 0
      pkefdn = 0.0
      lunp = 0
      luns = 0
      lunw = 0
      lun1 = 0
      noout = 0
      ipass = 0
      jyear = 0
      iyfile = 0
      iyear  = 0
      lyear = 0
      tmelt = 0.0
      tiro = 0.0
      train = 0.0
      tirig = 0.0
      totir  = 0.0
      totenr = 0.0
      totsed = 0.0
      hilsed = 0.0
      hilir  = 0.0
      hilenr = 0.0
      yirrig = 0.0
      ymelt = 0.0
      mirig = 0.0
      miro = 0.0
      morait = 0.0
      momrot = 0.0
      mondet = 0.0
      mondep = 0.0
      runom  = 0.0
c
      do 5 j=1,12
        tora(j)   = 0.0
        toirr(j)  = 0.0
        toraro(j) = 0.0
        tomero(j) = 0.0
        toirro(j) = 0.0
        todet(j)  = 0.0
        toirde(j) = 0.0
        todep(j)  = 0.0
        toseye(j) = 0.0
        toenra(j) = 0.0
c
        moirt(j)  = 0.0
c
    5 continue
c
c     OPEN OUTPUT FILES
c
      call outfil(lunp,luns,lunw,lun1)
c
c     OPEN INPUT FILES
c
      call infile(ncrop,jstruc,nsurf,nrots,nyears,iofe,
     1    iwpass)
c
c     CONVERT INDEX VALUES FROM INFILE TO INPUT PARAMETERS FOR WEPP
c
      call input(ncrop,jstruc,nsurf,iwpass)
c
c     ASSIGN PLANT/MANAGEMENT INPUT PARAMETERS
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
c     PRINT HEADING FOR WINTER OUTPUT FILE
c
      if (snoflg.eq.1) then
        call winthd(snodpy,frdp,thdp,nplane,1)
      end if
c
      do 20 iplane = 1, nplane
c
c       generate default set of particles for soil if not inputted
c
        call prtcmp(iplane)
c
        iflag = 0
c
c       initialize slope, water balance and crop/range routines
c
        if (lanuse(iplane).eq.1) then
c
c         INITIALIZE RESIDUE MASS AND DECOMPOSITION PARAMETERS
c
          call init1(nowcrp(iplane))
c
        else
c
c         INITIALIZE RANGE PLANT GROWTH PARAMETERS
c
          call rngint(ncrop,nowcrp(iplane))
c
        end if
c
        iflag = 0
c
c       subroutine scon computes constants for bulk density (bd)
c       and surface roughness (rro)
c
c       COMPUTE SOIL CONSTANTS
c
        call scon(lanuse(iplane))
c
c       INITIALIZE SOIL PARAMETERS
c
        call soil(nowcrp(iplane))
c
c       INITIALIZE SOIL WATER CONTENT
c
        call watbal(lunp,luns,lunw,nowcrp(iplane),elev)
c
c       INITIALIZE DECOMPOSITION INDICES
c
        call initd(iplane)
c
c       INITIALIZE WINTER PARAMETERS
c
        call winit
c
   20 continue
c
      iflag = 1
      isimyr = 1
c
c     start loop calls
c     nday   = a day for a specific month in a specific year
c     nyear  = number of years in simulation
c
      if (imodel.eq.1) then
c
c       determine maximum number of years available in management file
c
        myear = nyears * nrots
        limyr = min0(numyr,myear)
        write (6,1300)
c
c       read in number of years to run the simulation
c
        nyear=0
        read (5,*,err=30) nyear
c
   30   if(nyear.lt.1)then
          nyear=1
          write (6,3000)
        endif
c
c       determine limiting datafile, if numyr (climate years) is
c       less than nyear or myear (management years) is less than
c       nyear then reset nyear to limiting number of years
c
        if (nyear.gt.limyr) then
          write (6,1000) limyr, limyr
          nyear = limyr
        end if
c
        year = ibyear - 1
        nmonth = 12
c
c       ipass event routing section
c
        if (ivers.eq.1) then
c
c         if hillslope version then users have the choice
c         not to route small runoff events (ipass = 1)
c
          write (6,1400)
          read (5,2100,err=40) ipass
c
          if ((ipass.lt.0).or.(ipass.gt.1)) then
            ipass = 0
            write (6,1100)
            go to 50
          end if
c
          go to 50
   40     ipass = 0
          write (6,1100)
   50     continue
        else
c
c         otherwise (hillslope/watershed version) all
c         small runoff events are routed
c
          ipass = 0
        end if
c

        if (bigflg.eq.1) then
          if (units .eq. 0) then
            call bighdr(nyear,iofe,ver)
          else
            call hdreng(nyear,iofe,ver)
          endif
        endif
        
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
c***************************** year loop ******************************
c
      iyear = 0
c
      do 280 i = 1, nyear
c
        yrro=0.0
        yrain=0.0
        yrmro=0.0
        yrir =0.0
        yrdep=0.0
        yrdet=0.0
        yrsed=0.0
        yirig=0.0
        yiro=0.0
c
        iiyear = i
c
        do 70 mm = 1, nplane
          do 60 nn = 1, 100
            dsyear(mm,nn) = 0.0
   60     continue
   70   continue
c
        avsoly = 0.0
        enryy1 = 0.0
        enryy2 = 0.0
c
        do 80 kk = 1, npart
          frcyy1(kk) = 0.0
          frcyy2(kk) = 0.0
   80   continue
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
        do 90 iplane = 1, nplane
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
   90   continue
c
c*************************** month loop *******************************
c
        do 260 j = 1, nmonth
c
          if(isum.eq.1)then
            mirrig=0.0
          endif
c
          nmon = j
c
          do 110 mm = 1, nplane
            do 100 nn = 1, 100
              dsmon(mm,nn) = 0.0
  100       continue
  110     continue
c
c         imout = 0
c
          avsolm = 0.0
          enrmm1 = 0.0
          enrmm2 = 0.0
c
          do 120 kk = 1, npart
            frcmm1(kk) = 0.0
            frcmm2(kk) = 0.0
  120     continue
c
c***************************** daily loop *****************************
c
          do 250 k = 1, nday(j,lp)
c
            sdate = sdate + 1
c
c           call hyinp
c
c           mxint is equal to the maximum rainfall intensity.
c           before irs is called there is a test for mxint > ks
            call stmget(mxint)
c
            do 140 mm = 1, nplane
              do 130 nn = 1, 100
                dslost(mm,nn) = 0.0
  130         continue
  140       continue
c
            do 150 ijk = 1, nplane
              runoff(ijk) = 0.0
              norun(ijk) = 0
              iuprun(ijk) = 0
              wmelt(ijk) = 0
              bigcrp(ijk) = itype(nowcrp(ijk),ijk)
              watblf(ijk)    = 0
              surdra(ijk) = 0.0
              xmxint(iplane) = 0.0
  150       continue
c
c           hillslope/watershed version requires daily initialization
c           of sediment concentration and fraction of each particle
c           type leaving hillslope to zero
c
            do 170 mm = 1, npart
              do 160 nn = 1, nplane
                sedcon(mm,nn) = 0.0
                frcflw(mm,nn) = 0.0
  160         continue
  170       continue
c
c           do parameter updating and rainfall runoff computations
c
            idout = 0
            qsout = 0.0
            qout = 0.0
c
c           if temperature less than zero then todays precip is snowfall
c
            snow = 0.0
            nomelt = 0
            warain = 0.0
c
            do 180 iplane = 1, nplane
c
            if (manver.gt.98.4)then
c
              if (conseq(nowcrp(iplane),iplane).ne.0.and.
     1          sdate.ge.cntday(conseq(nowcrp(iplane),iplane)).and.
     1          sdate.le.cntend(conseq(nowcrp(iplane),iplane)))then
c
                 contrs(nowcrp(iplane),iplane)=1
c
                if (sdate.eq.cntday(conseq(nowcrp(iplane),iplane)))
     1            write(6,*)'CONTOUR ROUTING ENABLED ON PLANE', iplane,
     1            ' ON DAY ',sdate
c
                if (sdate.eq.cntend(conseq(nowcrp(iplane),iplane)))
     1            write(6,*)'CONTOUR ROUTING DISABLED ON PLANE', iplane,
     1            ' ON DAY ',sdate
c
              else
c
                contrs(nowcrp(iplane),iplane)=0
c
              endif
c
            else

              if (conseq(nowcrp(iplane),iplane).ne.0)then

                contrs(nowcrp(iplane),iplane)=1

              else

                contrs(nowcrp(iplane),iplane)=0

              endif

            endif
c
c
              rain(iplane) = prcp
              if(snodpy(iplane).le.0.0 .and. tmin.ge.0.0 .and.
     1          rain(iplane).gt.0.0) warain = rain(iplane)
c
c             SET MAXIMUM WATER INPUT RATE OF EACH OFE
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
c             calls to aspect and sunmap moved to get radpot each day
c             for evap
c
              call aspect(deglat,azm(1),avgslp(iplane))
c
c             Reset frost cycle variable when 50 days have passed
c             and no frost has been present in the soil.
c
              if(frdp(iplane).le.0.0)then
c
c               Correction to prevent integer overflow for case of
c               no frost occurring for many, many years.  dcf  9/19/94
c               froday(iplane) = froday(iplane) + 1
                if(froday(iplane).lt.51)
     1            froday(iplane) = froday(iplane) + 1
              else
                froday(iplane) = 0
              endif
c
              if (froday(iplane).gt.50) then
                  fcycle(iplane) = 0
                  fgcycl(iplane) = 0
              endif
c
              wmelt(iplane) = 0.0
              frara(iplane) = 0.0
              avdatm = tave
c
c             WINTER - if snowdepth > 0 or minimum temperature < 0 or
c             the frost depth > 0
c

              if ((snodpy(iplane).gt.0.0).or.(tmin.le.0.0).or.
     1            (frdp(iplane).gt.0.0)) then
                 call winter(rain(iplane),snoflg)

c               Following code change from David Hall - dcf 3/7/2000
c               if (rain(iplane).gt.0.0)
                fuzzr = 0.001
                if (rain(iplane).gt.fuzzr)
     1            frara(iplane) = rans/rain(iplane)
                
                rain(iplane) = 0.0
                norain(iplane) = 0
                wntflg(iplane) = 1
                if (wmelt(iplane).gt.0.0001) nomelt = 1
                if (warain.le.0.0) xmxint(iplane) = 0.0
              else
                wntflg(iplane) = 0
                call sunmap(rcalsl,hday,dsunmp)
              end if
c
c XXX         Isn't the following statement going to mess up the
c             rainfall distribution on an adjacent OFE of a
c             multiple OFE hillslope where rainfall may be
c             falling on bare soil???   dcf  5/17/94
c
              if(norain(iplane).eq.0 .and. nomelt.eq.0)
     1           ninten(iplane) = 0
c
              tave = avdatm
c
              if(tmin.ge.0.0 .and. warain.gt.0.0)then
                rain(iplane) = warain
                norain(iplane) = 1
                wmelt(iplane) = 0.0
                nomelt = 0
              endif
cd    Added by S. Dun, 11/06/2007 for checking the effective hydraulic condictivity
cd    write(60, 1550) year,mon,day,iplane,ks(iplane),dpress(iplane)
1550  format(1x, 4I6, E12.3, F6.3)
cd    End adding
c
  180       continue
c
c           Move call to SR IRRIG here - so that it is after calls to
c           SR SOIL and SR DECOMP  - thus we assume that tillage and its
c           impacts on infiltration parameters and soil/residue occur
c           on a day of tillage BEFORE irrigation water is applied.
c           dcf  2/2/93
c
            if (irsyst.ne.0) call irrig(iiyear,nowcrp,pkrmax,
     1                                  runmax)
c
c XXX       Limited the execution of the following code to RANGELAND
c XXX       conditions ONLY - since the antecendent moisture is
c XXX       used for rangeland decomposition, burning, and herbicide
c XXX       application only.    dcf   12/19/94
c
            if (lanuse(1).eq.2 .and. imodel.eq.1) then
              if (sdate.lt.6.and.jyear.eq.1) then
                do 185 iplane = 1, nplane
                  r5(iplane,sdate) = rain(iplane) + wmelt(iplane)
     1                               + irdept(iplane)
c XXX           Following code commented out - assume for now that
c XXX           in rangeland conditions there will NOT be FURROW
c XXX           irrigation applications.    dcf  12/19/94
c               if (noirr.gt.0.and.irsyst.eq.2) r5(sdate) = r5(sdate) +
c    1              splyvm / totlen(nplane) / rw(nowcrp(irofe),irofe)
c
                  am(iplane) = am(iplane) + r5(iplane,sdate) / rx(sdate)
                  am2(iplane) = am(iplane)
                  if (am(iplane).gt..01) am(iplane) = .01
 185            continue
              else
                do 193 iplane = 1, nplane
                  r5(iplane,6) = rain(iplane) + wmelt(iplane)
     1                           + irdept(iplane)
c XXX           Following code commented out - assume for now that
c XXX           in rangeland conditions there will NOT be FURROW
c XXX           irrigation applications.    dcf  12/19/94
c               if (noirr.gt.0.and.irsyst.eq.2) r5(6) = r5(6) + splyvm /
c    1              totlen(nplane) / rw(nowcrp(irofe),irofe)
c
                  am(iplane) = 0.0
c
                  do 190 i9 = 1, 5
                    r5(iplane,i9) = r5(iplane,i9+1)
                    am(iplane) = am(iplane) + r5(iplane,i9) / rx(i9)
  190             continue
c
                  am2(iplane) = am(iplane)
                  if (am(iplane).gt..01) am(iplane) = .01
  193           continue
c
              end if
c
              am2(iplane) = am(iplane)
              if (am(iplane).gt..01) am(iplane) = .01
            end if
c
c           check for occurrence of rainfall and/or irrigation
c           (if rainfall norain = 1, if stationary sprinkler irrigation
c           noirr > 0 and irsyst = 1) or for snow melt nomelt > 0
c
c XXX       Added code to check for a nonzero norain flag on any OFE
c           dcf  6/3/94
c
            norflg = 0
c
            do 195 jjkkll = 1,nplane
              if(norain(jjkkll).gt.norflg)norflg=norain(jjkkll)
 195        continue
c
            if ((norflg.eq.1).or.((noirr.ne.0).and.(irsyst.eq.1)).or.(
     1          nomelt.ne.0)) then
c
cd              call irs(xmxint,nowcrp,wmelt,ibrkpt,iuprun,runmax,
            call irs(nowcrp,wmelt,ibrkpt,runmax,pkrmax,pkefdn,effdrr)
c
c           change made for irrigation noirr > 0 and irsyst = 2
c           means furrow irrigation occured today and therefore
c           these values should not be set to zero
c
            else if (noirr.eq.0.or.irsyst.ne.2) then
              pkrmax = 0.0
              pkefdn = 0.0
              runmax = 0.0
            end if
c
c           test to see whether event is too small for sediment routing.
c
            if (runmax.le.0.010.and.pkrmax.le.2.78e-06) then
              passby = 1
            else
              passby = 0
            end if
c
c           ****** loop through hillslope overland flow elements ******
c
            iirout = 0
c
c           if hillslope pass file or hillslope/watershed versions then
c           initialize variables needed to calculate average Manning's
c           roughness for a runoff event on the hillslope
c
            if ((iwpass.eq.1).or.(ivers.eq.2)) then
              hyrad = 0.0
              hfric = 0.0
            end if
c
c           loop through planes
c
            do 210 iplane = 1, nplane
              ofelod(iplane)=0.0
cd            Added by S. Dun, June 19,2008
c             to prevent dividing by zero when contour infulence sets qout = 0 in line1160
              if ((qout.lt. 1e-12).and.(runoff(iplane).lt.1e-12)) then
                  norun(iplane) = 0
              endif
c             End adding  
c
              if (ipass.eq.0.or.passby.eq.0) then
                if (norun(iplane).eq.1) then
c
                  call frcfac(nowcrp(iplane))
c
c                 update variables affected by overland flow
c
                  call xinflo(nowcrp(iplane))
c
c                 compute rill and interrill erosion parameters
c
                  call param(effdrr,nowcrp(iplane),frara(iplane))
c
c                 if hillslope pass file or hillslope/watershed versions
c                 then calculate average Manning's roughness on hillslope
c
                  if ((iwpass.eq.1).or.(ivers.eq.2)) then
c
                    hyrad = hyrad + (hydrad(iplane) * slplen(iplane))
                    hfric = hfric + (frctrl(iplane) * slplen(iplane))
c
                    if (iplane.eq.nplane) then
c
c                     calculate overland flow manning's n
c
                      hyrad = hyrad / totlen(nplane)
                      hfric = hfric / totlen(nplane)
c
c                     convert from Darcy-Weisbach roughness to Manning's
c                     roughness for overland flow
c
c                     hmann = sqrt(hfric * (hyrad**(1.0/3.0)) /
c    1                    (8.0 * accgav))
c
c                     Following changes from David Hall - dcf 3/7/00
c
                      hyradz = hyrad
                      powerz = 1.0/3.0
                      call undflo(hyradz,powerz)
                      hmann = sqrt(hfric * (hyradz**powerz) /
     1                        (8.0 * accgav))
                    end if
                  end if
                end if
              end if
c
c
c             **** Update cumulative kinetic energy.
c
c             Correction made here so that RKINE is set to zero for
c             cases of snow melt only.  The value is already zero for
c             cases of furrow irrigation.  dcf  12/15/94
c
              if (wmelt(iplane).gt.0.0) rkine = 0.0
              rkecum(iplane) = rkecum(iplane) + rkine
c
c             update water balance and plant growth
c
              if ((imodel.eq.1) .and. (watblf(iplane).eq.0)) 
     1                call watbal(lunp,luns,lunw,nowcrp(iplane),elev)
c
              if (lanuse(iplane).eq.1) then
c
c               update tillage index to next tillage date on day of
c               tillage
c
                if (tilseq(nowcrp(iplane),iplane).gt.0.and.
     1              indxy(iplane).gt.0) then
                  if (sdate.eq.mdate(indxy(iplane),
     1                tilseq(nowcrp(iplane),iplane)))
     1            call newtil(tilseq(nowcrp(iplane),iplane),
     1                ntill(tilseq(nowcrp(iplane),iplane)),iplane,
     1                oldind)
                end if
              end if
c
              if (imodel.eq.1) then
                if (passby.eq.1.and.ipass.eq.1) then
                  qsout = 0.
                  qout = 0.
                  avsole = 0.
                  enrato(iplane) = 0.
                  runvol(iplane) = 0.
                  sbrunv(iplane) = 0.0
                  effint(iplane) = 0.
                  peakro(iplane) = 0.
                  irdgdx(iplane) = 0.
                  avsolc(iplane) = 0.
                  avedet = 0.
                  maxdet = 0.
                  ptdet = 0.
                  avedep = 0.
                  maxdep = 0.
                  ptdep = 0.
c
                  go to 200
c
                end if
              end if
c
c             check for occurrence of runoff (if runoff norun = 1)
c
              if (norun(iplane).eq.1) then
c
                if (iplane.eq.nplane.and.runoff(iplane).gt.0.0) then
                  iroute = 1
                else
                  iroute = 0
                end if
c
c               write routing information to screen
c
                if (iirout.eq.0) then
c
                  if (iplane.eq.nplane) then
c
c                   continuous simulation:  write event date to screen
c
                    if (imodel.eq.1) then
c
                      if (iplane.eq.1) then
c
c                       hillslope composed of a single ofe
c
                        write (6,1500) day, text2(mon), lyear, ihill
                        iirout = 1
                      else
c
c                       hillslope composed of multiple ofe's
c
                        write (6,1600) day, text2(mon), lyear, nplane,
     1                      ihill
                        iirout = 1
                      end if
c
                    else
c
                      if (iplane.eq.1) then
c
c                       hillslope composed of a single ofe
c
                        write (6,1700) ihill
                        iirout = 1
                      else
c
c                       hillslope composed of multiple ofe's
c
                        write (6,1800) nplane, ihill
                        iirout = 1
                      end if
                    end if
                  end if
                end if
c
c               route sediment down hillslope profile
c
                call route
c
c               compute event sediment yield and concentration
c
                call sloss(idout,dslost,wmelt(iplane),
     1              nowcrp(iplane))
c
                if (contrs(nowcrp(iplane),iplane).ne.0) qout = 0.0
c
                idout = 1
c               imout = 1
c               iyout = 1
c
c               sum up total runoff volume and number of events
c
                if (imodel.eq.1) call sumrun(wmelt(iplane))
c
c
c               calculate runoff volume on last plane
c
                if (iplane.eq.nplane) runvol(iplane) = runoff(iplane) *
     1              (fwidth(1)*totlen(iplane)) * efflen(iplane) /
     1              totlen(iplane)
c
              else
c
                qsout = 0.
                qout = 0.
                avsole = 0.
                enrato(iplane) = 0.
                runvol(iplane) = 0.
                sbrunv(iplane) = 0.0
                effint(iplane) = 0.
                effdrn(iplane) = 0.0
                peakro(iplane) = 0.
                irdgdx(iplane) = 0.
                avsolc(iplane) = 0.
                avedet = 0.
                maxdet = 0.
                ptdet = 0.
                avedep = 0.
                maxdep = 0.
                ptdep = 0.
c
              end if
c
c             store temporary hillslope runoff, sediment concentration,
c             and particle size information needed by the watershed version
c
              if ((iwpass.eq.1.or.ivers.eq.2).and.iplane.eq.nplane)
     1           then
                call sedseg(dslost,1,iyear,2)

                call wshpas(lyear,sdate,1)
              endif
c
              if (imodel.ne.1.and.irsyst.ne.2) 
     1               call print(effdrr(iplane))
c
  200         continue
c
              if (imodel.eq.1.and.lanuse(iplane).eq.1) call
     1            cutgrz(nowcrp(iplane),sdate,iplane)
c
              if (sdate.eq.switch(iplane)) then
                nowcrp(iplane) = nowcrp(iplane) + 1
                write (6,*) 'NEW CROP #', nowcrp(iplane), ' ON DATE',
     1              sdate
                nnc(iplane) = 1
                indxy(iplane) = 0
c
                if (tilseq(nowcrp(iplane),iplane).gt.0)
     1              call newtil(tilseq(nowcrp(iplane),iplane),
     1              ntill(tilseq(nowcrp(iplane),iplane)),iplane,oldind)
c
                if (nowcrp(iplane).lt.nycrop(iplane)) then
c
                  if (tilseq(nowcrp(iplane)+1,iplane).gt.0) then
                    call nowup(tilseq(nowcrp(iplane)+1,iplane),
     1                  ntill(tilseq(nowcrp(iplane)+1,iplane)),
     1                  jdplt(nowcrp(iplane)+1,iplane),
     1                  jdharv(nowcrp(iplane)+1,iplane),
     1                  jdharv(nowcrp(iplane),iplane),
     1                  jdstop(nowcrp(iplane),iplane),switch(iplane))
                  else
                    call nowup(0,0,jdplt(nowcrp(iplane)+1,iplane),
     1                  jdharv(nowcrp(iplane)+1,iplane),
     1                  jdharv(nowcrp(iplane),iplane),
     1                  jdstop(nowcrp(iplane),iplane),switch(iplane))
                  end if
                end if
              end if
c
  210       continue
c
            if ((ioutpt.eq.1.or.isum.eq.1.or.ievt.eq.1.or.lun1.gt.1.or.
     1          ifofe.eq.1).and.idout.eq.1) then
c
              if (ioutpt.ne.1) then
                noout = 2
              else if (ioutpt.eq.1.and.ievt.eq.1) then
                noout = 1
              end if
c
              call sedout(iyear,dslost,isum,ievt,ifofe,lun1,
     1            noout,1,nowcrp(nplane))
c
              noout = 0
c
            else if (ifofe.eq.1.and.(day.eq.1.or.day.eq.15)) then
c
              do 230 iplane = 1, nplane
                watcon = 0.0
c
                do 220 ilay = 1, nsl(iplane)
                  watcon = watcon + soilw(ilay,iplane)
  220           continue
c
                nowres = 1
                tmpvar = 0.0
c
                write (33,1200) iplane, day, mon, year - ibyear + 1,
     1              rain(iplane) * 1000.0,
     1              tmpvar, tmpvar, tmpvar, tmpvar, tmpvar, ks(iplane) *
     1              3.6e06, watcon * 1000, lai(iplane), canhgt(iplane),
     1              cancov(iplane) * 100, inrcov(iplane) * 100,
     1              rilcov(iplane) * 100, vdmt(iplane), rmagt(iplane) +
     1              rmogt(nowres,iplane), ki(iplane) * kiadjf(iplane) /
     1              1000000, kr(iplane) * kradjf(iplane) * 1000,
     1              shcrit(iplane) * tcadjf(iplane), width(iplane),
     1              ofelod(iplane)
  230         continue
c
            end if
c
            if (lun1.gt.1) then
              do 240 iplane = 1, nplane
                bigflg = 0
                if(units .eq. 0) then
                  call bigout(bigcrp(iplane),iiyear)
                else
                  call outeng(bigcrp(iplane),iiyear)
                endif
  240         continue
            end if
c
            if (imodel.eq.2) then
              call close(lunp,luns,lunw,lun1)
              return
            end if
c
c
c           Save value of yesterday's rainfall to be passed to decomposition
c           routine.  Movement of call to DECOMP out of SR WATBAL and to
c           top of daily loop requires that DECOMP uses yesterday's rainfall
c           amount for standing residue computations to be consistent with
c           it's use of yesterday's soil moisture in soil layer 1.
c           dcf  2/3/93
c
c
c           values to be printed at end of simulation for each year
c
c           runoff for each year off of hillslope
c           average detachment from hillslope for each year of simulation
c           sediment leaving profile for each year of simulation
c
            if(isum.eq.1)then
c
              do 2,l=1,nplane
                yrir=yrir+(slplen(l)/totlen(nplane)*irdgdx(l))
                if(ioutpt.eq.2)then
                  moirt(j)=moirt(j)+(slplen(l)/totlen(nplane)*irdgdx(l))
                endif
    2         continue
c
              yrsed=yrsed+ avsole
            endif
c
  250     continue
c
          call sumfrc(enrmm1,enrmm2,enrmon,frcmm1,frcmm2,frcmon,dsmon,
     1        ioutpt,2,iyear,lun1,noout,nowcrp(nplane))
c
        if(isum.eq.1)then
c
c       ...Monthly write to temp summary file
c

          if(ioutpt.eq.2)then
            write(52,3155)jyear,text2(j),mrain,mirig,
     1         mrro,mmelt,miro*1000,avedet,moirt(j),avedep,avsolm,
     1         enrmon
          endif
          if(ioutas.eq.1)then
            yrmro=yrmro+mmelt
            tora(j)=tora(j)+mrain
            toirr(j)=toirr(j)+mirig
            toraro(j)=toraro(j)+mrro
            tomero(j)=tomero(j)+mmelt
            toirro(j)=toirro(j)+miro
            todet(j)=todet(j)+avedet
            toirde(j)=toirde(j)+moirt(j)
            todep(j)=todep(j)+avedep
            toseye(j)=toseye(j)+avsolm
            toenra(j)=toenra(j)+enrmon
            yrdet=yrdet+avedet
            yrdep=yrdep+avedep
            if(ioutpt.eq.3.and.ioutas.eq.1)yrain=trainy
            if(ioutpt.eq.2)then
              yrro=yrro+mrro
              yrain=yrain+mrain
              ymelt=ymelt+mmelt
              yiro=yiro+miro*1000
              yirig=yirig+mirig
            endif
          endif
c        trainm=0.0
          moirt(j)=0.0
          mrro=0.0
          mrain=0.0
          mmelt=0.0
          miro=0.0
          mirig=0.0
          avedep=0.
        endif
c
  260   continue
c
        call sumfrc(enryy1,enryy2,enryr,frcyy1,frcyy2,frcyr,dsyear,
     1       ioutas,1,iyear,lun1,noout,nowcrp(nplane))
c
        do 270 jj = 1, nplane
          indxy(jj) = 0
          switch(jj) = 0
          ncount(jj) = 0
          nnc(jj) = 0
  270   continue
c
        if(isum.eq.1)then
          totsed=totsed+yrsed
          totir =totir+yrir
          totenr=totenr+enryr
c
c
c

          if(ioutpt.eq.3.and.ioutas.eq.1)then
            yrro=yrro*(1-irper)
            yiro=yiro*irper
          endif
c         ...monthly summary
          if(ioutas.eq.1)write(52,*)jyear,yrain,yirig,
     1          yrro,ymelt,yiro,avedet,yrir,avedep,yrsed,enryr
          tmelt=tmelt+ymelt
          ymelt=0.0
          trro=trro+yrro
          yrro=0.0
          tiro=tiro+yiro
          yiro=0.0
          train=train+yrain
          yrain=0.0
          tirig=tirig+yirig
          yirig=0.0
        endif

c
c       get next years tillage information
c
        isimyr = 2
        if (i.ne.nyear) call tilage(nowcrp)
        if (yldflg.eq.1) write (46,*) '  '
c
  280 continue
c
      if(isum.eq.1)then
        hilsed=totsed/nyear
        hilir =totir/nyear
        airo=tiro/nyear
        arro=trro/nyear
        airig=(tirig/nplane)/nyear
        hilenr=totenr/nyear
        amelt=tmelt/nyear
        arain=train/nyear
      endif
c
      ioutfl = 3
      iyear = 1
c
      do 300 mm = 1, nplane
        do 290 nn = 1, 100
          dsavg(mm,nn) = dsavg(mm,nn) / float(jyear)
  290   continue
  300 continue
c
      avsolf = avsolf / float(jyear)
c
      call sumfrc(enrff1,enrff2,enravg,frcff1,frcff2,frcavg,dsavg,
     1    ioutfl,ioutfl,iyear,lun1,noout,nowcrp(nplane))
c
c     write minimum and maximum values for large graphical output file
c     to file "plotfile.mnx"
c
      if (lun1.gt.1) then
        bigflg = 1
        if (units .eq. 0) then
          call bigout(1,iiyear)
        else
          call outeng(1,iiyear)
        endif
      end if
c
c     compute and output average annual yields
c
      if (yldflg.eq.1) then
        write (46,1900)
        do 320 iii = 1, nplane
          do 310 jjj = 1, ncrop
            if (iyldct(jjj,iii).gt.0) then
              aveyld = sumyld(jjj,iii) / float(iyldct(jjj,iii))
              if (aveyld.gt.0.0)write (46,2000) iii, jjj,
     1            iyldct(jjj,iii), aveyld
            end if
c
  310     continue
c
  320   continue
      end if
c
c     write note to unit 45 if no grazing occured
c
      if (rngout.eq.2) then
        if (ianflg.lt.1)then
          if(rnganm.eq.2)write (45,2200)
        end if
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
        j=itype(nowcrp(1),1)

        write(47,2800)
c
c       write plant name from management file
c
        write(47,2900)crpnam(j)
        write(47,2950) (mancom(i),i=1,3)
c
c       write plant associated with ofe 1 at end of simulation for
c       cropland residue parameters
c
        write (47,2300) lanuse(1)

        if(lanuse(1).eq. 1)then
c
          write(47, *) 'WeppWillSet'
          write(47,*)bb(j), bbb(j), beinp(j), btemp(j), cf(j),
     1          crit(j), critvm(j), cuthgt(j), decfct(j), diam(j)
          write(47,*)dlai(j), dropfc(j), extnct(j), fact(j),
     1          flivmx(j), gddmip(j), hi(j), hmax(j)
c
          if (mfocod(j).eq.1) then
            write(47,*) mfocod(j), '  # mfo - <Fragile>'
          else
            write(47,*) mfocod(j), '  # mfo - <NonFragile>'
          endif
c
          write(47,*) oratea(j), orater(j), otemp(j), pltol(j),
     1          pltsp(j), rdmax(j), rsr(j), rtmmax(j), spriod(j),
     1          tmpmax(j)
          write(47,*) tmpmin(j), xmxlai(j), yld(j)
c
        else if(lanuse(1).eq.2)then
c
          write(47,*) aca(j), aleaf(j), ar(j), bbb(j), bugs(j), cf1(j),
     1          cf2(j), cn(j), cold(j), ffp(j)
          write(47,*) gcoeff(j), gdiam(j), ghgt(j), gpop(j), gtemp(j),
     1          hmax(j), plive(j,1), pltol(j), pscday(j), rgcmin(j)
c
          if(scday2(j).gt.365)then
            write (6,*)' *** WARNING ***'
            write (6,*)' calculated value for SCDAY2 = ',
     1          scday2(j)
            write (6,*)' resetting value for SCDAY2 to 365 in initial',
     1          ' condition scenario file'
            write (6,*)' *** WARNING ***'
            scday2(j) = 365
          endif
c
          write(47,*)  root10(j), rootf(j), scday2(j), scoeff(j),
     1          sdiam(j), shgt(j), spop(j), tcoeff(j), tdiam(j),
     1          tempmn(j)
          write(47,*) thgt(j), tpop(j), wood(j)
c
        else
c
c       no other plant types supported at this time
c
c       initial condition creation section
c
        endif
c
        write(47,2801)
        call strip (scefil,inifil)
c
        write(47,2900)inifil
        if(inifil.eq.'        ')inifil=crpnam(j)
c
        write(47,2950) (mancom(i),i=1,3)
        write (47,2300) lanuse(1)
c
        if (lanuse(1).eq.1) then
          write (47,2400) ibd / 1000, icanco, idaydi, idshar, ifrdp,
     1        iinrco
          write (47,2100) itemp, '# iresd'
          write (47,2100) imngm1(j), '# mgmt'
          write (47,2500) irfcum * 1000, irhini, irilco, irrini, irspac
          write (47,2100) rwflag(1), '# rtyp'
          write (47,2500) isnodp, ithdp, itill1, itill2, iwidth
c
c         ADD new initial condition values for dead root mass and
c         submerged residue mass.   dcf  5/3/94
c
          sumrtm = 0.0
          sumsrm = 0.0
c
          do 555 ijk=1,3
            sumrtm = sumrtm + rtm(ijk,1)
            sumsrm = sumsrm + smrm(ijk,1)
 555      continue
c
          write (47,2550) sumrtm, sumsrm
c
        else if (lanuse(1).eq.2) then
c
          write (47,2600)frdp(1),pptg(1),rmagt(1),irmogt,rrough(1),
     1        snodpy(1),thdp(1),tillay(1,1),tillay(2,1)
          write (47,2600)rescov(1),bascov(1),rokcov(1),crycov(1),
     1        fresr(1),frokr(1),fbasr(1),fcryr(1),cancov(1)
c
        end if
c
        write (47,2700) nyear
c
      end if
c
      if (isum.eq.1)then
        rewind (52)
c
c       yearly summary
c
        if(ioutpt.eq.1)write(31,3700)
        if(ioutpt.gt.1)write(31,3200)
c
c       ... Annual Detailed summary
c
        if(ioutpt.eq.3.and.ioutas.eq.1)then
c
          do 400, i=1,nyear
            read(52,*)jyear,yrrain,yirrig,yrrro,yrmro,yirrro,det,
     1              yrir,dep,yrsed,enryr
            write(53,3275)jyear,yrrain,yirrig,yrrro,yrmro,yirrro,det,
     1              yrir,dep,yrsed,enryr
            write(31,3275)jyear,yrrain,yirrig,yrrro,yrmro,yirrro,det,
     1              yrir,dep,yrsed,enryr
  400     continue
            write(53,3600)
            write(31,3600)
c
c          if(ioutpt.gt.1)then
c            write(53,3600)
c            write(31,3600)
c          endif
c
c       ... monthly summary
c
        elseif(ioutpt.eq.2)then
c
          do 450,i=1,nyear
            do 475,j=1,12
              read(52,3155)jyear,text2(j),morait,mirrig,runom,
     1          momrot,mirrro,mondet,moirt(j),mondep,avsolm,enrmon
              write(53,3155)jyear,text2(j),morait,mirrig,runom,
     1          momrot,mirrro,mondet,moirt(j),mondep,avsolm,enrmon
              write(31,3155)jyear,text2(j),morait,mirrig,runom,
     1          momrot,mirrro,mondet,moirt(j),mondep,avsolm,enrmon
  475       continue
            read(52,*)jyear,yrrain,yirrig,yrrro,yrmro,yirrro,yrdet,
     1            yrir,yrdep,yrsed,enryr
            write(53,3175)jyear,yrrain,yirrig,yrrro,yrmro,yirrro,
     1            yrdet,yrir,yrdep,yrsed,enryr
            write(31,3175)jyear,yrrain,yirrig,yrrro,yrmro,yirrro,
     1            yrdet,yrir,yrdep,yrsed,enryr
  450     continue
c
          do 460, j=1,12
            write(53,3575)text2(j),tora(j)/nyear, toirr(j)/nyear,
     1              toraro(j)/nyear,tomero(j)/nyear,toirro(j)*1000/
     1              nyear,todet(j)/nyear,toirde(j)/nyear,todep(j)/
     1              nyear,toseye(j)/nyear,toenra(j)/nyear
            write(31,3575)text2(j),tora(j)/nyear, toirr(j)/nyear,
     1              toraro(j)/nyear,tomero(j)/nyear,toirro(j)*1000/
     1              nyear,todet(j)/nyear,toirde(j)/nyear,todep(j)/
     1              nyear,toseye(j)/nyear,toenra(j)/nyear
          if(j.eq.12)write(53,3600)
          if(j.eq.12)write(31,3600)
  460     continue
c
        endif
c
c       ... Average Annual summary for all options of output
c
c        if(ioutas.eq.2)then
          arain=traint/nyear
          airig=tirrt/nyear
          amelt=tmunot(nplane)/nyear
          airo=irrunt(nplane)*1000/nyear
          arro=(trunot(nplane)/nyear)-airo
c        endif
        if(ioutpt.gt.1)then
          write(53,3500)arain,airig,arro,amelt,
     1          airo,avedet,hilir,avedep,hilsed,hilenr
          write(31,3500)arain,airig,arro,amelt,
     1          airo,avedet,hilir,avedep,hilsed,hilenr
          write(53,3550)
          write(31,3550)
        endif
      endif
c
c     close all open files
c
      call close(lunp,luns,lunw,lun1)
c
      return
c
 1000 format (' *** WARNING ***'/
     1    ' Number of years to simulate can"t be larger than ',i3,/,i3,
     1    ' years used ',/,' *** WARNING ***')
 1100 format (' *** WARNING ***',/,'Invalid option for routing ',
     1    'of small events: 0 assumed',/,' *** WARNING ***')
 1200 format (3(1x,i2),1x,i4,1x,f8.3,1x,f8.3,3(1x,f7.3),f6.3,1x,f7.3,1x,
     1    f7.3,1x,f7.3,1x,f6.3,3(1x,f7.3),6(1x,f6.3),1x,f8.3)
 1300 format (/,'  Enter number of years to simulate --> ')
 1400 format (/,'  To bypass erosion calculations for very small ',
     1    'events, enter 1',/,'  Otherwise, to route all events enter',
     1    ' 0   (Small events have',/,'  runoff less than 10 mm',
     1    ' and peak runoff less than 10 mm/hr) [0] -->',/)
 1500 format (1x,i2,1x,a3,1x,i4,2x,'routing runoff & erosion event',
     1    ' on OFE 1 of hillslope ',i2)
 1600 format (1x,i2,1x,a3,1x,i4,2x,'routing runoff & erosion event',
     1    ' on OFEs 1 to ',i2,' of hillslope ',i2)
 1700 format (12x,'routing OFE 1 of hillslope ',i2)
 1800 format (12x,'routing OFEs 1 to ',i2,' of hillslope ',i2)
 1900 format (//,'    CROP YIELD SUMMARY - AVERAGE YIELDS BY OFE',
     1    ' AND CROP TYPE',/,
     1    '             ***********',/,
     1    '     SILAGE VALUES NOT SUMMARIZED',/,
     1    '             ***********',//)
 2000 format ('FOR OFE NUMBER ',i2,', CROP TYPE NUMBER ',i2,/,
     1    'Average Yield of ',i3,' Harvests = ',f8.3,' kg/m**2',/)
 2100 format (i3,a20)
 2200 format (//,'    NO GRAZING OCCURED DURING THIS SIMULATION')
 2300 format (i1,10x,'# land use class')
 2400 format (2(f10.5,1x),f10.1,1x,i10,2(1x,f10.5))
 2500 format (5(f10.5,1x))
 2550 format (2(f10.5,1x))
 2600 format (9(f10.5,1x))
 2700 format ('#',/,
     1    '####################################################',/,
     1    '# Number of years simulated to create this initial #',/,
     1    '# condition scenarios: ',i3,'                         #',/,
     1    '####################################################',/,'#')
 2800 format (/,'#################',/,
     1        '# Plant Section #',/,
     1        '#################',/,
     1        '#',/,
     1        '1         # looper; number of plant scenerios')
 2801 format (/,'##############################',/,
     1        '# Initial Conditions Section #',/,
     1        '##############################',/,
     1        '#',/,
     1        '1         # looper;',
     1        ' number of initial conditions scenerios')
 2900 format (a)
 2950 format (a60,/,a60,/,a60)
 3000 format( '*** WARNING *** Assuming 1 year simulation')
 3200 format(/,/,/,
     1' IV.  EROSION OUTPUT SUMMARIES FOR SIMULATION',/,
     1'      ------- ------ --------- --- ----------',/,/,
     1'               1         2          3         4         5      ',
     1'   6         7         8         9        10',/,
     1'Year Month    Tot.      Tot.    ---------- Runoff ----------   ',
     1' Total  Interrill   Total    Sediment   Enrich.',/,
     1'             Precip.   Irrig.    Rain   Snowmelt   Irrig.      ',
     1'Detach.  Detach.    Depos.     Yield     Ratio',/,
     1'           --------------------(mm)-------------------------  -',
     1'--------(kg/m^2)----------    (kg/m)   (m^2/m^2)',/,110('-'))
 3275 format(i4,5x,5f10.2,5f10.3)
 3155 format(i4,1x,a4,5f10.2,5f10.3)
 3175 format(111('-'),/,'Year',1x,i4,5f10.2,5f10.3,
     1/,111('-'),/)
 3500 format('AVG ANN  ',5f10.2,5f10.3)
 3550 format(111('-'),/,/,/,' 1:Total Precipitation depth',/,
     1' 2:Total Irrigation depth',/,
     1' 3:Total runoff from rainfall',/,' 4:Total runoff from snowmelt',
     1  /,' 5:Total runoff from irrigation',/,
     1' 6:Total Detachment',/,' 7:Interrill detachment',/,
     1' 8:Total Deposition',/,' 9:Sediment yield',/,
     1' 9:Sediment yield',/,
     1'10:Sediment enrichment ratio',/,/,
     1'Note: Runoff values refer to runoff from the entire hillslope.',
     1'  Total detachment',/,
     1'      is the average net detachment over all areas on the ',
     1'hillslope having',/,
     1'      net detachment.  Interrill detachment is the weighted ',
     1'(by OFE length)',/,
     1'      average over the entire hillslope.  Total deposition ',
     1'refers to the',/,
     1'      average net deposition over all the areas on the ',
     1'hillslope experiencing',/,
     1'      net deposition.  Irrigation depths are averages over ',
     1'the entire hillslope.')
 3575 format('AVG  ',a4,5f10.2,5f10.3)
 3600 format(111('-'))
 3700 format(/,/,/,/
     1       5x,'******************************************************'
     1     ,'*****',/,
     1       5x,'*                                                     '
     1     ,'    *',/,
     1       5x,'* SUMMARY OUTPUT NOT AVAILABLE WHEN EVENT OUTPUT SELEC'
     1     ,'TED *',/,
     1       5x,'*                                                     '
     1     ,'    *',/,
     1       5x,'* SELECT EITHER DETAILED ANNUAL, ABBREVIATED ANNUAL OR'
     1     ,'    *',/,
     1       5x,'* MONTHLY SOIL LOSS OUTPUT                            '
     1     ,'    *',/,
     1       5x,'*                                                     '
     1     ,'    *',/,
     1       5x,'******************************************************'
     1     ,'*****')
      end
