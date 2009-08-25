      subroutine range(wst,nsl,nowcrp)
c
c     NOTE: Because of changes to the dimensions of PLIVE in RNGINT,
c           the recoded versions of common blocks RINPT1, RINPT3, &
c           ROUT are required.  Also in INPUT, the read of PLIVE(JJ)
c           should be changed to PLIVE(jj,1).
c
c     + + + PURPOSE + + +
c     This is the main rangeland plant growth subroutine.  It estimates
c     plant growth (canopy cover, canopy height, root depth, root mass,
c     and leaf area index) on rangelands.  This routine calls all
c     management options.
c
c     Called from WATBAL
c     Author(s): Weltz, Meyer
c     Reference in User Guide: Chapter 8
c
c     Changes:
c          1) Added RGC to parameter list in call to GCURVE.
c          2) Changed GROTYP from type "real" to "integer".
c          3) Dereferenced the following common blocks:
c                CCOVER.INC, CENDS.INC, CPARAME.INC, CRINPT5.INC
c          4) Added the following variable declarations:
c                real      x1,x2,x3,x4,x5,x6,tmpave,rmagy,rmogy
c                real      tlivey,tempvar,smrati,rratio
c                integer   nowres,istart,idecom
c          5) RGC added to parameter list in call to GCURVE, since it is
c             *computed* in GCURVE, but *printed* from RANGE.  Re: telcon
c             w/Weltz.
c          6) Rewrote 4-day average water stress calculations.  Saved IT;
c             divided running total by IT rather than SDATE; re-dimensioned
c             STRSS from (5,mxplan) to (3,mxplan); saved STRSS; deleted
c             DATA initialization of STRSS; eliminated local parameter
c             MPLN5; changed STRESS(IPLANE) to STRSS4(IPLANE), a 4-day
c             total, and used an unsubscripted variable STRESS for the
c             original purpose.
c          7) Equation to compute conversion of standing dead wood to fallen
c             (Equation 8.6.27) was incorrect.  It was changed from:
c                 rmogt(nowres,iplane) = rmogt(nowres,iplane) + (dwood(iplane)-
c    1                            (((1-smrati)/4)*dwood(iplane)))
c             to:
c                 rmogt(nowres,iplane) = rmogt(nowres,iplane) + dwood(iplane)*
c    1                            (1.0-smrati)/4.0
c             Re: 4/1/93 telcon w/Weltz (No foolin'!)
c          8) The following code moved from PATRIB to RANGE:
c                 if(yield(iplane) .gt. 0.0 .or. oldplt .gt. 0.0)then
c                   utiliz = tfood / (yield(iplane)+oldplt)
c                 else
c                   utiliz = 0.0
c                 endif
c             This meant OLDPLT & TFOOD can be removed from the dummy parameter
c             list for PATRIB.  UTILIZ which was incorrectly treated as a local
c             variable in PATRIB, can now be ignored there.
c          9) Deleted dummy parameters TMPFLG, ISTART, & TRTMAS from
c             GROW, and its calls from PTGRA, PTGRP, and RANGE.
c         10) Changed variable IFROST to JFROST to make it distinct
c             from variable in winter routines.  Re: 5/6/93 conversation
c             w/Weltz -- CRM.
c         11) ROOTY(NSL) changed to ROOTY(NSL,IPLANE).  Re: 5/6/93 conversation
c             w/Weltz -- CRM.
c         12) Deleted SAVE of ANS, OLDPLT, T2, RGC.
c         13) Modified loop which initializes EPIC parameters on the first
c             day of the first year of simulation, to initialize ALL the
c             plants if this is the first OFE.  (Not just the plants that
c             happen to be growing *somewhere* on th efirst day of simulation,
c             but all the plants that are ever used in the simulation!)
c         14) Changed local variable RGC to a non-subscripted variable.
c         15) Dimensioned local variable IDECOM to MXPLAN, as in PTGRP.
c         16) Dimensioned local variables X5 & X6 to MXCROP.
c         17) Code involving PLIVE(PLANT) changed to PLIVE(PLANT,IPLANE).
c         18) Moved location of statement "it(iplane)=it(iplane)+1"
c         19) Changed data statement for IT from 0 to 1    dcf 5/21/93
c         20) Added back in save of T2, otherwise it is undefined on
c             sdate greater than 1 in SR GCURVE     dcf 5/21/93
c         21) Added back in save of OLDPLT, otherwise it is undefined on
c             sdate greater than 1 in SR RANGE      dcf 5/21/93
c         22) Moved IT(MXPLAN) to common block crinpt1.inc and removed
c             RGC from data statement jca 8/31/93
c
c     Version: This module recoded from WEPP Version 92.25.
c     Date recoded: 03/29/93 04/01/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxelm.inc'
      include 'pmxnsl.inc'
      include 'pntype.inc'
      include 'ptilty.inc'
      include 'pmxpnd.inc'
      include 'pmxres.inc'
      include 'pmxpln.inc'
      include 'pmxgrz.inc'
      include 'pmxcrp.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxhil.inc'
c     parameter (mpln5 = 5*mxplan)
c
c     + + + ARGUMENT DECLARATIONS + + +
      real wst
      integer nsl, nowcrp
c
c     + + + ARGUMENT DEFINITIONS + + +
c     wst    - water stress (0-1) 1=no stress; 0=total stress
c     nsl    - number of soil layers
c     nowcrp - current crop
c
c     + + + COMMON BLOCKS + + +
      include 'ccrpgro.inc'
c       write: be, otemp
c
      include 'ccrpvr1.inc'
c        read: rtm
c      modify: rmogt(mxres,mxplan),rmagt(mxplan)
c
      include 'ccrpvr2.inc'
c        read: cn, aca, ar
c      modify: vdmt(mxplan)
c
      include 'ccrpvr3.inc'
c       write: sumgdd, gddmax, dlai
c
      include 'ccrpvr5.inc'
c        read: ncount
c
      include 'cclim.inc'
c        read: tave, am, am2
c
      include 'ccrpout.inc'
c      modify: rtmass(mxplan)
c
      include 'ccrpprm.inc'
c        read: itype, grazig
c       write: btemp
c
      include 'chydrol.inc'
c        read: rain(mxplan)
c
      include 'crinpt1.inc'
c        read: access(mxplan), plive(ntype,mxplan)
c      modify: yield(mxplan), pyield(mxplan) ,suppmt(mxplan)
c
      include 'crinpt2.inc'
c        read: rootf, gtemp, pscdst, pscd2s
c       write: pscday(ntype), strrgc(ntype), scday2(ntype)
c
      include 'crinpt3.inc'
c        read: jfdate
c
      include 'crinpt4.inc'
c        read: ptlive(mxplan), ihdate, xlive
c      modify: dwood
c       write: first
c
      include 'crinpt6.inc'
c      modify: totsup(mxplan), rtotsu(mxplan), rsuppm(mxplan),
c              sdead(mxplan), food(mxplan), growth
c       write: dinner(mxplan), ideath(mxplan)
c
      include 'crout.inc'
c      modify: tlive(mxplan), tfood(mxplan)
c       write: rooty(mxnsl,mxplan), slive, droot ,utiliz
c
      include 'cstmflg.inc'
c        read: jyear
c
      include 'cstruc.inc'
c        read: iplane
c
      include 'cupdate.inc'
c        read: sdate
c
c     + + + LOCAL VARIABLES + + +
      real oldplt, t2, death, trans, rknock
      real strss(3,mxplan), strss4(mxplan), stress, add(mxplan)
      real remove(mxplan), tstres(mxplan)
      real tau, tau2
      integer grotyp, jfrost, iwarn, plant, i
      real x1, x2, x3, x4, x5(mxcrop), x6(mxcrop), tmpave, rmagy, rmogy
      real tlivey, smrati, rratio, rgc
      integer nowres, idecom(mxplan)
c
c     + + + LOCAL DEFINITIONS + + +
c     oldplt -
c     t2     - number of days in second growth period, using non-EPIC routines.
c     death  - fraction of plant material killed by drought today
c     trans  - fraction of standing residue knocked to the ground by rain
c     rknock - amount of standing residue knocked to the ground by rain
c     strss  - last 3 day's water stress.  Used for 4-day average.
c     strss4 - running 4-day sum used to calculate STRESS.
c     stress - 4-day average water stress coefficient.  Fraction of material
c              surviving water stress.
c     add    - amount added to residue today if plants have started losing
c              leaf material.
c     remove - amount of plant material killed by drought today
c     rgc    - fraction of potential (unstressed) leaf biomass accrued to
c              date for first growing period (non-EPIC routines).
c     tstres - cumulative percent of material killed by water stress
c     tau    - weighted time variable for surface residue
c     tau2   - weighted time variable for submerged residue
c     grotyp - flag.  1=use EPIC plant growth routines; 0=use GCURVE & RGRCUR
c     jfrost - flag.  1=killing frost has occurred.
c     iwarn  - flag.  0=no warning generated by GCURVE this simulation year
c     plant  - set to ITYPE(NOWCRP,IPLANE) -- Should probably be changed.
c     x1     - (SCURV variable) frost damage fraction for 1st point.
c     x2     - (SCURV variable) frost damage fraction for 2nd point.
c     x3     - (SCURV variable) [negative of] temp. associated with 1st point.
c     x4     - (SCURV variable) [negative of] temp. associated with 2nd point.
c     x5     - (SCURV variable) 1st constant returned by SCURV for this plant.
c     x6     - (SCURV variable) 2nd constant returned by SCURV for this plant.
c     tmpave - Average daily temperature.  Always above zero (freezing).
c     nowres - current residue type
c     rmagy  - standing residue yesterday
c     rmogy  - residue on the ground yesterday
c     tlivey - TLIVE yesterday
c     smrati - fraction of surface residue not decomposed (non-Stott)
c     rratio - fraction of roots not decomposed (non-Stott)
c     idecom - Flag.  When IDECOM is set (1), the residue parameters
c              are updated; ie, RESUP is called.
c
c     + + + SAVES + + +
      save strss4, tstres, remove, iwarn, jfrost
c     save      oldplt,t2,ans,x5,x6
      save x5, x6, t2, strss, oldplt
c
c     + + + SUBROUTINES CALLED + + +
c     SCURV, GROW, RESUP, RTPART, GCURVE, RGRCUR, RGRAZE, RBURN, RHERB,
c     PATRIB
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + INPUT FORMATS + + +
c
c     + + + OUTPUT FORMATS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c     initialization of local variables
c
      rgc = 0.0
c
c     ---- Force use of non-EPIC plant growth
      grotyp = 0
c
c     ---- current plant type (community)
      plant = itype(nowcrp,iplane)
c
c     Initialize for EPIC plant growth routines
c
c
c     ---- average daily temperature
      tmpave = tave
      if (tmpave.lt.0.0) tmpave = 0.0
c
      nowres = 1
c
      if (rmagt(iplane).lt.0.00001) rmagt(iplane) = 0.00001
      if (rmogt(nowres,iplane).lt.0.00001) rmogt(nowres,iplane) =
     1    0.00001
c
      rmogy = rmogt(nowres,iplane)
c     ---- standing residue, minus biomass killed by herbicide or burning
      rmagy = rmagt(iplane) - dwood(iplane)
      if (rmagy.le.0.00001) rmagy = 0.00001
c
c
c     Reset to initial values for the beginning of a simulation year.
c
      if (sdate.eq.1) then
        t2 = 0.0
        strss4(iplane) = 0.0
        strrgc(plant) = first(plant)
        oldplt = rmagt(iplane)
        sdead(iplane) = 0.0
        jfrost = 0
        utiliz = 0.0
        tfood(iplane) = 0.0
        food(iplane) = 0.0
        yield(iplane) = 0.0
        pyield(iplane) = 0.0
        totsup(iplane) = 0.0
        rtotsu(iplane) = 0.0
        ideath(iplane) = 0
        iwarn = 0
        tstres(iplane) = 0.0
        remove(iplane) = 0.0
        dinner(iplane) = suppmt(iplane)
        suppmt(iplane) = 0.0
        rsuppm(iplane) = 0.0
        pscday(plant) = pscdst(plant)
        scday2(plant) = pscd2s(plant)
        sumgdd(iplane) = 0.0
      end if
c
c     calculate relative growth curve
c
      tlivey = tlive(iplane)
c
c     (EPIC GROWTH CALCULATIONS)
      if (grotyp.eq.1) then
c
c       ------ First OFE on the first day of the first simulation year
        if (iplane.eq.1.and.sdate.eq.1.and.jyear.eq.1) then
          do 10 i = 1, mxcrop
c           ---------- adjusted biomass conversion factor (kg/MJ)
            be(i) = 17.0
c           ---------- optimum temperture for plant growth (C)
            otemp(i) = 30.0
c           ---------- growing degree days at maturity (C)
            gddmax(i) = 2000.0
c           ---------- fraction of growing season when LAI begins decline
            dlai(i) = 0.5
            x1 = 0.01
            x2 = 0.95
            x3 = 5.0
            x4 = 15.0
c           ---------- constants (X5 & X6) for symetric assymtotic S-shaped frost-sensi-
c           tivity curve with 1% kill at -5 degrees C and 95% kill at -15.
            call scurv(x1,x2,x3,x4,x5(i),x6(i))
   10     continue
        end if
c
c       ------ set base air temp. to temp. required to start spring growth
c       -- XXX -- Huh? -- CRM -- 3/31/93.
        btemp(iplane) = gtemp(iplane)
c       Original Code:
c       call grow(nowcrp,iplane,ncount(iplane),idecom
c       1          ,istart,tempvar,x5,x6,tmpflg)
        call grow(nowcrp,iplane,x5(plant),x6(plant),ncount(iplane),
     1      idecom(iplane))
c
c       ------- fraction of potential (unstressed) leaf biomass accrued
c       today, minus fraction accrued yesterday (can be negative)
        growth(iplane) = 0.001
c     -- XXX -- Shouldn't this at least be based on VDMT? -- CRM -- 3/30/93.
c
c     (NON-EPIC GROWTH CALCULATIONS)
      else
c       Original Code:
c       call gcurve(sdate,iplane,plant,plive(plant),
c       1    iwarn,jfrost,t2,growth(iplane))
        call gcurve(sdate,iplane,plant,iwarn,jfrost,t2,rgc,
     1      plive(plant,iplane),growth(iplane))
      end if
c
c     -- XXX -- This seems a strange place to "range-check" TLIVE.  Seems
c     like it would make more sense AFTER "tlive" is computed.
c     CRM -- 3/30/93.
c     ---- prevent leafdrop in evergreens
      if (tlive(iplane).lt.xlive(iplane)) tlive(iplane) =
     1    xlive(iplane)
      if (tlive(iplane).lt.0.00001) tlive(iplane) = 0.00001
c
c     ---- add (ALL) material that died since yesterday, to standing dead
      if (tlivey.gt.tlive(iplane)) rmagt(iplane) = rmagt(iplane) +
     1    tlivey - tlive(iplane)
c     Note: The preceeding equation is OK.  M. Weltz & I checked it.
c     Other adjustments to rmagt all involve DEAD material.
c     TLIVE is LIVE material.  -- CRM -- 5/6/93.
c
c
c     **********************
c     * NON-STOTT DECOMP *
c     **********************
c     ---- weighted-time decomp variables calculated from antecedant moisture,
c     average temp., and carbon-to-nitrogen ratio.
c     WEPP Equation (8.3.4 -- August 89 version of User Doc.)
      tau = am(iplane) * tmpave / cn(plant)
      tau2 = am2(iplane) * tmpave / cn(plant)
c     WEPP Equation (8.3.3)
c     ---- fraction of surface residue remaining after decomposition
      smrati = (1.0-aca(plant)*tau) ** 2
      if (smrati.lt.0.95) smrati = 0.95
c     ---- fraction of roots remaining after decomposition
      rratio = (1.0-ar(plant)*tau2) ** 2
      if (rratio.lt.0.95) rratio = 0.95
c
c     ********************************************
c     * NON-EPIC four day average water stress *
c     ********************************************
c     After day 4
      if (it(iplane).gt.3) then
c       ------ delete oldest value, add current one
        strss4(iplane) = strss4(iplane) - strss(1,iplane) + wst
cWarning from ftnchek
cVariables may be used before set in module RANGE:
c       STRSS
c       ------ 4-day average
        stress = strss4(iplane) / 4.0
c       ------ save values for tomorrow
        strss(1,iplane) = strss(2,iplane)
        strss(2,iplane) = strss(3,iplane)
        strss(3,iplane) = wst
c     First 4 days
      else
        strss4(iplane) = strss4(iplane) + wst
        stress = strss4(iplane) / float(it(iplane))
        strss(it(iplane),iplane) = wst
        it(iplane) = it(iplane) + 1
      end if
c
c     reset water stress after rainfall
c
      if (rain(iplane).ge.0.008) tstres(iplane) = 0.0
      tstres(iplane) = tstres(iplane) + (1.0-stress)
      if (tstres(iplane).gt.20.0) growth(iplane) = 0.0
c
c     ---- total root mass
c     WEPP Equation (8.5.27)
      if (growth(iplane).gt.0.0) rtmass(iplane) = rtmass(iplane) +
     1    rootf(plant) * growth(iplane) * stress * rtm(nowres,iplane)
c     -- XXX -- U.D. is somewhat ambiguous, but doesn't agree.  RTM is the
c     *non-living* root mass! -- CRM -- 4/01/93.
c
c     Range test data set TEST.RNG was bombing model, because equation
c     above resulted in negative value for rtmass, then rooty, used
c     in subroutine SOIL.FOR to compute hydraulic conductivity.
c     Fix to prevent root mass going negative.  Weltz, etc. need to
c     determine why this occurs and fix better.  dcf  12/3/96
c
      if (rtmass(iplane).le.0.0)rtmass(iplane)=0.0001
c
c     ---- root decomposition
c     WEPP Equation (8.5.28)
      rtmass(iplane) = rtmass(iplane) * rratio
c
c     ---- root mass in each soil layer
      do 20 i = 1, nsl
        rooty(i,iplane) = droot(i) * rtmass(iplane)
   20 continue
c
c     ---- plant growth (non-EPIC), adjusted for drought stress
c     Original Code:
c     1  call rgrcur(wst,growth(iplane),plive(plant),ptlive(iplane),
c     2         slive,tlive(iplane),xlive(iplane),add(iplane),death,
c     3         remove(iplane),stress(iplane),rmagt(iplane),rmagy)
      if (grotyp.ne.1) call rgrcur(wst,growth(iplane),
     1    plive(plant,iplane),ptlive(iplane),xlive(iplane),stress,rmagy,
     1    slive,tlive(iplane),add(iplane),death,remove(iplane),
     1    rmagt(iplane))
c
c
c     *************************************************************
c     * Convert standing residue to fallen, because of rainfall *
c     *************************************************************
c
      if (rain(iplane).gt.0.0) then
c       ------ fraction of standing residue knocked down by rain
c       WEPP Equation (8.5.11)
c       -- XXX -- This equation is incorrect in the User Doc.  RE: 3/30/93
c       telcon w/Weltz -- CRM -- 3/30/93.
        trans = exp(-(3.5-rain(iplane)))
c       ^         (U.D. shows a '*' here.)
c       CRM -- 3/31/93.
        if (trans.gt.0.05) trans = 0.05
c
c       ------ amount of standing residue knocked down by rain
        rknock = rmagt(iplane) * trans
        if (rknock.le.rmagt(iplane)) then
          rmogt(nowres,iplane) = rmogy + rknock
          rmagt(iplane) = rmagt(iplane) - rknock
        else
          rmogt(nowres,iplane) = rmogy + rmagt(iplane)
          rmagt(iplane) = 0.0
        end if
      else
c       -- XXX -- Why bother with this? -- CRM -- 3/31/93.
        rmogt(nowres,iplane) = rmogy
      end if
      if (rmagt(iplane).lt.0.00001) rmagt(iplane) = 0.00001
      if (rmogt(nowres,iplane).lt.0.00001) rmogt(nowres,iplane) =
     1    0.00001
c
c
c     Yield is calculated as total standing biomass at begining of the
c     year plus all new biomass produced in the year.  It is used to
c     calculate percent utilization of forage by livestock and is useful
c     in establishing correct stocking rates.
c
      if (slive.gt.0.0) then
        yield(iplane) = pyield(iplane) + slive
      else
        yield(iplane) = pyield(iplane)
      end if
c
c     ---- total standing biomass (live, dead, and wood)
      vdmt(iplane) = tlive(iplane) + rmagt(iplane) + dwood(iplane)
      if (vdmt(iplane).lt.0.00001) vdmt(iplane) = 0.00001
c
c     If temperature is < critical temperature kill deciduous and
c     annual plants.  Evergreen plants retain leaves.
c
      if (jfrost.eq.1) then
        rmagt(iplane) = rmagy + tlive(iplane) - xlive(iplane)
        tlive(iplane) = xlive(iplane)
        if (rmagt(iplane).lt.0.00001) rmagt(iplane) = 0.00001
      end if
c
c     ---- grazing
      if (grazig(iplane).gt.0.0.and.access(iplane).gt.0.0) then
        call rgraze(plant)
        if (yield(iplane).gt.0.0.or.oldplt.gt.0.0) then
          utiliz = tfood(iplane) / (yield(iplane)+oldplt)
        else
          utiliz = 0.0
        end if
      end if
c
c     ---- burning (if there is adequate material for fuel)
      if (vdmt(iplane).gt.0.08.and.sdate.eq.jfdate(iplane)) call
     1    rburn(plant)
c
c     ---- herbicide
      if (sdate.eq.ihdate(iplane)) call rherb(plant)
c
c     ---- update standing and fallen residue following herbicide application
      if (sdead(iplane).gt.0.0) then
c       WEPP Equation (8.6.26)
c       -- XXX -- Seem to be adding DECAYED material to that on the ground.
c       CRM -- 4/1/93.
c       ------ add decayed leaves to ground litter
        rmogt(nowres,iplane) = rmogt(nowres,iplane) + sdead(iplane) * (
     1      1.0-smrati)
c       **********************************************************
c       * Note: Assumes standing residue decomposes at same rate *
c       *       as that on the ground. -- CRM -- 3/30/93.        *
c       **********************************************************
c       ------ remove decayed leaves from standing residue
        sdead(iplane) = sdead(iplane) * smrati
        if (dwood(iplane).gt.0.0) then
c         -------- add decayed twigs to ground litter (decompose at 25% the
c         rate of leaves)
c         WEPP Equation (8.6.27)
c         -- XXX -- This equation is not correct in the User Guide.
          rmogt(nowres,iplane) = rmogt(nowres,iplane) + dwood(iplane) *
     1        (1.0-smrati) / 4.0
          dwood(iplane) = dwood(iplane) * (1.0-(1.0-smrati)/4.0)
c
        end if
      end if
c
c     ---- total standing biomass
      vdmt(iplane) = tlive(iplane) + rmagt(iplane) + dwood(iplane) +
     1    sdead(iplane)
      if (vdmt(iplane).lt.0.00001) vdmt(iplane) = 0.00001
c
c     ---- calculate herbacious plant height, canopy & ground cover etc
c     Original Code:
c     call patrib(iplane,plant,tlive(iplane),ptlive(iplane),tmpave,
c     1            oldplt,nowres,smrati,tfood(iplane),dwood(iplane))
      call patrib(iplane,plant,tlive(iplane),tmpave,nowres,smrati,
     1    dwood(iplane),ptlive(iplane))
c
c
c     ***********************************
c     * detailed output for rangeland *
c     ***********************************
c
c     if (jyear.eq.1.and.sdate.eq.1.and.iplane.eq.1) then
c
c     open statements now in infile
c
c     30    write (*,1100)
c     read (*,1000,err=30) ans
c     if (ans.eq.'Y'.or.ans.eq.'y') then
c
c     ... open file for range output ...
c     ... unit = 44, status = 1 (unknown)... plant output
c     ... unit = 45, status = 1 (unknown)... animal grazing output
c
c     detailed plant growth output
c
c     open (unit=44,file='range.out',status='unknown')
c     write (44,1200)
c
c     detailed grazing animal output affects on vegetation
c
c     open (unit=45,file='animal.out',status='unknown')
c     write (45,1300)
c     end if
c     end if
      if (rngout.eq.2) then
c       write(44,2004)sdate,rgc(iplane),growth(iplane),vdmt(iplane),
        if(rngplt.eq.2)
     1      write (44,1000) sdate, rgc, growth(iplane), vdmt(iplane),
     1      tlive(iplane), slive, rmagy, sdead(iplane),
     1      rmogt(nowres,iplane)
        if (grazig(iplane).eq.1) then
          if(rnganm.eq.2)
     1      write (45,1100) sdate, yield(iplane), utiliz, tfood(iplane),
     1        food(iplane), suppmt(iplane), totsup(iplane),
     1        rsuppm(iplane), rtotsu(iplane)
        end if
      end if
c
      return
c
c     labels 1000, 1100, 1200, and 1300 have not been referenced
c     12-16-93 09:15am  sjl
c
c     1000 format (a)
c     1100 format (/,'  Do you want rangeland outputs (y/n)? -->',
c     1    '                   ')
c     1200 format (' sdate    rgc   growth     vdmt     tlive    slive',
c     1    'rmagy    sdead    rmogt')
c     1300 format (' date   yield   utiliz    tfood',
c     1    '     food   suppmt   totsup    rsupp    rtotsupp  ')
 1000 format (1x,i5,2x,f6.4,2x,f6.4,6(2x,f7.4))
 1100 format (1x,i3,5(1x,f8.4),1x,f8.3,1x,f8.4,1x,f9.3)
      end
