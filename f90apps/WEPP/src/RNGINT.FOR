      subroutine rngint(ncrop,nowcrp)
c
c     NOTE: BECAUSE "GDAY" IS USED TO DETERMINE WHICH EQUATION IS USED
c           FOR DEFAULT VALUES OF "TLIVE", "TILLAGE" WHICH READS GDAY
c           *MUST* ALWAYS BE CALLED BEFORE "RNGINT".
c
c   ***************************************************************
c   * NOTE: ALL SOURCE FILES THAT REFERENCE "CRINPT1.INC" MUST BE *
c   *       COMPILED WITH THE *NEW* CRINPT1.INC TO WORK WITH THIS *
c   *       FILE.                                                 *
c   ***************************************************************
c
c   ***************************************************************
c   * NOTE: ALL SOURCE FILES THAT REFERENCE "CRINPT3.INC" MUST BE *
c   *       COMPILED WITH THE *NEW* CRINPT3.INC TO WORK WITH THIS *
c   *       FILE.                                                 *
c   ***************************************************************
c
c   ***************************************************************
c   * NOTE: ALL SOURCE FILES THAT REFERENCE "CROUT.INC" MUST BE   *
c   *       COMPILED WITH THE *NEW* CROUT.INC TO WORK WITH THIS   *
c   *       FILE.                                                 *
c   ***************************************************************
c
c     Note: Need definitions of PSCDST & PSCD2S in include file CRINPT2.INC.
c           CRM -- 4/09/93.
c
c     + + + PURPOSE + + +
c     Initializes variables used in RANGE, when land use is rangeland.
c     Called by OFE from CONTIN.
c
c     Called from CONTIN
c     Author(s): Weltz, Meyer
c     Reference in User Guide: Chapter 8
c
c     Changes:
c          1) Deleted references to common block UPDATE.
c          2) Deleted local variables SHEEP & NOWRES.
c          3) Added SPAI, & TPAI to CRINPT3.INC permit single initial
c             calculation of SPAI & TPAI in RNGINT, rather than daily
c             computation in PATRIB.  This required a change in the
c             logic: a loop to initialize each plant type.  Deleted
c             the appropriate code in PATRIB.
c          4) Deleted generic SAVE of all variables.
c          5) Dimensioned rooty(MXNSL) to rooty(MXNSL,MXPLAN) in CROUT.INC.
c             Re: 5/6/93 conversation w/Weltz. -- CRM
c          6) Subscripts were reversed:
c                 if(gday(i,1,iplane) .gt. 0) then
c             changed to:
c                 if(gday(1,i,iplane) .gt. 0) then
c          7) Did a major overhaul relative to the variable PLIVE.  In
c             its original state, PLIVE would not handle more than one
c             OFE correctly since it is modified if burning or herbicide
c             are used.  It also would not supply a default value for a
c             plant that was grazed on one OFE and not grazed on another.
c             Since modules need to be able to swap between plant types
c             on an OFE during a simulation, PLIVE(NTYPE) was redimen-
c             sioned to PLIVE(NTYPE,IPLANE).  This permits an OFE to be
c             altered without affecting the other OFE's.  CRINPT1.INC
c             was altered to accommodate this.  -- CRM -- 5/13/93.
c          8) PROOT was altered analogous to PLIVE. -- CRM -- 5/14/93.
c          9) ROOT(MXPLAN) was removed from common CROUT.INC and made
c             a local variable only - since it is computed and used now
c             only in this routine.  dcf  5/19/93
c         10) statement setting rdf(plant) to 0.43 moved to top of
c             code - otherwise it is undefined at first point of use.
c         11) variable rdf was a constant value of 0.43, and was only used
c             in this subroutine.  Thus it was removed from common CRINPT1.INC
c             and uses in RNGINT were changed to values of 0.43.  dcf  5/21/93
c
c     Version: This module recoded from WEPP Version 92.25.
c     Date recoded: 04/07/93 - 4/09/93.
c                   05/06/93 - 5/13/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pntype.inc'
      include 'pmxnsl.inc'
      include 'ptilty.inc'
      include 'pmxres.inc'
      include 'pmxpln.inc'
      include 'pmxgrz.inc'
      include 'pmxcrp.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxhil.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer ncrop, nowcrp
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ncrop  - number of input crops (rangeland plant communities)
c     nowcrp - current crop
c
c     + + + COMMON BLOCKS + + +
      include 'ccrpgro.inc'
c      modify: ytn
c       write: daymin
c
      include 'ccrpvr1.inc'
c       write: rtm(mxres,mxplan)
c
      include 'ccrpvr2.inc'
c     modify: vdmt(mxplan), cf(ntype)
c
      include 'ccrpvr3.inc'
c      modify: bbb(ntype)
c
      include 'ccover.inc'
c       write: canhgt(mxplan),inrcov(mxplan),rilcov(mxplan)
c      modify: cancov(mxplan),gcover(mxplan)
c
      include 'ccrpout.inc'
c       write: lai(mxplan), rtd(mxplan)
c      modify: rtmass(mxplan),
c
      include 'ccrpprm.inc'
c        read: itype(mxcrop,mxplan)
c
      include 'cends.inc'
c      modify: rspace(mxplan) (?)
c
      include 'crinpt1.inc'
c      modify: ffk(ntype),plive(ntype,mxplan),proot(ntype,mxplan),
c              yield(mxplan)
c       write: pyield(mxplan)
c
c     include 'crinpt1a.inc'
c
      include 'crinpt2.inc'
c
      include 'crinpt3.inc'
c      modify: spai(ntype), tpai(ntype), ghgt(ntype), basden
c
      include 'crinpt4.inc'
c      modify: dwood(mxplan)
c       write: ptlive(mxplan),first(ntype),xlive(mxplan)
c
      include 'crinpt5.inc'
c        read: tdiam(ntype), tcoeff(ntype), tpop(ntype), thgt(ntype)
c
      include 'crout.inc'
c      modify: tlive(mxplan),slive
c       write: droot(mxnsl),rooty(mxnsl,mxplan)
c
      include 'crinpt6.inc'
c      modify: bcover(mxplan), rufcov(mxplan)
c
      include 'cstruc.inc'
c        read: iplane
c
      include 'cwater.inc'
c       write: watstr(mxplan)
c
      include 'cwint.inc'
c        read: deglat
c
c     + + + LOCAL VARIABLES + + +
      real gpai, totpai, tarea, prevrt, ch, h
      real tmpvr2, tmpvr3, tmpvr4, tmpvr5, tmpvr6, root(mxnsl)
      integer plant, i, j
c
c     + + + LOCAL DEFINITIONS + + +
c     gpai   - total projected area from the side, intercepted for grass &
c              herbaceous plants, per 100 meter transect; ie, vertical area
c              "blocked" by plants, measured perpendicular to the slope.
c              Used for snowdrift calculations.
c     totpai - total projected area from the side, for all plants combined
c     tarea  - area of the vertical rectangle circumscribed around the
c              largest plant type present (trees, shrubs, & herbs).
c     prevrt - root mass in the previous layer (one directly above)
c     ch     -
c     h      -
c
c     + + + END SPECIFICATIONS + + +
c
c
      plant = itype(nowcrp,iplane)
c
c     ---- root distribution coefficient for mass by depth
c     rdf(plant) = 0.43
c     NOTE - problems with use of RDF in code below - and since it is
c     always set to a constant value of 0.43 - change from use
c     of an array to a numerical constant in the appropriate
c     equations.    dcf  5/21/93
c
c     ---- Julian date peak standing crop occurs, for first growth peak
      pscdst(plant) = pscday(plant)
c     ---- Julian date peak standing crop occurs, for second growth peak
      pscd2s(plant) = scday2(plant)
c
c     From a user-supplied value of PLIVE for each plant, generate
c     an initial value of PLIVE for each plant and each OFE.  This
c     value may be modified later by RHERB or RBURN, and the plant
c     type may change on this OFE during the simulation.  Also, if
c     the user entered a zero for the current plant, estimate a
c     starting value of PLIVE which depends on whether the OFE is
c     grazed in the first year or not.
c
c     *** L1 IF ***
c     ---- Execute this code the first time it is encountered (OFE #1).
      if (iplane.eq.1) then
c       *** BEGIN L2 DO-LOOP ***
c       Initialize PLIVE values for each plant by OFE.
        do 30 i = 1, ncrop
c         Next line added because NCROP is not readily available to RNGINT.
          if (plive(i,1).ge.0.0) then
c
c           *** L3 IF ***
c           -------- user supplied an initial value
            if (plive(i,1).ne.0.0) then
              do 10 j = 1, mxplan
c               Next line added because NELMT is not readily available to RNGINT.
                if (plive(i,1).gt.0.0) then
c                 ------------ Set PLIVE for each OFE to user-input value.
                  plive(i,j) = plive(i,1)
c                 ------------ maximum potential root biomass
c                 (WEPP Equation 8.5.25)
c                 proot(i,j) = root10(i) / (10.0**rdf(i))
c                 NOTE - assuming that value for rdf=0.43 - using this to simplify equation
c                 dcf 5/21/93
c
                  proot(i,j) = root10(i) / 2.6915348
                end if
   10         continue
c
c           *** L3 ELSE ***
c           -------- model must generate initial value...
            else
c             ---------- set PLIVE for this plant for *all* OFE's
              do 20 j = 1, mxplan
c               Next line added because NELMT is not readily available to RNGINT.
                if (plive(i,1).gt.0.0) then
c                 ---------- check for grazing on this OFE the first year.
                  if (gday(1,i,j).gt.0) then
c                   -------------- grazed areas
c                   (WEPP Equation 8.5.6)
                    plive(i,j) = 0.3569 + 0.0036 * pptg(i)
c                 ------------ non-grazed areas
                  else
c                   (WEPP Equation 8.5.7)
                    plive(i,j) = 0.7723 + 0.0030 * pptg(i)
                  end if
c                 ------------ maximum potential root biomass
c                 (WEPP Equation 8.5.25)
c                 proot(i,j) = root10(i) / (10.0**rdf(i))
c                 NOTE - assuming that value for rdf=0.43 - using this to simplify equation
c                 dcf 5/21/93
c
                  proot(i,j) = root10(i) / 2.6915348
                end if
   20         continue
c           *** L3 ENDIF ***
            end if
          end if
c
c       *** END L2 DO-LOOP ***
   30   continue
c     *** L1 ENDIF ***
      end if
c
c
c     ---- Assume rooting depth (RTD) equals depth to bottom of last soil layer
      rtd(iplane) = solthk(nsl(iplane),iplane)
c
c
c     *******************************************
c     * Calculate root distribution by depth. *
c     *******************************************
c
c     ---- maximum potential root biomass
c     (WEPP Equation 8.5.25)
c     proot(plant) = root10(plant) / (10.0**rdf(plant))
c
      prevrt = 0.0
      do 40 i = 1, nsl(iplane)
c       ------ cumulative total roots to bottom of current layer
c       (WEPP Equation 8.5.24)
        rtmass(iplane) = rootf(plant) * proot(plant,iplane) * (100.0*
     1      solthk(i,iplane)) ** 0.43
c
c       1                         (100.0*solthk(i,iplane))**rdf(plant)
c       NOTE - since RDF is only used in SR RNGINT - removed from common,
c       and since value is always 0.43 for all plant types - use
c       this.  Problems in do 8 loop with values of rdf(i)
c       dcf 5/21/93
c
c       ------ root mass in current soil layer today
        root(i) = rtmass(iplane) - prevrt
c       ------ save root mass to bottom of this layer (top of next layer)
        prevrt = rtmass(iplane)
   40 continue
c
      if (rtmass(iplane).gt.0.0) then
        do 50 i = 1, nsl(iplane)
c         -------- fraction of rootmass in each soil layer (constant throughout sim'n.)
c         (WEPP Equation 8.5.26)
          droot(i) = root(i) / rtmass(iplane)
c         -------- initialize root mass yesterday
          rooty(i,iplane) = root(i)
   50   continue
      else
c       ------ ensure non-negative rootmass
        rtmass(iplane) = 0.0
      end if
c
c     ---- fraction of growth permitted by water stress
      watstr(iplane) = 1.0
c     ---- new plant growth today (min_fraction_live_biomass * max_amt_live_biomass)
      slive = rgcmin(plant) * plive(plant,iplane)
c     ---- total live plant material today
      tlive(iplane) = slive
c     ---- previous day's total live plant material
      ptlive(iplane) = tlive(iplane)
c     ---- total above ground plant production for a simulation year
      yield(iplane) = slive
c     ---- daily net primary plant production
      pyield(iplane) = yield(iplane)
c     ---- previous year's date of initial growth
      first(plant) = strrgc(plant)
c     ----  standing dead biomass left after burning or herbicide
      dwood(iplane) = wood(plant) * rmagt(iplane)
c     ---- evergreen leafy component
      xlive(iplane) = slive
c
c     *************************************************************
c     * NOTE: New equations to calculate  canhgt, cancov, rcov,   *
c     *       rilcov, lai for first day of simulation, should be  *
c     *       added to this section when plant growth subroutine  *
c     *       is updated.                                         *
c     *************************************************************
c
c     ---- total above ground dry weight
      vdmt(iplane) = slive + rmagt(iplane)
c     ---- leaf area index
      lai(iplane) = tlive(iplane) * aleaf(plant)
c
c     --- If this is the first OFE, compute SPAI & TPAI for ALL plant groups.
      if (iplane.eq.1) then
        do 60, i = 1, ncrop
c         -------- projected area for shrubs (remains constant throughout simulation)
c         (WEPP Equation 8.5.17)
          if (sdiam(i).gt.0.0) then
            tmpvr4 = sdiam(i) * scoeff(i) * spop(i)
c           ---------- prevent horizontal measure from exceeding 100 m total
            if (tmpvr4.gt.100.0) tmpvr4 = 100.0
            spai(i) = shgt(i) * tmpvr4
          else
            spai(i) = 0.0
          end if
c
c         -------- projected area for trees (remains constant throughout simulation)
c         (WEPP Equation 8.5.17)
          if (tdiam(i).gt.0.0) then
            tmpvr5 = tdiam(i) * tcoeff(i) * tpop(i)
            if (tmpvr5.gt.100.0) tmpvr5 = 100.0
            tpai(i) = thgt(i) * tmpvr5
          else
            tpai(i) = 0.0
          end if
   60   continue
      end if
c
c     ---- projected area of herbaceous plants (changes daily)
c     (WEPP Equation 8.5.17)
      if (gdiam(plant).gt.0.0) then
        tmpvr6 = gdiam(plant) * gcoeff(plant) * gpop(plant)
        if (tmpvr6.gt.100.0) tmpvr6 = 100.0
        gpai = ghgt(plant) * tmpvr6
      else
        gpai = 0.0
      end if
c
c     **********************************************************************
c     * NOTE: We assume NO OVERLAP between the grass, shrubs, and trees.   *
c     *       Since the current purpose of TOTPAI is solely to compute     *
c     *       a weighted average for canopy height, adding the complexity  *
c     *       of considering overlap did not seem worth the cost.          *
c     *       CRM -- 4/7/93.                                               *
c     **********************************************************************
c     ---- total projected area for vegetation
c     (WEPP Equation 8.5.16)
      totpai = spai(plant) + gpai + tpai(plant)
c
c     ---- vertical area required for largest plant type, using 100 meter transect.
c     (WEPP Equation 8.5.18)
      tarea = max(thgt(plant),shgt(plant),ghgt(plant)) * 100.0
c
c     ---- basal density of plants; ie, the fraction of the "vertical rectangle"
c     (re: TAREA) covered by plants
c     (WEPP Equation 8.5.15)
      if (tarea.gt.0.0) then
        basden = totpai / tarea
      else
        basden = 0.0
      end if
c
c     ---- canopy height, a weighted mean.
c     (WEPP Equation 8.5.13)
      if (basden.gt.0.0) then
        canhgt(iplane) = (ghgt(plant)*gpai+shgt(plant)*spai(plant)+
     1      thgt(plant)*tpai(plant)) / totpai
      else
        canhgt(iplane) = 0.0
      end if
c
c     ... calculate rill spacing
c     (WEPP Equation 8.5.19)
c     rspace(iplane)=100.0/(spop(plant)+gpop(plant)+tpop(plant)+1)
c     if(rspace(iplane) .gt. 5.0) rspace(iplane)=5.0
c     if(rspace(iplane) .lt. 0.5) rspace(iplane)=0.5
c
c     ******************************************************************
c     * NOTE: To calculate grass height requires height coefficient    *
c     *       "bbb", which is estimated from the following regression  *
c     *       equations.                                               *
c     ******************************************************************
c     ---- vegetative dry matter at maturity
      vdmmax(iplane) = plive(plant,iplane) + rmagt(iplane) -
     1    dwood(iplane)
c     ----  parameter value for canopy height equation
      if (bbb(plant).ge.0.0) then
        if (bbb(plant).lt.1.5) then
          bbb(plant) = 2.0 * (vdmmax(iplane)**(-1.0)) + 0.3031455e-06
        else if (bbb(plant).lt.2.5) then
c         Original Code:
c         bbb(plant)=4.0*vdmmax(iplane)**(-1.0)+.23155e-05
          bbb(plant) = 4.0 / vdmmax(iplane) + 0.23155e-05
        else
c         Original Code:
c         bbb(plant)=6.0*vdmmax(iplane)**(-1.0)-.1694262e-04
          bbb(plant) = 6.0 / vdmmax(iplane) - 0.1694262e-04
        end if
      end if
c
c     ---- calculate residue cover from residue mass
c     (WEPP Equation 8.5.20)
c     XXX - commented out by Kidwell/Weltz  3/95   dcf
c     tmpvr2 = 1.085 * (1.0+1.069*exp(-12.61*rmogt(1,iplane))) - 0.5838
c     cf(plant) = 10.0 ** (tmpvr2)
c     (WEPP Equation 8.5.20)
c     rescov(iplane) = 1.0 - exp(-cf(plant)*rmogt(1,iplane))
c
c     **Added by Kidwell 5/25/95**
      if (rescof(iplane) .le. 0.0001) then
        if (rescov(iplane).le.0.0 .or. rmogt(1,iplane).le.0.0) then
                cf(plant) = -63.9
                rescov(iplane) = 0.0
                rmogt(1,iplane) = 0.0
        else
                tmpvr2 = log(1.0 - rescov(iplane))
                cf(plant) = tmpvr2/rmogt(1,iplane)
        end if
      else if (rescof(iplane) .gt. 0.0) then
        if(rescov(iplane).le.0.0 .or. rmogt(1,iplane).le.0.0) then
                rescov(iplane) = 0.0
                rmogt(1,iplane) = 0.0
        else
                cf(plant) = rescof(iplane)
        end if
      end if
c
c     ---- calculate canopy cover from plant mass corresponding to 100% cover (COLD)
c     (WEPP Equation 8.5.22)
c     Original Code:
c     ffk(plant) =21.39148-54.90758*cold(plant)+61.11016*cold(plant)
c     1  **2-30.44471*cold(plant)**3+5.561994*cold(plant)**4
c     XXX - commented out next 3 lines (Kidwell/Weltz) 3/95  dcf
c     tmpvr3 = cold(plant) ** 2
c     ffk(plant) = 21.39148 - 54.90758 * cold(plant) + 61.11016 * tmpvr3
c    1    - 30.44471 * cold(plant) * tmpvr3 + 5.561994 * tmpvr3 ** 2
c     -- XXX -- Need a second equation number in the User Doc.
c     (WEPP Equation 8.5.22)
c     XXX Next 2 lines commented out by Kidwell/Weltz  3/95  dcf
c     cancov(iplane) = 1.0 - exp(-ffk(plant)*vdmt(iplane))
c     if (cancov(iplane).lt.0.0) cancov(iplane) = 0.0
c     **Added by Kidwell 5/25/95**
      if (cancof(iplane) .le. 0.0001) then
        if (cancov(iplane).le.0.0 .or. vdmt(iplane).le.0.0) then
                ffk(plant) = -31.5
                cancov(iplane) = 0.0
                vdmt(iplane) = 0.0
        else
                tmpvr3 = log(1.0 - cancov(iplane))
                ffk(plant) = tmpvr3/vdmt(iplane)
        end if
      else if (cancof(iplane) .gt. 0.0) then
        if(cancov(iplane).le.0.0 .or. vdmt(iplane).le.0.0) then
                cancov(iplane) = 0.0
                vdmt(iplane) = 0.0
        else
                ffk(plant) = cancof(iplane)
        end if
      end if

c
c     -- XXX -- Can't find this equation in the User Doc. -- CRM -- 4/07/93.
c     ---- basal plant cover (ground area occupied by stalks, stems, trunks)
c     XXX Next 5 lines commented out by Kidwell/Weltz  3/95   dcf
c     if (wood(plant).gt.0.0) then
c       bcover(iplane) = 0.3354242 * cancov(iplane)
c     else
c       bcover(iplane) = 0.429118 * cancov(iplane)
c     end if
c
c     -- XXX -- Can't find this equation in the User Doc. -- CRM -- 4/07/93.
c     ---- calculate hydraulic roughness cover from basal cover and moss & lichens
c     XXX Next 2 lines changed by Kidwell/Weltz  3/95   dcf
c     rufcov(iplane) = bcover(iplane) + crypto(iplane)
c     if (rufcov(iplane).lt.0.0) rufcov(iplane) = 0.0
      rufcov(iplane) = bascov(iplane)*fbasr(iplane) +
     1                 crycov(iplane)*fcryr(iplane)
c
c     ---- total ground cover (residue, rocks & gravel, stems and moss & lichens)
c     XXX Next line changed by Kidwell/Weltz  3/95   dcf
c     gcover(iplane) = rescov(iplane) + wcf(iplane) + rufcov(iplane)
      gcover(iplane) = rescov(iplane) + rokcov(iplane) + bascov(iplane)
     1                 + crycov(iplane)
      if (gcover(iplane).gt.1.0) gcover(iplane) = 1.0
c
c     -- XXX -- Need an equation number in the User Doc.
c     Following changed by Mary Kidwell 3/95  dcf
c     rilcov(iplane) = rescov(iplane) + wcf(iplane)
c
c     -- XXX -- Need an equation number in the User Doc.
c     inrcov(iplane) = gcover(iplane)
c
c     --- total rill cover
c
      rilcov(iplane) = rescov(iplane)*fresr(iplane) +
     1                 rokcov(iplane)*frokr(iplane) +
     1                 bascov(iplane)*fbasr(iplane) +
     1                 crycov(iplane)*fcryr(iplane)
c
c     ---- total interrill cover
c
      inrcov(iplane) = rescov(iplane)*fresi(iplane) +
     1                 rokcov(iplane)*froki(iplane) +
     1                 bascov(iplane)*fbasr(iplane) +
     1                 crycov(iplane)*fcryi(iplane)
c
c     ---- initial value of dead root mass for day 1 and when management changes
      rtm(1,iplane) = rtmass(iplane)
c
c     -- XXX -- Is DAYMIN needed here?  It is computed in WATBAL before calling
c     RANGE. -- CRM -- 5/14/93.
c     calculate minimum day length for the input latitude
c
c     original code 10/1/91
c     ytn = tan(deglat / 58.09)
c
c     code changed because of problem with Lahey compiler and
c     tangent function when running on AT&T 6300 machine  dcf
c
c     ---- function of latitude - used in day length calculation (EPIC 2.194)
      ytn = (sin(deglat/58.09)) / (cos(deglat/58.09))
      ch = 0.4349 * abs(ytn)
      if (ch.ge.1.0) then
        h = 0.0
      else
        h = acos(ch)
      end if
c     ---- minimum day length
      daymin = 7.72 * h
c
      return
      end
