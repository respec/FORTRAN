      subroutine grow(nowcrp,iplane,x5,x6,ncount,idecom)
c
c     + + + PURPOSE + + +
c     Simulates plant growth processes that are common to all
c     vegetation; ie, annual, perennial, and range plants.
c
c     Computes growing degree days (gdd), cumulative gdd (sumgdd),
c     canopy height & cover (canhgt & cancov), total above ground dry
c     weight (vdmt), root mass in each soil layer (by calling RTPART),
c     root depth (rtd), leaf area index (lai), total grazable
c     forage (tlive), and residue (rmogt).  Adjusts harvest index
c     for moisture stress and temperature deficit (hia).
c
c     Called from PTGRA, PTGRP, & RANGE
c
c     Author(s): Arnold, Weltz, Ferris, Meyer
c     Reference in User Guide: Chapter 8.  Also see "EPIC -- Erosion/
c            Productivity Impact Calculator". 1990. USDA-ARS Tech. Bul.
c            No. 1768. Sharpley, A.N. and J.R. Williams
c
c     Changes:
c             1) FPHU was computed a second time before being used.
c                Unnecessary.
c             2) The expression:
c                             tlive(iplane) - dec - dec *(1-reg)
c                was changed to:
c                             tlive(iplane) + dec * (reg - 2.0)
c             3) In the temperature stress calculation (temstr),
c                GDD was substituted for it's re-calculation:
c                             (tave - btemp(itype(nowcrp, iplane))
c             4) Neither of common blocks RIDGE or CRPGRO had
c                definitions for their variables.  This was
c                corrected.
c             5) RTMASY was being SAVE'd even though it was being
c                re-computed each time GROW was executed.  The SAVE
c                was eliminated.
c   NOTE - save added back in because other part of if-else logic
c          did not compute RTMASY - leaving it undefined.  ALSO
c          RTMASY now dimensioned to MXPLAN and tracked by IPLANE
c          and earlier uses for multiple OFEs were incorrect.
c          dcf  12/10/92
c
c             6) Since ISENES needs to be tracked by OFE, ISENES was
c                changed to ISENES(MXPLAN).
c             7) In V-91.50, in EPIC Equation 2.252, the IF was commented
c                out.  Jeff Arnold put it back in, making it consistent
c                with the EPIC documentation.  It does make some minor
c                differences in the output from TEST1, but I also put
c                it IN, because without it the value of FHU get really
c                squirrelly -- it goes negative when FPHU gets less
c                than 0.3 or greater than 0.9.  -- CRM -- 6/16/92.
c             8) The line:
c                   if(fphu.gt.1.0) fphu = 1.0
c                was moved from immediately below where "residue type
c                is set to the current crop on first day of senescence",
c                to where FPHU is first computed.  After change #7,
c                this made no difference -- before then, it did.
c             9) The ISENES(MXPLAN) used in this routine was changed
c                the one in CSENES.INC, like in version 92.25.
c            10) Local variable HIAWS was eliminated from the equation
c                to "compute harvest index adjusted for water stress
c                (hia)".  This would in some cases increase precision
c                by not causing intermediate results calculated for
c                HIAWS to be rounded and stored in a memory location,
c                but would permit them to remain in a register, which
c                higher precision.  As expected, a few minor differences
c                in outputs were observed.  This equation was further
c                computationally simplified.
c            11) EPIC equation 2.251 corrected.  In the original code
c                HIA was being deleted from the calculation.  Also had
c                a missing set of parentheses.  This changes the results
c                from TEST1 and TEST2.  Re: 6/26/92 & 6/29/92 telcon
c                J. Wms.
c            12) EPIC equation 2.206 corrected.  It had an extra term
c                (hufh) on the right side of the equation.
c            13) CANCMX was not originally being SAVE'd.  It is now.
c            14) Redundant calculation of FPHU eliminated from code to
c                calculate frost effect on rangeland.
c            15) Variable HUFHY added when EPIC Equation 2.205 was
c                corrected.  Re: 6/26/92 telcon J. Wms.
c            16) Local variable NSENES was eliminated.  In it's place
c                ISENES (which was previously used as a zero/non-zero
c                flag) is now being incremented to contain the count
c                of the days since senescence began.
c            17) Local variable NOWRES eliminated.
c            18) The name of argument IDECOMP was changed to IDECOM,
c                since FORTRAN ignores anything past the first 6
c                letters in a variable name.
c            19) In order to conform to the WEPP Coding Convention, the
c                5th - 8th arguments moved after the 2nd one, and the
c                last argument moved after the 3rd one.
c            20) Canopy cover equation (WEPP 8.2.4) was incorrect for
c                non-senescent plants.
c            21) In the section of code executed when senescence has
c                occurred, the code was split into 2 parts: one executed
c                on cropland, and one executed on rangeland.  I pointed
c                out to Jeff A. that this was not consistent with the
c                EPIC documentation, and we changed it: one part for
c                cropland annuals, and the other for everything else
c                (rangeland and cropland perennials).  Quite predictably,
c                this resulted in output changes for my cropland preren-
c                nial (alfalfa) runs. -- CRM -- 6/30/92.
c            22) In EPIC Equation 2.255, changed TLIVE to VDMT; ie,
c                   dec = 0.5 * tlive(iplane) * (1 - fphu) * max(fhr,frst)
c                became
c                   dec = 0.5 * vdmt(iplane) * (1 - fphu) * max(fhr,frst)
c            23) EPIC Equation 2.254 was corrected from:
c                       frst=-100 * (tmin/(tmin+exp(x5+x6*tmin)))
c                     to:
c                       frst=100.0 * (tmin/(tmin-exp(x5+x6*tmin)))
c                       if(frst .gt. 1.0) frst = 1.0
c                (FRST *can* get as large as 100....)
c
c                NOTE: EPIC Equations 2.205, 2.206, and 2.251 were
c                      corrected following 6/26/92 and 6/29/92 telephone
c                      calls from J. Arnold to J. Williams.
c
c            24) Deleted dummy arguments TMPFLG, ISTART, & TRTMAS from
c                GROW, and its calls from PTGRA, PTGRP, and RANGE.
c            25) Now use different canopy cover equations for cropland
c                depending on whether crop is annual or perennial -
c                now consistent with SR INITGR.   dcf  4/93
c            26) Added code for use with macroporosity adjustment in
c                SR INFPAR to indicate management type (mantyp in
c                CGCOVR.INC).   dcf  2/93
c            27) Added code to use input extinction coefficient instead
c                of default value of 0.65 following conversation with
c                Jeff Arnold.  Input file/reads also changed. dcf 12/92
c            28) Fixed root mass computation so that it is computed now
c                as a function of RSR times the change in above-ground
c                growth.  Without this change, perennials under both
c                cutting and grazing management have incorrect root
c                mass amounts.   dcf  11/18/93
c            29) Altered root depth computation for perennials, since
c                the old equation uses the ratio of (basically)
c                SUMGDD/GDDMAX - and sumgdd gets reset with cuttings -
c                thus root depth incorrectly is decreased at cuttings.
c                dcf  11/18/93
c
c     Version: This module recoded from WEPP version 91.50, with some
c              of the "stuff" from 92.25 and beyond.
c     Date recoded: 06/08/92 - 7/06/92.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxcrp.inc'
      include 'pmxcut.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nowcrp, iplane, ncount, idecom
      real x5, x6
c
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nowcrp - Index of the current crop.
c     iplane - Index of the current OFE.
c     x5     - One of two constants computed to force the crop's S-shaped
c              frost sensitivity curve through two specified  min.-temp.
c              & freeze-damage points.  See in-line documentation in SCURV.
c              Also see EPIC Doc., Eqn. 2.254.
c     x6     - The other SCURV constant.  (See X5 above.)
c     ncount - Counter for number of days after senescence; ie, when
c              canopy cover starts to decay.  Note that NCOUNT is sub-
c              scripted in the routines calling GROW, and not in GROW.
c              This does not cause FORTRAN a problem since GROW only
c              needs to access the *array element* at that *address*,
c              not the *entire array*.
c     idecom - Flag.  When IDECOM is set (1), the residue parameters
c              are updated; ie, RESUP is called.
c
c     + + + COMMON BLOCKS + + +
      include 'ccover.inc'
c       read: lanuse(mxplan)
c     modify: cancov(mxplan)
c      write: canhgt(mxplan)
c
      include 'ccrpgro.inc'
c       read: be, otemp(ntype), hi(ntype), daymin, daylen, extnct(ntype)
c     modify: hia(mxplan), vdmx(mxplan)
c
      include 'ccrpout.inc'
c     modify: rtmass(mxplan), lai(mxplan)
c      write: rtd(mxplan)
c
      include 'ccrpvr1.inc'
c      write: rmogt(mxres,mxplan), rmagt(mxplan)
c
      include 'ccrpvr2.inc'
c     modify: vdmt(mxplan)
c
      include 'ccrpvr3.inc'
c       read: fgs(mxplan), hmax(ntype), crit(ntype), gddmax(ntype),
c             bb(ntype), bbb(ntype), rdmax(ntype), dlai(ntype),
c             rsr(ntype), xmxlai(ntype), spriod(ntype)
c     modify: gdd, sumgdd(mxplan)
c
      include 'cgcovr.inc'
c     modify: mantyp(mxplan)
c
      include 'cridge.inc'
c     modify: rilrm(mxres,mxplan), rigrm(mxres,mxplan)
c
      include 'crout.inc'
c     modify: tlive(mxplan)
c
      include 'cclim.inc'
c       read: tmin, tave, radly, tmnavg
c
      include 'ccrpprm.inc'
c       read: itype(mxcrop,mxplan), dap(mxplan),
c             dtm(mxcrop,mxplan), btemp(mxcrop)
c
      include 'cends4.inc'
c       read: width(mxplan),rspace(mxplan)
c
      include 'cperen.inc'
c       read: imngmt(mxcrop,mxplan), rtmmax(mxcrop)
c
      include 'cwater.inc'
c       read: solthk(mxnsl,mxplan),watstr(mxplan)
c
      include 'csenes.inc'
c     modify: isenes(mxplan)
c
      include 'cupdate.inc'
c       read: sdate
c
c     + + + LOCAL VARIABLES + + +
      real fphu, hufh(mxplan), hufhy, par, ddm, reg(mxplan),
     1    rtmasy(mxplan), xlaimx(mxplan), fphumx(mxplan), vdmy,
     1    cancmx(mxplan), delvd(mxplan), delcc(mxplan), fhr, dec, fhu,
     1    frst, veg, wght1,rtdmin,exparg,exptrm
c
c     + + + LOCAL DEFINITIONS + + +
c     fphu   - ratio of total growing degree days received so far, to
c              total growing degree days expected at senescence (0-1).
c     hufh   - heat unit adjustment to harvest index today
c     hufhy  - value of HUFH yesterday.
c     par    - photosynthetically-active radiation
c     ddm    - daily above-ground biomass production (kg/m^2)
c     reg    - the maximum of water stress or temperature stress
c     rtmasy - root mass yesterday (kg/m^2)
c     xlaimx - leaf area index (LAI) on the day senescence begins.
c              (Saved on last day before senescence.)
c     fphumx - FPHU on the day senescence begins.  (Saved on last day
c              before senescence.)
c     cancmx - maximum canopy cover at maturity (0-1)  (Saved on last
c              day before senescence.)
c     delvd  - daily decline in vegetative biomass (leaf drop)
c              during senescence (kg/m^2)
c     delcc  - daily decline in canopy cover during senescence (kg/m^2)
c     fhr    - daylength reduction factor for perennials.
c     dec    - decrease in VDMT due to frost or decrease in day length.
c     vdmy   - yesterday's total above ground dry weight (vdmt).
c
c     + + + SAVES + + +
      save xlaimx, fphumx, cancmx, hufh, rtmasy, reg, delcc, delvd
c
c     + + + SUBROUTINES CALLED + + +
c     resup
c     rtpart
c
c     + + + END SPECIFICATIONS + + +
c
c
c     Plant growth starts after the minimum degree days for crop
c     emergence is accumulated.
c
c----- If the average of the daily minimum and maximum temperatures
c      (tave) exceeds the base temperature for the crop (btemp),
c      compute daily and cumulative growing degree days (gdd & sumgdd).
c      (EPIC Equation 2.190)
c
      if (tave.gt.btemp(itype(nowcrp,iplane))) then
        gdd = tave - btemp(itype(nowcrp,iplane))
        sumgdd(iplane) = sumgdd(iplane) + gdd
      else
        gdd = 0.0
      end if
c
c     *** L0 IF ***
c     ---- If this is cropland and accumulated degree days (sumgdd) exceeds
c     amount necessary for emergence (crit), OR this is rangeland and
c     the 5-day min. temp. avg. (tmnavg) exceeds the "base temperature"
c     necessary for the current plant to grow (btemp), then compute
c     the increase to harvest index (hia).
c
      if ((lanuse(iplane).eq.1.and.sumgdd(iplane).ge.
     1    crit(itype(nowcrp,iplane))).or.(lanuse(iplane).eq.2.and.tmnavg
     1    .ge.btemp(itype(nowcrp,iplane)))) then
c
c       Compute "fraction of potential heat units" accumulated to
c       date (fphu) from the gdd already received by the crop (sumgdd)
c       and the gdd at maturity (gddmax).
c       of potential heat units").
c       (EPIC Equation 2.191)
c
        fphu = sumgdd(iplane) / gddmax(itype(nowcrp,iplane))
        if (fphu.gt.1.0) fphu = 1.0
c
c       If HIA has not been set to zero, save yesterday's HUFH.
c
        if (hia(iplane).gt.0.0) then
c         comment from ftnchek 01-12-94 08:37am  sjl
c         Variables may be used before set in module GROW:
c         HUFH
          hufhy = hufh(iplane)
c       Warning from ftnchek
c       Variables may be used before set in module GROW:
c       HUFH
        else
          hufhy = 0.0
        end if
c
c       Compute heat unit adjustment to harvest index (hufh) for today.
c       (EPIC Equation 2.206)
c
        hufh(iplane) = fphu / (fphu+exp(6.5-10.*fphu))
c
c       exparg = 6.5 - 10.0*fphu
c       hufh(iplane) = fphu / (fphu + exp(exparg))
c
c       Compute the adjusted harvest index (hia) by adding to yester-
c       day's HIA (hiay).  The amount added is the difference between
c       yesterday's HUFH (hufhy) and today's (hufh), times the harvest
c       index (hi).  Note that at this point, HIA is cumulative through
c       the current day, but today's contribution is adjusted for heat
c       units *only*.
c
c       EPIC Equation 2.205 -- Original Code:
c       hia(iplane) = hi(itype(nowcrp, iplane)) * hufh(iplane)
c
c       EPIC Equation 2.205 -- Corrected Code:
        hia(iplane) = hia(iplane) + hi(itype(nowcrp,iplane)) * (
     1      hufh(iplane)-hufhy)
c
c
c       Estimate water demand as a function of plant growth stage.
c       (EPIC Equation 2.252)
c
        if (fphu.gt..3.and.fphu.lt..9) then
          fhu = sin(1.5708*(fphu-.3)/.3)
        else
          fhu = 0.
        end if
c
c       Note: FHU is a function of FPHU that defines a symetric
c       bell-shaped (sine) curve for values of FPHU between
c       0.3 & 0.9, peaking when FPHU is 0.6.  When FPHU is
c       not between 0.3 and 0.9, FHU is set to zero.
c       CRM -- 6/16/92.
c
c
c       Compute harvest index adjusted for water stress *and* heat
c       units (hia).
c       (EPIC Equation 2.251)
c
        hia(iplane) = hia(iplane) - hi(itype(nowcrp,iplane)) * (1.0-1.0/
     1      (1.0+0.01*fhu*(0.9-watstr(iplane))))
c
c       Add a lower limit of zero on HIA to prevent negative yields
c       from occurring.   dcf  4/5/95
c
        if(hia(iplane).lt. 0.0)hia(iplane) = 0.0
c
        if (hia(iplane).gt.hi(itype(nowcrp,iplane))) hia(iplane) =
     1      hi(itype(nowcrp,iplane))
c
c       SENESCENCE HAS NOT OCCURRED.  If ratio of heat units received,
c       to total heat units; ie "fraction of potential heat units"(fphu)
c       is less than the user-inputted fraction of growing season to
c       reach senescence (dlai), assume senescence has NOT occurred.
c       Thus, grow crop with water and temperature stress.  Calculate
c       total above ground dry weight (vdmt), canopy cover (cancov) and
c       height (canhgt), root mass and depth, up to the time of
c       senescence.
c
c       Note: This is somewhat imprecise since *fphu* is in heat units,
c       and *dlai* is expressed in *time*; however, days to
c       maturity is not available as an input.  CRM -- 6/25/92.
c
c
c       Compute temperature stress parameter (temstr) from
c       ratio of growing degree days (gdd) to difference between
c       optimum temperature for plant growth (otemp) and base
c       daily air temperature (btemp).  TEMSTR is really the
c       *complement* of temperature stress; ie, 1 - temperature
c       stress, like WATSTR.  By the way it is defined, TEMSTR
c       must range between zero and one.
c       (EPIC Equation 2.235)
c
        temstr(iplane) = sin(1.5708*gdd/(otemp(itype(nowcrp,iplane))-
     1     btemp(itype(nowcrp,iplane))))
c
        if (temstr(iplane).lt.0.0) temstr(iplane) = 0.0
c
c       Determine the "crop growth regulating factor" (reg).  This
c       is the extent of growth permitted by whatever *limits*
c       growth of the crop.  It is the maximum [the minimum of the
c       complements] of water stress (watstr) and temperature
c       stress (temstr).
c
        reg(iplane) = amin1(watstr(iplane),temstr(iplane))
c
c       *** L1 IF ***
c       if (fphu.le.dlai(itype(nowcrp,iplane))) then
c       if (fphu.lt.dlai(itype(nowcrp,iplane)) .or.
c    1    (imngmt(nowcrp,iplane).eq.2 .and. mgtopt(nowcrp,iplane).eq.3
c    1    .and. jdsene(nowcrp,iplane).eq.0) ) then
        if (fphu.lt.dlai(itype(nowcrp,iplane)) .or.
     1    (imngmt(nowcrp,iplane).eq.2 .and.
cd    Modified by S. Dun, March 11, 2006
cd    For perennial crop growth
cd     1     jdsene(nowcrp,iplane).eq.0)) then
     1     (jdsene(nowcrp,iplane).eq.0 .or.
     1      sdate.lt.jdsene(nowcrp,iplane)))) then
cd    End modifying
c
c         Compute photosynthetically active radiation (par) from
c         daily solar radiation (radly) and leaf area index (lai).
c         (EPIC Equation 2.192 with modifications.  0.2092 is used to
c         convert the units from Langleys to MJ/m**2.  0.05 is added
c         to LAI to "get the equation started"; otherwise, the values
c         never seem to depart from the baseline.  CRM -- 6/25/92.)
c
          par = 0.02092 * radly * (1.0-
     1        exp(-extnct(itype(nowcrp,iplane))*(lai(iplane)+0.05)))
c
c         Following code changes from David Hall - dcf 3/6/2000
c
c         exparg = -extnct(itype(nowcrp,iplane))*(lai(iplane)+0.05)
c         if (exparg .lt. -10.) then
c           exptrm = 0.0
c         else if (exparg .gt. 0.0)then
c           exptrm = 1.0
c         else
c           exptrm = exp(exparg)
c         endif
c         par = 0.02092 * radly * (1.0 - exptrm)
c
c         Compute daily increase in dry matter (ddm) from adjusted
c         biomass conversion factor (be) and PAR.
c         (EPIC Equation 2.193 with modifications.  Change in daylength
c         is ignored.  The right side is divided by 10 to implement a
c         change in units.  CRM -- 6/25/92.)
c
          ddm = 0.0001 * be(itype(nowcrp,iplane)) * par
c
c         Update total above ground dry weight (vdmt) and total live
c         plant material (tlive) from DDM & REG.
c         (EPIC Equation 2.233)
c
          vdmy = vdmt(iplane)
          vdmt(iplane) = vdmt(iplane) + ddm * reg(iplane)
c
c         XXX - Why not just set TLIVE equal to VDMT? -- CRM -- 6/30/92.
          tlive(iplane) = tlive(iplane) + ddm * reg(iplane)
          vdmx(iplane) = vdmt(iplane)
c
c         CANOPY COVER AND CANOPY HEIGHT CALCULATIONS
c
c         Compute canopy cover (cancov) from an empirically-derived
c         constant (bb) and total above ground dry weight (vdmt).
c         (WEPP Equation 8.2.4)
c
          if (imngmt(nowcrp,iplane).eq.1) then
            cancov(iplane) = 1.0 - exp(-bb(itype(nowcrp,iplane))*
     1          vdmt(iplane)*(1.-hia(iplane)))
c           exparg = -bb(itype(nowcrp,iplane))*
c    1                 vdmt(iplane)*(1.-hia(iplane))
c
c         - NOTE - for perennial crops will compute canopy cover
c         without use of adjusted harvest index - we assume that
c         all live biomass for perennials is vegetative.
c
          else
            cancov(iplane) = 1.0 -
     1          exp(-bb(itype(nowcrp,iplane))*vdmt(iplane))
c           exparg = -bb(itype(nowcrp,iplane))*vdmt(iplane)
          end if
c         if (exparg .lt. -10.) then
c           exptrm = 0.0
c         else if (exparg .gt. 0.) then
c           exptrm = 1.0
c         else
c           exptrm = exp(exparg)
c         endif
c         cancov(iplane) = 1.0 - exptrm
c
          if (cancov(iplane).lt.0.0) cancov(iplane) = 0.0
          if (cancov(iplane).ge.1.0) cancov(iplane) = 0.999
c
c         Compute canopy height (canhgt) from an empirically-derived
c         constant (bbb) and total above ground dry weight (vdmt)
c         and maximum plant height (hmax).
c         (WEPP Equation 8.2.6)
c
          canhgt(iplane) = (1.-
     1        exp(-bbb(itype(nowcrp,iplane))*vdmt(iplane))) *
     1        hmax(itype(nowcrp,iplane))

c         exparg=(-bbb(itype(nowcrp,iplane))*vdmt(iplane))
c         if(exparg .lt. -10.) then
c           exptrm = 0.0
c         else if (exparg .gt. 0.0) then
c           exptrm = 1.0
c         else
c           exptrm = exp(exparg)
c         endif
c         canhgt(iplane) = (1.0 - exptrm) * hmax(itype(nowcrp,iplane))

c
c         ROOT MASS AND ROOT DEPTH CALCULATIONS
c
c         Update root mass yesterday (rtmasy).  Compute
c         root mass (rtmass) from root to shoot ratio (rsr) and VDMT.
c
          rtmasy(iplane) = rtmass(iplane)
c
c         If this is a perennial, and root mass (rtmass) exceeds the
c         max. expected for this crop (rtmmax [user input]), set it
c         equal to the max. mass, and set root depth (rtd) equal to
c         the max. root depth (rdmax).  Check to make sure that
c         rooting depth can not exceed the maximum input soil depth
c
          if (imngmt(nowcrp,iplane).eq.2.and.rtmass(iplane).ge.
     1        rtmmax(itype(nowcrp,iplane))) then
            rtmass(iplane) = rtmmax(itype(nowcrp,iplane))
            if (rdmax(itype(nowcrp,iplane)).lt.
     1          solthk(nsl(iplane),iplane)) then
              rtd(iplane) = rdmax(itype(nowcrp,iplane))
            else
              rtd(iplane) = solthk(nsl(iplane),iplane)
            end if
c
c         If this is a perennial that has not "max'ed out" OR it is
c         an annual...
c
          else
c
c           ROOT MASS CALCULATION
c
c           New changes to how root mass is updated - change to
c           increase incrementally as VDMT increases incrementally
c           This prevents problems when perennials are cut or
c           grazed.     dcf   11/17/93
c
            rtmass(iplane) = rtmasy(iplane) + (vdmt(iplane)-vdmy) *
     1          rsr(itype(nowcrp,iplane))
c
            if (imngmt(nowcrp,iplane).eq.2.and.rtmass(iplane).gt.
     1          rtmmax(itype(nowcrp,iplane))) rtmass(iplane) =
     1          rtmmax(itype(nowcrp,iplane))
c
c           ROOT DEPTH CALCULATION
c
c           Calculate root depth (rtd) from maximum root depth for the
c           current crop (rdmax) and the ratio of cumulative gdd to
c           gdd at maturity (fphu).  Note that this is not *exactly*
c           how Eq. 8.2.12 appears in the WEPP documentation.  There
c           the ratio of: days since planting, to days to maturity,
c           is used, instead of FPHU.
c
c           CHANGE SO THAT ONLY ANNUALS USE EQUATION 8.2.12 - this
c           equation does not work well for perennials for several
c           reasons. Main reason is that FPHU is not clearly defined
c           for perennials, AND it varies as perennials are cut for
c           harvest.  For now, use a simple linear ratio of root mass
c           to maximum root mass to determine the root depth.
c           (This is better than previous versions which simply set
c           the root depth for a harvested perennial to the maximum)
c           dcf   11/18/93
c
c           ANNUALS
            if (imngmt(nowcrp,iplane).eq.1) then
c
c             (WEPP Equation 8.2.12)
              rtd(iplane) = rdmax(itype(nowcrp,iplane)) * 0.5 * (1.0+
     1            sin(3.03*fphu/dlai(itype(nowcrp,iplane))-1.47))
            else
c
c             PERENNIALS
c XXX         Change perennial root depth equation to now compute
c             depth of perennial roots as the sum of the current
c             root depth plus an additional depth based on the
c             incremental addition of root mass.   dcf  8/30/94
              rtd(iplane) = rtd(iplane) +
     1            ((rtmass(iplane) - rtmasy(iplane))
     1            /rtmmax(itype(nowcrp,iplane)))
     1            * rdmax(itype(nowcrp,iplane))
c
c XXX         ADD IN A MINIMUM VALUE FOR A PERENNIAL ROOT DEPTH SINCE
c             problems arise because we start out at zero vdmt - thus
c             zero rtd.  We need a way to initialize root depth from
c             the time of emergence, because in reality when the
c             perennial crop emerges - it already has a root system
c             of some depth.  Assume for now that minimum root depth
c             of a perennial can be estimated using the equation for
c             annual root depths.
c             dcf  8/30/94
c
              rtdmin = rdmax(itype(nowcrp,iplane)) * 0.5 * (1.0+
     1                 sin(3.03*fphu/dlai(itype(nowcrp,iplane))-1.47))
              if(rtd(iplane).lt.rtdmin)rtd(iplane) = rtdmin
            end if

            if (rtd(iplane).gt.solthk(nsl(iplane),iplane)) rtd(iplane) =
     1          solthk(nsl(iplane),iplane)
c
          end if
c
c         Partition total root mass into soil layers
          if (rtmass(iplane).gt.rtmasy(iplane)) call
     1        rtpart(iplane,rtmasy(iplane))
c
          mantyp(iplane) = imngmt(nowcrp,iplane)
c
c         LEAF AREA INDEX (LAI) Calculations
c
c         ANNUALS
          if (imngmt(nowcrp,iplane).eq.1) then
c
c           Compute amount of vegetative material (veg) from total above
c           ground dry weight (vdmt) and percent of vegetative material
c           (1-hia).
c
            veg = vdmt(iplane) * (1.0-hia(iplane))
c
c           Update LAI, based on max potential LAI (xmxlai) and the
c           amount of vegetative material (veg).
c
            lai(iplane) = (xmxlai(itype(nowcrp,iplane))*veg) / (veg+
     1          0.5512*exp(-6.8*veg))
c
c         PERENNIALS
          else if (imngmt(nowcrp,iplane).eq.2) then
c
c           Compute leaf area index (lai) from leaf area index at sene-
c           scence (xmxlai) and total above ground dry weight (vdmt).
c
            lai(iplane) = (xmxlai(itype(nowcrp,iplane))*vdmt(iplane)) /
     1          (vdmt(iplane)+0.2756*exp(-13.6*vdmt(iplane)))
c
c           Changes to code from David Hall - dcf  3/7/2000
c
c           exparg = -13.6*vdmt(iplane)
c           if (exparg .lt. -10.) then
c             exptrm = 0.0
c           else if (exparg .gt. 0.) then
c             exptrm = 1.0
c           else
c             exptrm = exp(exparg)
c           endif
c           lai(iplane) = (xmxlai(itype(nowcrp,iplane))*vdmt(iplane)) /
c    1                    (vdmt(iplane) + 0.2756*exptrm)
c
          end if
c
c         Save LAI, FPHU, and CANCOV in case plant senesces tomorrow.
c
          xlaimx(iplane) = lai(iplane)
          fphumx(iplane) = fphu
          cancmx(iplane) = cancov(iplane)
c
c
c       *** L1 ELSE ***
c       SENESCENCE HAS OCCURRED.
        else
c
c         Save yesterday's total above ground dry weight (vdmt).
          vdmy = vdmt(iplane)
c
c         CROPLAND ANNUALS
c         *** M2 IF ***
          if (lanuse(iplane).eq.1.and.imngmt(nowcrp,iplane).eq.1) then
c
c           *** M3 IF ***
c           If this is the first day of senescence....
            if (ncount.eq.0) then
c
c             Indicate "senescence" for RESUP.
              isenes(iplane) = -2
c
c             Set residue type to the current crop.
              call resup(nowcrp,isenes(iplane))
c
c             Indicate "harvest after senescence" for RESUP.
              isenes(iplane) = 1
c
c             Following commented out - on 1st day of senescence this
c             code incremented ncount to 1 then code after M3 endif
c             also incremented ncount again (to 2).   dcf  4/26/94
c             Increment "days since senescence".
c             ncount = 1
c
c             Compute daily decline in canopy cover (delcc) from the
c             canopy cover when senescence began (cancmx), the fraction
c             by which canopy decays (1-decfct), and the length of the
c             senescence period (spriod).
c
              delcc(iplane) = cancmx(iplane) * (1.0-
     1            decfct(itype(nowcrp,iplane))) /
     1            float(spriod(itype(nowcrp,iplane)))
c
c             Compute daily decline in biomass (delvd) from total above
c             ground dry weight (vdmt), [the complement of] unadjusted
c             harvest index (hi) [ie the fraction of vegetative matter]
c             the fraction by which biomass drops after senescence
c             (dropfc), and the length of the senescence period(spriod)
c
              delvd(iplane) = vdmx(iplane) * (1.-
     1            hi(itype(nowcrp,iplane))) * (1.-
     1            dropfc(itype(nowcrp,iplane))) /
     1            float(spriod(itype(nowcrp,iplane)))
c
c           Note: DECFCT & DROPFC are the fraction REMAINING, not
c           the fraction that is LOST.  CRM -- 6/29/92.
c
c           *** M3 ENDIF ***
            end if
c
c           Update days since senescence (ncount).
            ncount = ncount + 1
c
c           if days since senescence (ncount) is less than the length
c           of the senescence period (spriod), decrease canopy cover
c           (cancov) and total above ground dry weight (vdmt).
c
            if (ncount.le.spriod(itype(nowcrp,iplane))) then
              cancov(iplane) = cancov(iplane) - delcc(iplane)
              vdmt(iplane) = vdmt(iplane) - delvd(iplane)
            end if
c
c         RANGELAND & CROPLAND PERENNIALS
c         *** M2 ELSE ***
          else
c
c           CROPLAND PERENNIALS
c           *** M4 IF ***
            if (lanuse(iplane).eq.1.and.imngmt(nowcrp,iplane).eq.2) then
c
c             If this is the first day of senescence....
              if (ncount.eq.0) then
c
c               Indicate "senescence" for RESUP.
                isenes(iplane) = -2
c
c               Set residue type to the current crop.
                call resup(nowcrp,isenes(iplane))
c
              endif
              ncount = ncount + 1
c
c             Compute daily decline in canopy cover (delcc) from the
c             canopy cover when senescence began (cancmx), the fraction
c             by which canopy decays (1-decfct), and the length of the
c             senescence period (spriod).
c             Compute daily decline in biomass (delvd) from total above
c             ground dry weight (vdmt),
c             the fraction by which biomass drops after senescence
c             (dropfc), and the length of the senescence period(spriod)
c
              if(sdate.eq.1 .and. year.eq.1)
     1          cancmx(iplane) = cancov(iplane)
              if(spriod(itype(nowcrp,iplane)).gt.0)then
                delcc(iplane) = cancmx(iplane) * (1.0-
     1            decfct(itype(nowcrp,iplane))) /
     1            float(spriod(itype(nowcrp,iplane)))
                delvd(iplane) = vdmx(iplane) * (1.-
     1            dropfc(itype(nowcrp,iplane))) /
     1            float(spriod(itype(nowcrp,iplane)))
              else
                delcc(iplane) = 0.0
                delvd(iplane) = 0.0
              endif
c
c             if days since senescence (ncount) is less than the length
c             of the senescence period (spriod), decrease canopy cover
c             (cancov) and total above ground dry weight (vdmt).
c
              if (ncount.le.spriod(itype(nowcrp,iplane))) then
                cancov(iplane) = cancov(iplane) - delcc(iplane)
                vdmt(iplane) = vdmt(iplane) - delvd(iplane)
c
c             Compute leaf area index(lai) from leaf area index at sene-
c             scence (xmxlai) and total above ground dry weight (vdmt).
c
                lai(iplane)=(xmxlai(itype(nowcrp,iplane))*vdmt(iplane))
     1                   /(vdmt(iplane)+0.2756*exp(-13.6*vdmt(iplane)))
              end if
c
c
c           RANGELAND
c           *** M4 ELSE ***
            else
c
c             Compute the daylength reduction factor (fhr) from day
c             length of the current day [in hours] (daylen) and minimum
c             day length for a year at the site (daymin).
c             (EPIC Equation 2.253)
c
              fhr = 0.35 - (1.0-daylen/(daymin+1))
c
c             Compute the frost damage factor (frst) from the minimum
c             temperature on the current day (tmin) and parameters
c             expressing the crop's frost sensitivity (x5 & x6).
c             (EPIC Equation 2.254)
c
              if (tmin.lt.-1.0) then
c
c               frst = 100.0 * (tmin/(tmin-exp(x5+x6*tmin)))
c
c             Following code changes from David Hall - dcf 3/7/2000
c deh -- still a problem if exptrm is very near tmin
c
                exparg = x5 + x6*tmin
c               if (exparg .lt. -10.0) then
c                 exptrm = 0.0
c               else if (exparg .gt. 0.) then
c                 exptrm = 1.0
c               else
c                 exptrm = exp(exparg)
c               end if
                if (exparg .gt. 88.0) exparg=88.0
                exptrm = exp(exparg)
                frst = 100.0 * (tmin/(tmin-exptrm))
                if (frst.gt.1.0) frst = 1.0
              else
                frst = 0.0
              end if
c
c             Compute the reduction in standing live biomass (dec) using
c             the above ground biomass(tlive-total live plant material),
c             the heat unit index(fphu), and maximum of the daylength
c             and frost reductions (fhr & frst).
c             (EPIC Equation 2.255)
c
cC            dec = 0.5 * tlive(iplane) * (1 - fphu) * max(fhr,frst)
              dec = 0.5 * vdmt(iplane) * (1-fphu) * max(fhr,frst)
c
              vdmt(iplane) = vdmt(iplane) - dec
              if (vdmt(iplane).lt.0.0) vdmt(iplane) = 0.0
c
c             XXX - REG is the minimum percent growth allowed by water
c             or temperature stress.  It is only computed during active
c             growth, it is never updated during senescence.  Is this a
c             mistake? -- CRM -- 6/30/92.
c
c             XXX - REG is the minimum percent growth allowed by water
c             or temperature stress.  Wouldn't something like:
c             tlive(iplane) = tlive(iplane)*reg(iplane) - dec
c             make more sense for the next equation? -- CRM -- 6/30/92.
c
c             Original Code:
c             tlive(iplane) = tlive(iplane) - dec - dec *(1-reg)
              tlive(iplane) = tlive(iplane) + dec * (reg(iplane)-2.0)
c
c             Compute canopy cover (cancov) from an empirically-derived
c             constant (bb), total above ground dry weight (vdmt), and
c             fraction vegetative dry matter (1 - hia).
c             (WEPP Equation 8.2.4 )
c
c             cancov(iplane) = 1.0 -
c             1    exp( -bb(itype(nowcrp, iplane)) * vdmt(iplane) *
c             2    (1. - hia(iplane)))
c
c             Changed by dcf to not use 1-hia term - this is not the
c             equation in the original model documentation, and use of
c             the (1-hia) term makes no physical sense in a canopy cover
c             equation.  Jeff Arnold needs to check this. (The following
c             equation is compatible with the one used in subroutine
c             INITGR to solve for vdmt based on initial input canopy
c             cover.    dcf  4/9/93
c
              cancov(iplane) = 1.0 -
     1          exp(-bb(itype(nowcrp,iplane))*vdmt(iplane))
c
c             Changes from David Hall below - dcf 3/6/2000
c
c             exparg = -bb(itype(nowcrp,iplane))*vdmt(iplane)
c             if (exparg .lt. -10.) then
c               exptrm = 0.0
c             else if (exparg .gt. 0.0) then
c               exptrm = 1.0
c             else
c               exptrm = exp(exparg)
c             end if
c             cancov(iplane) = 1.0 - exptrm
              if (cancov(iplane).lt.0.0) cancov(iplane) = 0.0
              if (cancov(iplane).ge.1.0) cancov(iplane) = 0.999
c
c
c             Compute leaf area index (lai) during leaf decline.
c             (Note: FPHUMX is the LAI saved on the last day before
c             senescence.)
c             (EPIC Equation 2.199  with adj set to 2)
c
c             Added IF to prevent error when a perennial is predicted to
c             already be in senescence on 1st day of simulation before
c             xlaimx and fphumx have been determined.  In this case, LAI
c             used is that already determined in INITGR.  dcf  4/22/94
c
              if(sdate.eq.1 .and. year.eq.1)then
                xlaimx(iplane) = lai(iplane)
                fphumx(iplane) = fphu
              else
                if(fphumx(iplane).lt.1.0)then
                  lai(iplane) =
     1            xlaimx(iplane) * ((1.-fphu)/(1.-fphumx(iplane))) ** 2
                else
                  lai(iplane) = xlaimx(iplane)
                endif
              endif
c
c           *** M4 ENDIF ***
            endif
c
c         *** M2 ENDIF ***
          end if
c
c         Compute residue mass on the rills (rilrm) and on the ridges
c         (rigrm) by adding the decrease in total above ground dry
c         matter (vdmy-vdmt) to each.
c         (WEPP Equation 8.2.10) - adjust flat residue masses.
c
          rilrm(1,iplane) = rilrm(1,iplane) + (vdmy-vdmt(iplane))
          rigrm(1,iplane) = rigrm(1,iplane) + (vdmy-vdmt(iplane))
c
c         Average RILRM & RIGRM to determine the "residue mass on the
c         ground today" (rmogt).
c
          wght1 = (rspace(iplane)-width(iplane)) / rspace(iplane)
          rmogt(1,iplane) = wght1 * rigrm(1,iplane) + (1.0-wght1) *
     1        rilrm(1,iplane)
c
c         XXX - Is this flag setting of IDECOM really needed here???
c         Or could it be removed from GROW entirely???  Is it needed for
c         rangeland??  Cropland settings are all made in PTGRP.
c         dcf 12/1/93
c         Flag commented out  4/26/94  by dcf  -  resetting of
c         idecom to 1 daily here could cause errors in growth of
c         perennials which are in senescence and are entering a
c         freeze period (only want to update residue indices once).
c         Set flag to update residue parameters.
c         idecom = 1
c
c       *** L1 ENDIF ***
        end if
c
c     *** L0 ENDIF ***
      end if
c
      return
      end
