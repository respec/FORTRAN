      subroutine patrib(iplane,plant,tlive,tmpave,nowres,smrati,dwood,
     1    ptlive)
c
c   ***************************************************************
c   * NOTE: ALL SOURCE FILES THAT REFERENCE "CRINPT3.INC" MUST BE *
c   *       COMPILED WITH THE *NEW* CRINPT.INC TO WORK WITH THIS  *
c   *       FILE.                                                 *
c   ***************************************************************
c
c     NOTE: Equation numbers refer to the August 1989 User Doc.
c
c     NOTE: Definitions in CRINPT3.INC need improvement: gcoeff, scoeff.
c           GHGT was incorrectly defined.  Definitions in CRINPT5.INC
c           also need improvement: tcoeff.
c           Be sure ~/wepp/v9225/weltz/crinpt3.inc in included with recoded
c           files.  -- CRM 3/23/93.
c
c     NOTE: In CRINPT3.INC need definitions for: RROUGH, BCOVER, RUFCOV.
c           CRM -- 3/25/93.
c
c     + + + PURPOSE + + +
c     Updates plant attributes on rangeland, including: canopy and ground
c     cover, LAI, plant height, and projected (side view) plant area, and
c     canopy height.
c
c     Called from RANGE
c     Author(s): Weltz, Meyer
c     Reference in User Guide: Chapter 8
c
c     Changes:
c          1) Moved 4th parameter to last position to conform to WEPP
c             recoding convention.  Same needed in RANGE.
c          2) Deleted reference to common block PTILTY.INC.
c          3) This code:
c                 if(thgt(plant) .ge. shgt(plant) .and.
c                1   thgt(plant) .ge. ghgt(plant))then
c                    tarea=thgt(plant)*100.
c                 endif
c
c                  (WEPP Equation 8.5.18)
c                 if(shgt(plant) .ge. thgt(plant) .and.
c                1   shgt(plant) .ge. ghgt(plant))then
c                    tarea=shgt(plant)*100.
c                 endif
c
c                  (WEPP Equation 8.5.18)
c                 if(ghgt(plant) .ge. thgt(plant) .and.
c                1   ghgt(plant) .ge. shgt(plant))then
c                    tarea=ghgt(plant)*100.
c                 endif
c             changed to:
c                 tarea=max(thgt(plant),shgt(plant),ghgt(plant))*100.0
c          4) Modified code relating to decomposition and bugs.
c          5) Changed canopy height (by factoring) from:
c                 canhgt(iplane) = ghgt(plant)*(gpai/tarea)/basden+
c                1                 shgt(plant)*(spai/tarea)/basden+
c                2                 thgt(plant)*(tpai/tarea)/basden
c             to:
c                 canhgt(iplane) = (ghgt(plant)*gpai+shgt(plant)*spai+
c                1                  thgt(plant)*tpai)/totpai
c          6) Added range check to horizontal portion of GPAI calc.
c             Changed:
c                 gpai = ghgt(plant) * gdiam(plant) * gcoeff(plant) * gpop(plant)
c             to:
c                 tmpvr4 = gdiam(plant) * gcoeff(plant) * gpop(plant)
c                 if(tmpvr4 .gt. 100.0) tmpvr4 = 100.0
c                 gpai = ghgt(plant) * tmpvr4
c             Did same thing for shrubs & trees in the code in RNGINT.
c          7) The following code moved from PATRIB to RANGE:
c                 if(yield(iplane) .gt. 0.0 .or. oldplt .gt. 0.0)then
c                   utiliz = tfood / (yield(iplane)+oldplt)
c                 else
c                   utiliz = 0.0
c                 endif
c             This meant OLDPLT & TFOOD can be removed from the dummy parameter
c             list for PATRIB.  UTILIZ which was incorrectly treated as a local
c             variable in PATRIB (never passed to RANGE where it is needed),
c             can now be ignored in PATRIB.
c          8) Added SPAI, & TPAI to CRINPT3.INC permit single initial
c             calculation of SPAI & TPAI in RNGINT, rather than daily
c             computation in PATRIB.  Deleted appropriate code in
c             PATRIB.
c
c     Version: This module recoded from WEPP Version 92.25.
c     Date recoded: 03/18/93 4/05/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxgrz.inc'
      include 'pmxcrp.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pmxnsl.inc'
      include 'pntype.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer iplane, plant, nowres
      real tlive, tmpave, smrati, dwood, ptlive
      real tmpvr1, tmpvr4
c     real tmpvr1, tmpvr2, tmpvr3, tmpvr4
c
c     + + + ARGUMENT DEFINITIONS + + +
c     iplane - current OFE
c     plant  - current plant type
c     nowres - current residue type
c     tlive  - total current live plant material
c     tmpave - average daily temperature
c     smrati - fraction of yesterday's residue remaining today
c     dwood  - standing dead biomass after burning or herbicide
c     ptlive - previous total live leaf biomass
c
c     + + + COMMON BLOCKS + + +
      include 'ccover.inc'
c      modify: cancov, gcover
c       write: canhgt, inrcov, rilcov
c
      include 'ccrpout.inc'
c      modify: rescov
c       write: lai
c
      include 'ccrpvr1.inc'
c        read: rmagt
c      modify: rmogt
c
      include 'ccrpvr2.inc'
c        read: vdmt
c      modify: cf
c
      include 'ccrpvr3.inc'
c        read: hmax, bbb, vdmmax
c
      include 'crinpt1.inc'
c        read: aleaf, wcf, crypto, cold, bugs, wood, yield
c      modify: ffk
c       write: pyield
c
      include 'crinpt3.inc'
c      modify: ghgt, basden
c        read: spai, tpai, shgt, spop, sdiam, scoeff, gpop, gdiam, gcoeff
c
      include 'crinpt5.inc'
c        read: thgt, tpop, tdiam, tcoeff
c
      include 'crinpt6.inc'
c      modify: bcover, rufcov
c
c     + + + LOCAL VARIABLES + + +
      real gpai, totpai, tarea, rmagy, produc
c
c     + + + LOCAL DEFINITIONS + + +
c     gpai   - total projected area from the side, intercepted for grass &
c              herbaceous plants, per 100 meter transect; ie, vertical area
c              "blocked" by plants, measured perpendicular to the slope.
c              Used for snowdrift calculations.
c     totpai - total projected area from the side, for all plants combined
c     tarea  - area of the vertical rectangle circumscribed around the
c              largest plant type present (trees, shrubs, & herbs).
c     rmagy  - amount of standing residue yesterday
c     produc - test variable used in exponential underflow traps
c
c     + + + END SPECIFICATIONS + + +
c
c ---- herbacious plant height
      if (bbb(plant).gt.0.0) then
c       -- XXX -- This equation is incorrect in the User Doc.  RE: 3/23/93
c       telcon w/Weltz -- CRM -- 3/23/93.
c       (WEPP Equation 8.5.14)
c       ghgt(plant) = hmax(plant)*(1.0-exp(-bbb(plant)*
c       1                                         (tlive+rmagt(iplane))))
        produc = (bbb(plant)*(tlive+rmagt(iplane)))
        if (produc.lt.20.0) then
          ghgt(plant) = hmax(plant) * (1.0-exp(-produc))
        else
          ghgt(plant) = hmax(plant)
        end if
      else if (bbb(plant).eq.0.0) then
        ghgt(plant) = 0.0
      else
        ghgt(plant) = hmax(plant) * (tlive+rmagt(iplane)/
     1      vdmmax(iplane))
      end if
c
c     ---- projected area of herbaceous plants (changes daily)
c     (WEPP Equation 8.5.17)
      if (gdiam(plant).gt.0.0) then
        tmpvr4 = gdiam(plant) * gcoeff(plant) * gpop(plant)
        if (tmpvr4.gt.100.0) tmpvr4 = 100.0
        gpai = ghgt(plant) * tmpvr4
      else
        gpai = 0.0
      end if
c
c     **********************************************************************
c     * NOTE: We assume NO OVERLAP between the grass, shrubs, and trees.   *
c     *       If we did, a random overlap would be represented by the      *
c     *       product of the 2 PAI's involved; ie, for overlap of ALL      *
c     *       3 types of vegetation:                                       *
c     *          totpai=spai+gpai+tpai -spai*gpai -spai*tpai -gpai*tpai    *
c     *       Should we assume that grass & trees don't overlap, but       *
c     *       shrubs overlap with both grass and trees?  Then we get:      *
c     *          totpai=spai+gpai+tpai -spai*gpai -spai*tpai               *
c     *       ie:                                                          *
c     *          totpai=spai+gpai+tpai -spai*(gpai+tpai)                   *
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
cC    -- XXX -- This calculation would be simpler -- particularly if the
cC    "max" value were saved above (call it MAXHT):
cC    canhgt(iplane) = max(thgt(plant),shgt(plant),ghgt(plant))*basden
      if (basden.gt.0.0) then
        canhgt(iplane) = (ghgt(plant)*gpai+shgt(plant)*spai(plant)+
     1      thgt(plant)*tpai(plant)) / totpai
      else
        canhgt(iplane) = 0.0
      end if
c
c     ---- leaf area index
c     -- XXX -- This seems to be simply "leaf area"; ie, m**2 -- CRM -- 3/24/93.
      lai(iplane) = tlive * aleaf(plant)
c
c     ---- ( Update residue mass; correct for removal by insects. )
c     ---- today's residue amount, after decomposition
      tmpvr1 = rmogt(nowres,iplane) * smrati
c     ---- average temp. exceeds 8 degrees (bugs active).
      if (tmpave.ge.8.0) then
c       ------ more present than the bugs will eat
        if (tmpvr1.gt.bugs(plant)) then
          rmogt(nowres,iplane) = tmpvr1 - bugs(plant)
c       ------ bugs finish it off.
        else
          rmogt(nowres,iplane) = 0.0
        end if
c     ---- average temp. is less than 8 degrees (bugs inactive).
      else
        rmogt(nowres,iplane) = tmpvr1
      end if
      if (rmogt(nowres,iplane).lt.0.00001) rmogt(nowres,iplane) =
     1    0.00001
c
c     ---- calculate residue cover from residue mass
c     (WEPP Equation 8.5.20)
c*      produc = 12.61 * rmogt(nowres,iplane)
c
c     NOTE - When I fixed the underflow problem below caused by VDMT
c     becoming very large (4462.15 kg/m**2)  this caused another
c     error to rear its ugly head.  At this point - the value
c     of RMOGT for the range test data set becomes 1730.51 kg/m**2
c     and causes another underflow error using the old Lahey
c     compiler.  This may be related to the VDMT problem.
c     WELTZ needs to determine the error and fix the problems.
c
c     ADDED a temporary fix to prevent underflow - trap.     dcf  6/4/93
c     if(produc.gt.20.0)write (6,*)'SPOT 2  produc= ',produc
c     if(produc.gt.20.0)write (6,*)'rmogt = ',rmogt(nowres,iplane)
c     tmpvr2 = 1.085*(1.0+1.069*exp(-12.61*rmogt(nowres,iplane)))-0.5838
c
c*      if (produc.lt.20.0) then
c*        tmpvr2 = 1.085 * (1.0+1.069*exp(-produc)) - 0.5838
c*      else
c*        tmpvr2 = 1.085 - 0.5838
c*      end if
c
c*      cf(plant) = 10.0 ** (tmpvr2)
c     -- XXX -- Need a second equation number in the User Doc.
c
c     NOTE - PROBLEM HERE WITH range test data set - value for
c     rmogt(nowres,iplane) going too high (1730.51 kg/m**2).
c     WELTZ needs to check and fix.  For now add a trap to prevent
c     underflow using old Lahey compiler.    dcf   6/4/93
c
c     rescov(iplane) =  1.0 - exp(-cf(plant)*rmogt(nowres,iplane))
c
c*      produc = cf(plant) * rmogt(nowres,iplane)
c*      if (produc.lt.20.0) then
c*        rescov(iplane) = 1.0 - exp(-produc)
c*      else
c*        rescov(iplane) = 1.0
c*      end if
c*      if (rescov(iplane).lt.0.0) rescov(iplane) = 0.0
c
c       **Added by Mary Kidwell on 5/25/95
        rescov(iplane) = 1.0 - exp(cf(plant)*rmogt(nowres, iplane))
c
c     ---- calculate canopy cover from plant mass corresponding to 100% cover (COLD)
c     (WEPP Equation 8.5.22)
c     Original Code:
c     ffk(plant) =21.39148-54.90758*cold(plant)+61.11016*cold(plant)
c     1  **2-30.44471*cold(plant)**3+5.561994*cold(plant)**4
c*      tmpvr3 = cold(plant) ** 2
c*      ffk(plant) = 21.39148 - 54.90758 * cold(plant) + 61.11016 * tmpvr3
c*     1    - 30.44471 * cold(plant) * tmpvr3 + 5.561994 * tmpvr3 ** 2
c     -- XXX -- Need a second equation number in the User Doc.
c     (WEPP Equation 8.5.22)
c
c     NOTE - value of VDMT goes to 4462.15 kg/m**2 running WEPP V93.06
c     and the range test data set (range.dat soil.dat slope.dat
c     clim.dat).  Weltz needs to check and correct the error
c     causing the large values for VDMT (which causes a bomb
c     using the old Lahey compiler.  FOR NOW - add a trap to
c     prevent the underflow from occurring.    dcf  6/4/93
c*      produc = ffk(plant) * vdmt(iplane)
c     if(produc.gt.20.0)write (6,*)'POSITION 3  produc= ',produc
c     if(produc.gt.20.0)write (6,*)'VDMT = ',vdmt(iplane)
c     cancov(iplane) =  1. - exp(-ffk(plant)*vdmt(iplane))
c*      if (produc.lt.20.0) then
c*       cancov(iplane) = 1. - exp(-produc)
c*     else
c*        cancov(iplane) = 1.0
c*      end if
c*      if (cancov(iplane).lt.0.0) cancov(iplane) = 0.0
c       **Added by Mary Kidwell on 5/25/95
        cancov(iplane) = 1.0 - exp(ffk(plant)*vdmt(iplane))
c
c     -- XXX -- Can't find this equation in the User Doc. -- CRM -- 3/25/93.
c     ---- basal plant cover (ground area occupied by stalks, stems, trunks)
c     XXX Following If/Else commented out by Mary Kidwell 3/95
c     XXX Question is how is basal cover to be calculated for a
c     XXX continuous WEPP model simulation??????   dcf  3/9/95
c      if (wood(plant).gt.0.0) then
c        bcover(iplane) = 0.3354242 * cancov(iplane)
c      else
c        bcover(iplane) = 0.429118 * cancov(iplane)
c      end if
c
c       **Added by Mary Kidwell on 5/25/95
      If (wood(plant).gt.0.0) then
        bascov(iplane) = 0.3354242 * cancov(iplane)
      Else
        bascov(iplane) = 0.429118 * cancov(iplane)
      End if
c
c     -- XXX -- Can't find this equation in the User Doc. -- CRM -- 3/25/93.
c     ---- calculate hydraulic roughness cover from basal cover and moss & lichens
c     Change from Mary Kidwell,  3/95   dcf
c     rufcov(iplane) = bcover(iplane) + crypto(iplane)
c     bcover unset at this time so taken out of eqn. 04-20-95 05:28pm
c*      rufcov(iplane) = crycov(iplane)
c*      if (rufcov(iplane).lt.0.0) rufcov(iplane) = 0.0
c     **Added by Mary Kidwell on 5/25/95
      rufcov(iplane) = crycov(iplane) * fcryr(iplane) +
     1                 bascov(iplane) + fbasr(iplane)
      if (rufcov(iplane).lt.0.0) rufcov(iplane) = 0.0
c
c     ---- total ground cover (residue, rocks & gravel, stems and moss & lichens)
c     Change from Mary Kidwell,  3/95   dcf
c     gcover(iplane) = rescov(iplane) + wcf(iplane) + rufcov(iplane)
      gcover(iplane) = rescov(iplane) + rokcov(iplane) + rufcov(iplane)
      if (gcover(iplane).gt.1.0) gcover(iplane) = 1.0
c
c     -- XXX -- THIS STUFF LOOKS LIKE IT SHOULD BE DONE IN "RANGE" AFTER THE
c     CALL TO "PATRIB" -- NOT HERE. -- CRM 3/25/93.
c     ---- previous day's total live plant material
      ptlive = tlive
c     -- XXX -- WHY even save this? -- CRM -- 3/25/93.
c     ---- above ground plant production for simulation year
      pyield(iplane) = yield(iplane)
c     -- XXX -- Why compute this.  It never seems to get USED... CRM -- 3/29/93.
c     ---- residue mass yesterday (residue mass today + dead wood)
      rmagy = rmagt(iplane) + dwood
      if (rmagy.le.0.00001) rmagy = 0.0
c
c
c     -- XXX -- Need an equation number in the User Doc.
c     ---- rill cover (residue + rocks & gravel)
c     Change from Mary Kidwell,  3/95   dcf
c     rilcov(iplane) = rescov(iplane) + wcf(iplane)
c*     rilcov(iplane) = rescov(iplane) + rokcov(iplane)
c     **Added by Mary Kidwell on 5/25/95
      rilcov(iplane) = rescov(iplane) * fresr(iplane)
     1               + rokcov(iplane) * frokr(iplane)
     1               + bascov(iplane) * fbasr(iplane)
     1               + crycov(iplane) * fcryr(iplane)
c
c     -- XXX -- Need an equation number in the User Doc.
c     ---- interrill cover (residue + rocks & gravel + moss & lichens)
c*      inrcov(iplane) = gcover(iplane)
c     **Added by Mary Kidwell on 5/25/95
      inrcov(iplane) = rescov(iplane) * fresi(iplane)
     1               + rokcov(iplane) * froki(iplane)
     1               + bascov(iplane) * fbasi(iplane)
     1               + crycov(iplane) * fcryi(iplane)
c

c
      return
      end
