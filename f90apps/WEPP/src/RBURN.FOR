      subroutine rburn(plant)
c
c     + + + PURPOSE + + +
c     Updates range vegetation attributes as the result of burning.
c     (Burning may only be done once a year.)
c
c     Called from RANGE
c     Author(s): Weltz
c     Reference in User Guide: Chapter 8
c
c     Changes:
c           1) Dereferenced the following common blocks:
c              CRPVR3, CRPVR5, COVER, CRPOUT, CRPPRM, ENDS,
c              PARAME, RINPT2, RINPT6, UPDATE.
c           2) Dereferenced: PTILTY.INC & PMXPND.INC.
c           3) Changed:
c                nowres = 1
c                rmogt(nowres,iplane) = rmogt(nowres,iplane) * reduce(iplane)
c              to:
c                rmogt(1,iplane) = rmogt(1,iplane) * reduce(iplane)
c           4) Removed generic SAVE of all local variables.
c           5) Changed PLIVE(PLANT) to PLIVE(PLANT,IPLANE). -- CRM -- 5/14/93.
c           6) PROOT was altered analogous to PLIVE. -- CRM -- 5/14/93.
c
c     Version: This module recoded from WEPP Version 92.25.
c     Date recoded: 01/28/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pntype.inc'
      include 'pmxelm.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxgrz.inc'
      include 'pmxcrp.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxhil.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer plant
c
c     + + + ARGUMENT DEFINITIONS + + +
c     plant  - current plant type; set to itype(nowcrp,iplane) in RANGE
c
c     + + + COMMON BLOCKS + + +
      include 'ccrpvr1.inc'
c      modify: rmogt(mxres,mxplan), rmagt(mxplan)
c
      include 'ccrpvr2.inc'
c       write: vdmt(mxplan)
c
      include 'chydrol.inc'
c        read: rain(mxplan)
c
      include 'crinpt1.inc'
c      modify: plive(ntype,mxplan), proot(ntype,iplane)
c       write: access(mxplan)
c
      include 'crinpt3.inc'
c        read: alter(mxplan),burned(mxplan),change(mxplan),
c              hurt(mxplan),reduce(mxplan)
c      modify: jfdate(mxplan),shgt(ntype),spop(ntype)
c
      include 'crinpt4.inc'
c      modify: xlive(mxplan), dwood(mxplan)
c
      include 'crinpt5.inc'
c      modify: tpop(ntype),thgt(ntype)
c
      include 'crout.inc'
c      modify: tlive(mxplan)
c
      include 'cstruc.inc'
c        read: iplane
c
      include 'cclim.inc'
c        read: am2(mxplan)
c
c     + + + END SPECIFICATIONS + + +
c
c
c      *** L0 IF ***
c      If rainfall is <= 7.5 mm, AND five day antecedent moisture for
c      submerged residue <= 25 mm, proceed with burning.
c
c XXX NOTE - may want to change "rain(iplane)" to "rain+melt+irdept"
c XXX        so that impact of snow melt and irrigation can also be
c XXX        accounted for.   dcf  12/19/94
c
      if (rain(iplane).le.0.0075.and.am2(iplane).le.0.025) then
c
c       *** L1 IF ***
c       ------ (EVERGREENS PRESENT)
        if (xlive(iplane).gt.0.0) then
c         -------- (MIXED POPULATION)
          if (tlive(iplane).gt.xlive(iplane)) then
c           **************************************************************
c           * Note: The amount of live material is reduced by the same   *
c           *       fractions as the dead material.  The accuracy to be  *
c           *       gained, did not seem worth the complexity it would   *
c           *       cost to handle them separately. -- CRM -- 3/17/93.   *
c           **************************************************************
c           -----------total live plant material
c           (WEPP Equation 8.6.14)
            tlive(iplane) = ((tlive(iplane)-xlive(iplane))*
     1          reduce(iplane)) + (xlive(iplane)*hurt(iplane))
c         -------- (ONLY EVERGREENS)
          else
            xlive(iplane) = xlive(iplane) * hurt(iplane)
c           (WEPP Equation 8.6.13)
            tlive(iplane) = tlive(iplane) * hurt(iplane)
          end if
c
c       *** L1 ELSE ***
c       ------ (NO EVERGREENS PRESENT)
        else
          tlive(iplane) = tlive(iplane) * reduce(iplane)
c       *** L1 ENDIF ***
        end if
c
c       ------ update on-ground residue by non-evergreen reduction fraction
c       **************************************************************
c       * Note: If present, evergreen residue is reduced by the same *
c       *       fraction as non-evergreen.  The accuracy to be       *
c       *       gained, did not seem worth the complexity required   *
c       *       to handle them separately. -- CRM -- 3/17/93.        *
c       **************************************************************
c       -- XXX -- We are assuming evergreen residue (if present) is reduced
c       at the same rate as non-evergreens. -- CRM 1/28/93.
c       (WEPP Equation 8.6.16)
        rmogt(1,iplane) = rmogt(1,iplane) * reduce(iplane)
c       ------ update standing herbaceous residue by non-evergreen reduction fraction
c       (WEPP Equation 8.6.15)
        rmagt(iplane) = rmagt(iplane) * reduce(iplane)
c       ------ standing woody biomass
c       (WEPP Equation 8.6.10)
        dwood(iplane) = dwood(iplane) * burned(iplane)
c       ------ update fraction of forage available for consumption, to coefficient
c       of increase in availability
c       ------ replace the fraction of forage available, with fraction available
c       after burning
        access(iplane) = alter(iplane)
c       ------ update maximum [potential] standing live biomass, using fraction
c       change in above and below ground biomass
c       (WEPP Equation 8.6.13)
        plive(plant,iplane) = plive(plant,iplane) * change(iplane)
c       ------ update peak root biomass, using fraction change in above and
c       below ground biomass
c       (WEPP Equation 8.6.12)
        proot(plant,iplane) = proot(plant,iplane) * change(iplane)
c       ------ update total above ground dry weight (VDMT), using updated:
c       total live plant material on day of simulation (TLIVE),
c       surface residue mass above ground today (RMAGT), and standing
c       dead biomass left after burning or herbicide (DWOOD).
        vdmt(iplane) = tlive(iplane) + rmagt(iplane) + dwood(iplane)
c       **************************************************************
c       * Note: The average shrub & tree heights, and the average    *
c       *       shrub & tree populations are reduced by the factor   *
c       *       for reduction of woody biomass (BURNED).  It was     *
c       *       decided that this simplification would give adequate *
c       *       accuracy.  -- CRM -- 3/17/93.                        *
c       **************************************************************
c       ------ update average shrub height, using reduction in standing dead
c       wood after burning
c       (WEPP Equation 8.6.11)
        shgt(plant) = shgt(plant) * burned(iplane)
c       ------ average tree height
        thgt(plant) = thgt(plant) * burned(iplane)
c       ------ average number of shrubs along a 100 m transect
        spop(plant) = spop(plant) * burned(iplane)
c       ------ average number of trees along a 100 m transect
        tpop(plant) = tpop(plant) * burned(iplane)
c
c     *** L0 ELSE ***
c     If rainfall is > 7.5 mm, or five day antecedent moisture for
c     submerged residue > 25 mm, set day of burning 1 day later; ie,
c     delay burning 1 day.
c
      else
        jfdate(iplane) = jfdate(iplane) + 1
c
c     *** L0 ENDIF ***
      end if
c
      return
      end
