      subroutine rherb(plant)
c
c     NOTE: The definitions in CRINPT6.INC were missing.  Some have
c           been supplied from other sources in the new include file
c           (~/wepp/v9225/weltz/crinpt4.inc).  This file should be
c           included with the recoded files, and the missing definitions
c           must be supplied.  -- CRM -- 3/16/93.
c
c     NOTE: The definitions in CRINPT4.INC have been corrected.  This
c           new include file (~/wepp/v9225/weltz/crinpt4.inc) should
c           be included with the recoded files.  -- CRM -- 3/04/93.
c
c     NOTE: Variable IDEATH seems to serve no purpose outside RHERB.
c           It should be removed from common block RINPT6, and used
c           as a local variable in RHERB.  It should be removed from
c           RANGE, and set to zero at the top of RHERB. -- CRM -- 3/16/93.
c           Mark agrees.  RE: 3/16/93 telcon.
c
c     + + + PURPOSE + + +
c     Simulates killing of live plants with either soil herbicide or
c     foliar herbicide.  Herbicide application can either increase
c     or decrease potential production, and can change access of forage
c     for grazing livestock.  Application will change canopy cover,
c     LAI, litter cover, and root biomass.
c
c     Called from RANGE
c     Author(s): Weltz, Meyer
c     Reference in User Guide: Chapter 8
c
c     Changes:
c          1) Dereferenced common blocks: CRPVR2, CRPVR3, CRPVR5, CONS,
c             CRPOUT, CRPPRM, ENDS, PARAME, RINPT2, RINPT3, and RINPT5.
c          2) Dereferenced include files: 'ptilty.inc' & 'pmxpnd.inc'.
c          3) Eliminated global SAVE of local variables.
c          4) Combined the following to IF's:
c                if(tlive(iplane).gt.0.0)then
c                  if(sdate.eq.ihdate(iplane))then
c          5) Changed references to NOWRES to '1'.
c          6) Changed
c                ideath(iplane) = 5
c             to:
c                ideath(iplane) = 0
c             Re: 3/4/93 telcon with Mark Weltz.  (Setting ideath.ne.1)
c          7) Calculation for tlive did not include evergreen fraction:
c                 tlive(iplane)= (tlive(iplane)-xlive(iplane))*
c    1                                                   dleaf(iplane)
c             Changed to:
c                 tlive(iplane) = tlive(iplane) - hold - adhere
c             Re: 3/4 & 3/16 telcon w/Weltz and 3/17/93 telephone message
c                 from Mark Weltz.
c          8) Changed PLIVE(PLANT) to PLIVE(PLANT,IPLANE). -- CRM -- 5/14/93.
c          9) PROOT was altered analogous to PLIVE. -- CRM -- 5/14/93.
c         10) Changed variable "woody" usage from real to integer. 2/16/94 dcf
c
c
c
c     Version: This module recoded from WEPP Version 92.25.
c     Date recoded: 01/29/93 - 03/16/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pntype.inc'
      include 'pmxelm.inc'
      include 'pmxnsl.inc'
      include 'pmxres.inc'
      include 'pmxpln.inc'
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
c      modify: rmogt(mxres,mxplan)
c
      include 'cclim.inc'
c        read: am2(mxplan)
c
      include 'chydrol.inc'
c        read: rain(mxplan)
c
      include 'crinpt1.inc'
c        read: woody(mxplan)
c      modify: plive(ntype,mxplan), proot(ntype,iplane)
c       write: access(mxplan)
c
      include 'crinpt4.inc'
c        read: active, herb, update, regrow, dleaf
c      modify: ihdate(mxplan), xlive(mxplan)
c
      include 'crinpt6.inc'
c      modify: ideath
c       write: sdead
c
      include 'crout.inc'
c      modify: tlive(ntype,mxplan)
c
      include 'cstruc.inc'
c        read: iplane
c
      include 'cupdate.inc'
c        read: sdate
c
c     + + + LOCAL VARIABLES + + +
      real hold, adhere, drop
c
c     + + + LOCAL DEFINITIONS + + +
c     hold   - amount of non-evergreen leaves killed by herbicide (kg/m^2)
c     adhere - amount of evergreen leaves killed by herbicide
c     drop   - amount of grass and forb (herbaceous) leaves killed by herbicide
c
c     + + + END SPECIFICATIONS + + +
c
c
c      *** L0 IF ***
c      LIVE MATERIAL EXISTS, and this is the herbicide application date.
      if ((tlive(plant).gt.0.0).and.(sdate.eq.ihdate(iplane))) then
c
c       *** L1 IF ***
c       (FOLIAR HERBICIDE)
        if (active(iplane).eq.0) then
c
c         *** L2 IF ***
c         -------- If rainfall <= 10 mm, permit herbicide application.
          if (rain(iplane).le.0.01) then
c           ---------- set flag to kill plants immediately
            ideath(iplane) = 1
c           ---------- (EVERGREENS PRESENT)
            if (xlive(iplane).gt.0.0) then
c             ------------ (MIXED POPULATION)
              if (tlive(plant).gt.xlive(iplane)) then
c               -------------- amount of non-evergreen leaf biomass killed
c               (WEPP Equation 8.6.18)
                hold = (tlive(plant)-xlive(iplane)) * (1.0-
     1              dleaf(iplane))
c               -------------- amount of evergreen leaf biomass killed
c               (WEPP Equation 8.6.19)
                adhere = xlive(iplane) * (1.0-herb(iplane))
c               -------------- total amount of leaf biomass remaining
c               tlive(plant) = tlive(plant)-hold-adhere
c               -------------- amount of evergreen leaf biomass remaining
                xlive(iplane) = xlive(iplane) * herb(iplane)
c             ------------ (ONLY EVERGREENS)
              else
c               -------------- amount of evergreen leaf biomass killed
c               (WEPP Equation 8.6.19)
                adhere = xlive(iplane) * (1.0-herb(iplane))
c               -------------- amount of evergreen leaf biomass remaining
                xlive(iplane) = xlive(iplane) * herb(iplane)
c               -------------- total amount of live biomass today
                tlive(plant) = xlive(iplane)
              end if
c           ---------- (NO EVERGREENS PRESENT)
            else
c             ------------ amount of grass and forb (herbaceous) leaves killed
c             (WEPP Equation 8.6.17)
              drop = tlive(plant) * (1.0-dleaf(iplane))
c             ------------ total amount of live biomass today
              tlive(plant) = tlive(plant) - drop
            end if
c
c         -------- Rainfall exceeds 10 mm.  Postpone herbicide application 1 day.
c         *** L2 ELSE ***
          else
            ihdate(iplane) = ihdate(iplane) + 1
c
c         *** L2 ENDIF ***
          end if
c
c       *** L1 ELSE ***
c       (GRANULAR [SOIL] HERBICIDE)
        else
c
c         *** M2 IF ***
c         -------- Either 12.5 mm of rain has fallen, or the 5-day antecedent
c         moisture for submerged residue >= 12.5 mm.
c
c XXX     NOTE - in future may want to change "rain(iplane)" to
c XXX     "rain+melt+irdept" to allow impact of snow melt and
c XXX     irrigation water.   dcf  12/19/94

          if (rain(iplane).gt.0.0125.or.am2(iplane).gt.0.0125) then
c           ---------- set flag to kill plants
            ideath(iplane) = 1
c
c           *** M3 IF ***
c           ---------- (EVERGREENS PRESENT)
            if (xlive(iplane).gt.0.0) then
c             ------------ (MIXED POPULATION)
              if (tlive(plant).gt.xlive(iplane)) then
c               -------------- non-evergreen biomass killed
c               (WEPP Equation 8.6.18)
                hold = (tlive(plant)-xlive(iplane)) * (1.0-
     1              dleaf(iplane))
c               -------------- amount of evergreen leaf biomass killed
c               (WEPP Equation 8.6.19)
                adhere = xlive(iplane) * (1.0-herb(iplane))
c               -------------- amount of evergreen leaf biomass remaining
                xlive(iplane) = xlive(iplane) * herb(iplane)
c               -------------- total amount of leaf biomass remaining
                tlive(plant) = tlive(plant) - hold - adhere
c
c             ------------ (ONLY EVERGREENS)
              else
c               (WEPP Equation 8.6.19)
c               -------------- amount of evergreen leaf biomass killed
                adhere = xlive(iplane) * (1.0-herb(iplane))
c               -------------- amount of evergreen leaf biomass remaining
                xlive(iplane) = xlive(iplane) * herb(iplane)
c               -------------- total amount of live biomass today
                tlive(plant) = xlive(iplane)
              end if
c
c           *** M3 ELSE ***
c           ---------- (NO EVERGREENS PRESENT)
            else
c             ------------ amount of grass and forb (herbaceous) leaves killed
c             (WEPP Equation 8.6.17)
              drop = tlive(plant) * (1.0-dleaf(iplane))
c             ------------ total amount of live biomass today
              tlive(plant) = tlive(plant) - drop
c
c           *** M3 ENDIF ***
            end if
c
c         *** M2 ELSE ***
c         -------- 12.5 mm of rain has NOT fallen, and the 5-day antecedent
c         moisture for submerged residue < 12.5 mm.
          else
            ihdate(iplane) = ihdate(iplane) + 1
c
c         *** M2 ENDIF ***
          end if
c
c       *** L1 ENDIF ***
        end if
c
c     *** L0 ENDIF ***
      end if
c
c
c     *********************
c     *  UPDATE RESIDUE *
c     *********************
c
c     *** N0 IF ***
c     ---- If there has been death by herbicide...
      if (ideath(iplane).eq.1) then
c
c       *** N1 IF ***
c       ------ (NO EVERGREENS PRESENT)
        if (xlive(iplane).eq.0.0) then
c         -------- no woody plants ==> death is immediate and residue falls to ground
          if (woody(iplane).eq.0) then
c           ---------- residue on ground
c           (WEPP Equation 8.6.23)
            rmogt(1,iplane) = rmogt(1,iplane) + drop
          else
c           ---------- standing residue of woody plants ==> may take months to fall
            sdead(iplane) = drop
          end if
c
c       *** N1 ELSE ***
c       ------ (EVERGREENS PRESENT)
        else
c
c         -------- no woody plants ==> death is immediate and residue falls to ground
c         *** N2 IF ***
          if (woody(iplane).eq.0) then
c           ---------- (MIXED POPULATION)
            if (tlive(plant).gt.xlive(iplane)) then
c             ------------ residue on ground
c             (WEPP Equation 8.6.24)
              rmogt(1,iplane) = rmogt(1,iplane) + adhere + hold
c           ---------- (ONLY EVERGREENS)
            else
c             ------------ residue on ground
              rmogt(1,iplane) = rmogt(1,iplane) + adhere
            end if
c
c         *** N2 ELSE ***
c         -------- woody plants ==> standing residue may take months to fall
          else
c           ---------- (MIXED POPULATION)
            if (tlive(plant)-xlive(iplane).gt.0.0) then
c             ------------ standing residue
c             (WEPP Equation 8.6.25)
              sdead(iplane) = hold + adhere
c           ---------- (ONLY EVERGREENS)
            else
c             ------------ standing residue
              sdead(iplane) = adhere
            end if
c
c         *** N2 ENDIF ***
          end if
c
c       *** N1 ENDIF ***
        end if
c
c       ------ change access of forage for livestock to fraction available after
c       herbicide application
        access(iplane) = update(iplane)
c
c       ------ either increase or decrease potential plant growth because of herbicide
c       (WEPP Equation 8.6.20)
        plive(plant,iplane) = plive(plant,iplane) * regrow(iplane)
c
c       ------ either increase or decrease potential root growth because of herbicide
c       (WEPP Equation 8.6.21)
        proot(plant,iplane) = proot(plant,iplane) * regrow(iplane)
c
c       ideath(iplane) = 5
        ideath(iplane) = 0
c     -- XXX -- See note RE: ideath, at top of routine. -- CRM -- 3/16/93.
c
c     *** N0 ENDIF ***
      end if
c
      return
      end
