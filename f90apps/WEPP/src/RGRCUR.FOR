      subroutine rgrcur(wst,growth,plive,ptlive,xlive,stress,rmagy,
     1    slive,tlive,add,death,remove,rmagt)
c
c     + + + PURPOSE + + +
c     Estimates range vegetation plant growth (mass) assimilated today,
c     corrected for water stress.
c
c     Called from RANGE
c     Author(s): Weltz, Meyer
c     Reference in User Guide: Chapter 8
c
c     Changes:
c          1) Changed the parameter order to conform to the WEPP Coding
c             Convention.  Changed:
c    i                    wst,growth,plive,ptlive,
c    m                    slive,tlive,
c    i                    xlive,
c    m                    add,death,remove,
c    i                    stress,
c    m                    rmagt,
c    i                    rmagy)
c             to read:
c    i                    wst,growth,plive,ptlive,xlive,stress,rmagy,
c    m                    slive,tlive,add,death,remove,rmagt)
c          2) If-elseif-elseif was structured such that last elseif
c             could never be executed.
c          3) Did a major re-write of the logic structure.  Code was
c             originally written with a structure that had a
c             prior-to-peak section, an at-the-peak section, and an
c             after-peak section, within both a water-stress section
c             and a non-stress section; ie, 6 sections of largely
c             redundant code.  The prior-to-peak and at-the-peak
c             sections were recoded such that they were collapsed
c             into one section.  The water stress code was inserted
c             into it the prior-to-peak section.  The result was 2
c             sections of code.
c
c     Version: This module recoded from WEPP Version 92.25.
c     Date recoded: 02/05/93 to 3/29/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real wst, growth, plive, ptlive, xlive, stress, rmagy, slive,
     1    tlive, add, death, remove, rmagt
c
c     + + + ARGUMENT DEFINITIONS + + +
c     wst    - water stress (0-1) 1=no stress; 0=total stress
c     growth - fraction of potential (unstressed) leaf biomass accrued
c              today, minus fraction accrued yesterday (can be negative)
c     plive  - potential (wo / stress) standing live biomass to date.
c     ptlive - previous day's total live leaf biomass
c     xlive  - total live evergreen leafy component on this day
c     stress - coefficient used to calculate amount of material surviving
c              water stress.
c     rmagy  - standing residue yesterday
c     slive  - new plant growth on day on simulation (can be negative)
c     tlive  - total live plant material on day of simulation
c     add    - amount added to residue today if plants have started losing
c              leaf material.
c     death  - fraction of plant material killed by drought today
c     remove - amount of plant material killed by drought today
c     rmagt  - standing residue today
c
c     + + + END SPECIFICATIONS + + +
c
c
c      *** L0 IF ***
c ---- Growth has not peaked-out yet.  Plants are gaining leaf material.
      if (growth.ge.0.0) then
c       ------ potential amount of plant growth today
c       (WEPP Equation 8.5.9)
        slive = growth * plive
c
c       (NO DROUGHT STRESS)
        if (wst.ge.0.99) then
          remove = 0.0
c       (DROUGHT STRESS)
        else
c         -------- fraction remaining alive after water stress
c         (WEPP Equation 8.5.8)
          death = 1 - exp(-(3.5+stress))
c         -------- amount of plant material killed by drought today
          remove = ptlive * (1.0-death)
        end if
c
c       ------ amount of total live plant material
        tlive = ptlive + slive - remove
c       -- XXX -- This is also done in RANGE -- CRM -- 3/30/93.
c       ------ do not let leafdrop occur in evergreens
        if (tlive.lt.xlive) tlive = xlive
c       ------ add material killed by drought to standing residue
        rmagt = rmagy + remove
        if (rmagt.lt.0.00001) rmagt = 0.00001
c
c     *** L0 ELSE ***
c     ------ Growth peak has occurred.  Plants are loosing leaf material.
      else
c       ------ There was live leaf mass yesterday....
        if (ptlive.gt.0.0) then
c         -------- potential amount of plant material lost today (growth < 0)
c         (WEPP Equation 8.5.9)
          slive = growth * plive
c         -------- amount of total live plant material (declining)
          tlive = ptlive + slive
c         -- XXX -- This is also done in RANGE -- CRM -- 3/30/93.
c         -------- do not let leafdrop occur in evergreens
          if (tlive.le.xlive) then
            tlive = xlive
            add = 0.0
          else
            add = -slive
          end if
c         -------- add dead material to residue
          rmagt = rmagy + add
          if (rmagt.lt.0.00001) rmagt = 0.00001
        end if
c
c     *** L0 ENDIF ***
      end if
c
      return
      end
