      subroutine rgraze(plant)
c
c     NOTE: THE VARIABLES IN CRINPT6.INC NEED DEFINITIONS. -- CRM -- 2/4/93.
c
c     NOTE: The new include file (~/wepp/v9225/weltz/crout.inc) should be
c           included with the recoded files.  -- CRM -- 3/16/93.
c
c     NOTE: The variable DIGEST is distinct from the DIGEST in CPEREN.INC.
c           It is a local variable, and does NOT need to be dimensioned to
c           MXPLAN.  -- CRM -- 3/17/93.
c
c     + + + PURPOSE + + +
c     Calculates the amount of mass consumed by livestock.
c     RGRAZE will alter canopy, surface cover and LAI.
c
c     Called from RANGE
c     Author(s): Weltz
c     Reference in User Guide: Chapter 8
c
c     Changes:
c          1) Dereferenced the following common blocks:
c             CRPVR3, CRPVR5, CLIM, COVER, CRPOUT, CRPPRM, ENDS,
c             HYDROL, PARAME, RINPT3, RINPT5.
c          2) Dereferenced include files: 'ptilty.inc' and 'pmxpnd.inc'.
c          3) Removed local variable NOWRES.
c          4) Introduced local variable TMPVR1.
c          5) Deleted local variable RMINIT (which is really a global
c             variable in common block CCRPVR2).
c          6) Added following code:
c              else
c                rmogt(1,iplane) = rmogt(1,iplane) + rmagt(iplane)
c                rmagt(iplane) = 0.0
c          7) Code involving PLIVE modified because grazing/nongrazing
c             on different OFES, herbicide application, and burning
c             had been overlooked in calculation of PLIVE in RNGINT. --
c             CRM -- 5/11/93.
c
c     Version: This module recoded from WEPP Version 92.25.
c     Date recoded: 02/02/93 - 03/17/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pntype.inc'
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
c      modify: rmagt(mxplan), rmogt(mxres,mxplan)
c
      include 'ccrpvr2.inc'
c       write: vdmt(mxplan)
c
      include 'crinpt1.inc'
c        read: bodywt, digmin, digmax, access, area, gday, gend, ssday,
c              send, jgraz, plive(ntype,mxplan), animal
c      modify: suppmt(mxplan)
c
      include 'crinpt2.inc'
c        read: rgcmin
c
      include 'crinpt4.inc'
c        read: decomp
c      modify: xlive
c
      include 'crinpt6.inc'
c        read: dinner, growth
c      modify: tdsup, rtotsu, rsuppm(mxplan), food
c
      include 'crout.inc'
c      modify: tfood(mxplan), feed, unbio, tlive(mxplan)
c
      include 'cstruc.inc'
c        read: iplane
c
      include 'cupdate.inc'
c        read: sdate
c
c     + + + LOCAL VARIABLES + + +
c -- XXX -- (See note RE: mxplan above.)
      real digest(mxplan)
      real dl, dlr, avabio, atlive, asdead, sd, tr, tmpvr1
      integer i
c
c     + + + LOCAL DEFINITIONS + + +
c     digest -  weighted average digestability of forage
c     dl     -  ratio of live to dead standing biomass
c     dlr    -  "live to dead ratio" of what is grazed.  Used to calculate
c               DIGEST from DL.
c     avabio -  forage available for consumption by livestock
c     atlive -  available live forage for livestock consumption
c     asdead -  available dead forage for livestock consumption
c     sd     -  stock density (number of livestock per ha)
c     tr     -  standing dead mass transfered to residue by trampling
c               action of livestock
c
c     + + + END SPECIFICATIONS + + +
c
c
c*********************************************************************
c                                                                    *
c   GLOBAL variables                                                 *
c     rsuppm :  emergency food supplied by model to prevent          *
c               livestock from straving and dieing.  If this is      *
c               activated the user needs to be altered that senerio  *
c               has been altered and the stocking rate should be     *
c               altered to reflect realistic grazing practices.      *
c     rtotsu :  running total of emergency food supplied by model    *
c     totsup :  running total of supplemental feed consumed          *
c     feed   :  daily mass of forage per animal when not             *
c               supplemental feeding                                 *
c     food   :  daily mass of forage per animal when supplemental    *
c               feeding is being utilized                            *
c     tfood  :  running total of forage consumed by livestock        *
c     unbio  :  forage not avaliable for consumption by livestock    *
c                                                                    *
c*********************************************************************
c
c
c      *** Begin L0 Do-Loop ***
c      For each grazing cycle in this OFE...
      do 10 i = 1, jgraz(iplane)
c       ------ If today is past the day grazing stops, reset FOOD & RSUPPM to zero.
        if (sdate.gt.gend(i,1,iplane)) then
          food(iplane) = 0.0
          rsuppm(iplane) = 0.0
        end if
c
c       *** L1 IF ***
c       (DAYS WITHIN GRAZING CYCLE)
        if (sdate.ge.gday(i,1,iplane).and.sdate.le.gend(i,1,iplane))
     1      then
c
c         -------- ratio of live-to-dead standing (above ground) biomass
          if (rmagt(iplane).gt.0.0) then
            dl = tlive(iplane) / rmagt(iplane)
          else
            dl = 0.0
          end if
c
c         -------- digestability of forage based on live-to-dead ratio
          if (dl.le.0.1) then
            digest(iplane) = digmin(iplane)
          else if (dl.ge.1.0) then
            digest(iplane) = digmax(iplane)
          else
c           (WEPP Equation 8.6.2)
            dlr = 1.0 - exp(-5.0*dl)
c           (WEPP Equation 8.6.1)
            digest(iplane) = dlr * digmax(iplane) + (1.0-dlr) *
     1          digmin(iplane)
          end if
c
c         -------- daily forage requirement per grazing animal.
c         (WEPP Equation 8.6.3)
          feed = 0.1 * ((bodywt(i,1,iplane)**0.75)/digest(iplane))
          if (feed.gt.20.0) feed = 20.0
c
c         -------- unavailable forage
c         (WEPP Equation 8.6.7)
          unbio = (1.-access(iplane)) * (rmagt(iplane)+tlive(iplane))
c         -------- forage available for consumption by livestock
c         (Assumes stock does not eat material from ground. -- CRM)
          avabio = (rmagt(iplane)+tlive(iplane)) - unbio
c
c         -------- Live & dead forage available to animals, based on access.
          if (access(iplane).lt.1.0) then
            tmpvr1 = avabio / (rmagt(iplane)+tlive(iplane))
c           ---------- available live forage for livestock consumption
            atlive = tlive(iplane) * tmpvr1
c           ---------- available dead forage for livestock consumption
            asdead = rmagt(iplane) * tmpvr1
          else
            atlive = tlive(iplane)
            asdead = rmagt(iplane)
          end if
c
c         calculate supplemental feeding.
c         to correct forage consumption and place on a meter
c         square bases must divide by area (units = m**2).
c
          suppmt(iplane) = 0.0
          rsuppm(iplane) = 0.0
c
c         (SUPPLEMENTAL FEEDING)
          if (sdate.ge.ssday(i,iplane).and.sdate.lt.send(i,iplane)) then
c           ---------- average supplemental feed per day
            suppmt(iplane) = dinner(iplane)
c           ---------- daily total vegetative uptake in addition to supplemental
c           feeding, on an area basis.
c           (WEPP Equation 8.4.18)
c           -- XXX -- This is not EXACTLY WEPP Equation 8.4.18 -- the equation in
c           the user doc neglects SUPPMT; however, that equation is for
c           CROPLAND anyway! -- CRM -- 2/04/93.  Mark Weltz needs to add
c           this equation to the rangeland documentation. RE: 3/16/93
c           telcon w/Weltz.  -- CRM -- 3/17/93.
            food(iplane) = (feed-suppmt(iplane)) * animal(i,1,iplane) /
     1          area(iplane)
c           ---------- total supplemental feed required so far
            totsup(iplane) = totsup(iplane) + suppmt(iplane)
c         (NO SUPPLEMENTAL FEEDING)
          else
c           (WEPP Equation 8.4.18)
            food(iplane) = feed * animal(i,1,iplane) / area(iplane)
          end if
          if (food(iplane).lt.0.0) food(iplane) = 0.0
c
c         if reduced forage then model must supply supplemental feed
c         to stop livestock form starving to death
c
c         If the daily vegetative uptake is more than 25% of the
c         available forage, ....
          if (food(iplane).gt.0.25*avabio) then
            rsuppm(iplane) = food(iplane) / animal(i,1,iplane) *
     1          area(iplane)
            rtotsu(iplane) = rtotsu(iplane) + rsuppm(iplane)
            food(iplane) = 0.0
          end if
c
c         calculate consumption of forage.
c
          if (food(iplane).le.0.0) then
            continue
          else if (tlive(iplane).le.food(iplane)) then
            if (asdead.gt.food(iplane)) then
              rmagt(iplane) = rmagt(iplane) - food(iplane)
            end if
          else if (asdead.gt.food(iplane).and.atlive.gt.food(iplane))
     1        then
            if (dl.le.0.1) then
              rmagt(iplane) = rmagt(iplane) - food(iplane)
            else if (dl.ge.1.0) then
              tlive(iplane) = tlive(iplane) - food(iplane)
            else
c             (WEPP Equation 8.6.2)
              dlr = 1.0 - exp(-5.0*dl)
              tlive(iplane) = tlive(iplane) - (dlr*food(iplane))
              rmagt(iplane) = rmagt(iplane) - ((1.-dlr)*food(iplane))
            end if
          else
            if (asdead.lt.food(iplane).and.atlive.gt.food(iplane))
     1          tlive(iplane) = tlive(iplane) - food(iplane)
          end if
c
c         -------- If there is (now) more live evergreen biomass than total live
c         biomass, adjust the evergreen biomass.
          if (xlive(iplane).gt.tlive(iplane)) xlive(iplane) =
     1        tlive(iplane)
c
c         -------- lower the minimum leaf area as a function of grazing.
          if (rmagt(iplane).gt.0.0) then
c           ---------- stock density (animals / ha)
            sd = animal(i,1,iplane) / area(iplane) * 10000.0
c           ---------- trampling effect on cover
c           (WEPP Equation 8.6.8)
            tr = 0.05 * rmagt(iplane) * (1.0-exp(-0.01*sd))
c           ---------- convert standing residue to fallen by trampling.
            if (rmagt(iplane).gt.tr) then
              rmagt(iplane) = rmagt(iplane) - tr
              rmogt(1,iplane) = rmogt(1,iplane) + tr
c           ---------- convert remaining standing residue to fallen
            else
              rmogt(1,iplane) = rmogt(1,iplane) + rmagt(iplane)
              rmagt(iplane) = 0.0
            end if
          end if
c
          vdmt(iplane) = tlive(iplane) + rmagt(iplane) + dwood(iplane)
          tfood(iplane) = tfood(iplane) + food(iplane)
c
c       *** L1 ELSE ***
c       (NOT WITHIN GRAZING CYCLE)
        else
c         -------- Set evergreen phytomass to be the entire amount of live material.
c         -- XXX -- This doesn't look right.  It assumes there is no deciduous or
c         herbaceous foliage.  It also might be discontinuous.  -- CRM --
c         5/11/93.
c         Original code:
c         if((growth(iplane) .gt. 0.0) .and.
c         1       (xlive(iplane) .lt. rgcmin(plant)*plive(plant)) .and.
c         2       (xlive(iplane) .lt. tlive(iplane)))
c         3                                    xlive(iplane)=tlive(iplane)
c
c         -------- if there's growth today, and evergreen_live < total_live...
          if ((growth(iplane).gt.0.0).and.(xlive(iplane).lt.
     1        tlive(iplane)).and.(xlive(iplane).lt.rgcmin(plant)*
     1        plive(plant,iplane))) xlive(iplane) = tlive(iplane)
c
c       *** L1 ENDIF ***
        end if
c
c     *** End L0 Do-Loop ***
   10 continue
c
      return
      end
