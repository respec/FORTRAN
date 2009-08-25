      subroutine ptgrp(nowcrp)
c
c     + + + PURPOSE + + +
c     This is the plant growth subroutine for perennials.  It
c     predicts canopy cover, canopy height, root mass in the
c     soil layers, root depth, leaf area index, and plant basal
c     area.
c
c     Called from WATBAL
c     Author(s):  Alberts, Ghiddey, Ferris, Arnold, Flanagan
c     Reference in User Guide:
c
c     Changes:
c          1) Common blocks CLIYR & RIDGE are not used.  They
c             were dereferenced.
c          2) Blank lines and some block-IF's eliminated.
c          3) Embedded RETURN's & GOTO eliminated.
c          4) The line:
c               if(cancov(iplane).ge.1.0) cancov(iplane)=0.999
c             changed to:
c               if(cancov(iplane).gt.0.999) cancov(iplane)=0.999
c          5) Flag IFREEZ was backwards; ie, zero during freezing,
c             and one during warm temps.  This was corrected.
c          6) ICOUNT changed to an array; ie, ICOUNT(MXPLAN), for
c             V-92.25.  This lets PTGRP keep up with the number of
c             consecutive days below the upper temperature limit,
c             so growth can resume after 5 days, FOR EACH OFE.
c             This does not change ANY of the results reported by
c             TEST1, TEST2, or TEST3. -- CRM -- 5/19/92.
c          7) IDECOM changed to an array; ie, IDECOM(MXPLAN),
c             because it is never USED until the iteration AFTER
c             it is SET, and there is no guarantee it is processing
c             the same OFE. -- CRM -- Re: 5/21/92 conversation with
c             Jeff Arnold.
c          8) Order of grazing & cutting, and sections of code to
c             determine temperatures outside growth regime, were
c             switched to put ALL temperature code together.  This
c             resulted in NO CHANGES to results from Tests 1,2,& 3.
c             CRM -- 5/21/92.
c          9) IBURN changed to an array; ie, IBURN(MXPLAN) -- (like
c             IFREEZ), because it is never USED until the iteration
c             AFTER it is SET, and there is no guarantee it is
c             processing the same OFE. -- CRM -- 5/21/92.
c         10) Embedded RETURN's removed by use of XITFLG.
c         11) Where GROW is called to grow the plants, IDECOMP
c             (which is undefined) was changed to IDECOM(IPLANE).
c             CRM -- 6/09/92.
c         12) Variables IFLAG, X5, & X6 are not needed for the
c             call to GROW, since they are always zero.  They were
c             deleted.  6/09/92 -- CRM
c         13) The line:
c                  hia(iplane) = 0.0
c             was moved from *above* the equation that updates leaf
c             index, to *below* it.  6/26/92 -- CRM
c         14) Code added to zero HIA, when VDMT is zero'ed.
c         15) In order to conform to the WEPP Coding Convention, the
c             5th - 8th arguments to GROW moved after the 2nd one,
c             and the last argument moved after the 3rd one.  Cor-
c             responding changes made in the call to GROW from this
c             routine.
c         16) Following lines added above call to GROW:
c                   x1 = .01
c                   x2 = .95
c                   x3 = 5
c                   x4 = 15
c                   call scurv(x1,x2,x3,x4,x5,x6)
c             This permits cropland perennials to be handled like
c             rangeland perennials in GROW for calculating FRST.
c         17) Made some pretty major changes to the structure of
c             the code to make it easier to read/understand.
c         18) Deleted dummy parameters TMPFLG, ISTART, & TRTMAS from
c             GROW, and its calls from PTGRA, PTGRP, and RANGE.
c         19) Added code to allow computation of average yields
c             of perennial cuttings.  Addition of CYIELD.INC.
c             dcf  4/93
c         20) Changed code to no longer use hia(iplane) and
c             vdmx(iplane) in computations of perennial harvests
c             and associated variables.   Dereferenced CCRPGRO.INC
c             Added include CDECVAR.INC to now use "cuthgt" in the
c             calculation of perennial yields.    dcf  4/93
c
c     Version: This module recoded from WEPP version 91.40.
c     Date recoded: 10/04/91 - 10/9/91.
c     Note: This module is current with WEPP release version 91.50.
c     Date checked: 12/5/91.
c     Note: This module is current with WEPP release version 92.25.
c     Date recoded: 5/19/92 - 7/31/92.
c     Recoded by: Charles R. Meyer.
c     Note: This module is current with WEPP version 93.05   dcf 5/19/93
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pntype.inc'
      include 'pmxnsl.inc'
      include 'ptilty.inc'
      include 'pmxgrz.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxcrp.inc'
      include 'pmxcut.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxhil.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nowcrp
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nowcrp - index of current crop
c
c     + + + COMMON BLOCKS + + +
c
      include 'ccrpvr1.inc'
c     modify: rmagt(mxplan), rtm(mxres,iplane)
c
      include 'ccrpvr2.inc'
c     modify: vdmt(mxplan)
c
      include 'ccrpvr3.inc'
c       read: gdd, hmax, bb, bbb
c     modify: sumgdd(mxplan)
c
      include 'ccrpvr5.inc'
c       read: ncount(mxplan)
c
      include 'cclim.inc'
c       read: tmnavg, tmxavg
c
      include 'ccover.inc'
c       read: canhgt(mxplan)
c     modify: cancov(mxplan)
c
      include 'ccrpout.inc'
c       read: rtd(mxplan)
c     modify: rtmass(mxplan), rtm15(mxplan)
c      write: lai(mxplan), rtm30(mxplan), rtm60(mxplan)
c
      include 'ccrpprm.inc'
c       read: jdplt(mxcrop,mxplan), itype(mxcrop,mxplan)
c     modify: jdharv(mxcrop,mxplan)
c
      include 'cdecvar.inc'
c       read: cuthgt(ntype)
c
      include 'cflags.inc'
c     read: yldflg
c
c
      include 'cnew1.inc'
c       read: critvm(mxtype)
c
      include 'cperen.inc'
c       read: mgtopt,ncycle,digest,jdstop,tmpmin,tmpmax
c
      include 'cptgrow.inc'
c
c
      include 'crinpt1.inc'
c       read: animal,bodywt,gday,gend,area,yield
c
      include 'crout.inc'
c     modify: tlive(mxplan)
c
      include 'cstruc.inc'
c       read: iplane
c
      include 'cupdate.inc'
c       read: sdate
c
      include 'cyield.inc'
c       read: yldflg
c     modify: sumyld(ntype,mxplan),
c             iyldct(ntype,mxplan)
c
c     + + + LOCAL VARIABLES + + +
      real pyld, vdmact, tlfood, x1, x2, x3, x4, x5, x6
      integer xitflg, iadflg
c
c     + + + LOCAL DEFINITIONS + + +
c     tlfood - biomass removed by grazing (km/m^2)
c     iburn  - Flag.  Tells whether ambient temperature is above upper
c              limit for plant growth.  0=OK; 1=too hot.  Stops growth
c              for 5 days.
c     icount - number of consecutive days below upper temperature limit,
c              once limit is exceeded.  (Five days required to resume
c              growth.)
c     idecom - Flag to indicate whether the current perennial plant
c              has been through an initial freeze cycle which would
c              cause a call to RESUP to update residue parameters.
c              idecom=1 (call RESUP)   idecom=0 (don't call RESUP)
c     ngraz  - Index of the current grazing cycle.
c     ifreez - Flag.  IFREEZ is set to 1 the first day the ambient
c              temperature falls below that required for [this type of]
c              plant growth.  At this time, live biomass is converted
c              to dead biomass.  IFREEZ is reset to zero on the first
c              "warm" day, and at planting.
c     pyld   - projected yield at maturity in SI units
c     vdmact - vegetative dry matter after cutting (in kg/m**2)
c     xitflg - Flag.  1=exit PTGRP.  (Equivalent to a RETURN.)
c
c     + + + SAVES + + +
c      save iburn, icount, ifreez, idecom, ngraz
c
c     + + + SUBROUTINES CALLED + + +
c     resup
c     initgr
c     grow
c
c     + + + DATA INITIALIZATIONS + + +
c      data ngraz /mxplan * 0/, idecom /mxplan * 0/, ifreez /mxplan * 1/,
c     1    icount /mxplan * 0/, iburn /mxplan * 0/
c
c     + + + OUTPUT FORMATS + + +
c2000 format(' HARVEST date ',i3,' plane ',i4,' yield ',f8.3,
c    1       '(kg/m**2)',f8.3,' (ton/ac) harvest index',f8.2)
c
c     + + + END SPECIFICATIONS + + +
c
      xitflg = 0
      iadflg = 0
c
c     After the first growing season, growth for perennial crops
c     is re-initiated when the 5-day average minimum temp. exceeds
c     critical min. temp.
c
c
c     If current date (SDATE) is NOT a planting date....
c     *** L0 IF ***
      if (sdate.gt.jdplt(nowcrp,iplane)) then
c
c       If this is NOT the day when perennial crop growth stops....
c       *** L1 IF ***
        if (sdate.lt.jdstop(nowcrp,iplane).or.jdstop(nowcrp,iplane).eq.0
     1      ) then
c
c
c         Make Adjustments to Crop Variables for Various Residue
c         Management Options
c
c         *** L2 IF ***
c         Management Option 1 (cutting with removal OR without removal)
c         if (mgtopt(nowcrp,iplane).eq.1 .or.
c    1        mgtopt(nowcrp,iplane).eq.4) then
c
c         Management Option 1 (cutting with removal)
          if (mgtopt(nowcrp,iplane).eq.1) then
            if (sdate.eq.jdharv(nowcrp,iplane)) then
c
c             New way to compute yield for cut perennial crops is by use
c             of input variable "cuthgt"    ----  dcf  4/8/93
c
c             If canopy height is greater than cutting height - then you
c             will have a positive value for yield.  We assume here that
c             vegetative biomass being harvested through the cutting
c             operation is uniformly distributed with plant height.
c
              if (canhgt(iplane).gt.cuthgt(itype(nowcrp,iplane))) then
                canhgt(iplane) = cuthgt(itype(nowcrp,iplane))
c
c               Update vegetative dry matter today
c
                vdmact = log(1.0-canhgt(iplane)/
     1              hmax(itype(nowcrp,iplane))) / (-
     1              bbb(itype(nowcrp,iplane)))
                pyld = vdmt(iplane) - vdmact
c
c               Calculate initial crop parameter values for simulation
c               of next cutting period
c
c               Update vegetative dry matter today
c
                vdmt(iplane) = vdmact
c
c               Update LAI and cumulative growing degree days
c
                lai(iplane) = (xmxlai(itype(nowcrp,iplane))*
     1              vdmt(iplane)) / (vdmt(iplane)+0.2756*
     1              exp(-13.6*vdmt(iplane)))
c
                sumgdd(iplane) = gddmax(itype(nowcrp,iplane)) *
     1              lai(iplane) / xmxlai(itype(nowcrp,iplane))
c
c               Equation 8.2.4
c               CANCOV = 1 - e ^ (BB * VDMT)
c
c               update canopy cover, based on vegetative dry matter (VDMT).
c
                cancov(iplane) = 1. -
     1              exp(-bb(itype(nowcrp,iplane))*vdmt(iplane))
c
                if (cancov(iplane).lt.0.0) cancov(iplane) = 0.0
                if (cancov(iplane).gt.0.999) cancov(iplane) = 0.999
c
c             ELSE if the canopy height is less than the cutting height
c             you will have no yield and no impact on canopy or LAI
c
              else
                pyld = 0.0
              end if
c
c             if(mgtopt(nowcrp,iplane).eq.1)
c    1          write (6,1000) sdate, iplane, pyld
c
              if (ivers.ne.3) write (6,1000) sdate, iplane, pyld
              if (ivers.eq.3) write (6,1200) sdate, iplane, pyld
              iadflg = 1
c
c
c             ADD a write statement to a file to store crop yields
c             for George Foster  3/25/93   dcf
c
c             if (mgtopt(nowcrp,iplane).eq.1. and. yldflg.eq.1) then
              if (yldflg.eq.1) then
c
                write (46,1100) itype(nowcrp,iplane), sdate, iplane,
     1              pyld
c
c               ADD summation variables to total up the yields of the
c               various crops on the various OFE's.  ADD variable to
c               ADD up the number of harvests of the various crops
c               on the various OFE's.
c
                sumyld(itype(nowcrp,iplane),iplane) =
     1              sumyld(itype(nowcrp,iplane),iplane) + pyld
                iyldct(itype(nowcrp,iplane),iplane) =
     1              iyldct(itype(nowcrp,iplane),iplane) + 1
c
              end if
c
c             NEW CODE to put cut material on soil surface as residue
c             cover.   dcf  4/25/94
c
c             if(mgtopt(nowcrp,iplane).eq.4)
c    1            call resup2(nowcrp,iplane,pyld)
c
            end if
c
c         Management Option 2 (grazing)
c         *** L2 ELSEIF ***
          else if (mgtopt(nowcrp,iplane).eq.2) then
c
c           *** L3 IF ***
            if (ngraz(iplane).eq.0) then
              ngraz(iplane) = 1
c
c           *** L3 ELSEIF ***
            else if (ngraz(iplane).gt.0) then
c
c             If current day is during grazing interval ....
              if (sdate.ge.gday(ngraz(iplane),nowcrp,iplane).and.sdate
     1            .lt.gend(ngraz(iplane),nowcrp,iplane)) then
c
c               If there's enough veg. dry matter (VDMT) for grazing,
c               calculate amount removed, and update VDMT.
c
                if (vdmt(iplane).gt.critvm(itype(nowcrp,iplane))) then
                  tlfood = ((0.1*(bodywt(ngraz(iplane),nowcrp,iplane)**
     1                0.75/digest(itype(nowcrp,iplane))))*
     1                animal(ngraz(iplane),nowcrp,iplane)) /
     1                area(iplane)
                  vdmt(iplane) = vdmt(iplane) - tlfood
                  if (vdmt(iplane).lt.0.0) vdmt(iplane) = 0.0
                end if
c
c             If current day is the day grazing ENDS ....
              else if (sdate.eq.gend(ngraz(iplane),nowcrp,iplane)) then
c
c               Prepare for next grazing cycle, and set harvest
c               date to end of grazing cycle.
                if (ngraz(iplane).lt.ncycle(nowcrp,iplane)) then
                  ngraz(iplane) = ngraz(iplane) + 1
                  jdharv(nowcrp,iplane) =
     1                gend(ngraz(iplane),nowcrp,iplane)
                else
                  ngraz(iplane) = 0
                  jdharv(nowcrp,iplane) = 0
                end if
              end if
c
c           *** L3 ENDIF ***
            end if
c
c         *** L2 ENDIF ***
          end if
c
c
c         *** M2 IF ***
c         Not cutting harvest today (normal growth and/or grazing) ....
          if (iadflg.eq.0) then
c
c           Determine whether ambient temperatures are within the
c           ranges required for growth of this plant type.
c
c           *** M3 IF ***
c           --If temp was below upper limit LAST time through PTGRP ....
            if (iburn(iplane).eq.0) then
c
c             *** M4 IF ***
c             If temp NOW is above upper limit ....
              if (tmxavg.gt.tmpmax(itype(nowcrp,iplane))) then
                iburn(iplane) = 1
c
c               *** M5 IF ***
c               If this is the date to stop perennial growth ....
                if (sdate.eq.jdstop(nowcrp,iplane)) then
c
c                 If residue parameters are to be updated ....
                  call resup(nowcrp,-1)
                  idecom(iplane) = 1
                  rmagt(iplane) = rmagt(iplane) + vdmt(iplane)
                  sumgdd(iplane) = 0.0
                  cancov(iplane) = 0.0
                  canhgt(iplane) = 0.0
                  vdmt(iplane) = 0.0
                  lai(iplane) = 0.0
                  tlive(iplane) = 0.0
                  ncount(iplane) = 0
c
c               *** M5 ENDIF ***
                end if
c
c             *** M4 ENDIF ***
              end if
c
c           *** M3 ELSE ***
c           If temp LAST exceeded upper limit ....
            else
c             -Max. temp. (still) exceeds upper limit.  Reset the clock.
              if (tmxavg.gt.tmpmax(itype(nowcrp,iplane))) then
                icount(iplane) = 0
                xitflg = 1
c
c             -----------  If temp NOW is below upper limit ....
              else
c               ------ If 5 days has not elapsed, count off another day.
                if (icount(iplane).lt.5) then
                  icount(iplane) = icount(iplane) + 1
                  xitflg = 1
c               ------ After 5 days, allow plant growth to resume.
                else
                  iburn(iplane) = 0
                end if
              end if
c
c           *** M3 ENDIF ***
            end if
c
c           *** N3 IF ***
c           If no problems are detected so far, continue.
            if (xitflg.eq.0) then
c
c             *** N4 IF ***
c             ---If temp was below lower limit yesterday ....
              if (ifreez(iplane).ne.0) then
c               -- If temp. is still below min. temp. ....
                if (tmnavg.le.tmpmin(itype(nowcrp,iplane))) then
                  xitflg = 1
c               -- Temp exceeds min temp today, allow growth to resume
                else
                  ifreez(iplane) = 0
                  rmagt(iplane) = rmagt(iplane) + vdmt(iplane)
                  call initgr(nowcrp)
                end if
c
c             *** N4 ELSEIF ***
c             If today is the FIRST day ambient temp. falls below min.
c             temp., kill plants.
              else if (tmnavg.le.tmpmin(itype(nowcrp,iplane))) then
                ifreez(iplane) = 1
c
                if (idecom(iplane).eq.1.and.vdmt(iplane).gt.0.0) then
                  call resup(nowcrp,-2)
                  idecom(iplane) = 0
                end if
c
c               On freeze date - convert live biomass to standing
c               dead residue (rmagt)
c
                rmagt(iplane) = rmagt(iplane) + vdmt(iplane)
                sumgdd(iplane) = 0.0
c
c               XXX  When freezing does occur - should CANCOV be reset to
c               zero immediately???  Don't the frozen-dead plants still
c               provide significant canopy cover???  Is this canopy
c               sufficiently accounted for by RMAGT, or does additional
c               code need to be developed that decreases CANCOV from
c               its pre-frost value????   Arnold needs to determine.
c               dcf  7/30/93
c
                cancov(iplane) = 0.0
c
c               XXX  When freezing occurs - should CANHGT be reset to zero
c               immediately?? Don't the frozen-dead plants still provide
c               significant canopy???  By resetting CANHGT to zero here
c               we immediately eliminate the contribution of the plant
c               stems in reducing effective shear stress on the soil in
c               the transport and erosion routines. Arnold / Gilley need
c               to evaluate this effect.  dcf  7/30/93
c
                canhgt(iplane) = 0.0
                vdmt(iplane) = 0.0
                lai(iplane) = 0.0
                xitflg = 1
                tlive(iplane) = 0.0
c
c  XXX          Set NCOUNT to zero - this resets senescence day counter
c               so that a perennial with no management (such as grapes
c               in Italy) will have senescence each year.  dcf  5/20/94
c
                ncount(iplane) = 0
c
c             *** N4 ENDIF ***
              end if
c
c             *** O3 IF ***
c             TEMPERATURE CONDITIONS FAVORABLE FOR GROWTH
              if (xitflg.eq.0) then
c
                x1 = .01
                x2 = .95
                x3 = 5
                x4 = 15
                call scurv(x1,x2,x3,x4,x5,x6)
                call grow(nowcrp,iplane,x5,x6,ncount(iplane),
     1              idecom(iplane))
c
c             *** O3 ENDIF ***
              end if
c
c
c           *** N3 ENDIF ***
            end if
c
c         *** M2 ENDIF ***
          end if
c
c
c       At JDSTOP (the date when growing perennial crop is stopped)
c       live above ground biomass and rootmass are converted to dead.
c
c       This IS the day when perennial crop growth stops....
c       *** L1 ELSEIF ***
        else if (sdate.eq.jdstop(nowcrp,iplane)) then
          call resup(nowcrp,-1)
          idecom(iplane) = 1
          rmagt(iplane) = rmagt(iplane) + vdmt(iplane)
          sumgdd(iplane) = 0.0
          cancov(iplane) = 0.0
          canhgt(iplane) = 0.0
          vdmt(iplane) = 0.0
          lai(iplane) = 0.0
          tlive(iplane) = 0.0
          ncount(iplane) = 0
c       *** L1 ELSE ***
        else
          continue
c
c       *** L1 ENDIF ***
        end if
c
c     Since this IS a planting date, initialize variables.
c     *** L0 ELSE IF ***
      else if (sdate.eq.jdplt(nowcrp,iplane)) then
        if (ivers.ne.3) write (6,*) 'PLANTING CROP #',nowcrp,
     1      ' on OFE # ',iplane,' on DAY',sdate
        if (ivers.eq.3) write (6,*) 'PLANTING CROP #',nowcrp,
     1      ' on CHANNEL #',iplane,' on DAY',sdate
        ifreez(iplane) = 0
        idecom(iplane) = 1
c
c       Vegetative dry matter, canopy cover, canopy height, root mass
c       and root depth values are set to zero on the following days:
c       at harvest, before planting, or at burning, silage, or herbicide
c       date.
c
        rtd(iplane) = 0.0
        rtmass(iplane) = 0.0
        rtm15(iplane) = 0.0
        rtm30(iplane) = 0.0
        rtm60(iplane) = 0.0
        sumgdd(iplane) = 0.0
        cancov(iplane) = 0.0
        canhgt(iplane) = 0.0
        vdmt(iplane) = 0.0
        lai(iplane) = 0.0
        ncount(iplane) = 0
c
c     Today is not within a possible growing period for a perennial
c     *** L0 ELSE ***
      else
c     *** L0 ENDIF ***
      end if
c
      return
 1000 format (' HARVEST date ',i3,' plane ',i4,' yield ',f8.3,
     1    '(kg/m**2)')
 1100 format (' Crop Type # ',i2,' Date = ',i3,' OFE # ',i2,' yield= ',
     1    f8.3,'(kg/m**2)')
 1200 format (' HARVEST date ',i3,' channel ',i4,' yield ',f8.3,
     1    ' (kg/m**2)')
      end