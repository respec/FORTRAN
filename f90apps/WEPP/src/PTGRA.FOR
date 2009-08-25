      subroutine ptgra(nowcrp)
c
c     + + + PURPOSE + + +
c     This is the plant growth subroutine for annual crops.
c     This subroutine predicts canopy cover, canopy height, and
c     root mass at different soil zones, root depth, leaf area
c     index and plant basal area.
c
c     Called from WATBAL
c     Author(s): Alberts, Ghiddey, Arnold
c     Reference in User Guide:
c
c     Changes:
c           1) Parameter MXGRAZ not used, so PMAXGRZ.INC
c              was dereferenced.
c           2) Common blocks RINPT1, CLIYR, CLIM, RIDGE, & WATER were
c              not used, so they were dereferenced.
c           3) Imbedded RETURN's were converted to IF's.
c           4) ISENES changed to ISENES(MXPLAN). -- CRM -- 5/29/92
c           5) Since ISENES is now stored in common block SENES,
c              DATA statement which set it to zero was removed.
c              This is statement is now included in BLKDATA.
c              CRM -- 6/9/92.
c           6) Variables IDECOM[p], ISTART, TMPVAR, IFLAG, X5, and
c              X6 are not needed for the call to GROW, since in
c              PTGRA they are always zero.  ISTART & IFLAG were
c              the only ones that were even initialized.  All these
c              variables were deleted.  6/09/92 -- CRM
c           7) VDMT set to zero at planting.  6/26/92 -- CRM.
c           8) In order to conform to the WEPP Coding Convention, the
c              5th - 8th arguments to GROW moved after the 2nd one,
c              and the last argument moved after the 3rd one.  Cor-
c              responding changes made in the call to GROW from this
c              routine.
c           9) Deleted dummy parameters TMPFLG, ISTART, & TRTMAS from
c              GROW, and its calls from PTGRA, PTGRP, and RANGE
c          10) Adding setting back of TLIVE to 0.0 whenever VDMT was
c              set back to zero.  TLIVE is used by new interception
c              code from Savabi.  Also  XXX  Arnold needs to review
c              the meaning, computation, and use of variables VDMT,
c              TLIVE, RMOGT, RMAGT, CANCOV, CANHGT - as there appear
c              to be inconsistencies between CROPLAND and RANGELAND
c              usage - and RANGELAND usage seems to match variable
c              descriptions better.   dcf   7/30/93
c
c
c     Version: This module recoded from WEPP version 91.40.
c     Date recoded: 10/02/91.
c     Note: This module is current with WEPP release version 91.50.
c     Date checked: 12/5/91.
c     Note: This module is current with WEPP release version 92.25.
c     Date checked: 5/29/92 - 6/09/92.
c     Recoded by: Charles R. Meyer.
c     Note: This module is current with WEPP version 93.05  dcf 5/19/93
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pntype.inc'
      include 'pmxnsl.inc'
      include 'ptilty.inc'
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
      include 'ccrpvr1.inc'
c     modify: rmagt(mxplan),rtm(mxres,iplane)
c
      include 'ccrpvr2.inc'
c     modify: vdmt(mxplan)
c
      include 'ccrpvr3.inc'
c       read: fgs,dlai,gssen
c      write: sumgdd(mxplan)
c
      include 'ccrpvr5.inc'
c       read: ncount(mxplan)
c
      include 'ccover.inc'
c      write: canhgt(mxplan), cancov(mxplan), gcover(mxplan)
c
      include 'ccrpout.inc'
c      write: rtd(mxplan), rtmass(mxplan), lai(mxplan)
c
      include 'ccrpprm.inc'
c       read: jdharv(mxcrop,mxplan), jdplt(mxcrop,mxplan)
c
      include 'ccrpgro.inc'
c      write: hia(mxplan)
      include 'cdecvar1.inc'
c       read: cuthgt(ntype)
c
      include 'cflags.inc'
c       read: yldflg
      include 'cgcovr.inc'
c     modify: gcvplt(mxplan)
c
      include 'cperen.inc'
c       read: jdherb(mxcrop, mxplan),jdburn(mxcrop, mxplan),
c             jdslge(mxcrop, mxplan)
c
      include 'crout.inc'
c     modify: tlive(mxplan)
c
      include 'cstruc.inc'
c       read: iplane, ivers
c
      include 'cupdate.inc'
c       read: sdate
c
      include 'csenes.inc'
c     modify: isenes(mxplan)
c
c
c     + + + LOCAL VARIABLES + + +
      integer intcrp, idecom, nowres
      real silamt
c
c     + + + LOCAL DEFINITIONS + + +
c     intcrp - flag set at harvest, planting, burning, silage or
c              herbicide date. Vegetative dry matter, canopy cover,
c              canopy height, root mass, and root depth are set to
c              zero while the canopy cover decays.
c     idecom - dummy variable required by GROW. IDECOM is a flag used
c              to indicate whether residue updating needs to be done
c              at the first freezing of a perennial crop (call to RESUP)
c              setting of IDECOM is done in PTGRP
c     isenes - flag used in resup to indicate type of update for residue
c               1 - harvest after senescence
c               0 - harvest before senescence
c              -1 - stop/kill date of perennial
c              -2 - senescence (annual) or 1st freeze of perennial
c     silamt - silage harvest amount kg/m^2
c
c
c     + + + SUBROUTINES CALLED + + +
c     initpl
c     resup
c     grow
c
c     + + + END SPECIFICATIONS + + +
c
      intcrp = 0
c
c     Planting Date
c
      if (sdate.eq.jdplt(nowcrp,iplane)) then
        if (ivers.ne.3) write (6,*) 'PLANTING CROP #',nowcrp,
     1      ' on OFE #',iplane,' on DAY',sdate
        if (ivers.eq.3) write (6,*) 'PLANTING CROP #',nowcrp,
     1      ' on CHANNEL #',iplane,' on DAY',sdate
        isenes(iplane) = 0
        intcrp = 1
c
        vdmt(iplane) = 0.0
c
c       XXX - Variable TLIVE is never reset to 0.0 - this probably should
c       be done here when plant date is reached and other variables
c       set back to zero.   dcf   7/30/93
c
        tlive(iplane) = 0.0
c
        hia(iplane) = 0.0
c
c       Need to save the ground cover on the day of planting value -
c       this value will then be passed to subroutine INFPAR to compute
c       the macroporosity adjustment factor.  dcf  1/6/93
c
        gcvplt(iplane) = gcover(iplane)
      end if
c
c
c     TRAPS: If simulation date is greater than date of silage,
c     burning or herbicide application, then RETURN.
c
c     *** L0 IF ***
      if ((sdate.gt.jdslge(nowcrp,iplane)).and.(jdslge(nowcrp,iplane)
     1    .ne.0)) then
        continue
c
c     *** L0 ELSE-IF ***
      else if ((sdate.gt.jdburn(nowcrp,iplane)).and.(
     1    jdburn(nowcrp,iplane).ne.0).and.(jdburn(nowcrp,iplane).gt.
     1    jdplt(nowcrp,iplane))) then
        continue
c
c     *** L0 ELSE-IF ***
      else if ((sdate.gt.jdherb(nowcrp,iplane)).and.(
     1    jdherb(nowcrp,iplane).ne.0)) then
        continue
c
c     *** L0 ELSE ***
      else
c
c       *** L1 IF ***
c
c       silage modifications 3/24/98
c
        if (sdate.eq.jdslge(nowcrp,iplane)) then
c         set residue mass above ground to ratio of
c         vegetative dry matter-
c         (harvest cut height/canopy height)*vegetative dry matter
          intcrp = 1
          if(canhgt(iplane).le.0.0.or.vdmt(iplane).le.0.0)then
            write(6,1400)
          else
            if(canhgt(iplane).le.cuthgt(nowcrp))then
              silamt=((canhgt(iplane)-(canhgt(iplane)*0.05))/
     1           canhgt(iplane))*vdmt(iplane)
              write(6,1300)canhgt(iplane)*0.05
            else
            silamt=((canhgt(iplane)-cuthgt(nowcrp))/
     1         canhgt(iplane))*vdmt(iplane)
            end if
            rmagt(iplane)=rmagt(iplane)+vdmt(iplane)-silamt
c           Kill annual crop
            vdmt(iplane)=0.0
            if (ivers.ne.3) write (6,1000) sdate, iplane, silamt
            if (ivers.eq.3) write (6,1200) sdate, iplane, silamt
c
            if (yldflg.eq.1)
     1         write (46,1100)itype(nowcrp,iplane),sdate,iplane,silamt

            nowres = 1
            rtm(3,iplane) = rtm(2,iplane)
            rtm(2,iplane) = rtm(nowres,iplane)
            rtm(nowres,iplane) = rtm15(iplane)
          end if

c
c       Herbicide Application Date
c
c       *** L1 ELSE-IF ***
        else if (sdate.eq.jdherb(nowcrp,iplane).and.
     1      jdherb(nowcrp,iplane).ne.0) then
          rmagt(iplane) = rmagt(iplane) + vdmt(iplane)
          nowres = 1
          rtm(3,iplane) = rtm(2,iplane)
          rtm(2,iplane) = rtm(nowres,iplane)
          rtm(nowres,iplane) = rtm15(iplane)
          intcrp = 1
c
c       Harvest Date
c
c       *** L1 ELSE-IF ***
        else if (sdate.eq.jdharv(nowcrp,iplane)) then
c
          call resup(nowcrp,isenes(iplane))
          intcrp = 1
c
c       Burning Date
c
c       *** L1 ELSE-IF ***
        else if (sdate.eq.jdburn(nowcrp,iplane).and.
     1      jdburn(nowcrp,iplane).ne.0) then
          rmagt(iplane) = (rmagt(iplane)+vdmt(iplane))
          nowres = 1
          rtm(3,iplane) = rtm(2,iplane)
          rtm(2,iplane) = rtm(nowres,iplane)
          rtm(nowres,iplane) = rtm15(iplane)
          intcrp = 1
c
        end if
c
c       Vegetative dry matter, canopy cover, canopy height, root mass,
c       and root depth values are set to zero when any of the following
c       times: at harvest, before planting, or at burning, silage, and
c       herbicide dates.
c
        if (intcrp.eq.1) then
c
          rtd(iplane) = 0.0
          rtmass(iplane) = 0.0
          rtm15(iplane) = 0.0
          rtm30(iplane) = 0.0
          rtm60(iplane) = 0.0
          sumgdd(iplane) = 0.0
          cancov(iplane) = 0.0
          canhgt(iplane) = 0.0
          if(jdslge(nowcrp,iplane).ne.sdate)vdmt(iplane) = 0.0
          lai(iplane) = 0.0
c
c         XXX  - Variable TLIVE is never reset to 0.0 - this probably should
c         be done here when stop date is reached and other variables
c         set back to zero.   dcf   7/30/93
c
          tlive(iplane) = 0.0
        else if (intcrp.eq.0) then
c
c         Check whether simulation date is within the growing season.
c         -------- summer crop or winter crop
          if (((jdplt(nowcrp,iplane).lt.jdharv(nowcrp,iplane)).and.((
     1        sdate.ge.jdplt(nowcrp,iplane)).and.(sdate.le.
     1        jdharv(nowcrp,iplane)))).or.((jdplt(nowcrp,iplane).ge.
     1        jdharv(nowcrp,iplane)).and.((sdate.ge.
     1        jdplt(nowcrp,iplane)).or.(sdate.le.jdharv(nowcrp,iplane)))
     1        )) then
c
            call grow(nowcrp,iplane,0.0,0.0,ncount(iplane),idecom)
          end if
        end if
c
c     *** L0 ENDIF ***
      end if
c
      return
 1000 format(' SILAGE HARVEST DATE ',i3,' -- plane ',i2,
     1    ' yield ',f6.3,'(kg/m**2) '/)
 1100 format(' Crop Type # ',i2,' Date = ',i3,' OFE # ',i2,
     1    ' SILAGE= ',f8.3,'(kg/m**2)  ')
 1200 format(' SILAGE HARVEST DATE ',i3,' -- channel ',i2,
     1    ' yield ',f6.3,'(kg/m**2) '/)
 1300 format(//,' *** WARNING ***',/,' Cutting height greater than or',
     1    ' equal to canopy height!',/,' Cutting height set to 5% of',
     1    ' canopy height, cutting height for silage =',f5.2,' m',
     1    /,' *** WARNING ***',//)
 1400 format(//,' *** WARNING ***',/,' Canopy height or Vegetative dry',
     1    ' matter less than or equal to 0.0,',/,' No silage operation',
     1    ' performed!',/,' *** WARNING ***',//)
      end
