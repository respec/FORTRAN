      subroutine resup(nowcrp,isenes)
c
c     + + + PURPOSE + + +
c     This subroutine is called at harvest ,senescence and
c     when residue is added, to update the residue partitions.
c
c  *********************************************************************
c  * NOTE: Variable RESMGT is read by subroutine TILAGE, and is stored *
c  *       in common block CRPPRM, but is apparently NEVER USED!       *
c  *      -- CRM -- 12/01/92.                                          *
c  *********************************************************************
c
c  *********************************************************************
c  *   At senescence, or at harvest before senescence, the previous    *
c  *   residue is added to the old residue, and the current is shifted *
c  *   to the previous residue.  At senescence for annuals, and at the *
c  *   stop date for perennials, all current residues are initialized  *
c  *   to zero.  At harvest the residues are calculated as a fraction  *
c  *   of the biomass.  At harvest after senescence, the fraction of   *
c  *   the biomass is added to the current residue.                    *
c  *                                                                   *
c  *   At harvest date the root mass is updated.                       *
c  *********************************************************************
c
c     Called from DECOMP, GROW, PTGRA, and PTGRP.
c     Author(s):
c     Reference in User Guide: Chapter 8
c
c     Changes:
c          1) Removed local variable NUMRES, which was always set to '3',
c             and substituted '3' for it, and '2' for NUMRES-1.
c          2) Removed local variable NRES, which was always set to '4'.
c          3) Removed local variable NOWRES, which was always set to '1'.
c          4) The lines below seemed to exist for the sole purpose of
c             communicating with DECOMP.  Most of the values don't even
c             seem to be used.  The arrays are dimensioned to (mxcrop,mxplan)
c             in common block CRPPRM, and MXCROP is set to 5 in PMXCRP.INC.
c             Seems like a waste of memory space!  In DECOMP the code was
c             changed from XXXXXX(4,iplane) to XXXXXX(nowcrp,iplane).
c             -- CRM -- 12/01/92
c
c                jdplt(4,iplane)   = jdplt(nowcrp,iplane)
c                jdharv(4,iplane)  = jdharv(nowcrp,iplane)
c                rw(4,iplane)      = rw(nowcrp,iplane)
c                resmgt(4,iplane)  = resmgt(nowcrp,iplane)
c                jdherb(4,iplane)  = jdherb(nowcrp,iplane)
c                jdburn(4,iplane)  = jdburn(nowcrp,iplane)
c                jdslge(4,iplane)  = jdslge(nowcrp,iplane)
c                jdcut(4,iplane)   = jdcut(nowcrp,iplane)
c                jdmove(4,iplane)  = jdmove(nowcrp,iplane)
c                fbrnag(4,iplane)  = fbrnag(nowcrp,iplane)
c                fbrnog(4,iplane)  = fbrnog(nowcrp,iplane)
c                frmove(4,iplane)  = frmove(nowcrp,iplane)
c                jdstop(4,iplane)  = jdstop(nowcrp,iplane)
c                tothav(4,iplane)  = tothav(nowcrp,iplane)
c                frcut(4,iplane)   = frcut(nowcrp,iplane)
c
c     Version: This module recoded from WEPP version 91.50.
c     Date recoded: 12/06/91 - 12/24/91.
c     Updated to version 92.25.
c     Date recoded: 12/01/92 - 12/09/92.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pntype.inc'
      include 'pmxtls.inc'
      include 'pmxnsl.inc'
      include 'pmxtil.inc'
      include 'ptilty.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxcrp.inc'
      include 'pmxcut.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nowcrp, isenes
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nowcrp - index of current crop
c     isenes - flag used in resup to indicate type of update for residue
c              13 - residue removal with disturbance
c              12 - residue addition with disturbance
c              11 - residue removal with no disturbance
c              10 - residue addition with no disturbance
c               1 - harvest after senescence
c               0 - harvest before senescence
c              -1 - stop(kill) date for a perennial
c              -2 - senescence (annual)  or  1st freeze of perennial
c
c     + + + COMMON BLOCKS + + +
      include 'ccontcv.inc'
c     read: tilseq(
      include 'ccrpprm.inc'
c       read: itype
c     modify: iresd(mxres,mxplan), jdplt(mxcrop,mxplan),
c             jdharv(mxcrop,mxplan), rw(mxcrop,mxplan),
c             resmgt(mxcrop,mxplan), iroot(mxres,mxplan)
c
      include 'ccrpgro.inc'
c       read: vdmx(mxplan), hia(mxplan), y4(ntype)
c
      include 'ccrpout.inc'
c     modify: rtm15(mxplan)
c      write: rtm30(mxplan), rtm60(mxplan), rtmass(mxplan)
c
      include 'ccrpvr1.inc'
c     modify: smrm(mxres,mxplan), rmogt(mxres,mxplan), rmagt(mxplan)
c             rtm(mxres,mxplan)
c
      include 'ccrpvr2.inc'
c       read: vdmt(mxplan)
c
c     include 'ccrpvr3.inc'
c
      include 'ccrpvr5.inc'
c       read: pltsp(ntype), diam(ntype)
c      write: basmat(mxplan)
c
      include 'cdecvar.inc'
c     modify: benvin(mxres,mxplan), fenvin(mxres,mxplan)
c      write: senvin(mxres, mxplan)
c
      include 'cflags.inc'
c     read: yldflg
c
      include 'cperen.inc'
c       read: partcf(ntype)
c     modify: jdherb(mxcrop,mxplan), jdburn(mxcrop,mxplan),
c             jdslge(mxcrop,mxplan), jdcut(mxcrop,mxplan),
c             jdmove(mxcrop,mxplan), fbrnag(mxcrop,mxplan),
c             fbrnog(mxcrop,mxplan), frmove(mxcrop,mxplan),
c             jdstop(mxcrop,mxplan), tothav(mxcrop,mxplan),
c             frcut(mxcrop,mxplan), popmat(mxplan),
c             srmhav(mxplan)
c     modify: popmat(mxplan), srmhav(mxplan)
c
      include 'cptgrow.inc'
c     modify: idecom(mxplan)
c
      include 'cridge.inc'
c     modify: rilrm(mxres,mxplan), rigrm(mxres,mxplan)
c
      include 'cstruc.inc'
c     read: ivers
c
      include 'ctillge.inc'
c     read: resman(indxy(iplane),tillseq(nowcrp,iplane)
c
      include 'cupdate.inc'
c       read: sdate, indxy(iplane)
c
      include 'cyield.inc'
c     modify: sumyld(ntype,mxplan),sumbsh(ntype,mxplan),
c             iyldct(ntype,mxplan)
c
c     + + + LOCAL VARIABLES + + +
      real areacv, stemar, hyield
c
c     + + + LOCAL DEFINITIONS + + +
c     nowres - 1 = current residue
c              2 = previous residue
c              3 = total of all residue prior to the previous
c
c     + + + OUTPUT FORMATS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c      If this is a harvest (either before or after senescence),
c      calculate yield.
c
      if (isenes.ge.0..and.isenes.lt.10) then
        hyield = hia(iplane) * vdmx(iplane)
c       yldbsh = hyield * 8907.7 / y4(itype(nowcrp,iplane))
c
        if (ivers.ne.3) write (6,1000) sdate, iplane, hyield
        if (ivers.eq.3) write (6,1200) sdate, iplane, hyield
c
c       ADD a write statement to a file to store crop yields
c       for George Foster  3/25/93   dcf
c
        if (yldflg.eq.1) then
          write (46,1100) itype(nowcrp,iplane), sdate, iplane, hyield
c
c         ADD summation variables to total up the yields of the
c         various crops on the various OFE's.  ADD variable to
c         ADD up the total number of harvests of the various crops
c         on the various OFE's.
c
          sumyld(itype(nowcrp,iplane),iplane) =
     1        sumyld(itype(nowcrp,iplane),iplane) + hyield
          iyldct(itype(nowcrp,iplane),iplane) =
     1        iyldct(itype(nowcrp,iplane),iplane) + 1
        end if
c
      end if
c
c     *** L0 IF ***
c     If this is NOT a harvest after senescence....
      if (isenes.le.0) then
c
c       Add residue from previous crop; ie, crop before current crop
c       (RMOGT(2,iplane)), to oldest residue total (RMOGT(3,iplane))
c
        rmogt(3,iplane) = rmogt(3,iplane) + rmogt(2,iplane)
        rilrm(3,iplane) = rilrm(3,iplane) + rilrm(2,iplane)
        rigrm(3,iplane) = rigrm(3,iplane) + rigrm(2,iplane)
        smrm(3,iplane) = smrm(3,iplane) + smrm(2,iplane)
c
c       Shift residue type, buried environment coefficient, and
c       flat environment coefficient from "previous" to "oldest".
c
        iresd(3,iplane) = iresd(2,iplane)
        benvin(3,iplane) = benvin(2,iplane)
        fenvin(3,iplane) = fenvin(2,iplane)
c
c       Shift current residue to previous residue
c
        rmogt(2,iplane) = rmogt(1,iplane)
        rilrm(2,iplane) = rilrm(1,iplane)
        rigrm(2,iplane) = rigrm(1,iplane)
        smrm(2,iplane) = smrm(1,iplane)
        iresd(2,iplane) = iresd(1,iplane)
        benvin(2,iplane) = benvin(1,iplane)
        fenvin(2,iplane) = fenvin(1,iplane)
c
c       Set current residue type to current crop.
        iresd(1,iplane) = itype(nowcrp,iplane)
c
c       basal area at maturity (BASMAT)
c
c       (WEPP Equation 8.2.18)
c       areacv=pltsp(iresd(1,iplane))*rw(4,iplane)
        areacv = pltsp(iresd(1,iplane)) * rw(nowcrp,iplane)
c       (WEPP Equation 8.2.17)
        popmat(iplane) = (1.0/areacv)
c       (WEPP Equation 8.2.19)
c       stemar=3.14159*(diam(iresd(1,iplane)/2.0)**2)
        stemar = 3.14159 * (diam(iresd(1,iplane))/2.0) ** 2
c       (WEPP Equation 8.2.16)
        basmat(iplane) = popmat(iplane) * stemar
c
c       ------ standing residue mass at harvest
        srmhav(iplane) = vdmx(iplane) * partcf(itype(nowcrp,iplane))
        if (srmhav(iplane).le.0.1) srmhav(iplane) = 0.10
c
c       ------ submerged residue for new residue is zero until tillage
        smrm(1,iplane) = 0.00001
c
c
c       *** L1 IF ***
c       Harvest before senescence
        if (isenes.eq.0) then
          rmogt(1,iplane) = (vdmt(iplane)-hyield) * (1.-
     1        partcf(itype(nowcrp,iplane)))
          rmagt(iplane) = rmagt(iplane) + (vdmt(iplane)-hyield) *
     1        partcf(itype(nowcrp,iplane))
          rilrm(1,iplane) = (vdmt(iplane)-hyield) * (1.-
     1        partcf(itype(nowcrp,iplane)))
          rigrm(1,iplane) = (vdmt(iplane)-hyield) * (1.-
     1        partcf(itype(nowcrp,iplane)))
          idecom(iplane) = 1
c
c       *** L1 ELSE ***
        else
c
c         Senescence or stop date of perennial
c
          rmogt(1,iplane) = 0.0
          rilrm(1,iplane) = 0.0
          rigrm(1,iplane) = 0.0
c
c       *** L1 ENDIF ***
        end if
c
c       set environmental factors for decomposition to zero
c
        benvin(1,iplane) = 0.0
        fenvin(1,iplane) = 0.0
        senvin(iplane) = 0.0
c
c     *** L0 ELSE ***
c     Residue addition
      else if(isenes.eq.10)then

c
c       Add residue from previous crop; ie, crop before current crop
c       (RMOGT(2,iplane)), to oldest residue total (RMOGT(3,iplane))
c
        rmogt(3,iplane) = rmogt(3,iplane) + rmogt(2,iplane)
        rilrm(3,iplane) = rilrm(3,iplane) + rilrm(2,iplane)
        rigrm(3,iplane) = rigrm(3,iplane) + rigrm(2,iplane)
c
c
c       Shift current residue to previous residue
c
        rmogt(2,iplane) = rmogt(1,iplane)
        rilrm(2,iplane) = rilrm(1,iplane)
        rigrm(2,iplane) = rigrm(1,iplane)
c
c       Shift residue type,
        iresd(3,iplane) = iresd(2,iplane)
        iresd(2,iplane) = iresd(1,iplane)
c       ------ submerged residue for new residue is zero until tillage
        smrm(1,iplane) = 0.00001
c
c        if surface disturbance accompanies residue addition then
c         disturb surface and then add residue,
c
      elseif(isenes.ge.12)then
c
c         shift submerged residues from previous to oldest
c
        smrm(3,iplane) = smrm(3,iplane) + smrm(2,iplane)
c
c         shift submerged residues from current to previous
c
        smrm(2,iplane) = smrm(1,iplane)
c
c        Shift buried environment coefficient, and
c        flat environment coefficient from "previous" to "oldest".
c
        benvin(3,iplane) = benvin(2,iplane)
        fenvin(3,iplane) = fenvin(2,iplane)
c
c        Shift buried environment coefficient, and
c        flat environment coefficient from "previous" to "oldest".
c
        benvin(2,iplane) = benvin(1,iplane)
        fenvin(2,iplane) = fenvin(1,iplane)
c
c       Add residue from previous crop; ie, crop before current crop
c       (RMOGT(2,iplane)), to oldest residue total (RMOGT(3,iplane))
c
        rmogt(3,iplane) = rmogt(3,iplane) + rmogt(2,iplane)
        rilrm(3,iplane) = rilrm(3,iplane) + rilrm(2,iplane)
        rigrm(3,iplane) = rigrm(3,iplane) + rigrm(2,iplane)
c
c
c       Shift current residue to previous residue
c
        rmogt(2,iplane) = rmogt(1,iplane)
        rilrm(2,iplane) = rilrm(1,iplane)
        rigrm(2,iplane) = rigrm(1,iplane)
c
c       Shift residue type,
        iresd(3,iplane) = iresd(2,iplane)
        iresd(2,iplane) = iresd(1,iplane)


      else
c
c       Harvest after senescence - no shift of residue (done at senescence)
c
        rmogt(1,iplane) = rmogt(1,iplane) + (vdmt(iplane)-hyield) * (1.-
     1      partcf(itype(nowcrp,iplane)))
        rmagt(iplane) = rmagt(iplane) + (vdmt(iplane)-hyield) *
     1      partcf(itype(nowcrp,iplane))
        rilrm(1,iplane) = rilrm(1,iplane) + (vdmt(iplane)-hyield) * (1.-
     1      partcf(itype(nowcrp,iplane)))
        rigrm(1,iplane) = rigrm(1,iplane) + (vdmt(iplane)-hyield) * (1.-
     1      partcf(itype(nowcrp,iplane)))
c
c     *** L0 ENDIF ***
      end if
c
c     At harvest (annual) or kill date (perennial) update root mass.
c
      if (isenes.ne.-2.and.isenes.lt.10) then
        rtm(3,iplane) = rtm(3,iplane) + rtm(2,iplane)
        iroot(3,iplane) = iroot(2,iplane)
        rtm(2,iplane) = rtm(1,iplane)
        iroot(2,iplane) = iroot(1,iplane)
        rtm(1,iplane) = rtm15(iplane)
        iroot(1,iplane) = itype(nowcrp,iplane)
c
c       Reset live root mass to zero.
c
        rtmass(iplane) = 0.0
        rtm15(iplane) = 0.0
        rtm30(iplane) = 0.0
        rtm60(iplane) = 0.0
c
      end if
c
      return
 1000 format(' ANNUAL CROP HARVEST DATE ',i3,' -- plane ',i2,
     1    ' yield ',f6.3,'(kg/m**2) '/)
 1100 format(' Crop Type # ',i2,' Date = ',i3,' OFE # ',i2,'  yield= ',
     1    f8.3,'(kg/m**2)  ')
 1200 format(' ANNUAL CROP HARVEST DATE ',i3,' -- channel ',i2,
     1    ' yield ',f6.3,'(kg/m**2) '/)
      end
