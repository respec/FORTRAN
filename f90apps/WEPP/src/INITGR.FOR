      subroutine initgr(nowcrp)
c
c     + + + PURPOSE + + +
c     Called to initialize perennial growth after plant has been frozen
c     and is starting to grow.  Estimates the vegetative dry matter at
c     maturity (VDMT).
c
c     Called from PTGRP, INIT1
c     Author(s):  Alberts, Ghiddey, Ferris, Arnold
c     Reference in User Guide:
c
c     Changes:
c
c     Version: This module recoded from WEPP Version 92.25.
c     Date recoded: 08/25/92.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pntype.inc'
      include 'pmxpln.inc'
      include 'pmxcrp.inc'
      include 'pmxres.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxcut.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nowcrp
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nowcrp - the current crop
c
c     + + + COMMON BLOCKS + + +
      include 'ccover.inc'
c       read: cancov(mxplan)
c      write: canhgt(mxplan)
c
      include 'ccrpvr2.inc'
c     modify: vdmt(mxplan)
c
      include 'ccrpvr3.inc'
c       read: hmax(ntype), gddmaxi(ntype), bb(ntype), bbb(ntype),
c             xmxlai(ntype)
c     modify: sumgdd(mxplan)
c
      include 'ccrpprm.inc'
c       read: itype(mxcrop,mxplan)
c
      include 'ccrpout.inc'
c     modify: lai(mxplan)
c
      include 'cperen.inc'
c       read: imngmt(mxcrop,mxplan)
c
      include 'cstruc.inc'
c       read: iplane
c
c     + + + END SPECIFICATIONS + + +
c
c
c ---- Compute vegetative dry matter (VDMT), from canopy cover (CANCOV).
      vdmt(iplane) = log(1.0-cancov(iplane)) / (-
     1    bb(itype(nowcrp,iplane)))
      if (vdmt(iplane).lt.0.) vdmt(iplane) = 0.0
c
c     ---- Compute canopy height (CANHGT), from vegetative dry matter (VDMT)
c     and plant height at maturity (HMAX).
      canhgt(iplane) = (1.-exp(-bbb(itype(nowcrp,iplane))*vdmt(iplane)))
     1    * hmax(itype(nowcrp,iplane))
c
c     ---- Compute leaf area index (LAI), from maximum leaf area index
c     (XMXLAI) and vegetative dry matter (VDMT).
c     ** CROPLAND ANNUALS **
      if (imngmt(nowcrp,iplane).eq.1) then
        lai(iplane) = (xmxlai(itype(nowcrp,iplane))*vdmt(iplane)) / (
     1      vdmt(iplane)+0.5512*exp(-6.8*vdmt(iplane)))
c
c     ** PERENNIALS OR RANGELAND **
      else
        lai(iplane) = (xmxlai(itype(nowcrp,iplane))*vdmt(iplane)) / (
     1      vdmt(iplane)+0.2756*exp(-13.6*vdmt(iplane)))
      end if
c
c     ---- Compute cumulative growing degree days (SUMGDD), from growing
c     degree days at maturity (GDDMAX), leaf area index (LAI), and
c     maximum leaf area index (XMXLAI).
      sumgdd(iplane) = gddmax(itype(nowcrp,iplane)) * lai(iplane) /
     1    xmxlai(itype(nowcrp,iplane))
c
c     Handle the case of a winter annual which has relatively low
c     cover -- so low that calculation of sumgdd is below value of
c     the critical value of growing degree days for emergence (CRIT).
c
      if ((jdharv(nowcrp,iplane).lt.jdplt(nowcrp,iplane)).and.(
     1    sumgdd(iplane).lt.crit(itype(nowcrp,iplane))))
     1    sumgdd(iplane) = crit(itype(nowcrp,iplane)) + 0.1
c
      return
      end
