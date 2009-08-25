      subroutine   deplet
c
c     + + + PURPOSE + + +
c     This subprogram calculates the depletion level of the soil for the
c     current rooting depth and for the soil profile.  Also calculates
c     an irrigation depth if necessary.
c
c     Called from IRRIG
c     Author(s): E. R. Kottwitz
c     Reference in User Guide: Chapter 12
c
c     Changes:
c
c     Version: This module recoded from WEPP Version 93.06.
c     Date recoded: 07/06/93.
c     Recoded by: Charles R. Meyer.
c
c
c     + + + PARAMETERS + + +
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxsrg.inc'
      include 'pxstep.inc'
c
c     + + + COMMON BLOCKS + + +
      include 'ccrpout.inc'
c       read: rtd(mxplan)
c
      include 'chydrol.inc'
c       read: rain(mxplan)
c
      include 'cirdepl.inc'
c       read: deplev(mxplan),irdmin
c     modify: iramt
c      write: depsev
c
      include 'cirfurr.inc'
c       read: filrat(mxplan)
c
      include 'cirriga.inc'
c       read: irsyst
c
      include 'cirspri.inc'
c       read: aprati(mxplan),irdmax
c
      include 'cstruc.inc'
c       read: iplane
c
      include 'cwater.inc'
c       read: st(mxnsl,mxplan),thetdr(mxnsl,mxplan),
c             thetfc(mxnsl,mxplan),nsl(mxplan),solthk(mxnsl,mxplan),
c             dg(mxnsl,mxplan)
c
      include 'cwint.inc'
c       read: wmelt(mxplan)
c
      include 'cirflg.inc'
c
c     + + + LOCAL VARIABLES + + +
      integer   i,ii
      real      awtota,awtotb,depla,deplb,iramta,iramtb,sumsta,sumstb
c
c     + + + LOCAL DEFINITIONS + + +
c     i      - do loop counter for summations for soil layers
c     ii     - flag indicating whether bottom of soil layer is below
c              the current rooting depth (0-no, 1-yes)
c     awtota - available soil water over the current root depth (m)
c     awtotb - available soil water over the entire soil profile (m)
c     depla  - available soil water depletion over the current root
c              depth (m/m)
c     deplb  - available soil water depletion over the entire soil
c              profile (m/m)
c     iramta - irrigation amount required to fill the soil profile over
c              the current root depth multiplied by the ratio of
c              application depth to allowable irrigation depth
c              (aprati(mxplan)) (m)
c     iramtb - irrigation amount required to fill the soil profile over
c              the entire soil profile multiplied by aprati(mxplan) (m)
c     sumsta - maximum available soil water over the current root depth
c              (m)
c     sumstb - maximum available soil water over the entire soil profile
c              (m)
c
c     + + + END SPECIFICATIONS + + +
c
c     Initialize variables
c
      irdfg = 1  
c
      ii = 0
c
      sumsta = rain(iplane) + wmelt(iplane)
      sumstb = rain(iplane) + wmelt(iplane)
c
c     Do a summation of (a) current available soil water (st) and
c     maximum available soil water (aw) for the root depth and (b) st
c     and aw for all soil layers
c
      sumstb = sumstb+st(1,iplane)
      awtotb = (thetfc(1,iplane)-thetdr(1,iplane))*dg(1,iplane)
c
      if(solthk(1,iplane).le.rtd(iplane))then
        sumsta = sumstb
        awtota = awtotb
      else
        sumsta = sumsta+st(1,iplane)*rtd(iplane)/dg(1,iplane)
        awtota = (thetfc(1,iplane)-thetdr(1,iplane))*rtd(iplane)
        ii = 1
      endif
c
      do 10  i = 2, nsl(iplane)
        sumstb = sumstb+st(i,iplane)
        awtotb = awtotb+(thetfc(i,iplane)-thetdr(i,iplane))*dg(i,iplane)
        if(ii.eq.0)then
c
          if(solthk(i,iplane).le.rtd(iplane))then
            sumsta = sumstb
            awtota = awtotb
          else
            sumsta = sumsta+st(i,iplane)*(rtd(iplane)-
     1               solthk(i-1,iplane))/dg(i,iplane)
            awtota = awtota+(thetfc(i,iplane)-thetdr(i,iplane))*
     1               (rtd(iplane)-solthk(i-1,iplane))
            ii = 1
          endif
c
        endif
 10   continue
c
c     Calculate depletion level for the current rooting depth and for
c     all soil layers
c
c
c     correction by dcf 8/29/91  - divide by zero when awtota=0
c
c     if(rtd(iplane).gt.0.0001 .and. sumsta/awtota.lt.1.0)then
c
      if(rtd(iplane).gt.0.0001 .and. awtota.gt.0.0)then
        if(sumsta/awtota.lt.1.0)then
          depla = 1.0-sumsta/awtota
        else
          depla = 0.0
        endif
      else
        depla = 0.0
      endif
c
      if(awtotb.gt.0.0)then
        if(sumstb/awtotb.lt.1.0)then
          deplb = 1.0-sumstb/awtotb
        else
          deplb = 0.0
        endif
      else
        deplb = 0.0
      endif
c
c     If depletion level is greater than allowable then determine
c     irrigation parameters
c
c      *** M0 IF ***
      if(depla.ge.deplev(iplane) .or. deplb.ge.deplev(iplane))then
        depsev = (max(depla,deplb)-deplev(iplane))/deplev(iplane)
c
        if(irsyst.eq.1)then
c
c         Additional calculations for solid-set, side-roll, or hand-
c         move irrigation systems
c
          iramta = (awtota-sumsta)*aprati(iplane)
          iramtb = (awtotb-sumstb)*aprati(iplane)
        else
c
c         Additional calculations for furrow irrigation systems
c
          iramta = (awtota-sumsta)*filrat(iplane)
          iramtb = (awtotb-sumstb)*filrat(iplane)
        endif
c
        iramt = max(iramta,iramtb)
c
        if(iramt.lt.irdmin)then
          iramt = 0.0
          depsev = 0.0
        elseif(irsyst.eq.1 .and. iramt.gt.irdmax)then
          iramt = irdmax
        endif
c
c      *** M0 ELSE ***
      else
        depsev = 0.0
        iramt = 0.0
c
c      *** M0 ENDIF ***
      endif
c
      return
      end
