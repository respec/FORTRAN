      subroutine drain(soldep,nowcrp)
c
c     + + + PURPOSE + + +
c     This subroutine is called from WATBAL to calculate daily
c     drainage flux (m/d) if:
c       1) drainage system exists and,
c       2) water table  is above drainage tiles.
c     For more detail contact:
c            Reza Savabi, USDA-ARS, National Soil
c            Erosion Lab. (317-494-8673)
c
c     Called from WATBAL
c     Author(s): Savabi
c     Reference in User Guide:
c
c     Changes:
c             1) Deleted reference to include files: PNTYPE.INC,
c                PTILTY.INC, PMXTIM.INC, PMXTLS.INC, PMXTIL.INC,
c                PMXCRP.INC, & PMXCUT.INC.
c             2) Deleted reference to include files CFLAGS.INC
c                and CHYDROL.INC.
c             3) Changed dimensions on DRFC & DRAWAT from 10 to MXNSL.
c
c     Version: This module recoded from WEPP Version 92.25.
c     Date recoded: 01/12/93 - 1/15/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxnsl.inc'
      include 'pmxpnd.inc'
      include 'pmxpln.inc'
      include 'pmxhil.inc'
      include 'pntype.inc'
c     + + + ARGUMENT DECLARATIONS + + +
      real soldep
      integer nowcrp
c
c     + + + ARGUMENT DEFINITIONS + + +
c     soldep - soil profile depth
c     ddrain - depth from surface to drainage tiles (m)
c     drainc - drainage coefficient (m/day)
c     drdiam - drain tile diameter (m)
c     unsdep - unsaturated depth from surface to water table (m)
c     sdrain - drain spacing (m)
c     drainq - drainage flux (m/day)
c     satdep - saturated depth from soil surface (m)
c     satdep -
c     nowcrp - current crop
c     iplane - current overland flow element
c     drseq  - drainage sequence based on which crop and overland flow
c              element in use
c
c     + + + COMMON BLOCKS + + +
      include 'ccdrain.inc'
      include 'ccons.inc'
c        read: coca
c
      include 'cstruc.inc'
c        read: iplane
c
      include 'cparame.inc'
c        read: por
c
      include 'cwater.inc'
c        read: ssc, fc, thetfc, nsl, solthk, dg
c      modify: st
      include 'cwint.inc'
c
c     + + + LOCAL VARIABLES + + +
      real drfc(mxnsl), d,de,dranks,temp,totdg,totk,wattbl,watyld,vartmp
      integer jj, mn
c
c     + + + LOCAL DEFINITIONS + + +
c     drfc   - layer's field capacity, corrected for entrapped air
c     dranks - hydraulic conductivity for drainage calculation (m/d)
c
c     + + + SAVES + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c      Calculate average soil hydraulic conductivity in saturated zone
c      (below water table).
c
      totk = 0.0
      totdg = 0.0
      do 10 mn = 1, nsl(iplane)
        if (solthk(mn,iplane).gt.unsdep(iplane)) then
          totk = totk + (ssc(mn,iplane)*dg(mn,iplane))
          totdg = totdg + dg(mn,iplane)
        else
          totk = 0.0
          totdg = 0.0
        end if
   10 continue
c
c     Get average Ks of saturated layers and convert to m/day.
c     Assume horizontal flow KZ = KY, where KY = vertical K.
c
      dranks = (totk/totdg) * 86400.
c
c
c     Calculate drainage flux (m/day) for tile drains
c
      d = soldep - ddrain(drseq(nowcrp,iplane))
      temp = d / sdrain(drseq(nowcrp,iplane))
c
cc    Drain diameter is needed for DE calculations,
cc    0.1 m for Oregon; ie, rr=0.1
c
c     Calculate "equivalent depth (DE) using DRAINMOD equations
c     2-13 to 2/15.
c     (WEPP Equation 7b.2.6)
c
      if (temp.le..3) then
        de = d / (1.0+temp*((8.0/3.14)*
     1      log(d/drdiam(drseq(nowcrp,iplane)))-3.4))
      else
        de = (sdrain(drseq(nowcrp,iplane))*3.14) / (8.0*(log(
     1      sdrain(drseq(nowcrp,iplane))/drdiam(drseq(nowcrp,iplane)))-
     1      1.15))
      end if
c
      if (unsdep(iplane).lt.ddrain(drseq(nowcrp,iplane))) then
        wattbl = ddrain(drseq(nowcrp,iplane)) - unsdep(iplane)
c       sdrain=distance between tiles, m
c       ------ subsurface flow to drain tubes
c       (WEPP Equation 7b.2.5)
        drainq(iplane) = (8.0*dranks*de*wattbl) + (4.0*dranks*(wattbl**2
     1      )) / (sdrain(drseq(nowcrp,iplane))**2)
      else
        drainq(iplane) = 0.0
      end if
c
c     ---- Limit drain flux to the hydraulic capacity of the drainage system.
      if (drainq(iplane).gt.drainc(drseq(nowcrp,iplane)))
     1    drainq(iplane) = drainc(drseq(nowcrp,iplane))
c
cc    (Need to work on drainage ditch, and where main tile comes to the
cc    surface.   reza june 1990.
c
c     Convert drainage flux to soil water depth using average
c     drainable porosity (water yield).
      watyld = por(1,iplane) - (thetfc(1,iplane)+(1.0-coca(1,iplane)))
      unsdep(iplane) = unsdep(iplane) + (drainq(iplane)/watyld)
      if (unsdep(iplane).lt.0.0) unsdep(iplane) = 0.0
      satdep(iplane) = soldep - unsdep(iplane)
c
c     *** L0 IF ***
c     If there is drainage from the tiles (DRAINQ > 0), update the
c     available water content (ST) in each soil layer to reflect
c     the drainage from each layer, starting at the top.
c
      if (drainq(iplane).gt.0.0) then
c       *** Begin L1 DO-LOOP ***
        jj = 0
   20   continue
        jj = jj + 1
c       -------- layer's field capacity, corrected for entrapped air
        drfc(jj) = fc(jj) + ((1-coca(1,iplane))*dg(jj,iplane))
c
c       *** L2 IF ***
c       -------- If layer is above field capacity, drain it.
cd      Modified by S. Dun, March 04, 2008 for frozen soil 
cd        if (st(jj,iplane).gt.drfc(jj)) then
          vartmp = (dg(jj,iplane) - frozen(jj,iplane))/dg(jj,iplane)
          if (st(jj,iplane) .gt. drfc(jj)) then
c         ---------- potential water which can be drained from this layer
cd          drawat(jj) = st(jj,iplane) - drfc(jj)
            drawat(jj) = st(jj,iplane) - drfc(jj)*vartmp
cd    end modifying
c
c         *** L3 IF ***
c         ---------- If water runs out the drain...
          if (drainq(iplane).gt.0.0) then
c
c           ------------ If water excess in this layer exceeds or equals what has
c           run from drain...
            if (drawat(jj).ge.drainq(iplane)) then
c             -------------- subtract amount that has run out the drain fron this layer.
              st(jj,iplane) = st(jj,iplane) - drainq(iplane)
              if (st(jj,iplane).lt.1e-10) st(jj,iplane) = 1e-10
              drainq(iplane) = 0.0
c           ------------ If water excess in this layer is less than what has run
c           from drain...
            else
c             -------------- subtract from DRAINQ what this layer can contribute.
              drainq(iplane) = drainq(iplane) - drawat(jj)
c             -------------- adjust soil layer water content by same amount of water
cd              st(jj,iplane) = drfc(jj)
              st(jj,iplane) = drfc(jj)*vartmp
            end if
c
c         *** L3 ENDIF ***
          end if
c
c       *** L2 ENDIF ***
        end if
c
c       *** End L1 DO-LOOP ***
        if ((jj.lt.nsl(iplane)).and.(drainq(iplane).gt.0.0)) go to 20
c
c
c     *** L0 ENDIF ***
      end if
c
      return
      end
