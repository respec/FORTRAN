      subroutine swu(nowcrp)
c
c********************************************************************
c     note: subroutine to be modified according to plant growth     *
c           model input requirement                                 *
c********************************************************************
c
c     + + + PURPOSE + + +
c     Distributes potential plant evaporation through the root zone
c     and calculates actual plant water use based on soil water
c     availability.
c
c     Called from WATBAL
c     Author(s): Savabi
c     Reference in User Guide:
c
c     Changes:
c          1) Common blocks CLIM, PARAME, & UPDATE not used.
c             Dereferenced.
c          2) Parameter MXPOND not used.  Dereferenced.
c          3) Variable EP expanded to array EP(MXPLAN).
c
c     Version: This module originally recoded from WEPP version 91.38.
c     Date recoded: 08/19/91.
c     Version: This module updated from WEPP release version 91.50.
c     Date updated: 12/04/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pntype.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'ptilty.inc'
      include 'pmxcrp.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nowcrp
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nowcrp - current crop
c
c     + + + COMMON BLOCKS + + +
c
      include 'cclim.inc'
c
      include 'ccrpout.inc'
c     modify: rtd
c
      include 'ccrpprm.inc'
c       read: itype
c
      include 'ccrpvr1.inc'
c     modify: pltol
c
      include 'cstruc.inc'
c       read: iplane
c
      include 'cwater.inc'
c       read: nsl,ub,ul(mxnsl,mxplane),solthk(mxnsl,mxplan),plaint(mxplan)
c     modify: ep(mxplan),ul4
c      write: watstr(mxplan),st(mxnsl,mxplan)
c
c     + + + LOCAL VARIABLES + + +
      real  gx, sum, u(mxnsl),ub, uob, xx    
      integer k,ixx 
c
c     + + + LOCAL DEFINITIONS + + +
c       sum   - potential (maximum possible) water uptake by plant
c               roots.
c       u     - actual water uptake by plant roots from each soil
c               layer, m.
c       xx    - total water uptake for this OFE.
c       ixx   - index of deepest soil layer containing roots.
c       gx    - depth of roots in the current layer.
c       ub    - a plant water use rate-depth parameter = 3.065
c               (See eq. 7.3.3)
c       uob   -  Since UB is a constant, "1 - exp(-UB)" is too;
c                ie, 0.953346.
c
c     + + + DATA INITIALIZATIONS + + +
      data uob /0.953346/, ub /3.065/
c
c     + + + END SPECIFICATIONS + + +
c
c      If the roots are all within the soil layers....
      if (rtd(iplane).lt.solthk(nsl(iplane),iplane)) then
c       Initialize U and find deepest soil layer (IXX) containing roots.
c       (Reverse order of loop to find SHALLOWEST layer >= root depth.)
        do 10 k = nsl(iplane), 1, -1
          u(k) = 0.
          if (rtd(iplane).le.solthk(k,iplane)) ixx = k
   10   continue
c
c     If the root depth is greater than the soil layers....
      else
        rtd(iplane) = solthk(nsl(iplane),iplane)
        ixx = nsl(iplane)
      end if
c
c     new adjustment of ep for intercepted rain by live plants(intpla)
c     plaint will evaporate first, reza 7/27/93
c
cd    Modified by S. Dun July 26,2003
      if (plaint(iplane).gt.0.0) then
cd      if (ep(iplane).gt.0.0) then
        ep(iplane) = ep(iplane) - plaint(iplane)
      if (ep(iplane).gt.0.0) pintlv(iplane) = 0.0
        if (ep(iplane).lt.0.0) then
        pintlv(iplane) = -ep(iplane)
        ep(iplane) = 0.0        
      endif
      end if
cd    End modifying
c
c     If there is evapotranspiration....
      if (ep(iplane).gt.0.) then
        xx = 0.0
c
c       distribution of plant transpiration ep (m) in the root zone rtd (m)
c       (equations 7.3.3-4)
c
c       For all soil layers containing roots....
        do 20 k = 1, ixx
c
          if (k.lt.ixx) then
            gx = solthk(k,iplane)
          else
            gx = rtd(iplane)
          end if
c
c         (eq. 7.3.3)
          if (rtd(iplane).gt.0.0) then
            sum = (1.-exp(-ub*gx/rtd(iplane))) * ep(iplane) / uob
          else
            sum = ep(iplane) / uob
          end if
c
          u(k) = sum - xx
c
c         Determine tolerence to moisture stress for current crop;
c         ie, the fraction of UL that soil moisture reach before
c         moisture stress occurs, and water uptake is reduced.
          if (itype(nowcrp,iplane).gt.0) then
c           ---------- if no value is input for PLTOL; ie, PLTOL = 0, set PLTOL
c           to default value of 0.25.
            if (pltol(itype(nowcrp,iplane)).le.0.) then
              pltol(itype(nowcrp,iplane)) = 0.25
c           ---------- if value input for PLTOL is > 0.4, set it to 0.4.
            else if (pltol(itype(nowcrp,iplane)).gt.0.4) then
              pltol(itype(nowcrp,iplane)) = 0.4
c           ---------- if value input for PLTOL is < 0.1, set it to 0.1.
            else if (pltol(itype(nowcrp,iplane)).lt.0.1) then
              pltol(itype(nowcrp,iplane)) = 0.1
            end if
c
c           Determine the threshold for water stress (UL4).
            ul4 = pltol(itype(nowcrp,iplane)) * ul(k,iplane)
cd    Modified by S. Dun April 04,2003
cd          end if
cd    Move down one line.
c
c         --------- compute water uptake by plants, (not to exceed avail. moisture (ST))
            if (st(k,iplane).lt.ul4) u(k) = u(k) * st(k,iplane) / ul4
        end if
cd    End Modifying
          if (st(k,iplane).lt.u(k)) u(k) = st(k,iplane)
          if (u(k).lt.1e-10) u(k) = 0.0
c
c         -------- reduce water content of soil layer to reflect plant uptake.
          st(k,iplane) = st(k,iplane) - u(k)
          if (st(k,iplane).lt.1e-10) st(k,iplane) = 0.00
c
          xx = xx + u(k)
c
   20   continue
c
c       Calculate water stress factor (0-1); ie, ratio of actual uptake
c       to potential uptake.
        watstr(iplane) = xx / ep(iplane)
c       ------ set evapotranspiration = actual water uptake.
        ep(iplane) = xx
c
c     If there is no evapotranspiration....
      else
        watstr(iplane) = 1.0
      end if
c
c     7/27/93 daily actual plant transpiration was reduced before
c     by the amount of plaint, m,  so must add it back on -  reza
c
      ep(iplane) = ep(iplane) + plaint(iplane)
cd    Modified By S. Dun July 26, 2003
      if(pintlv(iplane).gt.0.0) then
        ep(iplane) = ep(iplane) - pintlv(iplane)
      else
        pintlv(iplane) = 0.0
      endif
cd    End modifying
c
c     8/16/93 - need to reset value of plaint to 0.0 so it does not
c     cause errors in following day when there is no rainfall - reza
c
c      plaint(iplane) = 0.0
c
      return
      end
