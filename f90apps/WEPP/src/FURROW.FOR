      subroutine   furrow(
     i                    nowcrp,pkrmax,runmax,
     m                    rwdth)
c
c     + + + PURPOSE + + +
c     This subprogram controls a kinematic wave model for specified
c     space intervals using the Kostiakov-Lewis infiltration function.
c     Subprogram FURROW mimics subprogram IRS in developing equivalent
c     plane parameters.
c
c     Called from IRRIG
c     Author(s): E. R. Kottwitz.
c     Reference in User Guide: Chapter 12
c
c     Changes:
c
c     Version: This module recoded from WEPP Version 93.06.
c     Date recoded: 07/07/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + PARAMETERS + + +
      include 'pmxelm.inc'
      include 'pmxcrp.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxsrg.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'pxstep.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer   nowcrp(mxplan)
      real      pkrmax,runmax,rwdth
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nowcrp - number of current crop
c     pkrmax - maximum of the peak runoff rates of the overland flow
c              elements making up the equivalent plane (m/s)
c     runmax - maximum of the runoff values of the overland flow
c              elements making up the equivalent plane (m)
c     rwdth  - row width of overland flow element into which water is
c              introduced (m)
c
c     + + + COMMON BLOCKS + + +
      include 'cconsta.inc'
c       read: accgav
c
      include 'ccrpout.inc'
c       read: rh(mxplan)
c
      include 'ccrpprm.inc'
c       read: rw(mxcrop,mxplan)
c
      include 'cdist2.inc'
c       read: slplen(mxplan)
c
      include 'cefflen.inc'
c     modify: efflen(mxplan)
c
      include 'cends4.inc'
c       read: rspace(mxplan)
c
      include 'cffact.inc'
c       read: frctrl
c
      include 'chydrol.inc'
c     modify: runoff(mxplan)
c      write: effdrn(mxplan),peakro(mxplan)
c
      include 'ciraflo.inc'
c     modify: botwid,sidslp
c
      include 'cirfurr.inc'
c       read: depsrg(mxplan),endofe,nqsppl(mxsrg),ntend(mxsrg),
c             ntstrt(mxsrg),surges(mxplan)
c     modify: advdis,irqin,nsurge,qspply(mxplan),srg,surge,tend(mxsrg),
c             timtot,tstart(mxsrg),xpostn(0:xsteps)
c      write: aflow(0:xsteps,2),infltr(0:xsteps,2),infvlm(mxplan),
c             inoptm(0:xsteps),irslp,qflow(0:xsteps,2),srgflg,
c             tadvan(0:xsteps),timflg
c
      include 'cirriga.inc'
c       read: irofe,noirr
c
      include 'cprams2.inc'
c      write: norun(mxplan)
c
      include 'cslope2.inc'
c       read: avgslp(mxplan)
c
      include 'cstmflg.inc'
c       read: nmon
c
      include 'cstruc.inc'
c       read: nplane
c     modify: iplane
c
      include 'csumirr.inc'
c     modify: irrund(mxplan),irrunm(mxplan),irrunt(mxplan),
c             irruny(mxplan),
c
      include 'cslpopt.inc'
c     read: totlen
c
      include 'cupsfl.inc'
c
c     + + + LOCAL VARIABLES + + +
      integer   i,itemp,j,lipl,retflg
      real      chezch,deltax
c
c     + + + LOCAL DEFINITIONS + + +
c     i      - primary kinematic wave calculation counter
c     itemp  - maximum advance increment counter
c     j      - secondary kinematic wave calculation counter
c     lipl   - local overland flow element counter
c     retflg - flag indicating model status when returning from
c              subprograms FURADV and FURRUN (retflg=0: generate runoff
c              information and initial value, retflg=1: furrow
c              irrigation simulation is complete, retflg=2: process next
c              OFE, retflg=3: restart process using depletion level
c              irrigation parameters, retflg=4: process next surge).
c     chezch - Chezy friction coefficient (m**(0.5)/s)
c     deltax - distance increment (m)
c
c     + + + SUBROUTINES CALLED + + +
c     frcfac
c     furadv
c     furrun
c     irprnt
c     kostia
c
c     + + + END SPECIFICATIONS + + +
c
c     ----- INITIALIZE VARIABLES -----
c
c     The following runon - runoff cases can occur (cases numbered as in
c     sr irs).  Rainfall excess is 0 for all cases as irrigation on days
c     with rainfall is not supported by the model.
c
c     case 1 : q(iplane-1) =  0        q(iplane) = 0
c     case 3 : q(iplane-1) >  0        q(iplane) > 0
c     case 4 : q(iplane-1) >  0        q(iplane) = 0
c
c     Case 1 occurs for all planes above water supply point.
c
      irqin = 0.0
      do 100  iplane = 1, irofe-1
        norun(iplane) = 0
        infvlm(iplane) = 0.0
        runoff(iplane) = 0.0
        peakro(iplane) = 0.0
        effdrn(iplane) = 0.0
        efflen(iplane) = slplen(iplane)
c
c       Write "single storm" output.
c
        if(imodel.ne.1)call irprnt
  100 continue
c
c     Case 3 or case 4 for plane where water is introduced.  Note that
c     this plane is not a "true" case 3 or case 4 since there was no
c     runoff from the previous plane.
c
      norun(irofe) = 1
      runoff(irofe) = 0.0
      peakro(irofe) = 0.0
      effdrn(irofe) = 0.0
      efflen(irofe) = slplen(irofe)
c
c     Row width used for furrow irrigation is equal to the crop row
c     spacing unless this value is less than 0.2 meters.  The second
c     choice for row width is rill spacing.  If rill spacing is less
c     than 0.2 meters then set row width for furrow irrigation
c     calculations equal to 0.2 meters.
c
      if(rw(nowcrp(irofe),irofe).ge.0.2) then
        rwdth = rw(nowcrp(irofe),irofe)
      elseif(rspace(irofe).ge.0.2) then
        rwdth = rspace(irofe)
      else
        rwdth = 0.2
      endif
c
c     Estimate furrow geometry.  User will eventually be asked to
c     provide botwid and sidslp.  If values are not provided then these
c     equations (or similar ones) will be used to estimate botwid and
c     sidslp
c
      botwid = 0.25*rwdth
      sidslp = botwid/rh(irofe)
c
c     Variables to be initialized depend on the irrigation scheduling
c     alternative used.
c
      if(noirr.eq.2)then
c
c       For fixed date scheduling initialize surge and advdis.
c
        surge = surges(irofe)
        advdis = 0.0
      else
c
c       For depletion level scheduling, find advance distance for first
c       surge.
c
        advdis = 0.0
        do 200  iplane = irofe, endofe
          advdis = advdis+slplen(iplane)
  200   continue
        if(depsrg(irofe).ge.3)advdis = advdis/float(depsrg(irofe))
      endif
c
c     Reset iplane for furrow irrigation simulation.
c
      iplane = irofe
c
c     ----- INITIALIZE OFE SPECIFIC VARIABLES -----
c
c     Label 10 marks the position where control is passed after
c     incrementing to the next OFE.
c
   10 continue
c
c     Calculate roughness coefficient for the rill or furrow.
c
      call frcfac(nowcrp(iplane))
c
c      ---- (WEPP Equation 9.1.1 (NSERL Report No. 2))
      chezch = sqrt(8.0*accgav/frctrl(iplane))
c
c     Determine slope to be used for furrow irrigation.  Kinematic wave
c     analysis is limited to slopes >= 0.1%.
c
      irslp = max(0.001,avgslp(iplane))
c
c     Determine furrow geometry parameters (aqcnst and aqexp)
c
      call furgps(chezch)
c
c     If the following is true then calculate timtot and, possibly,
c     irqin.  Otherwise, timtot has already been estimated (depletion
c     level scheduling) and calculating irqin is not necessary.
c
   30 if(noirr.eq.2)then
        timtot = 0.0
        do 300  srg = 1, surge
          timtot = timtot+tend(srg)-tstart(srg)
c
c         Irqin has units of m^2/s (flow rate per unit width).
c
          if(iplane.eq.irofe)irqin = max(irqin,qspply(srg)/rwdth)
  300   continue
      endif
c
c     Obtain Kostiakov-Lewis infiltration function constants (kosta,
c     kostf, and kostk).
c
      call kostia(rwdth)
c
c     Write "single storm" output.
c
      if(imodel.ne.1)call irprnt
c
c     Reset flow area and infiltrated depth arrays.
c
      do 400  i = 0, xsteps
        aflow(i,1) = 0.0
        aflow(i,2) = 0.0
        qflow(i,1) = 0.0
        qflow(i,2) = 0.0
        infltr(i,1) = 0.0
        infltr(i,2) = 0.0
        tadvan(i) = 0.0
        trec(i) = 0.0
  400 continue
c
c     Initialize miscellaneous variables.
c
      itemp = 0
      srgflg = 0
      timflg = 0
      srg = 1
      tadvan(0) = tstart(srg)
c
c     Determine xpostn(0).
c
      if(iplane.eq.irofe)then
        xpostn(0) = 0.0
      else
        xpostn(0) = xpostn(xsteps)
      endif
c
c     Determine deltax and xpostn(xsteps).
c
      if(noirr.eq.1 .and. slplen(irofe)+xpostn(0).ge.advdis)then
        deltax = 1.5*(advdis-xpostn(0))/float(xsteps)
        xpostn(xsteps) = advdis
      else
        deltax = 1.5*slplen(iplane)/float(xsteps)
        xpostn(xsteps) = xpostn(0)+slplen(iplane)
      endif
c
c     Determine xpostn(1), xpostn(2),    , xpostn(xsteps-1)
c     and inoptm(0), inoptm(1),    , inoptm(xsteps-1) AND
c     inoptm(xsteps).
c
      inoptm(0) = 0.0
      do 500  j = 1, xsteps-1
        if(j.eq.xsteps/3+1)deltax = deltax/2.0
        inoptm(j) = 0.0
        xpostn(j) = xpostn(j-1)+deltax
  500 continue
      inoptm(xsteps) = 0.0
c
c     -----  ADVANCE PHASE  -----
c
c     Label 20 marks the position where control is passed when
c     incrementing the surge if depletion and recession occurred for the
c     previous surge.
c
   20 retflg = 0
      call furadv(rwdth,i,retflg)
c
c     The value of the variable retflg returned from FURADV indicates
c     next task (see explanation in local variables list)
c
      if(retflg.eq.1)then
        goto 9999
      elseif(retflg.eq.2)then
        goto 10
      elseif(retflg.eq.3)then
        goto 30
      endif
c
c     -----  GENERATE RUNOFF INFORMATION  -----
c
      call furrun(i,rwdth,itemp,pkrmax,runmax,retflg)
      if(retflg.eq.4)goto 20
c
c     If next statement is true then increment OFE counter, reinitialize
c     parameters where necessary, and pass control to label 10.
c
cd   Added by S. Dun April 04, 2004
      if((runoff(iplane).gt.0.000001).and. (iplane.le.(nplane-1))) then
          iuprun(iplane+1) = 1
      endif
cd   end adding
      if(nsurge.ne.0)then
        iplane = iplane+1
        surge = nsurge
        nsurge = 0
        norun(iplane) = 1
        runoff(iplane) = 0.0
        peakro(iplane) = 0.0
        effdrn(iplane) = 0.0
        efflen(iplane) = efflen(iplane-1)+slplen(iplane)
        do 600  srg = 1, surge
          qspply(srg) = nqsppl(srg)
          tend(srg) = ntend(srg)
          tstart(srg) = ntstrt(srg)
  600   continue
c
c       Write "single storm" output.
c
        if(imodel.ne.1)call irprnt
        goto 10
      endif
c
c     Case 1 occurs for all remaining planes.
c
      do 700  lipl = iplane+1, nplane
        norun(lipl) = 0
        runoff(lipl) = 0.0
        peakro(lipl) = 0.0
        effdrn(lipl) = 0.0
        efflen(lipl) = slplen(lipl)
        infvlm(lipl) = 0.0
  700 continue
c
c     Update variables of common block sumirr.
c
cd    Now the total irrigation attributed runoff depth is for 
cd    the total length of the OFE which means counting from 
cd     top of the hillslope  
      do 800  lipl = 1, nplane
        irrund(lipl) = runoff(lipl)
        irrunt(lipl) = irrunt(lipl) + irrund(lipl)
     1    * efflen(iplane) / totlen(iplane)
        irruny(lipl) = irruny(lipl) + irrund(lipl)
     1    * efflen(iplane) / totlen(iplane)
        irrunm(lipl) = irrunm(lipl) + irrund(lipl)
     1    * efflen(iplane) / totlen(iplane)
  800 continue
c
 9999 return
      end
