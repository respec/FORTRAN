      subroutine   furadv(
     i                    rwdth,
     m                    ii,
     o                    retflg)
c
c     + + + PURPOSE + + +
c     This subprogram performs the advance and continuing phase
c     calculations for the furrow irrigation component.
c
c     Called from FURROW
c     Author(s): E. R. Kottwitz.
c     Reference in User Guide: Chapter 12
c
c     Changes:
c          1) Changed dummy parameter 'I' to a more unique 'II'.
c          2) Before recoding this module had 20 GOTO's.
c          3) Made all the addresses consecutive.  (Radical!)
c          4) Inverted 2 IF-ELSE-ENDIF's under "CONTINUING PHASE"
c             to move GOTO's to the bottom.  This makes it easier
c             to eventually eliminate them.
c          5) Added 22 equation numbers supplied by Kottwitz,
c             and verified the accuracy of the numbers, inverted
c             IF-ELSE-ENDIF's and all.
c
c     Version: This module recoded from WEPP Version 93.06.
c     Date recoded: 07/02/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + PARAMETERS + + +
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxsrg.inc'
      include 'pxstep.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer   ii,retflg
      real      rwdth
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ii     - primary kinematic wave calculation counter
c     retflg - flag indicating model status when returning to subprogram
c              FURROW
c     rwdth  - assumed row width for OFE into which water is introduced
c              (m)
c
c     + + + COMMON BLOCKS + + +
      include 'cdist2.inc'
c       read: slplen(mxplan)
c
      include 'chydrol.inc'
c     modify: runoff(mxplan),peakro(mxplan)
c
      include 'ciraflo.inc'
c       read: aqcnst,aqexp
c     modify: ircon2
c      write: ircon1
c
      include 'cirfurr.inc'
c       read: advdis,spavz,surge,tend(mxsrg),tstart(mxsrg)
c     modify: aflow(0:xsteps,2),infltr(0:xsteps,2),infvlm(mxplan),
c             inoptm(0:xsteps),irapld(mxplan),nsurge,ntend(mxsrg),
c             ntstrt(mxsrg),qspply(mxplan),runosg,srg,srgflg,
c             tadvan(0:xsteps),timflg,timtot,xpostn(0:xsteps)
c      write: nqsppl(mxsrg)
c
      include 'cirinfl.inc'
c       read: kosta,kostf,kostk
c
      include 'cirriga.inc'
c     modify: noirr
c      write: irsyst
c
      include 'cprams2.inc'
c      write: norun(mxplan)
c
      include 'cstruc.inc'
c       read: nplane
c     modify: iplane
c
c     + + + LOCAL VARIABLES + + +
      integer   j,minflg,npriod
      real      aest,atoler,deltat,deltax,dtcnst,dtime,dtmin,spava,
     1          tdtmin,tiavq,ttoler
c
c     + + + LOCAL DEFINITIONS + + +
c     j      - secondary kinematic wave calculation counter
c     minflg -
c     npriod - number of continuing phase time increments
c     aest   - initial estimate of flow area (m**2)
c     atoler - flow area tolerance value for Newton-Raphson iterations
c              (m**2)
c     deltat - computed time increment (s)
c     deltax - advance distance increment (m)
c     dtcnst - coefficient used to extimate next time increment
c     dtime  - computed time increment for continuing phase (s)
c     dtmin  -
c     spava  - space averaging coefficient for flow area for advance
c              phase
c     tdtmin -
c     tiavq  - time averaging coefficient for flow rate
c     ttoler - advance time tolerance value for Newton-Raphson
c              iterations (s)
c
c     + + + FUNCTION DECLARATIONS + + +
      external  irflow
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/' Water supply rate for depletion level scheduling is too'
     1       /' low.  Depletion level irrigation will be skipped.'/)
c
c     + + + DATA INITIALIZATIONS + + +
      data      atoler/0.000001/,dtcnst/1.08/,spava/0.65/,tiavq/0.35/,
     1          ttoler/1.0/
c
c     + + + SUBROUTINES CALLED + + +
c     depirr
c     furrec
c     irflow (access is through subprogram newrap)
c     irprnt
c     newrap
c
c     + + + END SPECIFICATIONS + + +
c
c     -----  ADVANCE PHASE  -----
c
c     Initialize variable
c
      runosg = 0.0
c
c     Calculate flow rate for upper end of OFE
c
      qflow(0,2) = qspply(srg)
c
c     Calculate flow area for upper end of OFE
c      ---- (WEPP Equation 12.10, solved for A)
c
      aflow(0,2) = (qspply(srg)/aqcnst)**(1.0/aqexp)
c
c     Initial estimate of advance time to end of first length increment
c
      tadvan(1) = tstart(srg)+spava*aflow(0,2)*(xpostn(1)-xpostn(0))/
     1            qspply(srg)
c
c     If next statement is true estimated advance time is greater than
c     time at end of surge so set tadvan(1)=tend(srg)
c
      if(tadvan(1).gt.tend(srg))then
        tadvan(1)=tend(srg)
        timflg = 1
      endif
c
c     Label 10 marks the position where control is passed when iterating
c     to find time step for first length increment
c
   10 continue
c      ---- (WEPP Equation 12.4)
      infltr(0,2) = kostk*(inoptm(0)+tadvan(1)-tadvan(0))**kosta+kostf*
     1              (inoptm(0)+tadvan(1)-tadvan(0))
c
c      ---- (special case of WEPP Equation 12.14)
      deltat = (spava*aflow(0,2)+spavz*(infltr(0,2)-infltr(0,1)))*
     1         (xpostn(1)-xpostn(0))/qspply(srg)-tadvan(1)+tadvan(0)
c
c     If next statement is true then advance time to end of first length
c     increment has not been found so try again
c
c      *** L0 IF ***
      if(abs(deltat).gt.ttoler)then
        tadvan(1) = tadvan(1)+deltat
c
c       If next statement is true then advance might not reach end of
c       first length increment before end of surge
c
c        *** L1 IF ***
        if(tadvan(1).ge.tend(srg))then
c
c         If next statement is true then set tadvan(1) =  tend(srg)
c         (Note: srg = 1 if noirr = 1)  Otherwise the model is still trying
c         to find duration of first surge (for depletion level scheduling).
c
c          *** L-1.5 IF ***
          if(noirr.ne.1)then
            tadvan(1) = tend(srg)
c
c           If next statement is true then advance might not reach end
c           of first length increment by end of surge so set timflg=1
c           and continue.  If statement is false advance will not reach
c           end of first length increment by end of surge.
c
c            *** L2 IF ***
            if(timflg.ne.1)then
              timflg = 1
c
c            *** L2 ELSE ***
            else
c
c             Determine new position of calculation cell boundary relative
c             to the upper end of the OFE
c              ---- (special case of WEPP Equation 12.14, solved for
c                   DELTAX)
              deltax = qspply(srg)*(tadvan(1)-tadvan(0))/(spava*
     1                 aflow(0,2)+spavz*(infltr(0,2)-infltr(0,1)))
c
c             If next statement is true then increment surge and continue
c             advance phase.  If statement is false then go to depletion
c             phase
c
c              *** L3 IF ***
              if(srg.lt.surge)then
c
c             If next statement is true then complete recession will occur
c             before beginning of next surge so go to depletion phase.  If
c             statement is false inflow is continuous but changing so
c             continue advance phase
c
                if(tstart(srg+1)-tend(srg).gt.1.0)then
                  xpostn(1) = xpostn(0)+deltax
                  ii = 1
                  goto 170
                else
                  srg = srg+1
                  qflow(0,2) = qspply(srg)
c
c                  ---- (WEPP Equation 12.10, solved for A)
                  aflow(0,2) = (qspply(srg)/aqcnst)**(1.0/aqexp)
                  inoptm(1) = inoptm(0)+(inoptm(1)-inoptm(0))*deltax/
     1                        (xpostn(1)-xpostn(0))
                  inoptm(0) = tend(srg-1)-tadvan(0)
                  tadvan(0) = tstart(srg)
                  xpostn(1) = xpostn(0)+deltax
                  srgflg = 1
                  timflg = 0
                  goto 20
                endif
c
c              *** L3 ELSE ***
              else
                xpostn(1) = xpostn(0)+deltax
                ii = 1
                goto 170
c
c              *** L3 ENDIF ***
              endif
c
c            *** L2 ENDIF ***
            endif
c
c          *** L-1.5 ELSE ***
          else
c            ---- supply rate is too low: continue without irrigation.
            goto 180
c
c          *** L-1.5 ENDIF ***
          endif
c
c        *** L1 ENDIF ***
        endif
        goto 10
c
c      *** L0 ENDIF ***
      endif
c
c     If next statement is true then advance time to end of first length
c     increment was calculated as being equal to end of surge without
c     shifting calculation cell boundary (a rare special case)
c
c      *** M0 IF ***
      if(tadvan(1).eq.tend(srg))then
c
c       If next statement is true then the model is still trying to
c       find duration of first surge (for depletion level scheduling).
c
c        *** M1 IF ***
        if(noirr.eq.1)then
c
c         If next statement is true then the supply rate is too low and
c         the model will continue without irrigation.
c
          if(abs(tend(1)-86400.0).le.1.0) goto 180
c
c        *** M1 ELSEIF ***
        elseif(tstart(srg+1)-tend(srg).gt.1.0 .or. srg.eq.surge)then
c
c         Go to depletion phase
c
          ii = 1
          goto 170
c
c        *** M1 ELSE ***
        else
c
c         Increment surge
c
          srg = srg+1
          qflow(0,2) = qspply(srg)
c
c          ---- (WEPP Equation 12.10, solved for A)
          aflow(0,2) = (qspply(srg)/aqcnst)**(1.0/aqexp)
          inoptm(0) = tend(srg-1)-tadvan(0)
          tadvan(0) = tstart(srg)
          srgflg = 1
c
c        *** M1 ENDIF ***
        endif
c
c      *** M0 ENDIF ***
      endif
   20 ii = 1
c
c     Label 25 marks the position where control is passes to increment
c     primary counter for kinematic wave calculations
c
   25 ii = ii+1
      dtmin = 1e6
      minflg = 0
      tdtmin = 0.0
c
c     Shift aflow and infltr arrays
c
      do 30 j = 0, ii-2
        aflow(j,1) = aflow(j,2)
        qflow(j,1) = qflow(j,2)
        infltr(j,1) = infltr(j,2)
   30 continue
c
c     Make initial estimate of advance time to end of length increment i
c
      if(tadvan(ii-1).eq.tstart(srg))then
        tadvan(ii) = tstart(srg)+inoptm(ii-2)*(xpostn(ii)-xpostn(ii-1))/
     1               (xpostn(ii-1)-xpostn(ii-2))*dtcnst
      else
        tadvan(ii) = tadvan(ii-1)+(tadvan(ii-1)-tadvan(ii-2))*
     1               (xpostn(ii)-xpostn(ii-1))/(xpostn(ii-1)-
     2               xpostn(ii-2))*dtcnst
      endif
c
c     If next statement is true then adjust estimated advance time
c
      if(srgflg.eq.1)then
        tadvan(ii) = tadvan(ii)*qspply(srg-1)/qspply(srg)
        srgflg = 0
      endif
c
c     If next statement is true then advance time estimate exceeds time
c     at end of surge so set tadvan(ii)=tend(srg).  Otherwise, model is
c     still trying to find duration of first surge (depletion level
c     scheduling)
c
      if(tadvan(ii).ge.tend(srg))then
        tadvan(ii) = tend(srg)
        timflg = 1
      endif
c
c     Label 35 marks the position where control is passed while
c     iterating for the proper advance time to end of length increment i
c
c     Calculate infiltration at upper end of OFE
c      ---- (WEPP Equation 12.4)
  35  infltr(0,2) = kostk*(inoptm(0)+tadvan(ii)-tadvan(0))**kosta+kostf*
     1              (inoptm(0)+tadvan(ii)-tadvan(0))
c
c     Do loop to calculate flow area and infiltration (infiltrated
c     volume per unit furrow length) for rectangular cells
c
      do 40 j = 1, ii-1
c        ---- (WEPP Equation 12.4)
        infltr(j,2) = kostk*(inoptm(j)+tadvan(ii)-tadvan(j))**kosta+
     1                kostf*(inoptm(j)+tadvan(ii)-tadvan(j))
c        ---- (WEPP Equation 12.13)
        ircon2 = (tiavq/aqcnst*(qflow(j,1)-qflow(j-1,1))-(1.0-
     1           tiavq)/aqcnst*qflow(j-1,2)+(spava*(aflow(j-1,2)-
     2           aflow(j-1,1))-(1.0-spava)*aflow(j,1)+spavz*
     3           (infltr(j-1,2)-infltr(j-1,1))+(1.0-spavz)*(infltr(j,2)-
     4           infltr(j,1)))*(xpostn(j)-xpostn(j-1))/aqcnst/
     5           (tadvan(ii)-tadvan(ii-1)))/(1.0-tiavq)
c
c       If next statement is true then area of flow is very small.  if
c       statement is false then calculate flow area and continue
c
        if(ircon2.ge.0.0)then
          aflow(j,2) = 0.00005
          qflow(j,2) = aqcnst*aflow(j,2)**aqexp
        else
c
c         Method of making initial estimate of flow area depends on
c         value of j relative to i
c
          if(j.ne.ii-1)then
            aest = aflow(j-1,2)+aflow(j,1)-aflow(j-1,1)
            if(aest.le.0.0)aest = aflow(j-1,1)
          else
            aest = aflow(j-1,1)
          endif
c
c          ---- (WEPP Equation 12.12)
          ircon1 = (1.0-spava)*(xpostn(j)-xpostn(j-1))/(1.0-tiavq)/
     1             aqcnst/(tadvan(ii)-tadvan(ii-1))
c
c         NOTE:  The call to IRFLOW from NEWRAP includes the expected
c                argument.  The argument is not required here.
c
          call newrap(aest,atoler,irflow,aflow(j,2))
          qflow(j,2) = aqcnst*aflow(j,2)**aqexp
        endif
   40 continue
c
c     Calculate change in estimate of advance time
c
c      ---- (WEPP Equation 12.14)
      deltat = (xpostn(ii)-xpostn(ii-1))*(spava*aflow(ii-1,2)+spavz*
     1         (infltr(ii-1,2)-infltr(ii-1,1)))/(1.0-tiavq)/
     2         qflow(ii-1,2)-tadvan(ii)+tadvan(ii-1)
c
c     If next statement is true then advance time to end of length
c     increment has not been found so try again and check for possible
c     advance time greater than time at end if surge
c
c      *** N0 IF ***
      if(abs(deltat).gt.ttoler .and. minflg.eq.0)then
        if(abs(deltat).ge.abs(dtmin))then
          if(minflg.eq.0)minflg = 1
          tadvan(ii) = tdtmin+dtmin
        else
          dtmin = deltat
          tdtmin = tadvan(ii)
          tadvan(ii) = tadvan(ii)+deltat
        endif
c
c       If next statement is true then advance might not reach end of
c       length increment and adjustments might be necessary.  If
c       statement is false then then try new estimate of advance time
c
c        *** N1 IF ***
        if(tadvan(ii).ge.tend(srg))then
c
c         If next statement is not true then the model is still trying to
c         find duration of first surge (for depletion level scheduling).
c         So set tadvan(1) =  tend(srg) (Note: srg = 1 if noirr = 1)
c
c          *** N-1.5 IF ***
            if(noirr.ne.1)then
c
c             The supply rate is not too low and the model will continue
c             with irrigation.
c
              tadvan(ii) = tend(srg)
c
c           If next statement is true then advance will not reach end of
c           length increment before end of surge so increment surge.  If
c           statement is false then advance might not reach end of length
c           increment before end of surge so set timflg=1 and continue
c
c            *** N2 IF ***
            if(timflg.eq.1)then
              timflg = 0
c              ---- (WEPP Equation 12.14,  solved for DELTAX)
              deltax = (1.0-tiavq)*qflow(ii-1,2)*(tadvan(ii)-
     1                 tadvan(ii-1))/(spava*aflow(ii-1,2)+spavz*
     2                 (infltr(ii-1,2)-infltr(ii-1,1)))
c
c             If next statement is true then shift prior calculation
c             boundary downslope.  If statement is false then shift
c             current calculation boundary upslope.  In both cases
c             linearly interpolate for inoptm and adjust aflow, infltr,
c             and tadvan variables
c
              if(deltax/(xpostn(ii)-xpostn(ii-1)).le.0.5 .or.
     1                                                ii.eq.xsteps) then
                inoptm(ii-1) = inoptm(ii-1)+(inoptm(ii)-inoptm(ii-1))*
     1                         deltax/(xpostn(ii)-xpostn(ii-1))
                xpostn(ii-1) = xpostn(ii-1)+deltax
                tadvan(ii-1) = tend(srg)
                aflow(ii-1,2) = 0.0
                qflow(ii-1,2) = 0.0
c
c                ---- (WEPP Equation 12.4)
                infltr(ii-1,1) = kostk*inoptm(ii-1)**kosta+kostf*
     1                           inoptm(ii-1)
                infltr(ii-1,2) = infltr(ii-1,1)
                ii = ii-1
              else
                inoptm(ii) = inoptm(ii-1)+(inoptm(ii)-inoptm(ii-1))*
     1                       deltax/(xpostn(ii)-xpostn(ii-1))
                xpostn(ii) = xpostn(ii-1)+deltax
                tadvan(ii) = tend(srg)
c
c                ---- (WEPP Equation 12.4)
                infltr(ii,1) = kostk*inoptm(ii)**kosta+kostf*inoptm(ii)
                infltr(ii,2) = infltr(ii,1)
              endif
c
c             If next statement is true inflow is continuous but
c             changing so continue with advance phase after incrementing
c             srg.  If statement is false then complete recession will
c             occur before beginning of next surge so go to depletion
c             phase.
c
              if(tstart(srg+1)-tend(srg).le.1.0 .and. srg.ne.surge)then
                srg = srg+1
                qflow(0,2) = qspply(srg)
c
c                ---- (WEPP Equation 12.10, solved for A)
                aflow(0,2) = (qspply(srg)/aqcnst)**(1.0/aqexp)
                do 50 j = 0, ii-1
                  inoptm(j) = tend(srg-1)-tadvan(j)
                  tadvan(j) = tstart(srg)
   50           continue
                srgflg = 1
                timflg = 0
              else
c                -- XXX
                goto 170
              endif
c
c            *** N2 ELSE ***
            else
              timflg = 1
              goto 35
c
c            *** N2 ENDIF ***
            endif
c
c          *** N-1.5 ELSE ***
          else
c            ---- supply rate is too low: continue without irrigation.
            goto 180
c
c          *** N-1.5 ENDIF ***
          endif
c
c        *** N1 ELSE ***
        else
          goto 35
c
c        *** N1 ENDIF ***
        endif
c
c      *** N0 ENDIF ***
      endif
c
c  CORRECTION FOR ADVANCE TIME TO END OF INCREMENT i EQUAL TO END TIME
c  OF SURGE WITHOUT SHIFTING CALCULATION BOUNDARY
c
      if(abs(tadvan(ii)-tend(srg)).le.ttoler)then
        if(tstart(srg+1)-tend(srg).le.1.0 .and. srg.ne.surge)then
          srg = srg+1
          qflow(0,2) = qspply(srg)
c
c          ---- (WEPP Equation 12.10, solved for A)
          aflow(0,2) = (qspply(srg)/aqcnst)**(1.0/aqexp)
          do 60 j = 0, ii-1
            inoptm(j) = tend(srg-1)-tadvan(j)
            tadvan(j) = tstart(srg)
   60     continue
          srgflg = 1
          timflg = 0
        else
c          -- XXX
          goto 170
        endif
      endif
c
c  END CORRECTION    gkottwitz  9/19/91      dcf  9/21/91
c
c
c     Pass control to label 25 if advance phase calculations are not
c     complete
c
      if(ii.lt.xsteps)goto 25
c
c     If next statement is true then model is still trying to determine
c     duration of first surge for depletion level irrigation
c
      if(noirr.eq.1)then
        if(abs(xpostn(xsteps)-advdis).lt.1.0)then
          call depirr(rwdth)
          retflg = 3
        else
          qspply(1) = qspply(1)-kostf*slplen(iplane)
          if(qspply(1).gt.0.0)then
            timtot = timtot-tadvan(xsteps)
            if(timtot.lt.3600.0)timtot = 3600.0
            iplane = iplane+1
            retflg = 2
          else
c           -- XXX
c           ---- supply rate is too low: continue without irrigation.
            goto 180
          endif
        endif
c
c       Write "single storm" output
c
        if(imodel.ne.1)call irprnt
        goto 200
      endif
c
c     -----  CONTINUING PHASE  -----
c
c     Determine npriod and divide remaining irrigation time of current
c     surge into npriod periods of equal duration
c
c     If next statement is true calculate npriod and deltat.  If
c     statement is false then srg has just been incremented or may need
c     to be incremented before determining npriod and deltat.
c
      if(srgflg.ne.1 .and. tadvan(xsteps).ne.tend(srg))then
        if((tend(srg) - tadvan(xsteps)) .gt.
     1       4.0*(tadvan(xsteps) - tadvan(xsteps-1)))then
          npriod = 5
        else
          npriod = int((tend(srg)-tadvan(xsteps))/(tadvan(xsteps)-
     1             tadvan(xsteps-1)))+1
        endif
        deltat = (tend(srg)-tadvan(xsteps))/float(npriod)
c
      else
c
c       If next statement is true then last advance time calculated
c       corresponded to a calculation boundary without moving the
c       boundary (a rare special case) so it may be necessary to
c       increment srg before calculating npriod and deltat
c
        if(tadvan(xsteps).eq.tend(srg))then
c
c         If next statement is true then increment surge and continue
c         continuing phase.  If statement is false begin depletion
c         phase.
c
          if(tstart(srg+1)-tend(srg).le.1.0 .and. srg.ne.surge)then
            srg = srg+1
            qflow(0,2) = qspply(srg)
c
c            ---- (WEPP Equation 12.10, solved for A)
            aflow(0,2) = (qspply(srg)/aqcnst)**(1.0/aqexp)
            do 70 j = 1, xsteps-1
              inoptm(j) = tend(srg-1)-tadvan(j)
              tadvan(j) = tstart(srg)
   70       continue
          else
c            -- XXX
            goto 170
          endif
        endif
        npriod = 5
        deltat = (tend(srg)-tstart(srg))/float(npriod)
        srgflg = 0
      endif
c
c     Proceed with kinematic wave for 'npriod' periods
c
  100 continue
      do 130 ii = 1, npriod
c
c       Shift aflow and infltr arrays
c
        do 110 j = 0, xsteps
          aflow(j,1) = aflow(j,2)
          qflow(j,1) = qflow(j,2)
          infltr(j,1) = infltr(j,2)
  110   continue
c
c       Calculate time increment for contining phase
c
        dtime = float(ii)*deltat
c
c       Calculate infiltration at upper end of OFE
c
c        ---- (WEPP Equation 12.4)
        infltr(0,2) = kostk*(inoptm(0)+dtime+tadvan(xsteps)-tadvan(0))**
     1                kosta+kostf*(inoptm(0)+dtime+tadvan(xsteps)-
     2                tadvan(0))
c
c       Do loop to calculate aflow and infltr for rectangular cells
c
        do 120 j = 1, xsteps
c
c          ---- (WEPP Equation 12.4)
          infltr(j,2) = kostk*(inoptm(j)+dtime+tadvan(xsteps)-
     1                  tadvan(j))**kosta+kostf*(inoptm(j)+dtime+
     2                  tadvan(xsteps)-tadvan(j))
c
c          ---- (WEPP Equation 12.13)
          ircon2 = (tiavq/aqcnst*(qflow(j,1)-qflow(j-1,1))-(1.0-
     1             tiavq)/aqcnst*qflow(j-1,2)+(spava*(aflow(j-1,2)-
     2             aflow(j-1,1))-(1.0-spava)*aflow(j,1)+spavz*
     3             (infltr(j-1,2)-infltr(j-1,1))+(1.0-spavz)*
     4             (infltr(j,2)-infltr(j,1)))*(xpostn(j)-xpostn(j-1))/
     5             aqcnst/deltat)/(1.0-tiavq)
c
c         If next statement is true then flow area is very small.  If
c         statement is false then calculate flow area and continue
c
          if(ircon2.ge.0.0)then
            aflow(j,2) = 0.00005
            qflow(j,2) = aqcnst*aflow(j,2)**aqexp
          else
            aest = aflow(j-1,2)-aflow(j-1,1)+aflow(j,1)
            if(aest.le.0.0)aest = aflow(j,1)
c
c            ---- (WEPP Equation 12.12)
            ircon1 = (1.0-spava)*(xpostn(j)-xpostn(j-1))/(1.0-tiavq)/
     1               aqcnst/deltat
c
c           NOTE:  The call to IRFLOW from NEWRAP includes the expected
c                  argument.  The argument is not required here.
c
            call newrap(aest,atoler,irflow,aflow(j,2))
            qflow(j,2) = aqcnst*aflow(j,2)**aqexp
          endif
  120   continue
  130 continue
      ii = xsteps
c
c     If next statement is true then increment srg and continue
c     continuing phase.  If statement is false then go to depletion
c     phase.
c
c      *** P0 IF ***
      if(tstart(srg+1)-tend(srg).le.1.0 .and. srg.ne.surge)then
c
c       Calculate infiltrated volume for the current OFE
c
        infvlm(iplane) = 0.0
        do 150 j = 1, xsteps
          infvlm(iplane) = infvlm(iplane)+(spavz*infltr(j-1,2)+(1.0-
     1                     spavz)*infltr(j,2))*(xpostn(j)-xpostn(j-1))
  150   continue
c
c       Calculate OFE's inflow volume
c
        irapld(iplane) = 0.0
        do 160 j = 1, srg-1
          irapld(iplane) = irapld(iplane)+qspply(j)*(tend(j)-
     1                     tstart(j))
  160   continue
c
c       Calculate runoff volume for current surge on OFE iplane
c
        runosg = irapld(iplane)-infvlm(iplane)-runoff(iplane)
c
c       If next statement is true then calculate runoff information to
c       be used as input for next OFE and/or calculate variables
c       needed for erosion calculations
c
        if(runosg.gt.0.0)then
          if(iplane.lt.nplane)then
            nsurge = nsurge+1
            ntstrt(nsurge) = tadvan(xsteps)
            ntend(nsurge) = tend(srg)
            nqsppl(nsurge) = runosg/(ntend(nsurge)-ntstrt(nsurge))
          endif
          runoff(iplane) = runoff(iplane)+runosg
          peakro(iplane) = max(peakro(iplane),runosg/(tend(srg)-
     1                     tadvan(xsteps)))
          runosg = 0.0
        endif
        srg = srg+1
        qflow(0,2) = qspply(srg)
c
c        ---- (WEPP Equation 12.10, solved for A)
        aflow(0,2) = (qspply(srg)/aqcnst)**(1.0/aqexp)
        do 140 j = 0, xsteps
          inoptm(j) = tend(srg-1)-tadvan(j)
          tadvan(j) = tstart(srg)
  140   continue
        npriod = 5
        deltat = (tend(srg)-tstart(srg))/float(npriod)
        srgflg = 0
        goto 100
c
c      *** P0 ENDIF ***
      endif
c
c     -----  DEPLETION and RECESSION PHASES  -----
c
  170 if(ii.ge.2)then
        call furrec(ii)
      else
        inoptm(0) = inoptm(0)+tend(srg)-tstart(srg)
      endif
      if(srg.lt.surge)retflg = 4
      goto 200
c
c     Code to handle low supply rates and depletion level scheduling
c
  180 continue
c
c     The supply rate is too low and the model will continue
c     without irrigation.
c
      write (6,2000)
      noirr = 0
      retflg = 1
      do 190 iplane = irofe, nplane
        norun(iplane) = 0
  190 continue
c
  200 return
      end
