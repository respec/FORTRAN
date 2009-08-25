      subroutine   furrun(
     i                    ii,rwdth,
     m                    itemp,pkrmax,runmax,
     o                    retflg)
c
c     + + + PURPOSE + + +
c     This subprogram generates runoff information for the current OFE
c     and is part of the furrow irrigation component.
c
c     Called from FURROW
c     Author(s): E. R. Kottwitz
c     Reference in User Guide: Chapter 12
c
c     Changes:
c          1) Changed dummy parameter "I" to more unique "II".
c          2) Removed an embedded GOTO (that was in effect an embedded
c             RETURN).
c
c     Version: This module recoded from WEPP Version 93.06.
c     Date recoded: 07/06/93.
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
      integer   ii,itemp,retflg
      real      pkrmax,runmax,rwdth
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ii     - primary kinematic wave calculation counter
c     itemp  - maximum advance increment counter
c     retflg - flag indicating model status when returning from
c              subprogram FURADV
c     pkrmax - maximum of the peak runoff rates of the overland flow
c              elements making up the equivalent plane (m/s)
c     runmax - maximum of the runoff values of the overland flow
c              elements making up the equivalent plane (m)
c     rwdth  - row width of overland flow element into which water is
c              introduced (m)
c
c     + + + COMMON BLOCKS + + +
      include 'cdist2.inc'
c       read: efflen(mxplan)
c
      include 'cefflen.inc'
c      write: efflen(mxplan)
c
      include 'chydrol.inc'
c     modify: runoff(mxplan),peakro(mxplan),effdrn(mxplan)
c
      include 'cirfurr.inc'
c       read: irqin,qspply(mxplan),surge,tend(mxsrg),
c             tstart(mxsrg),spavz,xpostn(0:xsteps),trec(0:xsteps)
c     modify: infltr(0:xsteps,2),infvlm(mxplan),irapld(mxplan),nsurge,
c             ntend(mxsrg),ntstrt(mxsrg),runosg,srg,tadvan(0:xsteps)
c      write: aflow(0:xsteps,2),nqsppl(mxsrg),srgflg,timflg
c
      include 'cirriga.inc'
c       read: irofe
c
      include 'cstruc.inc'
c       read: iplane,nplane
c
      include 'cirflg.inc'
      include 'cslpopt.inc'
      include 'cstmflg.inc'
      include 'cupsfl.inc'
c
c     + + + LOCAL VARIABLES + + +
      integer   j,kplane,lipl
c
c     + + + LOCAL DEFINITIONS + + +
c     j      - secondary kinematic wave calculation counter
c     kplane - highest numbered overland flow element having runoff
c     lipl   - local overland flow element counter
c
c     + + + END SPECIFICATIONS + + +
c
c     Calculate OFE's inflow volume.
c
      irapld(iplane) = 0.0
      do 100  j = 1, srg
        irapld(iplane) = irapld(iplane)+qspply(j)*(tend(j)-tstart(j))
  100 continue
c      
      if ((irdfg.eq.1).and.(iuprun(iplane).eq.0)) then
        iraplo(iplane) = irapld(iplane)
      endif
c
c     Maintain record of maximum advance distance.
c
      itemp = max(itemp,ii)
c
c     Calculate infiltrated volume for the current OFE.
c
      if(itemp.lt.xsteps)then
        infvlm(iplane) = irapld(iplane)
      else
        infvlm(iplane) = 0.0
        do 200  j = 1, xsteps
          infvlm(iplane) = infvlm(iplane)+(spavz*infltr(j-1,2)+(1.0-
     1                     spavz)*infltr(j,2))*(xpostn(j)-xpostn(j-1))
  200   continue
        if(infvlm(iplane).gt.irapld(iplane))infvlm(iplane) =
     1                                                  irapld(iplane)
      endif
c
c     Calculate runoff volume for current surge on OFE iplane.
c
      runosg = irapld(iplane)-infvlm(iplane)-runoff(iplane)
c
c     If next statement is true then calculate runoff information to
c     be used as input for next OFE and/or calculate variables needed
c     for erosion calculations
c
      if(runosg.gt.0.0)then
c
        if(iplane.lt.nplane)then
          nsurge = nsurge+1
          ntstrt(nsurge) = tadvan(xsteps)
          ntend(nsurge) = trec(xsteps)
          nqsppl(nsurge) = runosg/(ntend(nsurge)-ntstrt(nsurge))
        endif
c
        runoff(iplane) = runoff(iplane)+runosg
        peakro(iplane) = max(peakro(iplane),runosg/(trec(xsteps)-
     1                   tadvan(xsteps)))
      endif
c
c     If next statement is true then increment surge, reinitialize
c     parameters as necessary and begin new advance phase.
c
c      *** M0 IF ***
      if(srg.lt.surge)then
        srg = srg+1
        srgflg = 0
        timflg = 0
c
        do 300  j = 0, ii
          aflow(j,1) = 0.0
          aflow(j,2) = 0.0
          qflow(j,1) = 0.0
          qflow(j,2) = 0.0
          infltr(j,1) = infltr(j,2)
          infltr(j,2) = 0.0
          tadvan(j) = 0.0
          trec(j) = 0.0
  300   continue
c
        tadvan(0) = tstart(srg)
        retflg = 4
c
c      *** M0 ELSE ***
      else
c
c       If next statement is true then case 3 occurs.  If statement is
c       false then case 4 occurs.  In both cases, calculate variables
c       needed for erosion calculations.
c
c        *** M1 IF ***
        if(runoff(iplane).gt.0.0)then
          runoff(iplane) = runoff(iplane)/efflen(iplane)/rwdth
          peakro(iplane) = peakro(iplane)/efflen(iplane)/rwdth
          infvlm(iplane) = infvlm(iplane)/efflen(iplane)/rwdth
          if(peakro(iplane).lt.3.6e-8)peakro(iplane)=3.6e-8
          effdrn(iplane) = runoff(iplane)/peakro(iplane)
          if(effdrn(iplane).gt.86400.0)effdrn(iplane)=86400.0
          kplane = iplane
          norain(iplane) = 0
c
c        *** M1 ELSE ***
        else
          efflen(iplane) = xpostn(ii)-xpostn(0)
          runoff(iplane) = 0.0
          peakro(iplane) = 0.0
          infvlm(iplane) = infvlm(iplane)/efflen(iplane)/rwdth
          kplane = iplane-1
c
          if(iplane.eq.irofe)then
            effdrn(iplane) = irapld(iplane)/irqin/rwdth
          else
            effdrn(iplane) = runoff(kplane)/peakro(kplane)
          endif
c
c        *** M1 ENDIF ***
        endif
c
c       Convert OFE's inflow volume to volume per unit area.
c
cd    Modified by S.Dun Dec. 01, 2003 to match the water balance part.
cd        irapld(iplane) = irapld(iplane)/slplen(iplane)/rwdth
        irapld(iplane) = irapld(iplane)/efflen(iplane)/rwdth
        iraplo(iplane) = iraplo(iplane)/slplen(iplane)/rwdth
cd    End of modifying
c
c       Skip do loop 400 if kplane < irofe which occurs when case 4
c       occurs on OFE where water is introduced.
c
c        *** N1 IF ***
        if(kplane.ge.irofe)then
c
c         Do loop 400 mimics do loop 50 of SR IRS.
c
          runmax = 0.0
          pkrmax = 0.0
c
          do 400  lipl = irofe, iplane
            effdrn(lipl)=effdrn(kplane)
            peakro(lipl)=runoff(lipl)/effdrn(lipl)
            if(runoff(lipl).gt.runmax)runmax=runoff(lipl)
            if(peakro(lipl).gt.pkrmax)pkrmax=peakro(lipl)
  400     continue
c
c        *** N1 ENDIF ***
        endif
c
c      *** M0 ENDIF ***
      endif
c
      return
      end
