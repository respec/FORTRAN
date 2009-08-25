      subroutine idat(pwmelt,ibrkpt,prain)
c
c     + + + PURPOSE + + +
c
c     If there is rainfall for the current day, IDAT takes the rainfall
c     statistics read in from the climate file and passes them to the
c     disaggregation routines.  It then inserts a time step into the
c     rainfall array for the infiltration computations.
c
c     Called from IRS
c     Author(s): Stone
c     Reference in User Guide:
c
c     Changes:
c          1) Order of parameters in call to DISAG reversed to conform
c             to Coding Convention.
c          2) Common block STRUCT not used.  It was de-referenced.
c          3) SAVE statement, which saves ALL local variables, removed.
c          4) Removed local variable DDT.  (Bad stuff anyway...)
c          5) Consolidated some code in and below the "do 30" loop.
c          6) Common block PASS not needed. De-referenced.
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 05/01/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer ibrkpt
      real pwmelt, prain, rm
c
c     + + + ARGUMENT DEFINITIONS + + +
c     smflg  - snow melt flag
c     pwmelt  - amount of snow melt water
c     ibrkpt - FLAG FOR BREAKPOINT RAINFALL INPUT
c     prain   - DEPTH OF RAINFALL
c     rm    - total water income to the OFE
c
c     + + + PARAMETERS + + +
      include 'pmxtim.inc'
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxtls.inc'
      include 'pmxnsl.inc'
      include 'pmxtil.inc'
      include 'pmxcrp.inc'
      include 'pntype.inc'
c
      include 'pmxelm.inc'
      include 'pmxsrg.inc'
      include 'pxstep.inc'
c
c     + + + COMMON BLOCKS + + +
c
      include 'cstruc.inc'
c       read: iplane
c
      include 'cdata1.inc'
c     modify: tr(mxtime),r(mxtime),rr(mxtime)
c
      include 'cdata3.inc'
c     modify: nf,nr,dt
c
      include 'cdata4.inc'
c     modify: trf(mxtime),rf(mxtime)
c
      include 'cdiss1.inc'
c       read: ninten(mxplan),dur,intsty(20),timem(20)
c
      include 'ccover.inc'
c       read: cancov(mxplan),lanuse(mxplan)
c
      include 'cwater.inc'
c     modify: plaint(mxplan),resint(mxplan)
c
      include 'ccrpout.inc'
c       read: rescov(mxplan)
c
      include 'crout.inc'
c       read: tlive(mxplan)
c
      include 'ccrpvr1.inc'
c       read: rmogt(mxres,mxplan)
c
      include 'ccrpvr2.inc'
c       read: vdmt(mxplan)
c
      include 'cke.inc'
c     modify: rkine
c
      include 'cwint.inc'
c
      include 'chydrol.inc'
c        read:  prain(mxplan)
c
      include 'cirriga.inc'
c        read: irdept
c
      include 'cirfurr.inc'
c        read: irapld(mxplan)
c
c
c     + + + LOCAL VARIABLES + + +
      real dtc, xx, test, tol, deadms, livems, totint
      real vtime, vint, vike, dtime, temp, tempi, xy ,wmlavg      
c
      integer i, j, loopfg, ijk, jj
c
c     + + + LOCAL DEFINITIONS + + +
c     dtc    - time step inserted in rainfall for infiltration computations
c     xx     - summation used to build RR(I)
c     test   - used in time step insertion
c     tol    - used in time step insertion
c     i      - index for disaggregated rainfall
c     j      - index for rainfall with time step inserted
c     loopfg - flag.  2=return to top of loop without resetting counter
c     livems - residue mass due to live plant = tlive(iplane) * 10000  (kg/ha)
c     deadms - residue mass due to dead plant = rmogt(nowres,iplane) * 10000
c             (kg/ha)
c     vtime, vint, and vike -temporary variables holding incremental
c                           time, intensity, and KE
c
c     + + + SUBROUTINES CALLED + + +
c     disag
c
c     + + + OUTPUT FORMATS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c                                                                 *
c******************************************************************
c
c     CALL DISAGGREGATION MODEL
c
      if(pwmelt .gt. 0.0) then
c dcf  Added trap to prevent divide by zero.  New code
c dcf  from WSU in IRS is calling IDAT with a positive
c dcf  value for pwmelt and a zero value for wmelt(iplane)
c dcf  2-25-04
        if(wmelt(iplane).gt.0.00001)then
          wmlavg       = pwmelt/wmelt(iplane)
        else
c dcf     Not sure what I should set wmlavg to, if wmelt(iplane)
c dcf     is zero???   dcf  2-25-04
          wmlavg = 1
        endif
      else 
        wmlavg       = 1
      endif
      if (ibrkpt .eq. 0 .or. (prain .le. 0.0 .and. pwmelt .gt. 0.0))
     1     call disag(wmlavg      )
c
c     Calculate rainfall kinetic energy using equation from Van Doren
c     and Allmaras where KE is approximated by:
c
c     KE=(3.812+0.874 log10 RI)*time*RI
c
c     where KE is in J/cm2, Time is the duration(hr), and RI is the rainfall
c     intensity (m/hr).  Note: This equation is also given by Wischmeier for
c     english units.  To gain accuracy we apply to each time step.  I have
c     also developed an analytical solution to calculate KE for the WEPP
c     double exponential storm, however, I thought it may be more reasonable
c     to calculate KE based on the disaggregated storm.  Risse 11/4/93.
c
      rkine = 0.0
      do 10 i = 1, ninten(iplane) - 1
        vtime = (timem(i+1)-timem(i)) / 3600
        vint = intsty(i) * 3600
c
c       If intensity is greater than 3 in/hr energy does not increase as
c       maximum drop size has been attained.
        if (vint.ge.0.0765) vint = 0.0765
        if ((vtime.gt.0.).and.(vint.gt.0.)) then
          vike = (3.812+0.3796*log(vint)) * vtime * vint
        else
          vike = 0
        end if
c       convert KE to J/m2
        vike = vike * 10000
        if (vike.gt.0.) rkine = vike + rkine
   10 continue
c
c
      nf = 0
      nr = ninten(iplane)
      do 20 i = 1, nr
        trf(i) = timem(i)
        if ((ibrkpt.eq.1).and.(prain.gt.0.0)) then
c
c         ADD SNOWMELT RATE TO BREAKPOINT RAINFALL INTENSITY
cc note the next line needs to be fixed, Reza 12/9/93
          rf(i) = intsty(i) + pwmelt / dur
        else
          rf(i) = intsty(i)
        end if
   20 continue
c     calculate time step for infiltration
c
      if (dur.le.0.0) then
        write (6,1000)
        stop
      else
        if (dur.le.1800.0) then
          dt = 60.0
        else if (dur.le.3600.0) then
          dt = 120.0
        else if (dur.le.7200.0) then
          dt = 180.0
        else if (dur.le.21600.0) then
          dt = 300.0
        else
          dt = 600.0
        end if
      end if
c
      dtc = dt
c
c
      xx = 0.0
      i = 2
      tr(1) = trf(1)
      r(1) = rf(1)
c
      do 40 j = 2, nr
c
c       Insert infiltration time step into disaggregated rainfall data
c
   30   continue
        loopfg = 0
c
c       Test whether disaggregated time step is close to inserted
c       time step.  If it is, use it instead.
c
        test = abs(dtc-trf(j))
        tol = min(54.0,0.9*dt)
c
        if (test.gt.tol) then
          if (dtc.lt.trf(j)) then
            r(i) = rf(j-1)
            tr(i) = dtc
            dtc = dtc + dt
            loopfg = 2
          else
            tr(i) = trf(j)
            r(i) = rf(j)
          end if
        else
          tr(i) = trf(j)
          r(i) = rf(j)
          dtc = dtc + dt
        end if
c
        xx = xx + r(i-1) * (tr(i)-tr(i-1))
        rr(i) = xx
        i = i + 1
        if (loopfg.eq.2) go to 30
   40 continue
      nf = i - 1
c
c     new changes for rainfall interception by crop residue and
c     leaves, reza 7/26/93
c
c     Sum up the residue mass on the ground in the three residue pools
c
      deadms = 0.0
      do 50 ijk = 1, 3
        deadms = deadms + rmogt(ijk,iplane) * 10000.0
   50 continue
c
c     XXX  For now use VDMT for CROPLAND and TLIVE for RANGELAND.
c     Jeff Arnold, Mark Weltz, and Diane Stott need to come to
c     agreement on the definition and use of the variables:
c     TLIVE, VDMT, RMOGT, RMAGT, CANCOV, CANHGT and assure that
c     meaning for all land uses is similar.   dcf  7/30/93
c
      if (lanuse(iplane).eq.1) then
        livems = tlive(iplane) * 10000.0
      else
        livems = tlive(iplane) * 10000.0
      end if
c
c     Set upper limits on dead mass and live mass values.  This
c     prevents errors with use of the second order equations below
c
      if (deadms.gt.8000.) deadms = 8000.
      if (livems.gt.8000.) livems = 8000.
c
      plaint(iplane) = cancov(iplane) * ((0.000627*livems-(3.73349*
     1    1.0e-8*(livems**2)))/1000.0)
      resint(iplane) = rescov(iplane) * ((0.000627*deadms-(3.73349*
     1    1.0e-8*(deadms**2)))/1000.0)
c
cx    added by Arthor Xu, Modified by S. Dun May 21, 2003 
      rm = rain(iplane) + wmelt(iplane) + irdept(iplane)
     1      + pintlv(iplane)
cd    Modified by S. Dun 03/01/2004, 
cd    Furrow irrigation is not involved in plant and residue interception.
cd     1     + irapld(iplane) + pintlv(iplane)
cx       
      if (rm.le.plaint(iplane)) then
        plaint(iplane) = rm
        resint(iplane) = 0.0
      else if (rm.le.(plaint(iplane) + resint(iplane))) then
        resint(iplane) = rm - plaint(iplane)    
      end if
c
c     Carry over expected interceptions on residues and plants
c     initially lir(i) = lip (i) = 0 for all i = 1, ..., ninten
c
cd    Modified by S.Dun Nov 11,2003
      totint = plaint(iplane)-pintlv(iplane) + resint(iplane)
cd      totint = plaint(iplane) + resint(iplane)
cd    End modifying.
      xy = 0.0
      do 60 jj = 2, nf
        if (totint.gt.0.0) then
          dtime = tr(jj) - tr(jj-1)
          temp = r(jj-1) * dtime
          if (temp.ge.totint) then
            tempi = temp - totint
            r(jj-1) = tempi / dtime
            totint = 0.0
          else
            totint = totint - temp
            temp = 0.0
            r(jj-1) = 0.0
          end if
        end if
        xy = xy + r(jj-1) * (tr(jj)-tr(jj-1))
        rr(jj) = xy
   60 continue
c
      return
 1000 format (' *** Error in Climate file'/
     1    ' *** Duration less than or equal to zero'/)
      end
