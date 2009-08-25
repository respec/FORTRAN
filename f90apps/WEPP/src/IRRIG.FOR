      subroutine   irrig(
     i                   iiyear,nowcrp,pkrmax,runmax)
c
c     + + + PURPOSE + + +
c     This subprogram determines if irrigation is available and if so,
c     reads parameters for the type of irrigation available and passes
c     control to the proper subprogram(s).
c
c     Called from CONTIN
c     Author(s): E. R. Kottwitz.
c     Reference in User Guide: Chapter 12
c
c     Changes:
c          1) Restructured code to eliminate embedded GOTO's.
c          2) Introduced TMPVR1 to eliminated some repeated calculations.
c          3) Made changes by Kottwitz, as indicated by ~kottwitz/JUNE.MOD1.
c             (Caused no changes in results from TEST6-10 & TEST18-20.)
c        E. Kottwitz; December, 1994; changes 4 and 5
c          4) Added iiyear to argument list, declaration, and definition.
c             All occurrences of the variable year replaced with iiyear.
c          5) Modified L1 if statement to include imodel.  If user specifies
c             single event run with irrigation, the model assumes irrigation
c             is to occur instead of checking dates.  This eliminates the
c             need to make the dates of the climate and irrigation files
c             coincide.
c
c     Version: This module recoded from WEPP Version 93.06.
c     Date recoded: 06/06/93 - 06/17/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxsrg.inc'
      include 'pmxtil.inc'
      include 'pmxtim.inc'
      include 'pmxtls.inc'
      include 'pxstep.inc'
      include 'pmxcrp.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer   iiyear,nowcrp(mxplan)
      real      pkrmax,runmax
c
c     + + + ARGUMENT DEFINITIONS + + +
c     iiyear - simulation year (iiyear = 1 for the first year of the
c              simulation)
c     nowcrp - number of current crop
c     pkrmax - maximum of the peak runoff rates of the overland flow
c              elements making up the equivalent plane (m/s)
c     runmax - maximum of the runoff values of the overland flow
c              elements making up the equivalent plane (m)
c
c     + + + COMMON BLOCKS + + +
      include 'cavloss.inc'
c       read: ioutas,ioutpt
      include 'cdat.inc'
c       read 'isfver.inc
      include 'cdata1.inc'
c       modify: tr, r, rr
      include 'cdata3.inc'
c       modify: nf
      include 'cdiss11.inc'
c       read: dur
c
      include 'cdiss3.inc'
c       read: ip
c
      include 'cdist2.inc'
c       read: slplen(mxplan)
c
      include 'chydrol.inc'
c       read: rain(mxplan), effdrn(mxplan), stmdur
c
      include 'cirdepl.inc'
c       read: deplev(mxplan),iramt
c     modify: depsev,irbeg(mxplan),irend(mxplan),yrbeg(mxplan),
c             yrend(mxplan)
c
      include 'cirfixd.inc'
c     modify: irday(mxplan),iryr(mxplan)
c
      include 'cirfurr.inc'
c       read: filrat(mxplan)
c     modify: depsrg(mxplan),endpln(mxplan),florat(mxplan),
c             qspply(mxsrg),splyvm,surges(mxplan),tend(mxsrg),
c             timest(mxplan),tstart(mxsrg)
c      write: endofe,infvlm(mxplan),irapld(mxplan),surge,timtot
c
      include 'cirriga.inc'
c     modify: irdept(mxplan),irofe,irschd(mxplan),irsyst,noirr
c      write: irfrac
c
      include 'cirspri.inc'
c       read: aprati(mxplan),nozzle(mxplan)
c     modify: irdur,irint(mxplan),irrate(mxplan)
c      write: noirr2(mxplan)
c
      include 'cparame.inc'
c       read: ks(mxplan)
c
      include 'cstmflg.inc'
c       read: norain(mxplan)
c
      include 'cstruc.inc'
c       read: nplane
c     modify: iplane
c
      include 'csumirr.inc'
c     modify: ncommm,ncommt,ncommy,nirrm,nirrt,nirry,tirrm,
c             tirrt,tirry
c
      include 'csumout.inc'
c     modify yirrig
      include 'cupdate.inc'
c       read: mon,sdate
c
cd    Added by S. Dun, April 04,2004
      include 'cirflg.inc'
cd    End adding
c
c     + + + LOCAL VARIABLES + + +
      integer   i,k,irtemp,ofeflg
      real      dpsvmx,liramt,minks,pkint,rwdth,totlen
      real      tmpvr1
c
c     + + + LOCAL DEFINITIONS + + +
c     i      - do loop counter
c     irtemp - variable used to determine whether all irrigations have
c              been completed
c     ofeflg - overland flow element flag which is compared to current
c              overland flow element being processed
c     dpsvmx - variable for use with non-uniform hydrology, the maximum
c              of the values calculated for the variable DEPSEV for the
c              current day of simulation (m/m)
c     liramt - previously determined irrigation depth
c     minks  - minimum ks(mxplan) for all planes
c     pkint  - peak intensity of a rainfall event, used to determine if
c              furrow irrigation is allowed (m/s)
c     rwdth  - row width of overland flow element into which water is
c              introduced (m)
c     totlen - total length of profile (m)
c
c     + + + SAVES + + +
      save      liramt
c
c     + + + SUBROUTINES CALLED + + +
c     deplet
c     furrow
c
c     + + + DATA INITIALIZATIONS + + +
      data      liramt/0.0/
c
c     + + + OUTPUT FORMATS + + +
 2010 format(
     1/2x,'Lines of data in the fixed date irrigation scheduling file'
     2/2x,'are in the incorrect order.  Irrigations will not occur as'
     3/2x,'planned.'/)
 2020 format(
     1/2x,'Lines of data in the depletion level irrigation scheduling'
     2/2x,'file are in the incorrect order.  Irrigations will not occur'
     3/2x,'as planned.'/)
c
c     + + + END SPECIFICATIONS + + +
c
c   **********************************************************************
c   * (NOTE from E. R. Kottwitz explaining variable IRSCHD:)             *
c   *                                                                    *
c   *  The variable IRSCHD(MXPLAN) keeps track of the irrigation sched-  *
c   *  duling option for each OFE.  The model expects IRSCHD(MXPLAN)     *
c   *  values to equal 0, 1, 2, or 3.  A value of 0 should be read from  *
c   *  the management file if IRSYST=0.  If IRSYST is not equal to 0     *
c   *  then a value of 1, 2, or 3 should be read from the management     *
c   *  file.                                                             *
c   *                                                                    *
c   *     1 = depletion level scheduling is used.                        *
c   *     2 = fixed date scheduling is used.                             *
c   *     3 = both depletion level and fixed date scheduling are used    *
c   *         (combination scheduling).                                  *
c   *                                                                    *
c   *  If IRSCHD(n)=1 ...                                                *
c   *    and the model finds no more depletion level periods for OFE n,  *
c   *    then IRSCHD(n) is set to 0 (no irrigation for OFE n).           *
c   *                                                                    *
c   *  If IRSCHD(n)=2 ...                                                *
c   *    and the model finds no more fixed date irrigations for OFE n,   *
c   *    IRSCHD(n) is set to 0 (no irrigation for OFE n).                *
c   *                                                                    *
c   *  If IRSCHD(n)=3 ...                                                *
c   *    and the model finds no more depletion level periods for OFE n,  *
c   *    IRSCHD(n) is set equal to 2 (fixed date scheduling for OFE n).  *
c   *                                                                    *
c   *    and the model finds that no more fixed date irrigations exist   *
c   *    for OFE n, then IRSCHD(n) is set to 1 (depletion level          *
c   *    scheduling for OFE n).                                          *
c   **********************************************************************
c
c     Initialize/reinititialize variables
c
      noirr = 0
      irofe = 0
      do 10  iplane = 1, nplane
        irdept(iplane) = 0.0
        irint(iplane) = 0.0
cd    Added by S. Dun, 03/03/2004
        iraplo(iplane) = 0.0
cd    End adding  
   10 continue
      irdur = 0.0
c
c      --- Initialize sprinkler irrigation variables
      if(irsyst.eq.1)then
        do 20  iplane = 1, nplane
          noirr2(iplane) = 0
   20   continue
c
c      --- Initialize furrow irrigation variables
      else
        totlen = 0.0
        splyvm = 0.0
        minks = 1.0
        do 30  iplane = 1 , nplane
          infvlm(iplane) = 0.0
          irapld(iplane) = 0.0
          totlen = totlen+slplen(iplane)
          minks = min(minks,ks(iplane))
   30   continue
      endif
c
c     This section checks whether all irrigations have been completed
c
      irtemp = 0
      do 40  iplane = 1, nplane
        irtemp = max(irtemp,irschd(iplane))
   40 continue
c
c      *** X0 IF ***
c     If the following is true then all irrigations were completed
c     earlier but the model needed variable reinitializations above
c
      if(irtemp.eq.0)then
        irsyst = 0
c
c      *** X0 ELSE ***
      else
c
c
c     ----- FIXED DATE SCHEDULING -----
c     For given scheduling routine determine if irrigation occurs now
c
c        ---- Loop through the planes
        do 50 iplane = 1, nplane        
c          *** L0 IF ***
          if(irschd(iplane).ge.2)then
c
c            *** L1 IF ***
c            ---- If irrigation occurs today or single event simulation
            if((sdate.eq.irday(iplane) .and. iiyear.eq.iryr(iplane))
     1          .or. imodel.eq.2)then
c
c              *** L2 IF ***
c              ---- If solid-set, side-roll or handmove irrig. system is
c                   available....
              if(irsyst.eq.1)then
                noirr = 2
                noirr2(iplane) = 1
c
c               fixed date sprinkler file less than currently available
c               version
                if(ifsver.lt.94.21)then
                  read(14,*)irint(iplane),irdept(iplane)
                  nozzle(iplane)=1.0
                  write(31,1000)irfsch
                else
                  read(14,*)irint(iplane),irdept(iplane),nozzle(iplane)
                endif
                read(14,*)ofeflg,irday(iplane),iryr(iplane)
                if(ofeflg.ne.iplane)write(6,2010)
c
c             This next line of code sets irofe equal to the first OFE
c             irrigated.  Each irrigated OFE might have different
c             operating parameters.  The calculation of r5 (in SR
c             CONTIN) can not be done properly until r5 is modified to
c             include the array dimension mxplan.  ERK 6-1-93.
c
                if(irofe.eq.0)irofe = iplane
c
c              *** L2 ELSE ***
c              ---- Furrow irrigation system is available
              else
                irofe = iplane
                noirr = 2
                read(14,*)surges(iplane)
c
                do 60  i = 1, surges(iplane)
                  read(14,*)qspply(i),tstart(i),tend(i)
                  splyvm = splyvm+qspply(i)*(tend(i)-tstart(i))
                  if(ivers.eq.3) then
                    if (i.eq.1) then
                      k = 1
                      tr(k) = tstart(i)
                      r(k) = qspply(i)
                      rr(k) = 0
                    else
                      k = k + 1
                      if(tstart(i).eq.tend(i-1)) then
                        tr(k) = tstart(i)
                        r(k) = qspply(i)
                        rr(k) = rr(k-1) + r(k-1)*(tr(k)-tr(k-1))
                      else
                        tr(k) = tend(i-1)
                        r(k) = 0
                        rr(k) = rr(k-1) + r(k-1)*(tr(k)-tr(k-1))
                        k = k + 1
                        tr(k) = tstart(i)
                        r(k) = qspply(i)
                        rr(k) = rr(k-1) + r(k-1)*(tr(k)-tr(k-1))
                      end if
                    end if
                  end if
   60           continue
                iraplo(iplane) = splyvm
                irdfg = 0
                if(ivers.eq.3) then
                  k = k + 1
                  tr(k) = tend(surges(iplane))
                  r(k) = 0
                  rr(k) = rr(k-1) + r(k-1)*(tr(k)-tr(k-1))
                  nf = k
                end if
c
                read(14,*)ofeflg,irday(iplane),iryr(iplane)
                if(ofeflg .ne. iplane)write(6,2010)
c
c                ---- Skip irrigation if rainfall occurs and the peak
c                     intensity is greater than the saturated hydraulic
c                     conductivity or rainfall exceeded 1 mm.
c
                if(rain(iplane).le.0.0001)then
                  pkint = 0.0
                else
                  pkint = rain(iplane)/stmdur*ip
                endif
c
                if((norain(iplane).eq.1 .and. pkint.gt.minks) .or.
     1             (rain(iplane).gt.0.001))then
                  irofe = 0
                  noirr = 0
                  iraplo(iplane) = 0.0
                endif
c
c              *** L2 ENDIF ***
              endif
c
c           Check for no more fixed date irrigations
c
              if(irday(iplane) .eq. 0)then
                if(irschd(iplane) .eq. 3)then
                  irschd(iplane) = 1
                else
                  irschd(iplane) = 0
                endif
              endif
c
c            *** L1 ENDIF ***
            endif
c
c          *** L0 ENDIF ***
          endif
   50   continue
c
c       ----- DEPLETION LEVEL SCHEDULING -----
c
        dpsvmx = 0.0
        iramt = 0.0
        irdfg =0
c
c        ---- Loop through the planes
        do 70 iplane = 1, nplane
c
c          *** M0 IF ***
          if(irschd(iplane).ne.2)then
c
c            *** M1 IF ***
            if((yrbeg(iplane).le.iiyear) .and. (irbeg(iplane).le.sdate)
     2        .and. (yrend(iplane).ge.iiyear)
     3        .and. (irend(iplane).ge.sdate))then
c
c              *** M2 IF ***
c             If a fixed date irrigation does not occur then calculate
c             depletion level
c
              if(noirr.eq.0)then
c
c                *** M3 IF ***
c               Call subprogram DEPLET if (1) a sprinkle irrigation
c               system exists or (2) a furrow irrigation system exists
c               and no rain occurs for the current simulation day.
c
                if(irsyst.eq.1 .or.
     1            (irsyst.eq.2 .and. norain(iplane).eq.0))then
                  liramt = iramt
                  call deplet
c
c                  *** M4 IF ***
c                 If the following is true then an irrigation will occur
c
                  if(depsev.gt.0.0)then
                    dpsvmx = max(dpsvmx,depsev)
c
c                    *** M5 IF ***
c                   If the current plane has the greatest value for
c                   depsev then this plane will be irrigated (unless a
c                   later plane has a still greater value for depsev)
c
                    if(dpsvmx.eq.depsev)then
                      irofe = iplane
                      noirr = 1
c
c                     If the following is true then calculate solid-set,
c                     side-roll, or hand-move irrigaion system parameters.
c                     Otherwise calculate furrow irrigation system
c                     parameters
c
                      if(irsyst.eq.1)then
                        irdept(iplane) = iramt
                        irint(iplane) = irrate(iplane)
                        noirr2(iplane) = 1
                      else
                        tstart(1) = 0.0
                        tend(1) = 86400.0
                        timtot = timest(iplane)
                        endofe = endpln(iplane)
                        surge = depsrg(iplane)
                        qspply(1) = florat(iplane)
                      endif
c
c                    *** M5 ENDIF ***
                    endif
c
c                  *** M4 ELSE ***
                  else
                    iramt = liramt
c
c                  *** M4 ENDIF ***
                  endif
c
c                *** M3 ENDIF ***
                endif
c
c              *** M2 ENDIF ***
              endif
c
c            *** M1 ENDIF ***
            endif
c
c          *** M0 ENDIF ***
          endif
   70   continue
c
c       ***********************************************************
c        * If irrigation occurs, determine irrigation parameters *
c       ***********************************************************
c
c        *** P0 IF ***
c        ---- If a depletion level or scheduled irrigation occurs....
        if(noirr.ne.0)then
c
c          *** P1 IF ***
c          ---- If a furrow irrigation system is used....
          if(irsyst.eq.2)then
            irfrac = 1.0
            call furrow(nowcrp,pkrmax,runmax,rwdth)
            irdur = effdrn(irofe)
c
c          *** P1 ELSE ***
c          ---- If a furrow irrigation system is NOT used....
          else
c
c            ---- Determine irrigation parameters specifically for
c                 sprinkler irrigation systems
            irdur = irdept(irofe)/irint(irofe)
            if(irdur.gt.86400.0)then
              irdur = 86400.0
              irdept(irofe) = irdur*irint(irofe)
            endif
c
            if(rain(iplane)+irdept(irofe).gt.0.5)then
              irdept(irofe) = 0.5 - rain(iplane)
              irdur = irdept(irofe)/irint(irofe)
            endif
c
c          *** P1 ENDIF ***
          endif
c
c         This section is common to all irrigation systems but used only
c         if an irrigation occurs
c
c         If the following is true then the supply rate is too low and
c         the model will continue without irrigation.
c         Note: NOIRR would have been changed in SR FURADV
c
c          *** Q1 IF ***
          if(noirr.ne.0)then
            nirrt = nirrt+1
            nirry = nirry+1
            nirrm = nirrm+1
c
            if(irsyst.eq.1)then
              tmpvr1 = irdept(irofe)*1000.0
              tirrt = tirrt+tmpvr1
              tirry = tirry+tmpvr1
              tirrm = tirrm+tmpvr1
              if(ioutpt.eq.3.and.ioutas.eq.1)yirig=yirig+tmpvr1
            else
              tmpvr1 = splyvm*1000.0/(totlen*rwdth)
              tirrt = tirrt+tmpvr1
              tirry = tirry+tmpvr1
              tirrm = tirrm+tmpvr1
              if(ioutpt.eq.3.and.ioutas.eq.1)yirig=yirig+tmpvr1
            endif
c
c XXX       Changed from using NORAIN to using PRCP  dcf 6/3/94
c           if(norain.eq.1)then
            if(prcp.gt.0.0001)then
              ncommt = ncommt+1
              ncommy = ncommy+1
              ncommm = ncommm+1
            endif
c
c          *** Q1 ENDIF ***
          endif
c
c        *** P0 ENDIF ***
        endif
c
c      *** X0 ENDIF ***
      endif
c
c     Read in new operating parameters for depletion level scheduling
c
      do 80 iplane = 1, nplane
c
c        *** N1 IF ***
        if(irschd(iplane).ne.2)then
c
c          *** N2 IF ***
c         If current day is the last day of a depletion level
c         scheduling period then read parameters for next period
c
          if(yrend(iplane).eq.iiyear .and. irend(iplane).eq.sdate)then
c
c            *** N3 IF ***
            if(irsyst.eq.1)then
c
c             Solid-set, side-roll, or hand-move irrigation system
c
              read(15,*)ofeflg,irrate(iplane),aprati(iplane),
     1                  deplev(iplane),nozzle(iplane),irbeg(iplane),
     2                  yrbeg(iplane),irend(iplane),yrend(iplane)
              if(ofeflg.ne.iplane)write(6,2020)
c
c            *** N3 ELSE ***
c            ---- Furrow irrigation system
            else
c
              read(15,*)ofeflg,endpln(iplane),florat(iplane),
     1                  timest(iplane),depsrg(iplane),filrat(iplane),
     2                  deplev(iplane),irbeg(iplane),yrbeg(iplane),
     3                  irend(iplane),yrend(iplane)
              if(depsrg(iplane).gt.6)depsrg(iplane) = 6
              if(depsrg(iplane).eq.3)depsrg(iplane) = 4
c
c            *** N3 ENDIF ***
            endif
c
c           Check for no more depletion level scheduling periods
c
            if(irbeg(iplane).eq.0)then
              if(irschd(iplane).eq.3)then
                irschd(iplane) = 2
              else
                irschd(iplane) = 0
              endif
            endif
c
c          *** N2 ENDIF ***
          endif
c
c        *** N1 IF ***
        endif
   80 continue
c
      return
 1000 format(/
     1,' *** WARNING ***',/,
     1' FIXED DATE SPRINKLER IRRIGATION ',
     1'FILE PRE ',f7.3,', nozzle factor set to 1.0',/,
     1' *** WARNING ***',/)
      end
