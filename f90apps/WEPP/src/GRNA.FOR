      subroutine grna(aveksm,avedep,nstemp,tf,re,effdrr,durre)               
c
c     + + + PURPOSE + + +
c     Calculates infiltration rates and depths for unsteady rain
c     using the Green and Ampt infiltration equation as modified
c     by Mein and Larson.
c
c     Called from IRS.
c     Author(s): Stone
c     Reference in User Guide:
c
c     Changes:
c         1) Order of parameters change:  3rd parameter goes to last
c            (7th) place, and the 4th parameter goes to 2nd place.
c            Corresponding changes made in IRS.
c         2) Lots of those nasty GOTO's and multiple RETURN's
c            eliminated.
c         3) IX & KX added to "do 20" loop to represent "i-1" & "k-1"
c            respectively.
c         4) Consolidated code in "do 20" loop by combining some IF's.
c            Eliminated local variable INSERT.
c         5) DURRE added in argument list as an output by Stone  5/93
c         6) Computations of ISQINT and DURRE commented out - Stone 5/93
c         7) New call to subroutine REID added - Stone 5/93
c         8) Savabi's changes for surface layer water storage
c            added 10/8/93 - dcf
c         9) Added fix to compute time to ponding for single storm
c            simulations where surface saturates but have a high
c            Keff value for soil.  dcf  4/95
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/23/91 - 8/7/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxelm.inc'
      include 'pmxnsl.inc'
      include 'pmxtim.inc'
      include 'pmxpnd.inc'
      include 'pmxpln.inc'
      include 'pmxhil.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      real aveksm, avedep, tf(mxtime), re(mxtime), effdrr(mxplan)
      real  durre
      integer nstemp, ntemp 
c
c     + + + ARGUMENT DEFINITIONS + + +
c     aveksm - average KS*SM for current OFE
c     avedep - depression storage depth
c     wmelt  - amount of snow melt water
c     nstemp - index for the last time of cumulative rainfall excess
c     ntemp  - index for the last time of cumulative infiltration
c     tf     - incremental times of infiltration
c     re     - rainfall excess rate
c     effdrr - duration of rainfall excess
c     durre  - duration of rainfall excess
c
c     + + + COMMON BLOCKS + + +
      include 'cdata.inc'
c       read: nf,tr(mxtime),r(mxtime),rr(mxtime)
c     modify: f(mxtime),ff(mxtime),recum(mxtime),rcum(mxtime),nm
c
      include 'cdiss1.inc'
c       read: dur
c     modify: sumint,timem(20)
c
      include 'chydrol.inc'
c     modify: runoff(mxplan),remax
c      write: exrain,effint
c
      include 'cparame.inc'
c     modify: cu,tp(mxpond),cp,pt(mxpond),ts,tt
c
      include 'cavepar.inc'
c       read: aveks(mxplan),avesm(mxplan)
c
      include 'cstruc.inc'
c       read: iplane
c
      include 'cwater.inc'
c       read: st(mxnsl,mxplan),ul(mxnsl,mxplan)
c
      include 'cwint.inc'
c
      include 'ccntfg.inc'
      include 'cdist2.inc'
      include 'cefflen.inc'
      include 'cmstart.inc'
      include 'cupsfl.inc'
c
c******************************************************************
c                                                                 *
c   Variables in Common Blocks                                    *
c                                                                 *
c   1. r(i) ..............rainfall rate                           *
c   2. rcum(i) ...........accumulated rainfall depth              *
c   3. f(i) ..............infiltration rate                       *
c   4. ff(i) .............accumulated infiltration depth          *
c   6. recum(i) ..........accumulated rainfall excess depth       *
c   7. cu ................indicator of ponding if no ponding at   *
c                         beginning                               *
c                         cu < 0 - no ponding  cu > 0 - ponding   *
c   8. cp ................indicator of ponding when ponded at     *
c                         beginning                               *
c                         cp < 0 - ponding stops during interval  *
c                         cp > 0 - ponding                        *
c   9. tr(i) .............rainfall times                          *
c  10. tp ...............time of ponding                          *
c  11. ts ...............pseudotime to adjust real time for       *
c                        infiltration                             *
c  12. t ................real time = tr(i)-tp+ts                  *
c  13. pt ...............accumulated rainfall at time of ponding  *
c                                                                 *
c******************************************************************
c
c     + + + LOCAL VARIABLES + + +
      real fsum, xx, dtime, retemp(mxtime), rrate(mxtime), dinf, relast,
     1    xmul, smrate, storge, perest, strofe,spcav,kagmin
      integer np, pond, it, ix, kx, loopfg, i, idep, k, l, kofe
c
c     + + + LOCAL DEFINITIONS + + +
c     fsum   - cummulative infiltration depth
c     xx     - amount of infiltration for pseudo time correction
c     dtime  - delta-t for infiltration rate computations
c     retemp - temporary array for rainfall excess
c     rrate  - array to hold rainfall intensity after time to ponding
c              has been inserted
c     smrate - snow melt rate
c     strofe - A variable for storage on different OFE
c     storge - storage capacity of the upper soil layers' available
c              porosity
c     perest - estimate of minimum amount of water percolated during
c              a storm event
c     np     - flag to check if there is only one time interval where
c              rainfall excess occurs. If this happens runoff is set
c              to zero and the subroutine returns.
c     pond   - ponding on previous infiltration interval?  1=yes; 0=no.
c     it     - index for times to ponding
c     ix     - previous value of I; ie, I-1.
c     kx     - previous value of K; ie, K-1.
c     kofe   - An index for counting the OFEs in the effective length.
c     loopfg - flag.  If = 1, get out of loop.
c
c
c     + + + SUBROUTINES CALLED + + +
c     newton
c     depsto
c     reid
c
c     + + + END SPECIFICATIONS + + +
c
c******************************************************************
c     The Green and Ampt infiltration equation as modified
c     by Mein and Larson has the form :
c
c        f    = ks + ks*sm/ff               and
c        ks*t = ff - sm*ln(1 + ff/sm)
c
c        where:
c
c        f  = infiltration rate (l/t)
c        ks = saturated conductivity (l/t)
c        sm = effective matric potential (l)
c        ff = cumulative infiltration (l)
c        ln = natural log
c
c     input:
c
c     1. two column file
c          col 1 = time (minutes)
c          col 2 = rainfall rate (millimeters/hour)
c        on the vax the file is an edr file
c        on the pc the file has a format 2f10.2
c
c     2. saturated conductivity (millimeters/hour)
c     3. effective matric potential (s*m) (millimeters)
c        where:
c          s = difference in average capilary potential
c              before and after wetting
c          m = difference in average soil moisture
c******************************************************************
c
c new 10/11/91 reza  10/93  dcf
      storge = 0.0
      perest = 0.0
c     total infiltration FF should be <= (ul-st-avedep) reza 10/11/91
cd    Modified by S. Dun 16/03/2004
cd    For using the soil moisture at beginning of day variable stold
cd    to calculate effective runoff.
c     Modified by S. Dun, March 04, 2008 for frozen soil
      do 10 l = 1, 2
        stold(l,iplane) = st(l,iplane)
cd        if (stold(l,iplane).lt.ul(l,iplane)) 
cd     1     storge = storge + (ul(l,iplane) - stold(l,iplane))
        spcav = ul(l,iplane) - frzw(l,iplane)
        if(spcav .lt. 0.0) spcav = 0.0
        if (stold(l,iplane).lt.spcav) 
     1     storge = storge + (spcav - stold(l,iplane))
   10 continue
cd    End modifying
c
c     add on minimum value for storm water percolation to storage
c     value - added 10/18/93 by reza  -  dcf
      if (storge.lt.0.0) storge = 0.0
cd    Modified by S. Dun, 08/11/2008
c     for more dynamic changing storage due to percolation
cd      perest = sscmin(iplane) * dur
cd      storge = storge + perest
cd      if (storge.lt.0.0) storge = 0.0
      Kagmin = sscmin(iplane)
cd    end modifying
c     end new reza 10/11/91      10/93 dcf
c
cd    Added by S. Dun, 03/16/2004 
cd    For calculating effective runoff for multi-OFEs
cd    The effective length of contour farming is different from
cd    the effective length for multi-OFEs. Contour farming condition
cd    is eliminated from the added codes 06/14/2004
      if(cntflg.eq.0) then
      storge = storge*slplen(iplane)
      kagmin = sscmin(iplane)*slplen(iplane)
c
      if (iuprun(iplane).GT.0) then
          kofe = 0
          strofe = 0.
          do while ((iuprun(iplane-kofe).gt.0).and.
     1              ((iplane-kofe-1).gt.0))
             kofe = kofe + 1
             do 15 l = 1, 2
cd                if (stold(l,iplane-kofe).lt.ul(l,iplane-kofe))  
cd     1             strofe = strofe + (ul(l,iplane-kofe) - 
cd     1                      stold(l,iplane-kofe))
                spcav = ul(l,iplane-kofe)- frzw(l,iplane-kofe)
                if (spcav.lt.0.0) spcav = 0.0
c
                if (stold(l,iplane-kofe).lt.spcav)  
     1             strofe = strofe + spcav - 
     1                      stold(l,iplane-kofe)
                if (strofe.lt.0.0) strofe = 0.0
   15        continue
c
c     add on minimum value for storm water percolation to storage
c     value - added 10/18/93 by reza  -  dcf
cd    Modified by S. Dun, 08/11/2008
c     for more dynamic changing storage due to percolation
cd             perest = sscmin(iplane-kofe) * dur
cd             strofe = strofe + perest
cd             if (strofe.lt.0.0) strofe = 0.0
             kagmin = kagmin + sscmin(iplane-kofe)*slplen(iplane-kofe)
cd    end adding
c     end new reza 10/11/91      10/93 dcf
             storge = storge + strofe * slplen(iplane-kofe)
         end do          
      endif
c
      kagmin = kagmin/efflen(iplane)
      storge = storge/efflen(iplane)
      endif
cd    End adding
c
c
c     Snowmelt
c
      smrate = wmelt(iplane) / dur
c
      np = 0
      sumint = 0.0
      durre = 0.0
      pond = 0
      fsum = 0.0
      k = 1
      ff(1) = 0.0
      recum(1) = 0.0
      rcum(1) = 0.0
      rrate(1) = r(1)
      tf(1) = 0.0
      remax(iplane) = 0.0
      it = 0
c
c     SET SO THAT CHECK IN IRS FOR HDRIVE OR APPMTH IS NOT UNDEFINED
c     JJS AUG 94
c
      tp(2) = 0.0
c
c     Add tp(1) statement so that tp(1) is not undefined when checked
c     below - to fix surface saturation problem.   dcf  4/95
c
      tp(1) = 0.0
c     do 15 kkk=1,mxpond
c        tp(kkk) = 0.0
c        pt(kkk) = 0.0
c15   continue
c
c
c     start run
c
c     *** Begin L0 Loop ***
c
c     Loop through time steps and compute cumulative infiltration depth
c
      do 20 i = 2, nf
        ix = i - 1
        kx = k
        k = k + 1
cd      Added by S. Dun, 08/11/2008
c       for more dynamic changing storage due to percolation
        perest = kagmin * (tr(i)-tr(ix))
        storge = storge + perest
        if (storge.lt.0.0) storge = 0.0
cd      end adding
c
c       case one: no ponding in previous interval
c
c       *** L1 IF ***
        if (pond.eq.0) then
c
c
c         rainfall rate less than conductivity; ie, no rainfall excess
c
c         *** L2 IF ***
c
          if (r(ix).le.aveks(iplane)) then
            if (r(ix).gt.0.0) fsum = rr(i) - recum(kx)
            ff(k) = fsum
            if (ff(k).gt.storge) ff(k) = storge
            recum(k) = recum(kx)
            rcum(k) = rr(i)
            rrate(k) = r(i)
            tf(k) = tr(i)
c
c         ponding indicator when no ponding in previous interval
c
c         *** L2 ELSE ***
          else
            cu = rr(i) - recum(kx) - aveksm / (r(ix)-aveks(iplane))
c
c           case one-a: no ponding
c
c           *** L3 IF ***
            if (cu.le.0.0001) then
              fsum = rr(i) - recum(kx)
              ff(k) = fsum
              if (ff(k).gt.storge) ff(k) = storge
              recum(k) = recum(kx)
              rcum(k) = rr(i)
              rrate(k) = r(i)
              tf(k) = tr(i)
c
c
c           case one-b: ponding - get time to ponding, tp
c
c           *** L3 ELSE ***
            else
              pond = 1
              it = it + 1
              tp(it) = (aveksm/(r(ix)-aveks(iplane))-rr(ix)+recum(kx)) /
     1            r(ix) + tr(ix)
c
              if (tp(it).le.tr(ix)) then
                tp(it) = tr(ix)
                pt(it) = rr(ix)
              else
c
                ff(k) = ff(kx) + r(ix) * (tp(it)-tr(ix))
                if (ff(k).gt.storge) ff(k) = storge
                recum(k) = recum(kx)
                pt(it) = rr(ix) + ff(k) - ff(kx)
                rcum(k) = pt(it)
                rrate(k) = r(i)
                tf(k) = tp(it)
                kx = k
                k = k + 1
              end if
c
c             pseudotime - get time shift due to infiltration, ts
c             If matric suction = 0 implies saturation and ponds
c             instantly.  No time shift.
c             correction from Charlie Luce - FS    dcf  4/7/92
c
              if (avesm(iplane).gt.0.0) then
                xx = (pt(it)-recum(ix)) / avesm(iplane)
                ts = avesm(iplane) / aveks(iplane) * (xx-dlog(1.d0+xx))
c             290                 ts = avesm(iplane) / aveks(iplane) * (xx-dlog(1.d0+xx))
c             ^
c             Warning near line 290 col 20: dble truncated to real
              else
                ts = 0.0
              end if
c
c             real time, t
c
              tt = tr(i) - tp(it) + ts
c
c             cumulative infiltration, newtons method ala ljl
c
              call newton(tt,ff(kx),aveks(iplane),avesm(iplane),ff(k))
c
              recum(k) = rr(i) - ff(k)
              rcum(k) = rr(i)
              rrate(k) = r(i)
              tf(k) = tr(i)
c
c           *** L3 ENDIF ***
            end if
c
c         *** L2 ENDIF ***
          end if
c
c       *** L1 ELSE ***
        else
c
c         case two: ponding in previous interval
c
          tt = tr(i) - tp(it) + ts
          call newton(tt,ff(kx),aveks(iplane),avesm(iplane),ff(k))
c
c         check if no ponding before end of interval
c
          cp = rr(i) - ff(k) - recum(kx)
c
c         case two-a: no ponding before end of interval
c
          if (cp.lt.0.0001) then
            ff(k) = rr(i) - recum(kx)
            if (ff(k).gt.storge) ff(k) = storge
            fsum = ff(k)
            recum(k) = recum(kx)
            rcum(k) = rr(i)
            rrate(k) = r(i)
            tf(k) = tr(i)
            pond = 0
c
c         case two-b: ponding continues merrily on
c
          else
            recum(k) = rr(i) - ff(k)
            rcum(k) = rr(i)
            rrate(k) = r(i)
            tf(k) = tr(i)
          end if
c
c       *** L1 ENDIF ***
        end if
c
c
c       *** End L0 Loop ***
c       reza  change 3/92 and now 9/93
c
        if (ff(k).ge.storge) then
c
          ff(k) = storge
          recum(k) = rr(i) - ff(k)
c
c         For case where surface profile saturates and no time
c         to ponding has been calculated (soils with high Keff
c         in a single storm simulation), set TP(1).  savabi and dcf 4/95
c
          if(recum(k).gt.0.0.and.tp(1).le.0.0)tp(1)=tr(k)
        end if
   20 continue
c
c
c     *** M0 IF ***
      if (recum(nf).gt.0.0) then
c
c       If there is rainfall excess, compute rainfall excess rate with
c       depression storage computation, and get rainfall intensity info
c       for erosion computations
c
c       Set NTEMP equal to the last infiltration time - JJS JUN 93
        ntemp = k
c
c       Note: The next 2 executable lines ARE needed, since
c       DEPSTO changes the values of IDEP & DINF.
c       CRM -- 8/7/91
c
        idep = 0
        dinf = 0.0
        retemp(1) = recum(1)
        relast = recum(1)
c
c       Change by van der Sweep -  from xmul =1 to  xmul =0  2/92  dcf
c
        xmul = 0
c
c       *** Begin M1 Loop ***
        loopfg = 0
        i = 0
   30   continue
c
c       Compute infiltration and rainfall excess rates
c
        i = i + 1
        dtime = tf(i+1) - tf(i)
c
        f(i) = (ff(i+1)-ff(i)) / dtime
        re(i) = (recum(i+1)-recum(i)) / dtime
c
c       SET NSTEMP EQUAL TO THE LAST RAINFALL EXCESS TIME
c
        if (re(i).gt.0.0) nstemp = i + 1
        if (f(i).lt.0.0) f(i) = 0.0
        if (re(i).lt.0.0) re(i) = 0.0
c
        retemp(i+1) = recum(i+1)
c
c
c       depression storage section -JJS, 16 NOV 90
c
c       *** M2 IF ***
        if (avedep.gt.0) then
c
c         If depression storage > 0, subtract the amount from
c         the rainfall excess
c
c         depression storage is greater than total rainfall excess
c
c         *** M3 IF ***
c
cd    Added by S. Dun, Nov 27, 2007 for checking runoff generation for Pendleton, OR
cd        write(61, *) avedep, recum(ntemp)
cd    End adding
c
          if (avedep.ge.recum(ntemp)) then
            np = 0
            loopfg = 1
c
c         *** M3 ELSE-IF ***
          else if (recum(i+1).gt.0) then
            call depsto(i,recum,f,dtime,avedep,idep,dinf,retemp,re,xmul,
     1          relast)
c
c           rainfall excess rate must be recalculated to account for
c           depression storage
c
c
            re(i) = (retemp(i+1)-retemp(i)) / dtime
c
c           Added check to prevent negative value for RE - dcf 2/17/94
c
            if (re(i).lt.0.0) re(i) = 0.0
c
c         *** M3 ENDIF ***
          end if
c
c       *** M2 ENDIF ***
        end if
c
        if (loopfg.eq.0) then
          if (i.eq.1) remax(iplane) = re(i)
          if (re(i).gt.remax(iplane)) then
            nm = i
            remax(iplane) = re(i)
          end if
c
          if (re(i).gt.0.0) then
c
c           SET NSTEMP TO THE LAST TIME OF CUMULATIVE RAINFALL
c           EXCESS - JJS JUN 93
            nstemp = i + 1
            np = 1
          end if
        end if
c
c       *** End M1 Loop ***
c
        if ((i.lt.(ntemp-1)).and.loopfg.eq.0) go to 30
c
c
c       *** N1 IF ***
        if (np.ne.0) then
c
c         GET DURATION OF RAINFALL EXCESS AND RAINFALL INTENSITY
c
          call reid(nstemp,re,tf,smrate,rrate,durre)
c
c
c         SET LAST INFILTRATION RATE TO ZERO - JJS JUN 93
          f(ntemp) = 0.0
          re(nstemp) = 0.0
          runoff(iplane) = retemp(nstemp)
c
c         USE NTEMP FOR INFILTRATION, NSTEMP FOR RAINFALL EXCESS - JJS
          ff(ntemp) = rcum(ntemp) - retemp(nstemp)
cd      Modified by S. Dun, June 04, 2004
cd          if (runoff(iplane).lt.1.0e-3) runoff(iplane) = 0.0
c         if (runoff(iplane).lt.1.0e-6) runoff(iplane) = 0.0
cd      End modifying
c dcf     Changing limit on minimum runoff to prevent errors in
c dcf     calculation of peak runoff rates, flow widths, and erosion
c dcf     for multiple OFE hillslopes  - dcf  - 10/2/2008
c
          if (runoff(iplane).lt.1.0e-4) runoff(iplane) = 0.0
c
          exrain = re(nm)
c
c         calculate effective rainfall intensity and duration
c
          timem(ninten(iplane)+1) = timem(ninten(iplane))
c
          if (durre.gt.0.0) then
            effint(iplane) = sumint / durre
            effdrr(iplane) = durre
          else
            effint(iplane) = 0.0
            effdrr(iplane) = 0.0
          end if
c
c       *** N1 ELSE ***
        else
          recum(nf) = 0.0
c
c       *** N1 ENDIF ***
        end if
c
c     *** M0 ENDIF ***
      end if
c

      return
      end
