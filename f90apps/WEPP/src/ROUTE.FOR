      subroutine route
c
c     + + + PURPOSE + + +
c     Calculates detachment (when shear stress exceeds critical shear
c     stress) or deposition at the upper end of the slope segment and
c     routes sediment through the hillslope profile.
c
c     Called from subroutine CONTIN.
c     Author(s): G. Foster, M. Nearing, D. Flanagan
c     Reference in User Guide:
c
c     Changes: 1) Since MSHEAR is a discrete integer variable (flag),
c                 computed GOTO's were substituted for IF's with .LE.'s
c                 to handle cases in "deposition at upper end of segment"
c                 section, and in "detachment at upper end of segment"
c                 section.  This seemed to reduce the runtime by about
c                 3-5% (A 9.9-10.0 second run was reduced to 9.5 seconds.)
c              2) Variable ERD was deleted.
c              3) KTRATO & QOSTAR were added to the argument lists of
c                 functions DEPEND & DEPC, to eliminate the need for
c                 them to access common blocks.
c              4) In the third call to EROD after 155, the 6th argument
c                 was changed from EATA to 0.0 (Dennis's Suggestion).
c                 CRM - 1/22/91.
c              5) Re: the values of MSHEAR, since 2 & 3 were lumped
c                 together, and a computed GOTO can't handle a zero,
c                 the original "3" was eliminated, and 0-2 were "pro-
c                 moted" to 1-3. CRM - 1/24/91.
c              6) GOTO's were massively changed to nested IF's.
c                 CRM - 2/04/91.
c              7) Removed old L2 IF-ELSE logic (v97.3) so that model
c                 always calculates where deposition ends, regardless
c                 of the shape of the segment.  This is because with the
c                 adjustments made in PARAM at OFE breaks - slope shape
c                 is not always true predictor of processes on OFE.
c                 dcf  (changed L3 to L2 IF/ELSE)
c
c     Version: This module recoded from WEPP version 90.92.
c     Date recoded: 12/21/90 - 2/08/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
      include 'pmxslp.inc'
      include 'pmxtls.inc'
c
c     + + + COMMON BLOCKS + + +
      include 'cends1.inc'
      include 'cends2.inc'
      include 'cends3.inc'
c       read: ktrato, qout, qin, strldn
      include 'cenrpa1.inc'
c     modify: frcflw(10,mxplan)
      include 'cerdva1.inc'
c      write: tc(101),load(101)
      include 'cinfco1.inc'
c       read: qostar
      include 'cinfco2.inc'
c       read: ainf(mxslp), binf(mxslp), cinf(mxslp)
      include 'cirriga.inc'
c       read: noirr
      include 'cpart1.inc'
c       read: npart, frac
      include 'cparva1.inc'
c       read: eata, tauc, theta, phi
      include 'cslope1.inc'
c       read: xl(mxslp,mxplan), nslpts(mxplan), a(mxslp,mxplan)
c     modify: xu(mxslp,mxplan)
      include 'cstruc.inc'
c       read: iplane
c
c     + + + LOCAL VARIABLES + + +
      real ldlast, loadup, lddend, xdend, xdbeg, xdetst, cdep, dl, du,
     1     xc1, xc2
      integer i, iendfg, ilast, jj, k, mshear, ndep
c
c     ldlast - n.d. sediment load calculated at last point
c     loadup - n.d. load computed entering deposition region - value
c              is sent and used in subroutine enrich.f
c     lddend - n.d. load computed at end of previous deposition
c              region or overland flow element (i.e., load at xdetst)
c     xdend  - n.d. distance where deposition computed to end
c     xdbeg  - n.d. distance where deposition begins on a segment
c     xdetst - n.d. distance at start of previous detachment area
c     ilast  - counter index variable at last point
c     iendfg - flag to subroutine enrich.f indicating last call at
c              end of an overland flow element
c     dl     - n.d. deposition rate
c     du     - n.d. deposition rate at the top of a segment
c     cdep   - portion of solution to deposition equation
c     erd    - n.d. rill erodibility parameter sent to erod.f
c              if shear < critical erd=0, otherwise erd=eata
c     mshear - flag indicating what shear conditions exist on segment
c
c     + + + SUBROUTINES CALLED + + +
c     xcrit
c     depos
c     erod
c
c     + + + FUNCTION DECLARATIONS + + +
      real depc
      real depend
c
c     + + + END SPECIFICATIONS + + +
c
c
c*********************************************************************
c
c ... initialize erosion variables at the top of an OFE.
c
      lddend = strldn
      ldlast = strldn
      ilast = 1
      ndep = 0
      iendfg = 0
      xdbeg = 0.0
      xdetst = 0.0
c
c     .... Set sediment loads and transport capacity at each point,
c     equal to zero.
      do 10 jj = 1, 101
        load(jj) = 0.0
        tc(jj) = 0.0
   10 continue
c
c     .... Set distance, transport capacity, and sediment load at
c     first point on OFE.
      xu(2,iplane) = 0.0
c     tc(1)=cinf(2)*ktrato
      tc(1) = cinftc(2) * ktrato
      load(1) = strldn
c
c     ... initialize particle fractions in the flow at top of OFE
c     with no inflow from previous OFE.
c
      if (qout.gt.0.0) then
        if (qin.le.0.0.or.(noirr.gt.0.and.irsyst.eq.2.and.iplane.eq.1))
     1      then
          do 20 i = 1, npart
            frcflw(i,iplane) = frac(i,iplane)
   20     continue
        else
c
c         ........... Initialize particle fractions in the flow for OFE's
c         with inflow from previous OFE.
          do 30 i = 1, npart
            frcflw(i,iplane) = frcflw(i,iplane-1)
   30     continue
        end if
      else
        do 40 i = 1, npart
          frcflw(i,iplane) = 0.0
   40   continue
      end if
c
c******************************************************************
c     upper boundary condition for overland flow elements
c
c     Determine if deposition is occuring at X=0 on OFE.
c     ...if qostar = 0.0 (no inflow) estimate dl from deposition equation
c     ...else, estimate dl from the incoming sediment load and flow rate
c
      if (abs(qostar).lt..0011) then
        dl = phi / (phi+1.0) * (ktrato*binftc(2)-theta)
      else
        dl = phi / qostar * (ktrato*cinftc(2)-ldlast)
      end if
c
c
c     ...  **************************************************************
c     ...  *** For Each Slope Segment Within an Overland Flow Element ***
c     ...  **************************************************************
c     ... perform detachment and deposition calculations.
c
c
c     *** Start of BIG DO-LOOP ***
      do 170 k = 2, nslpts(iplane)
c
c       For a CASE 4 plane - bypass all calculations if flow
c       has ended before current segment
c       *** Start of BIG IF ***
        if (qout.gt.0.0.or.xu(k,iplane).lt.-qostar) then
c
c         If a Case 2 or 3 plane, or a Case 4 plane segment
c         on which runoff does not end.
          if (qout.gt.0.0.or.(qout.le.0.0.and.xl(k,iplane).lt.-qostar))
     1        then
c
c           .......... Calculate shear conditions in segment, then
c           Find where shear equals critical shear for segment.
            call xcrit(ainf(k),binf(k),cinf(k),tauc,xu(k,iplane),
     1          xl(k,iplane),qostar,xc1,xc2,mshear)
c
c         For a Case 4 plane segment on which the flow ends
          else
            call xcrit(ainf(k),binf(k),cinf(k),tauc,xu(k,iplane),-
     1          qostar,qostar,xc1,xc2,mshear)
          end if
c
c         ... determine if there is deposition at the beginning of the
c         ... segment - if there is - calculate where deposition ends
c
          du = dl
c
c         *** L1 IF ***
          if (du.lt.0.) then
c
c           deposition at upper end of segment
c
            cdep = depc(xu(k,iplane),ainftc(k),binftc(k),phi,theta,du,
     1          ktrato,qostar)
c
c             check for deposition ending within segment
c
              xdend = depend(xu(k,iplane),xl(k,iplane),ainftc(k),
     1            binftc(k),cdep,phi,theta,ktrato,qostar)
c
c             deposition does not end
c
c             *** L2 IF ***
              if (xdend.ge.xl(k,iplane)) then
                xdend = xl(k,iplane)
c
                loadup = ldlast
                call depos(xu(k,iplane),xdend,cdep,ainftc(k),binftc(k),
     1              cinftc(k),phi,theta,ilast,dl,ldlast)
                ndep = 0
                if (ldlast.gt.0.0.and.qout.gt.0.0) then
                  call enrich(k,xu(k,iplane),xdend,xdetst,loadup,ldlast,
     1                lddend,theta,iendfg)
                  lddend = ldlast
                  xdetst = xdend
                end if
c             *** L2 ELSE ***
              else
c
c               Deposition ends in segment
c
                loadup = ldlast
                call depos(xu(k,iplane),xdend,cdep,ainftc(k),binftc(k),
     1              cinftc(k),phi,theta,ilast,dl,ldlast)
                ndep = 0
                if (ldlast.gt.0.0.and.qout.gt.0.0) then
                  call enrich(k,xu(k,iplane),xdend,xdetst,loadup,ldlast,
     1                lddend,theta,iendfg)
                  lddend = ldlast
                  xdetst = xdend
                end if
c
c
c               Detachment After Deposition
c
c
c               Note: MSHEAR is an integer from 1 to 5.  Notice that in the first two
c               cases, as well as the second two, the only difference is in the
c               value assigned to ERD.  Also, note the games played with the first
c               two arguments to EROD in the various cases.  Except for the first
c               argument passed to EROD, the 1st case is identical to the ELSE
c               of the 3rd, and the 2nd to the ELSE of the 4th.  There may be
c               some real opportunities to simplify the code here! -- CRM 1/7/91
c
                go to (50,60,70,80,90)mshear
c
c               ............. Shear below critical in entire segment.
c               ** MSHEAR = 1 **
   50           continue
                call erod(xdend,xl(k,iplane),ainf(k),binf(k),cinf(k),
     1              ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,phi,
     1              ilast,dl,ldlast,xdbeg,ndep)
                go to 100
c
c               ............. Shear exceeds critical in entire segment.
c               ** MSHEAR = 2 **
   60           continue
                call erod(xdend,xl(k,iplane),ainf(k),binf(k),cinf(k),
     1              ainftc(k),binftc(k),cinftc(k),eata,tauc,theta,phi,
     1              ilast,dl,ldlast,xdbeg,ndep)
                go to 100
c
c               ............. Shear increases down slope and exceeds critical at X=XC1.
c               ** MSHEAR = 3 **
   70           continue
                if (xdend.le.xc1) then
                  call erod(xdend,xc1,ainf(k),binf(k),cinf(k),
     1                ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,phi,
     1                ilast,dl,ldlast,xdbeg,ndep)
                  if (ndep.eq.0) call erod(xc1,xl(k,iplane),ainf(k),
     1                binf(k),cinf(k),ainftc(k),binftc(k),cinftc(k),
     1                eata,tauc,theta,phi,ilast,dl,ldlast,xdbeg,ndep)
                else
                  call erod(xdend,xl(k,iplane),ainf(k),binf(k),cinf(k),
     1                ainftc(k),binftc(k),cinftc(k),eata,tauc,theta,phi,
     1                ilast,dl,ldlast,xdbeg,ndep)
                end if
                go to 100
c
c               ............. Shear decreases down slope and drops below critical at X=XC1.
c               ** MSHEAR = 4 **
   80           continue
                if (xdend.le.xc1) then
                  call erod(xdend,xc1,ainf(k),binf(k),cinf(k),
     1                ainftc(k),binftc(k),cinftc(k),eata,tauc,theta,phi,
     1                ilast,dl,ldlast,xdbeg,ndep)
                  if (ndep.eq.0) call erod(xc1,xl(k,iplane),ainf(k),
     1                binf(k),cinf(k),ainftc(k),binftc(k),cinftc(k),0.0,
     1                tauc,theta,phi,ilast,dl,ldlast,xdbeg,ndep)
                else
                  call erod(xdend,xl(k,iplane),ainf(k),binf(k),cinf(k),
     1                ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,phi,
     1                ilast,dl,ldlast,xdbeg,ndep)
                end if
                go to 100
c
c               ............ Shear increases down slope -- exceeds critical at X=XCL,
c               then decreases from XCL to XC2, and drops below critical
c               at XC2.
c               ** MSHEAR = 5 **
   90           continue
                if (xdend.le.xc1) then
                  call erod(xdend,xc1,ainf(k),binf(k),cinf(k),
     1                ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,phi,
     1                ilast,dl,ldlast,xdbeg,ndep)
                  if (ndep.eq.0) then
                    call erod(xc1,xc2,ainf(k),binf(k),cinf(k),
     1                  ainftc(k),binftc(k),cinftc(k),eata,tauc,theta,
     1                  phi,ilast,dl,ldlast,xdbeg,ndep)
                    if (ndep.eq.0) call erod(xc2,xl(k,iplane),ainf(k),
     1                  binf(k),cinf(k),ainftc(k),binftc(k),cinftc(k),
     1                  0.0,tauc,theta,phi,ilast,dl,ldlast,xdbeg,ndep)
                  end if
                else if (xdend.gt.xc2) then
                  call erod(xdend,xl(k,iplane),ainf(k),binf(k),cinf(k),
     1                ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,phi,
     1                ilast,dl,ldlast,xdbeg,ndep)
                else
                  call erod(xdend,xc2,ainf(k),binf(k),cinf(k),
     1                ainftc(k),binftc(k),cinftc(k),eata,tauc,theta,phi,
     1                ilast,dl,ldlast,xdbeg,ndep)
                  if (ndep.eq.0) call erod(xc2,xl(k,iplane),ainf(k),
     1                binf(k),cinf(k),ainftc(k),binftc(k),cinftc(k),0.0,
     1                tauc,theta,phi,ilast,dl,ldlast,xdbeg,ndep)
                end if
  100           continue
c             *** L2 ENDIF ***
              end if
c
c         detachment at upper end of segment
c
c         *** L1 ELSE ***
          else
c
            dl = 0.0
            du = 0.0
c
c           Note: MSHEAR is an integer from 1 to 5.  Notice that in the first two
c           cases, as well as the second two, the only difference in in the
c           value assigned to ERD.  Also, note the games played with the first
c           two arguments to EROD in the various cases.  Except for the first
c           argument passed to EROD, the 1st case is identical to the last half
c           of the 3rd, and the 2nd to the last half of the 4th.  There may be
c           some real opportunities to simplify the code here! -- CRM 1/7/91
c
            go to (110,120,130,140,150)mshear
c
c           ............. Shear below critical in entire segment.
c           ** MSHEAR = 1 **
  110       continue
            call erod(xu(k,iplane),xl(k,iplane),ainf(k),binf(k),
     1          cinf(k),ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,
     1          phi,ilast,dl,ldlast,xdbeg,ndep)
            go to 160
c
c           ............. Shear exceeds critical in entire segment.
c           ** MSHEAR = 2 **
  120       continue
            call erod(xu(k,iplane),xl(k,iplane),ainf(k),binf(k),
     1          cinf(k),ainftc(k),binftc(k),cinftc(k),eata,tauc,theta,
     1          phi,ilast,dl,ldlast,xdbeg,ndep)
            go to 160
c
c           ............. Shear increases down slope and exceeds critical at X=XC1.
c           ** MSHEAR = 3 **
  130       continue
            call erod(xu(k,iplane),xc1,ainf(k),binf(k),cinf(k),
     1          ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,phi,ilast,
     1          dl,ldlast,xdbeg,ndep)
            if (ndep.eq.0) call erod(xc1,xl(k,iplane),ainf(k),binf(k),
     1          cinf(k),ainftc(k),binftc(k),cinftc(k),eata,tauc,theta,
     1          phi,ilast,dl,ldlast,xdbeg,ndep)
            go to 160
c
c           ............. Shear decreases down slope and drops below critical at X=XC1.
c           ** MSHEAR = 4 **
  140       continue
            call erod(xu(k,iplane),xc1,ainf(k),binf(k),cinf(k),
     1          ainftc(k),binftc(k),cinftc(k),eata,tauc,theta,phi,ilast,
     1          dl,ldlast,xdbeg,ndep)
            if (ndep.eq.0) call erod(xc1,xl(k,iplane),ainf(k),binf(k),
     1          cinf(k),ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,
     1          phi,ilast,dl,ldlast,xdbeg,ndep)
            go to 160
c
c           ......... Shear increases down slope -- exceeds critical at X=XCL,
c           then decreases from XCL to XC2, and drops below critical
c           at XC2.
c           ** MSHEAR = 5 **
  150       continue
            call erod(xu(k,iplane),xc1,ainf(k),binf(k),cinf(k),
     1          ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,phi,ilast,
     1          dl,ldlast,xdbeg,ndep)
            if (ndep.eq.0) then
              call erod(xc1,xc2,ainf(k),binf(k),cinf(k),ainftc(k),
     1            binftc(k),cinftc(k),eata,tauc,theta,phi,ilast,dl,
     1            ldlast,xdbeg,ndep)
              if (ndep.eq.0) call erod(xc2,xl(k,iplane),ainf(k),
     1            binf(k),cinf(k),ainftc(k),binftc(k),cinftc(k),0.0,
     1            tauc,theta,phi,ilast,dl,ldlast,xdbeg,ndep)
            end if
c
c         *** L1 ENDIF ***
          end if
c
  160     continue
c
c         If this was a detachment section on the segment of the
c         OFE which went into deposition (NDEP = 1) then call
c         the deposition routine from the point where load equals
c         transport capacity to the end of the segment.
c
          if (ndep.ne.0) then
            if (ilast.lt.102) then
              dl = 0.0
              du = 0.0
              cdep = depc(xdbeg,ainftc(k),binftc(k),phi,theta,du,ktrato,
     1            qostar)
              loadup = ldlast
              if (loadup.lt.lddend) loadup = lddend
              call depos(xdbeg,xl(k,iplane),cdep,ainftc(k),binftc(k),
     1            cinftc(k),phi,theta,ilast,dl,ldlast)
              ndep = 0
              if (ldlast.gt.0.0.and.qout.gt.0.0) then
                call enrich(k,xdbeg,xl(k,iplane),xdetst,loadup,ldlast,
     1              lddend,theta,iendfg)
                lddend = ldlast
                xdetst = xl(k,iplane)
              end if
            end if
          end if
c
c       End of BIG IF
        end if
c
c     End of BIG DO-LOOP
  170 continue
c
c     Compute enrichment ratio at the end of each
c     overland flow element.
c
      iendfg = 1
c
c     Changed following call to ENRICH 6/30/94 because new code
c     inserted in ENRICH by Nearing must have actual value for location
c     of the beginning and end of the deposition region at the last
c     point on the OFE - thus:  xtop = xbot = 1.0
c     dcf  6/30/94
c
      call enrich(k,1.0,1.0,xdetst,ldlast,ldlast,lddend,theta,iendfg)
c
      return
      end
