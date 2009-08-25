      subroutine irinpt(iplant)
c
c     + + + PURPOSE + + +
c     This subprogram reads information from the irrigation data file(s)
c     during initialization calls.  This subprogram is called from
c     subprogram INPUT.
c
c     Written by E. R. Kottwitz
c
c     + + + PARAMETERS + + +
      include 'pmxcrp.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxsrg.inc'
      include 'pxstep.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer iplant
c
c     + + + ARGUMENT DEFINITIONS + + +
c     iplant - code for vegetation type (1-crop, 2-range, 3-forest)
c
c     + + + COMMON BLOCKS + + +
      include 'ccliyr.inc'
c       read: ibyear
      include 'ccntour.inc'
c       read: rowspc(mxplan)
      include 'cdat.inc'
c       read: ifsver, idsver
      include 'cirdepl.inc'
c     modify: irbeg(mxplan),irdmin,yrbeg(mxplan)
c       read: deplev(mxplan),irend(mxplan),yrend(mxplan)
      include 'cirfixd.inc'
c     modify: irday(mxplan),iryr(mxplan)
      include 'cirfurr.inc'
c     modify: depsrg(mxplan)
c       read: endpln(mxplan),filrat(mxplan),florat(mxplan),
c             timest(mxplan)
      include 'cirriga.inc'
c     modify: irschd(mxplan),irsyst
      include 'cirspri.inc'
c       read: aprati(mxplan),irdmax,irrate(mxplan),nozzle(iplane)
      include 'cstruc.inc'
c     modify: iplane
      include 'cstruct.inc'
c       read: nplan
c
c     + + + LOCAL VARIABLES + + +
      integer irflag, ofeflg
c
c     + + + LOCAL DEFINITIONS + + +
c     irflag    - flag used to determine if irsyst will be set equal
c                 to zero
c     ofeflg    - flag used to affirm that data file has correct format
c
c     + + + OUTPUT FORMATS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c     Irrigation scheduling scheme is initially the same for all OFE's.
c
      if (nplane.gt.1) then
        do 10 iplane = 2, nplane
          irschd(iplane) = irschd(1)
   10   continue
      end if
      if (irsyst.eq.1) then
c
c       -----  SOLID-SET, SIDE-ROLL OR HAND-MOVE SYSTEM  -----
c
c       If the following is true then read parameters for depletion
c       level irrigation scheduling.
c
        if (irschd(1).ne.2) then
          read (15,*) irdmin, irdmax
          if (irdmin.lt.0.001) irdmin = 0.025
          do 20 iplane = 1, nplane
c
c           if idsver(depletion level data version read in)
c           is less than 94.21 NOZZLE is not
c           present, read old format and set NOZZLE(iplane)=1.0
c
            if(idsver.lt.94.21)then
              read (15,*) ofeflg, irrate(iplane), aprati(iplane),
     1            deplev(iplane),irbeg(iplane),
     2            yrbeg(iplane), irend(iplane), yrend(iplane)
                  nozzle(iplane)=1.0
            else
              read (15,*) ofeflg, irrate(iplane), aprati(iplane),
     1            deplev(iplane), nozzle(iplane),irbeg(iplane),
     2            yrbeg(iplane), irend(iplane), yrend(iplane)
              endif
            if (ofeflg.ne.iplane) write (6,1200)
            if (irbeg(iplane).eq.0) then
              if (irschd(iplane).eq.3) then
                irschd(iplane) = 2
              else
                irschd(iplane) = 0
              end if
            end if
   20     continue
          end if
c
c       If the following is true then read parameters for fixed date
c       irrigation scheduling.
c
        if (irschd(1).ge.2) then
          do 30 iplane = 1, nplane
            read (14,*) ofeflg, irday(iplane), iryr(iplane)
            if (ofeflg.ne.iplane) write (6,1200)
            if (irday(iplane).eq.0) then
              if (irschd(iplane).eq.3) then
                irschd(iplane) = 1
              else
                irschd(iplane) = 0
              end if
            end if
   30     continue
        end if
      else
c
c       -----  FURROW SYSTEM  -----
c
c       Furrow irrigation is not available with contour practices.
c
        if (rowspc(1).gt..000001) then
          write (6,1000)
          irsyst = 0
          go to 70
        end if
c
c       Furrow irrigation is not available for range or forest
c       vegetation.
c
        if (iplant.ne.1) then
          write (6,1100)
          irsyst = 0
          go to 70
        end if
c
c       If the following is true then read parameters for depletion
c       level irrigation scheduling.
c
        if (irschd(1).ne.2) then
          read (15,*) irdmin
          if (irdmin.lt.0.001) irdmin = 0.025
          do 40 iplane = 1, nplane
            read (15,*) ofeflg, endpln(iplane), florat(iplane),
     1          timest(iplane), depsrg(iplane), filrat(iplane),
     1          deplev(iplane), irbeg(iplane), yrbeg(iplane),
     1          irend(iplane), yrend(iplane)
            if (ofeflg.ne.iplane) write (6,1200)
            if (depsrg(iplane).gt.6) depsrg(iplane) = 6
            if (depsrg(iplane).eq.3) depsrg(iplane) = 4
            if (irbeg(iplane).eq.0) then
              if (irschd(iplane).eq.3) then
                irschd(iplane) = 2
              else
                irschd(iplane) = 0
              end if
            end if
   40     continue
        end if
c
c       If the following is true then read parameters for fixed date
c       irrigation scheduling.
c
        if (irschd(1).ge.2) then
          do 50 iplane = 1, nplane
            read (14,*) ofeflg, irday(iplane), iryr(iplane)
            if (ofeflg.ne.iplane) write (6,1200)
            if (irday(iplane).eq.0) then
              if (irschd(iplane).eq.3) then
                irschd(iplane) = 1
              else
                irschd(iplane) = 0
              end if
            end if
   50     continue
        end if
      end if
c
c     -----  CHECK WHETHER ANY IRRIGATIONS WILL OCCUR  -----
c
      irflag = 0
      do 60 iplane = 1, nplane
        irflag = max(irflag,irschd(iplane))
   60 continue
      if (irflag.eq.0) irsyst = 0
c
   70 return
 1000 format (/' Furrow irrigation has been indicated with contour'/
     1    ' farming practices.  This combination is not supported'/
     1    ' and simulation will continue without irrigation.')
 1100 format (/
     1    ' Furrow irrigation has been indicated on range or forest'/
     1    ' vegetation.  This combination is not supported and'/
     1    ' simulation will continue without irrigation.'/)
 1200 format (/
     1    ' Irrigation data file has improper format.  Irrigations',/
     1    ' will not occur as planned.')
      end
