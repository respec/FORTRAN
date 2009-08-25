      subroutine depirr(rwdth)
c
c     + + + PURPOSE + + +
c     This subprogram is used to develop the sequence of inflow rates
c     for depletion level irrigation scheduling.  This subprogram is
c     called from subprogram FURADV.
c
c     Written by E. R. Kottwitz
c
c     + + + PARAMETERS + + +
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxsrg.inc'
      include 'pxstep.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      real rwdth
c
c     + + + ARGUMENT DEFINITIONS + + +
c     rwdth  - assumed row width for OFE into which water is introduced
c              (m)
c
c     + + + COMMON BLOCKS + + +
      include 'cavepar.inc'
c       read: aveks(mxplan),avesm(mxplan)
      include 'cdist2.inc'
c       read: slplen(mxplan)
      include 'cirdepl.inc'
c       read: iramt
      include 'cirfurr.inc'
c       read: depsrg(mxplan),endofe,florat(mxplan)
c     modify: qspply(mxsrg),splyvm,surge,tend(mxsrg),tstart(mxsrg)
      include 'cirinfl.inc'
c       read: kosta,kostf,kostk
      include 'cirriga.inc'
c       read: irofe
c      write: noirr
      include 'cstruc.inc'
c       read: iplane
c
c     + + + LOCAL VARIABLES + + +
      integer cbsurg, i, srge
      real array1(4), array2(5), array3(6), cbdrtn, inopt
c
c     + + + LOCAL DEFINITIONS + + +
c     array1 - cycle time coefficients for 4 advance surges
c     array2 - cycle time coefficients for 5 advance surges
c     array3 - cycle time coefficients for 6 advance surges
c     cbdrtn - cumulative duration of cutback surges (s)
c     cbsurg - number of cutback surges
c     i      - do loop counter
c
c     + + + DATA INITIALIZATIONS + + +
      data array1 /.140, .200, .280, .380/
      data array2 /.120, .160, .200, .240, .280/
      data array3 /.088, .116, .150, .180, .208, .258/
c
c     + + + END SPECIFICATIONS + + +
c
c ... Calculate required infiltration opportunity time at lower end of
c     furrow
c
      inopt = (iramt-avesm(endofe)*log(1.+iramt/avesm(endofe))) /
     1    aveks(endofe)
c
c     ... Reset flow rate for first surge
c
      qspply(1) = florat(irofe)
      if (depsrg(irofe).eq.1) then
c
c       -----  CONTINUOUS  -----
c
        tend(1) = tadvan(xsteps) + inopt
        if (tend(1).gt.86400.) tend(1) = 86400.
      else if (depsrg(irofe).eq.2) then
c
c       -----  CUTBACK  -----
c
c       ..... Calculate cutback flowrate based on furrow infiltration 60 s
c       after advance
c
        qspply(2) = 0.
        do 10 i = irofe, iplane - 1
          qspply(2) = qspply(2) + slplen(i) * aveks(i) * rwdth
   10   continue
        qspply(2) = qspply(2) + (kosta*kostk*(60.+tadvan(xsteps)-
     1      tadvan(0))**(kosta-1.)+kostf) * .5 * (xpostn(1)-xpostn(0)) +
     1      (kosta*kostk*60.**(kosta-1.)+kostf) * .5 * (xpostn(xsteps)-
     1      xpostn(xsteps-1))
        do 20 i = 1, xsteps - 1
          qspply(2) = qspply(2) + (kosta*kostk*(60.+tadvan(xsteps)-
     1        tadvan(i))**(kosta-1.)+kostf) * .5 * (xpostn(i+1)-
     1        xpostn(i-1))
   20   continue
c
c       ..... Calculate begin, end, and depletion times for flow rates
c
        tend(1) = tadvan(xsteps)
        tstart(2) = tadvan(xsteps)
        tend(2) = tstart(2) + inopt
        if (tend(2).gt.86400.) tend(2) = 86400.
      else
c
c       -----  SURGE  -----
c
c       ..... Calculate advance surges
c
        tend(1) = tadvan(xsteps)
        srge = 1
   30   srge = srge + 1
        qspply(srge) = florat(irofe)
        tstart(srge) = 2. * tend(srge-1) - tstart(srge-1)
        if (tstart(srge).lt.86400.) then
          if (depsrg(irofe).eq.4) then
            tend(srge) = tstart(srge) + tend(1) / array1(1) *
     1          array1(srge)
          else if (depsrg(irofe).eq.5) then
            tend(srge) = tstart(srge) + tend(1) / array2(1) *
     1          array2(srge)
          else
            tend(srge) = tstart(srge) + tend(1) / array3(1) *
     1          array3(srge)
          end if
          if (tend(srge).lt.86400.) then
            if (srge.lt.surge) go to 30
          else
            tend(srge) = 86400.
            surge = srge
          end if
        else
          srge = srge - 1
          surge = srge
        end if
c
c       ..... Calculate cutback surges
c
        if (tend(surge).lt.86400.) then
          cbsurg = nint(inopt/.65/(tend(surge)-tstart(surge)))
          if (cbsurg+srge.gt.mxsrg) cbsurg = mxsrg - srge
          cbdrtn = inopt / float(cbsurg)
          surge = srge + cbsurg
   40     srge = srge + 1
          qspply(srge) = florat(irofe)
          tstart(srge) = 2. * tend(srge-1) - tstart(srge-1)
          if (tstart(srge).lt.86400.) then
            tend(srge) = tstart(srge) + cbdrtn
            if (tend(srge).lt.86400.) then
              if (srge.lt.surge) go to 40
            else
              tend(srge) = 86400.
              surge = srge
            end if
          else
            surge = srge - 1
          end if
        end if
      end if
c
c     ... Set miscellaneous variables
c
      noirr = 2
      iplane = irofe
      do 50 srge = 1, surge
        splyvm = splyvm + qspply(srge) * (tend(srge)-tstart(srge))
   50 continue
      return
      end
