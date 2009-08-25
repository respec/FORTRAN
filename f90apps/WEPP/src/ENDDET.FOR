      subroutine enddet(cnpart,ielmt,i)
c
c     + + + PURPOSE + + +
c
c     SR ENDDET determines the point where detachment ends.
c
c     Called from: SR CASE34
c     Author(s): Ascough II, R. van der Zweep, V. Lopes
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by: Jim Ascough II
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxcsg.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer i, ielmt, cnpart
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     i      -
c     ielmt  -
c     cnpart -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchcas.inc'
      include 'cchero.inc'
      include 'cchvar.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real sumpl, sumtc, xdbig, xdbmin, xdsmal
      integer ndep, nt, k
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     sumpl  -
c     sumtc  -
c     xdbig  -
c     xdbmin -
c     xdsmal -
c
c     Integer Variables
c
c     ndep -
c     nt   -
c     k    -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     trncap
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c     detachment must end at a position between x(i-1)
c     and x(i) - xdsmal and xdbig are set as starting
c     positions for this search
c
      xdsmal = x(i-1)
      xdbig = x(i)
c
      ndep = 0
c
   10 do 20 k = 1, cnpart
c
        if (potld(k).le.tcl(k)) then
          xdbeg(k) = x(i)
        else
          xdbeg(k) = (2.0*(tcl(k)*wfl-gstu(k)-dlat(k)*dx)/du(k)) +
     1        x(i-1)
        end if
c
   20 continue
c
      xdbmin = x(i)
c
      do 30 k = 1, cnpart
        xdbmin = amin1(xdbmin,xdbeg(k))
   30 continue
c
      if (xdbmin.le.xdsmal) xdbmin = xdsmal
c
   40 do 50 k = 1, cnpart
        potld(k) = (gstu(k)+dlat(k)*dx+du(k)*(xdbmin-x(i-1))/2.0) / wfl
   50 continue
c
      call trncap(effshl,potld,ielmt,tcl)
c
      nt = 0
      sumtc = 0.0
      sumpl = 0.0
      ndep = ndep + 1
c
c     after four iterations (ndep = 4) assume equilibrium
c     conditions for all particles ending detachment at xdbeg
c
c     set dl(k) = 0.0 and gsl(k) = tcl(k) and process next segment
c
      if (ndep.eq.4) return
c
      do 60 k = 1, cnpart
        sumtc = sumtc + tcl(k)
        sumpl = sumpl + potld(k)
        if (tcl(k).le.potld(k)) nt = nt + 1
   60 continue
c
c     if abs( ... ) < 0.01 then process next segment
c
      if (abs((sumtc-sumpl)/sumtc).lt.0.01) return
c
      if (nt.lt.cnpart) then
        xdsmal = xdbmin
        xdbmin = (xdsmal+xdbig) / 2.0
        go to 40
      else
        xdbig = xdbmin
        go to 10
      end if
c
      end
