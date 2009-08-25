      subroutine irprnt
c
c     + + + PURPOSE + + +
c     This subroutine prints information to the "single storm" output
c     file.  This subprogram is called from subprogram FURROW.
c
c     Written by E. R. Kottwitz
c
c     + + + PARAMETERS + + +
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxsrg.inc'
      include 'pmxtls.inc'
      include 'pxstep.inc'
c
c     + + + COMMON BLOCKS + + +
      include 'cavepar.inc'
c       read: aveks(mxplan),avesm(mxplan),avsat(mxplan)
      include 'ccons.inc'
c       read: avpor(mxplan)
      include 'cconsta.inc'
c       read: accgav
      include 'ccover.inc'
c       read: cancov(mxplan),gcover(mxplan)
      include 'cdist2.inc'
c       read: slplen(mxplan)
      include 'cffact.inc'
c       read: frctrl(iplane)
      include 'cirfurr.inc'
c       read: qspply(mxsrg),surge,tend(mxsrg),tstart(mxsrg)
      include 'cirinfl.inc'
c       read: kosta,kostf,kostk
      include 'cirriga.inc'
c       read: irofe
      include 'cparame.inc'
c       read: ks(mxplan),sm(mxplan),por(mxnsl,mxplan),sat(mxplan)
      include 'cslope2.inc'
c       read: avgslp(mxplan)
      include 'cstruc.inc'
c       read: iplane
      include 'cwater.inc'
c       read: nsl(mxplan)
c
c     + + + LOCAL VARIABLES + + +
      real chezyc
      integer prtflg
c
c     + + + LOCAL DEFINITIONS + + +
c     chezyc - Chezy friction coefficient (m**(0.5)/s)
c     prtflg - flag indicating whether this subprogram has been accessed
c              before (0-no, 1-yes)
c
c     + + + SAVES + + +
      save prtflg
c
c     + + + DATA INITIALIZATIONS + + +
      data prtflg /0/
c
c     + + + OUTPUT FORMATS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c ... Write output header if this is the first time this subprogram has
c     been called.
c
      if (prtflg.eq.0) then
        write (32,1000)
        prtflg = 1
      end if
c
c     ... Write overland flow element number.
c
      write (32,1900) iplane
c
c     ... Written output depends on whether current overland flow element is
c     above or below the irrigation water supply point.
c
      if (iplane.lt.irofe) then
        write (32,1100)
      else
        write (32,1500) ks(iplane) * 3.6e6, sm(iplane) * 1000.,
     1      por(nsl(iplane),iplane), sat(iplane) * 100.,
     1      cancov(iplane) * 100., gcover(iplane) * 100.
        write (32,1800) kostk, kosta, kostf
        chezyc = sqrt(8.*avgslp(iplane)*accgav/frctrl(iplane))
        write (32,1600) slplen(iplane), avgslp(iplane), chezyc
        write (32,1700) aveks(iplane) * 3.6e6, avesm(iplane) * 1000.,
     1      avpor(iplane), avsat(iplane)
        if (surge.ne.0) then
          write (32,1200)
          do 10 srg = 1, surge
            write (32,1300) srg, qspply(srg), tstart(srg), tend(srg)
   10     continue
          write (32,1400)
        end if
      end if
      return
 1000 format (//'I.   SINGLE STORM HYDROLOGY'/2x,9('-'),1x,5('-'),1x,36(
     1    '-')//)
 1100 format (/30x,27('*')/30x,'*  Overland flow element  *'/30x,
     1    '*  is above supply point  *'/30x,27('*')/)
 1200 format (/'  Inflow Rate - Duration Combination (IDC) Input'/2x,49(
     1    '-')/'          Supply   Begin Time  End Time'/
     1    '           Rate      of Flow    of Flow'/
     1    '   IDC    (m^3/s)      (s)        (s)'/2x,49('-'))
 1300 format (3x,i2,5x,f7.6,4x,f6.0,5x,f6.0)
 1400 format (2x,49('-')/)
 1500 format (/'  infiltration input parameters'/2x,54('-')/
     1    '     effective saturated cond.     ',f9.2,' (mm/h)'/
     1    '     effective matric potential    ',f9.2,' (mm)'/
     1    '     effective porosity            ',f9.2,' (mm/mm)'/
     1    '     saturation                    ',f9.2,' (%)'/
     1    '     canopy cover                  ',f9.2,' (%)'/
     1    '     surface cover                 ',f9.2,' (%)'/)
 1600 format (/'  input runoff parameters'/2x,54('-')/
     1    '     plane length                  ',f9.2,' (m)'/
     1    '     average slope of profile      ',f9.2/
     1    '     chezy coefficient             ',f9.2,' (m**0.5/s)'/)
 1700 format (/'  output runoff parameters'/2x,54('-')/
     1    '     equivalent sat. hydr. cond.   ',f9.2,' (mm/hr)'/
     1    '     equivalent matr. potential    ',f9.2,' (mm)'/
     1    '     average pore fraction         ',f9.2,' (m/m)'/
     1    '     average saturation fraction   ',f9.2,' (m/m)'/)
 1800 format (/'  calibrated Kostiakov-Lewis infil function parameters'/
     1'  (Z=k*t**a+f*t, Z = infil in m**2, t = infil opp time in s)'/2x,
     1    54('-')/'     k                             ',e9.4,
     1    ' (m**2/s**a)'/'     a                             ',e9.4/
     1    '     f                             ',e9.4,' (m**2/s)'/)
 1900 format (6x,27('*')/7x,'overland flow element ',i2/6x,27('*')/)
      end
