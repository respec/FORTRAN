      subroutine initd(iplane)
c
c     initialize cumulative environmental indices from harvest
c     of the previous crop to January 1 (1st day of simulation)
c     with the following assumtions:
c          1. use the calculated soil moisture (on January 1st)
c             for the entire period
c          2. Temperatures are observed average monthly max and min values
c          3. Rainfall is the observed average monthly
c
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
c
c
c
      include 'ccover.inc'
      include 'cdecvar.inc'
      include 'cobclim.inc'
      include 'cwater.inc'
c
      integer xmonth, iplane, i
      integer hdate, days, month, nc(13)
      real tave, envinx, fwatfc, opttmp, optwat, swatfc, t1, t2, tfc,
     1    tmpfac
      data nc /0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335,
     1    366/
c
c     initialize local variables
c
      optwat = 0.35
      tfc = 6.10
      opttmp = 30.0
c
c     Added trap to prevent HDATE from going negative for situation
c     of a fallow period (or perennial crop with over 1 year since
c     harvest).  This is probably not best fix - but it will give
c     one year of initialization for those situations.  dcf 2/14/94
c
      if (dsharv(iplane).le.365) then
        hdate = 366 - dsharv(iplane)
      else
        hdate = 1
      end if
c
      month = xmonth(hdate)
c
      do 10 i = month, 12
        days = nc(i+1) - nc(i)
        tave = (obmaxt(i)+obmint(i)) / 2
c
c
        if (obrain(i)/days.lt.4.0) then
          swatfc = obrain(i) / 4.0
        else
          swatfc = 1.0
        end if
c
c       number of days of the first month
c
        if (i.eq.month) then
          days = nc(i+1) - hdate
        end if
c
c       Changed following equations in IF-ELSE to match those
c       in DECOMP.FOR.   dcf  2/14/94
c
        if (soilw(1,iplane).lt.optwat) then
          fwatfc = (soilw(1,iplane)/dg(1,iplane)) / optwat
        else
          fwatfc = optwat / (soilw(1,iplane)/dg(1,iplane))
        end if
c
c       Determination of daily temperature factor
c
        if (tave.gt.-1.5.and.tave.lt.44.9) then
          t1 = tave + tfc
          t1 = t1 * t1
          t2 = opttmp + tfc
          t2 = t2 * t2
          tmpfac = (2*t1*t2-t1*t1) / (t2*t2)
        else
          tmpfac = 0.0
        end if
c
c       Calculation of cumulative environmental index; separate indices
c       for standing, flat, and buried residues; later calculations for
c       roots use the same indice as the buried residues
c
        envinx = min(swatfc,tmpfac) * days
        senvin(iplane) = senvin(iplane) + envinx
        envinx = min(fwatfc,tmpfac) * days
        fenvin(1,iplane) = fenvin(1,iplane) + envinx
   10 continue
      benvin(1,iplane) = fenvin(1,iplane)
c
      return
      end
