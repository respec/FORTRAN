      subroutine wshscs(peaksu,ieltmp,nh,qpmax)
c
c     + + + PURPOSE + + +
c
c     SR WSHSCS calculates SCS Triangular Hydrographs when
c     surface flows from hillslopes, channels and impoundments
c     merge onto a channel or into an impoundment. The hydrographs
c     are then superimposed to find a single peak runoff rate.
c
c     Called from: SR WSHPEK
c     Author(s): Ascough II
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real peaksu(3), qpmax
      integer ieltmp(3), nh
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     peaksu -
c     qpmax  -
c     ieltmp -
c     nh     -
c
c     + + + COMMON BLOCKS + + +
c
      include 'chydrol.inc'
      include 'cstore.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real delta1, delta2, h(3,1440), hcomp(1440), htot, tend(3),
     1    timeb(3), timep(3), tmax
      integer i, j, k, ntime
c
c     + + + LOCAL DEFINITIONS + + +
c
c     delta1 -
c     delta2 -
c     h(10,1440)  -
c     hcomp(1440) -
c     htot -
c     tend(10)    -
c     timeb(10)   -
c     timep(10)   -
c     tmax        -
c     i -
c     j -
c     k -
c     ntime -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      tmax = 0.0
c
      do 10 i = 1, nh
c
c       calculate the base time (mins) for the SCS Triangular
c       Hydrographs
c
c       WEPP Units   --> Area (m^2), Runoff (m), Peak Runoff (m^3/s)
c       SCS TH units --> Area (km^2), Runoff (mm), Peak Runoff (m^3/s),
c       (33.333 = 12.4844 * 2.67 from SCS TH Equation)
c
        timeb(i) = (33.333*(wsarea(ieltmp(i))/1.0e6)*(
     1      rofave(ieltmp(i))*1.0e3)) / peaksu(i)
c
        if (timeb(i).gt.tmax) tmax = timeb(i)
c
c       calculate the time to peak (mins)
c
        timep(i) = timeb(i) / 2.67
c
        tend(i) = timeb(i)
c
   10 continue
c
c     if tmax is greater than one day then limit the hydrograph
c     calculations
c
      if (tmax.gt.1440.0) tmax = 1440.0
      ntime = int(tmax)
c
c     calculate time-discharge relationship for each hydrograph
c
      do 30 j = 1, ntime
        do 20 i = 1, nh
          h(i,j) = 0.0
   20   continue
        hcomp(j) = 0.0
   30 continue
c
      do 50 i = 1, nh
c
        delta1 = peaksu(i) / timep(i)
        delta2 = peaksu(i) / (timeb(i)-timep(i))
c
        k = 0
c
        do 40 j = 1, ntime
c
          if (float(j).gt.tend(i)) then
            h(i,j) = 0.0
            go to 40
          end if
c
          if (float(j).lt.timep(i)) then
            if (j.eq.1) h(i,j) = delta1
            if (j.gt.1) h(i,j) = h(i,j-1) + delta1
          else
            if (k.eq.0) then
              h(i,j) = peaksu(i)
              k = 1
            else
              h(i,j) = h(i,j-1) - delta2
            end if
          end if
c
          if (h(i,j).lt.0.0) h(i,j) = 0.0
c
   40   continue
   50 continue
c
      qpmax = 0.0
c
      do 70 i = 1, ntime
        htot = 0.0
        do 60 j = 1, nh
          htot = htot + h(j,i)
   60   continue
        hcomp(i) = htot
        if (hcomp(i).gt.qpmax) qpmax = hcomp(i)
   70 continue
c
      return
      end
