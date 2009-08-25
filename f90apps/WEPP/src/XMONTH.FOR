      integer function xmonth(ida)
c
c     + + + PURPOSE + + +
c     Returns the month, given the Julian date.  Does not distinguish
c     between leap years and non-leap years.
c
c     Called from YPOTP, YLDOPT, INITD.
c     Author(s): Arnold
c     Reference in User Guide:
c
c     Changes:
c             1) To conform to the WEPP Coding Convention, changed
c                XMONTH from a subroutine to a function.
c
c     Version: This module recoded from Arnold's WEPP Post-version 92.25.
c     Date recoded: 07/07/92.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer ida
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ida    - Julian date
c
c     + + + END SPECIFICATIONS + + +
c
      if (ida.lt.32) then
        xmonth = 1
      else if (ida.lt.61) then
        xmonth = 2
      else if (ida.lt.92) then
        xmonth = 3
      else if (ida.lt.122) then
        xmonth = 4
      else if (ida.lt.153) then
        xmonth = 5
      else if (ida.lt.183) then
        xmonth = 6
      else if (ida.lt.214) then
        xmonth = 7
      else if (ida.lt.245) then
        xmonth = 8
      else if (ida.lt.275) then
        xmonth = 9
      else if (ida.lt.306) then
        xmonth = 10
      else if (ida.lt.336) then
        xmonth = 11
      else
        xmonth = 12
      end if
c
      return
      end
