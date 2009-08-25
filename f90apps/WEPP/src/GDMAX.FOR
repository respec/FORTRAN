      subroutine gdmax(istart,iend,sumgd,ityp)
c
c     + + + PURPOSE + + +
c     If the user does not input the number of growing degree days
c     in the growing season (gddmax), this subroutine computes the
c     growing degree days between two julian dates (istart and iend).
c     Monthly ave. max and min temperatures (obmint & obmaxt), and
c     threshold temperatures for crop growth (btemp) are read from
c     common blocks.  All years are handled as if they are leap
c     years.
c
c     Called from YOPTP and YLDOPT
c     Author(s): Arnold, Meyer
c     Reference in User Guide:
c
c     Changes:
c            1) Completely re-coded this routine.  Original one
c               set up a do-loop which was stepped through for
c               EACH DAY from ISTART to IEND.  Each time through
c               the loop, XMONTH was called.  I recoded it such
c               we dealt with the degree days in the initial
c               partial month, then those from the complete
c               months, and finally those from the final incomplete
c               month.  The call to XMONTH was eliminated.
c               CRM -- 7/07/92.
c
c
c     Version: This module recoded from Arnold's WEPP Post-version 92.25.
c     Date recoded: 07/07/92.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxcrp.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer istart, iend, ityp
      real sumgd
c
c     + + + ARGUMENT DEFINITIONS + + +
c     istart - Julian date for start of growing season.
c     iend   - Julian date for end of growing season.
c     ityp   - index of crop type.
c     sumgd  - sum of the growing degree days in the season.
c
c     + + + COMMON BLOCKS + + +
      include 'cobclim.inc'
c       read: obmint(12), obmaxt(12)
      include 'ccrpprm.inc'
c       read: btemp(10)
c
c     + + + LOCAL VARIABLES + + +
      integer juldat(13), iendaz, ienmon, istdaz, istmon, month,
     1    monlen(12)
      real gdd, tave
c     + + + LOCAL DEFINITIONS + + +
c     juldat - cumulative days to beginning of each month (through the
c              "13th" month; ie, the end of the 12th month).
c     monlen - length of each month (days).
c
c     + + + FUNCTION DECLARATIONS + + +
c
c     + + + DATA INITIALIZATIONS + + +
      data juldat /0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305,
     1    335, 366/
      data monlen /31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
c
c     + + + END SPECIFICATIONS + + +
c
c
c
c     write (6,*) " ISTART:", istart, " IEND:", iend, "-->", iend-istart
c
c ---- Find starting month.
      istmon = 13
c     *** BEGIN L0 LOOP ***
   10 continue
      istmon = istmon - 1
      if (istart.le.juldat(istmon)) go to 10
c     *** END L0 LOOP ***
c     ---- Determine number of days covered in starting month; ie, the number
c     from (ISTART - 1) to the end of the month.
      istdaz = juldat(istmon+1) - istart + 1
c
c     ---- Find ending month.
      ienmon = 13
c     *** BEGIN M0 LOOP ***
   20 continue
      ienmon = ienmon - 1
c     *** END M0 LOOP ***
      if (iend.le.juldat(ienmon)) go to 20
c
c     ---- Determine number of days covered in ending month; ie, the number
c     from the end of the preceeding month to IEND.
      iendaz = iend - juldat(ienmon)
c
      sumgd = 0.0
c
c     --- Compute growing degree days (gdd) contributed by starting month.
      tave = (obmaxt(istmon)+obmint(istmon)) / 2.0
      if (tave.gt.btemp(ityp)) then
        gdd = tave - btemp(ityp)
        sumgd = sumgd + gdd * istdaz
      end if
c
c     --- Compute growing degree days (gdd) contributed by months between
c     starting and ending months.
c
      do 30 month = istmon + 1, ienmon - 1
        tave = (obmaxt(month)+obmint(month)) / 2.0
        if (tave.gt.btemp(ityp)) then
          gdd = tave - btemp(ityp)
          sumgd = sumgd + gdd * monlen(month)
        end if
   30 continue
c
c     --- Compute growing degree days (gdd) contributed by ending month.
      tave = (obmaxt(ienmon)+obmint(ienmon)) / 2.0
      if (tave.gt.btemp(ityp)) then
        gdd = tave - btemp(ityp)
        sumgd = sumgd + gdd * iendaz
      end if
c
      return
      end
