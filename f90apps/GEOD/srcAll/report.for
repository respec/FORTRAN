*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/report.f,v 1.1 1998/07/07 19:32:34 grogers Exp $
*  report.f
*
      SUBROUTINE REPORT (LU, SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +                   SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +                   ADLAM, VDLAM, SDLAM, ADLOM, VDLOM, SDLOM,
     +                   ADLAS, VDLAS, SDLAS, ADLOS, VDLOS, SDLOS,
     +                   IPAGE, PAGE, KEY,dsel)

* Purpose: Prints out the statistics for the transformations
************************************************************
*
*  $Log: report.f,v $
*  Revision 1.1  1998/07/07 19:32:34  grogers
*  PR#0, initial load of nadcon_lib
*
*

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION ADLAM, VDLAM, ADLOM, VDLOM, SDLAM, SDLOM
      DOUBLE PRECISION ADLAS, VDLAS, ADLOS, VDLOS, SDLAS, SDLOS
      DOUBLE PRECISION SMDLAM, BGDLAM, SMDLOM, BGDLOM
      DOUBLE PRECISION SMDLAS, BGDLAS, SMDLOS, BGDLOS
      INTEGER LU, IPAGE, KEY
      LOGICAL PAGE,dsel

************
* NEW PAGE
************

      WRITE (LU,2)
*   2 FORMAT ('1')
    2 FORMAT ('')

      IF (PAGE) THEN

******************
* NUMBER THIS PAGE
******************

        WRITE (LU,15)  IPAGE
   15   FORMAT (70(' '), 'Page ', I4, /)
      ENDIF

* Print out the statistics. Note that the statistics are all gathered
* in terms of NAD 27 to NAD 83 conversions.  Thus, the signs
* for the range and mean values must be changed for the
* NAD 83 to NAD 27 conversions.

      IF (KEY .EQ. 1) THEN
	if(dsel) then
           WRITE (LU,90)
   90      FORMAT (28X, 'NAD 27 to NAD 83 Conversion')
	else
           WRITE (LU,92)
   92      FORMAT (28X, 'NAD 83 to HPGN Conversion')
	end if
        WRITE (LU,100)
  100   FORMAT (//, 30X, 'Statistics for Region', /, 1X, 79('='), /)
        WRITE (LU,900)
  900   FORMAT (36X, 'Latitude', 17X, 'Longitude')
	if(dsel) then
          WRITE (LU,910)
  910     FORMAT ('  NAD 83 - NAD 27 shifts',
     +           8X, 'MIN', 6X, 'MAX', 14X, 'MIN', 6X, 'MAX')
	else
          WRITE (LU,920)
  920     FORMAT ('  HPGN    - NAD 83 shifts',
     +           8X, 'MIN', 6X, 'MAX', 14X, 'MIN', 6X, 'MAX')
	end if
        WRITE (LU,110) SMDLAM, BGDLAM, SMDLOM, BGDLOM
  110   FORMAT ('  Range of shift (meters)   ', 2F9.3, 8X, 2F9.3)
        WRITE (LU,120) SMDLAS, BGDLAS, SMDLOS, BGDLOS
  120   FORMAT ('  Range of shift (seconds)  ', 2F9.3, 8X, 2F9.3)
        WRITE (LU,130) ADLAM, ADLOM
  130   FORMAT (/, 10X, 'Mean shift (meters)     ', F9.3, 17X, F9.3)
        WRITE (LU,131) VDLAM, VDLOM
  131   FORMAT (10X, 'Variance of mean shift  ', F9.3, 17X, F9.3)
        WRITE (LU,132) SDLAM, SDLOM
  132   FORMAT (10X, 'Std. Dev. of mean shift ', F9.3, 17X, F9.3)
        WRITE (LU,133) ADLAS, ADLOS
  133   FORMAT (/, 10X, 'Mean shift (seconds)    ', F9.3, 17X, F9.3)
        WRITE (LU,134) VDLAS, VDLOS
  134   FORMAT (10X, 'Variance of mean shift  ', F9.3, 17X, F9.3)
        WRITE (LU,135) SDLAS, SDLOS
  135   FORMAT (10X, 'Std. Dev. of mean shift ', F9.3, 17X, F9.3)
      ELSE
	if(dsel) then
          WRITE (LU,91)
   91     FORMAT (28X, 'NAD 83 to NAD 27 Conversion')
	else
          WRITE (LU,191)
  191     FORMAT (28X, 'HPGN   to NAD 83 Conversion')
	end if
        WRITE (LU,100)
        WRITE (LU,900)
	if(dsel) then
          WRITE (LU,911)
  911     FORMAT ('  NAD 27 - NAD 83 shifts',
     +           8X, 'MIN', 6X, 'MAX', 14X, 'MIN', 6X, 'MAX')
	else
          WRITE (LU,921)
  921     FORMAT ('  NAD 83 - HPGN   shifts',
     +           8X, 'MIN', 6X, 'MAX', 14X, 'MIN', 6X, 'MAX')
	end if
          WRITE (LU,110) -BGDLAM, -SMDLAM, -BGDLOM, -SMDLOM
        WRITE (LU,120) -BGDLAS, -SMDLAS, -BGDLOS, -SMDLOS
        WRITE (LU,130) -ADLAM, -ADLOM
        WRITE (LU,131) VDLAM, VDLOM
        WRITE (LU,132) SDLAM, SDLOM
        WRITE (LU,133) -ADLAS, -ADLOS
        WRITE (LU,134) VDLAS, VDLOS
        WRITE (LU,135) SDLAS, SDLOS
      ENDIF

      RETURN
      END
