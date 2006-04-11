*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/print1.f,v 1.1 1998/07/07 19:32:30 grogers Exp $
*  print1.f
*
      SUBROUTINE PRINT1 (LU, NCONV, NAME, VRSION, IDLA, IMLA, SLA,
     +                   IDLO, IMLO, SLO, IDLA2, IMLA2, SLA2,
     +                   IDLO2, IMLO2, SLO2, DLAM, DLOM, DLAS, DLOS,
     +                   RESP, IPAGE, PAGE, KEY,dsel)

* Purpose: Prints out the actual transformation results
*******************************************************
*
*  $Log: print1.f,v $
*  Revision 1.1  1998/07/07 19:32:30  grogers
*  PR#0, initial load of nadcon_lib
*
*
* This subroutine prints out the actual transformation results using
* a pretty format - not the same as the input file format (if there
* is one).  This subroutine is used by type-1 format input and
* interactive input

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION VRSION, AMAG
      DOUBLE PRECISION SLA, SLO, SLA2, SLO2
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      INTEGER LU, NCONV, IPAGE, KEY
      INTEGER IDLA, IMLA, IDLO, IMLO
      INTEGER IDLA2, IMLA2, IDLO2, IMLO2
      CHARACTER*15 RESP
      CHARACTER*80 NAME
      LOGICAL PAGE,dsel


      IF (NCONV .EQ. 1) THEN

********************
* FIRST PAGE HEADING
********************

        WRITE (LU,10) IPAGE
        WRITE (LU,5)
	IF(dsel) then
c nad 27, nad83
          IF (KEY .EQ. 1) THEN
            WRITE (LU,6)
          ELSE
            WRITE (LU,26)
          ENDIF
	ELSE
c nad 83, hpgn
	  IF(KEY.EQ.1) THEN
	    WRITE(LU,36)
	  ELSE
	    WRITE(LU,46)
	  END IF
	END IF

        WRITE (LU,7) VRSION
        WRITE (LU,8)
      ENDIF

      IF (PAGE) THEN
        IF (IPAGE .GT. 1) THEN
          WRITE (LU,2)
          WRITE (LU,10) IPAGE
        ENDIF
      ENDIF

   10 FORMAT (70(' '), 'Page ', I4, /)
    5 FORMAT (20X, '  North American Datum Conversion')
   36 FORMAT (20X, '         NAD 83 to HPGN'       )
   46 FORMAT (20X, '         HPGN   to NAD 83'     ) 
    6 FORMAT (20X, '         NAD 27 to NAD 83'     )
   26 FORMAT (20X, '         NAD 83 to NAD 27'     )
    7 FORMAT (20X, '    NADCON Program Version ', F4.2 )
    8 FORMAT (20X, '                          ', /, 1X, 79('=') )

*   2 FORMAT ('1')
    2 FORMAT ('')

      WRITE (LU,921) NCONV, RESP
      IF (NAME .NE. '    ') WRITE (LU,922) NAME
      WRITE (LU,900)
      IF(dsel) then
       WRITE (LU,923) IDLA, IMLA, SLA, IDLO, IMLO, SLO
       WRITE (LU,924) IDLA2, IMLA2, SLA2, IDLO2, IMLO2, SLO2
       IF (KEY .EQ. 1) THEN
         WRITE (LU,925) DLAS, DLOS
         WRITE (LU,927) DLAM, DLOM
       ELSE
         WRITE (LU,926) -DLAS, -DLOS
         WRITE (LU,927) -DLAM, -DLOM
       ENDIF
      ELSE
       WRITE (LU,933) IDLA, IMLA, SLA, IDLO, IMLO, SLO
       WRITE (LU,934) IDLA2, IMLA2, SLA2, IDLO2, IMLO2, SLO2
       IF (KEY .EQ. 1) THEN
         WRITE (LU,935) DLAS, DLOS
         WRITE (LU,927) DLAM, DLOM
       ELSE
         WRITE (LU,936) -DLAS, -DLOS
         WRITE (LU,927) -DLAM, -DLOM
       ENDIF
      END IF
       AMAG = DSQRT(DLAM**2 + DLOM**2)
       WRITE (LU,928) AMAG
c 930's values are for HPGN
  921 FORMAT (/, 27X, 'Transformation #: ', I4, 8X, 'Region: ', A15,/)
  922 FORMAT (2X, 'Station name:  ', A80, /)
  900 FORMAT (36X, 'Latitude', 17X, 'Longitude')
  933 FORMAT (2X, 'NAD 83 datum values:         ',
     +       (2x, I2, 1x, I2.2, F9.5, 10X, I3, 1X, I2.2, F9.5) )
  923 FORMAT (2X, 'NAD 27 datum values:         ',
     +       (2X, I2, 1X, I2.2, F9.5, 10X, I3, 1X, I2.2, F9.5) )
  934 FORMAT (2X, 'HPGN datum values:           ',
     +       (2X, I2, 1X, I2.2, F9.5, 10X, I3, 1X, I2.2, F9.5) )
  924 FORMAT (2X, 'NAD 83 datum values:         ',
     +       (2X, I2, 1X, I2.2, F9.5, 10X, I3, 1X, I2.2, F9.5) )
  925 FORMAT (2X, 'NAD 83 - NAD 27 shift values: ',
     +        6X, F9.5, 16X, F9.5, '(secs.)')
  935 FORMAT (2X, 'HPGN - NAD 83 shift values: ',
     +        6X, F9.5, 16X, F9.5, '(secs.)')
  926 FORMAT (2X, 'NAD 27 - NAD 83 shift values: ',
     +        6X, F9.5, 16X, F9.5, '(secs.)')
  936 FORMAT (2X, 'NAD 83 - HPGN shift values: ',
     +        6X, F9.5, 16X, F9.5, '(secs.)')
  927 FORMAT (37X, F8.3, 17X, F8.3, '  (meters)')
  928 FORMAT (2X, 'Magnitude of total shift:    ',
     +        19X, F8.3, '(meters)', /)

      RETURN
      END
