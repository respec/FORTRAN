*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/print3.f,v 1.1 1998/07/07 19:32:32 grogers Exp $
*  print3.f
*
      SUBROUTINE PRINT3 (LU, IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +                   IDLA2, IMLA2, SLA2, IDLO2, IMLO2, SLO2,
     +                   KEY, FIRST, LAST, IPREC)

* Purpose: Prints out the actual transformation results
*******************************************************
*
*  $Log: print3.f,v $
*  Revision 1.1  1998/07/07 19:32:32  grogers
*  PR#0, initial load of nadcon_lib
*
*
* This subroutine prints out the actual transformation results using
* the Blue Book (type 3) format.
* The precision is indicated by the number of blanks in the seconds
* field.  The output precision will match the precision of the
* input seconds of arc of latitude (the precision of the seconds of
* arc of the longitude is ignored).

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION SLA, SLO, SLA2, SLO2
      INTEGER LU, KEY, IPREC
      INTEGER IDLA, IMLA, IDLO, IMLO
      INTEGER IDLA2, IMLA2, IDLO2, IMLO2
      INTEGER ISLA, ISLO, ISLA2, ISLO2
      CHARACTER*44 FIRST
      CHARACTER*30 LAST

      IF (IPREC .LT. 0  .OR.  IPREC .GT. 5) THEN
        WRITE (6, 666) IPREC
  666   FORMAT (/, ' ******** PROGRAMMING ERROR **********',
     +          /, ' ILLEGAL PRECISION VALUE IN SUBROUTINE PRINT3', I5,
     +          /, ' TRANSFORMED COORDINATES MAY BE INCORRECT!!')
      ENDIF

      IF (KEY .EQ. -1) THEN

**********************
* FOR NAD 27 TO NAD 83
c or  NAD 83 TO HPGN
**********************

        ISLA = IDINT( SLA*10**IPREC )
        ISLO = IDINT( SLO*10**IPREC )

        IF (IPREC .EQ. 0) THEN
          WRITE (LU,6000) FIRST, IDLA, IMLA, ISLA, 'N',
     +                           IDLO, IMLO, ISLO, 'W', LAST
        ELSEIF (IPREC .EQ. 1) THEN
          WRITE (LU,6001) FIRST, IDLA, IMLA, ISLA, 'N',
     +                           IDLO, IMLO, ISLO, 'W', LAST
        ELSEIF (IPREC .EQ. 2) THEN
          WRITE (LU,6002) FIRST, IDLA, IMLA, ISLA, 'N',
     +                           IDLO, IMLO, ISLO, 'W', LAST
        ELSEIF (IPREC .EQ. 3) THEN
          WRITE (LU,6003) FIRST, IDLA, IMLA, ISLA, 'N',
     +                           IDLO, IMLO, ISLO, 'W', LAST
        ELSEIF (IPREC .EQ. 4) THEN
          WRITE (LU,6004) FIRST, IDLA, IMLA, ISLA, 'N',
     +                           IDLO, IMLO, ISLO, 'W', LAST
        ELSEIF (IPREC .EQ. 5) THEN
          WRITE (LU,6005) FIRST, IDLA, IMLA, ISLA, 'N',
     +                           IDLO, IMLO, ISLO, 'W', LAST
        ENDIF

      ELSE

**********************
* FOR NAD 83 TO NAD 27
c or  HPGN   TO NAD 83
**********************

        ISLA2 = IDINT( SLA2*10**IPREC )
        ISLO2 = IDINT( SLO2*10**IPREC )

        IF (IPREC .EQ. 0) THEN
          WRITE (LU,6000) FIRST, IDLA2, IMLA2, ISLA2, 'N',
     +                           IDLO2, IMLO2, ISLO2, 'W', LAST
        ELSEIF (IPREC .EQ. 1) THEN
          WRITE (LU,6001) FIRST, IDLA2, IMLA2, ISLA2, 'N',
     +                           IDLO2, IMLO2, ISLO2, 'W', LAST
        ELSEIF (IPREC .EQ. 2) THEN
          WRITE (LU,6002) FIRST, IDLA2, IMLA2, ISLA2, 'N',
     +                           IDLO2, IMLO2, ISLO2, 'W', LAST
        ELSEIF (IPREC .EQ. 3) THEN
          WRITE (LU,6003) FIRST, IDLA2, IMLA2, ISLA2, 'N',
     +                           IDLO2, IMLO2, ISLO2, 'W', LAST
        ELSEIF (IPREC .EQ. 4) THEN
          WRITE (LU,6004) FIRST, IDLA2, IMLA2, ISLA2, 'N',
     +                           IDLO2, IMLO2, ISLO2, 'W', LAST
        ELSEIF (IPREC .EQ. 5) THEN
          WRITE (LU,6005) FIRST, IDLA2, IMLA2, ISLA2, 'N',
     +                           IDLO2, IMLO2, ISLO2, 'W', LAST
        ENDIF

      ENDIF

      RETURN
 6000 FORMAT (A44, I2.2, I2.2, I2.2, 5X, A1,
     +             I3.3, I2.2, I2.2, 5X, A1, A11)
 6001 FORMAT (A44, I2.2, I2.2, I3.3, 4X, A1,
     +             I3.3, I2.2, I3.3, 4X, A1, A11)
 6002 FORMAT (A44, I2.2, I2.2, I4.4, 3X, A1,
     +             I3.3, I2.2, I4.4, 3X, A1, A11)
 6003 FORMAT (A44, I2.2, I2.2, I5.5, 2X, A1,
     +             I3.3, I2.2, I5.5, 2X, A1, A11)
 6004 FORMAT (A44, I2.2, I2.2, I6.6, 1X, A1,
     +             I3.3, I2.2, I6.6, 1X, A1, A11)
 6005 FORMAT (A44, I2.2, I2.2, I7.7, A1,
     +             I3.3, I2.2, I7.7, A1, A11)
      END
