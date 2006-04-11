*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/type3.f,v 1.1 1998/07/07 19:32:41 grogers Exp $
*  type3.f
*
      SUBROUTINE TYPE3 (IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +                  XPT, YPT, EOF, NOPT, FIRST, LAST, IPREC)

* Purpose: Read a record from a file of type 3 (Blue Book)
**********************************************************
*
*  $Log: type3.f,v $
*  Revision 1.1  1998/07/07 19:32:41  grogers
*  PR#0, initial load of nadcon_lib
*
*
* This format is defined in 'Input Formats and Specifications of the'
* National Geodetic Survey Data Base', Volume 1. Horizontal Control
* Data, and is available from the National Geodetic Survey for a
* fee by calling (301) 443-8631

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)
      CHARACTER*1 DOT
      PARAMETER ( DOT = '.' )

      DOUBLE PRECISION XPT, YPT
      DOUBLE PRECISION SLA, SLO
      INTEGER  IDLA, IMLA, IDLO, IMLO
      INTEGER IDOT, IPREC, IP1
      INTEGER IFLAG1, IFLAG2, N1, N2
      CHARACTER*44 FIRST
      CHARACTER*30 LAST
      CHARACTER*7 ASEC
      CHARACTER*1 DIRLAT, DIRLON
      LOGICAL EOF, NOPT

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      CHARACTER*80 CARD
      COMMON /CURNT/ CARD

      DATA IFLAG1 /1/, IFLAG2 /2/

***********************************
* FOR INPUT FILE OF ITYPE = 3
***********************************

      READ (NIN,6005,END=9999) CARD
 6005 FORMAT (A80)
      IF (CARD(8:9) .EQ. '80') THEN

* The station names and locations are in the *80* records

        READ (CARD,6000) FIRST, IDLA, IMLA, SLA, DIRLAT,
     +                   IDLO, IMLO, SLO, DIRLON, LAST
 6000   FORMAT (BZ, A44, I2, I2, F7.5, A1, I3, I2, F7.5, A1, A11)

* Check for illogical values

        IF (DIRLAT .NE. 'N'  .AND.  DIRLAT .NE. 'n') GOTO 9940
        IF (IDLA .LT.   0) GOTO 9940
        IF (IDLA .GT.  90) GOTO 9950
        IF (IMLA .LT. 0     .OR.  IMLA .GT. 60   ) GOTO 9950
        IF ( SLA .LT. 0.D0  .OR.   SLA .GT. 60.D0) GOTO 9950
        IF (DIRLON .NE. 'W'  .AND.  DIRLON .NE. 'w') GOTO 9940
        IF (IDLO .LT.   0) GOTO 9940
        IF (IDLO .GT. 360) GOTO 9950
        IF (IMLO .LT. 0     .OR.  IMLO .GT. 60   ) GOTO 9950
        IF ( SLO .LT. 0.D0  .OR.   SLO .GT. 60.D0) GOTO 9950

      ELSE
        WRITE (NOUT,6005) CARD
        NOPT = .TRUE.
        GOTO 9000
      ENDIF

      YPT = DBLE(IDLA) + DBLE(IMLA)/60.D0 + SLA/3600.D0
      XPT = DBLE(IDLO) + DBLE(IMLO)/60.D0 + SLO/3600.D0

* Get precision of seconds of latitude (i.e. the number of digits
* past the actual or implied decimal point). But since the output will
* have implied decimal points, the precision cannot be greater than 5.

      ASEC = CARD(49:55)

      IDOT = INDEX(ASEC(1:7), DOT)
   
      IF (IDOT .EQ. 0) THEN
        CALL NBLANK (ASEC(3:7), IFLAG2, N2)
        IPREC = N2
      ELSEIF (IDOT .LT. 7) THEN
        IP1 = IDOT + 1
        CALL NBLANK (ASEC(IP1:7), IFLAG2, N2)
        IPREC = N2
        IF (IDOT .EQ. 1  .AND.  IPREC .EQ. 6) IPREC = 5
      ELSEIF (IDOT .EQ. 7) THEN
        IPREC = 0
      ENDIF

 9000 RETURN

* Error messages

 9940 CONTINUE
      CALL NBLANK (CARD, IFLAG1, N1)
      CALL NBLANK (CARD, IFLAG2, N2)
      WRITE (LUOUT,9945) CARD(N1:N2)
 9945 FORMAT (' ERROR - in the following record:', /,
     +        9X, '''', A, '''', /,
     +        '         Latitude and Longitudes must be positive!', /,
     +        '         Longitude is positive west.', /)
      NOPT = .TRUE.
      GOTO 9000

 9950 CONTINUE
      CALL NBLANK (CARD, IFLAG1, N1)
      CALL NBLANK (CARD, IFLAG2, N2)
      WRITE (LUOUT,9955) CARD(N1:N2)
 9955 FORMAT (' ERROR - Illogical values for latitude or longitude',
     +        ' in the following record:', /,
     +        9X, '''', A, '''', /,
     +        '         This record will be skipped.', /)
      NOPT = .TRUE.
      GOTO 9000

 9999 CONTINUE
      EOF = .TRUE.
      GOTO 9000
      END
