*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/type1.f,v 1.1 1998/07/07 19:32:39 grogers Exp $
*  type1.f
*
      SUBROUTINE TYPE1 (NAME, IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +                  XPT, YPT, EOF, NOPT)

*  Purpose: Read a lat-lon file of type 1 format
************************************************
*
*  $Log: type1.f,v $
*  Revision 1.1  1998/07/07 19:32:39  grogers
*  PR#0, initial load of nadcon_lib
*
*
* Read a record from a file of type 1. In this type there is a station
* name (or blanks) in columns 1-40, and free-format latitude and
* longitude values in columns 41-80.  By free format we mean that the
* numbers making up the degrees, minutes and seconds of latitude,
* degrees, minutes, seconds of longitude must appear in that order in
* columns 41 through 80 but are not restricted to any specific columns.
* The latitude and longitude may be either (1) integer degrees, integer
* minutes, decimal seconds, or (2) integer degrees, decimal minutes, or
* (3) decimal degrees.

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)
      CHARACTER*1 DOT, BLK
      PARAMETER ( DOT = '.', BLK = ' ' )
      CHARACTER*20 B20
      CHARACTER*80 B80
      PARAMETER (B20 = '                   ', B80 = B20//B20//B20//B20)

      DOUBLE PRECISION XPT, YPT, RDLA, RDLO, DCARD
      DOUBLE PRECISION RMLA, RMLO, SLA, SLO
      INTEGER  IDLA, IMLA, IDLO, IMLO
      INTEGER IDOT, IBLK, LENG, IERR
      INTEGER IFLAG1, IFLAG2, N1, N2
      CHARACTER*80 NAME
      CHARACTER*40 DUMLA, DUMLO
      LOGICAL EOF, NOPT

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      CHARACTER*80 CARD
      COMMON /CURNT/ CARD

      DATA IFLAG1 /1/, IFLAG2 /2/

***********************************
* FOR INPUT FILE OF ITYPE = 1
***********************************

    1 READ (NIN,'(A80)',END=9999) CARD
      READ (CARD(1:40), '(A40)') NAME

* Check for blank line

      IF (CARD .EQ. B80) GOTO 1

* Find position of the first decimal point (to indicate the last
* number in the latitude)

      IDOT = INDEX(CARD(41:80), DOT)

* Error - no decimal point

      IF (IDOT .EQ. 0) GOTO 9980

* find position of the first blank after the first decimal point (to
* indicate the blank after the last number in the latitude)

      IDOT = IDOT + 40
      IBLK = INDEX(CARD(IDOT+1:80), BLK)
      IBLK = IBLK + IDOT

      DUMLA = CARD(41:IBLK)
      LENG = IBLK - 41
      RDLA = DCARD( DUMLA, LENG, IERR )
      IF (IERR .NE. 0) GOTO 9950
      IF (LENG .GT. 0) THEN

        RMLA = DCARD( DUMLA, LENG, IERR )
        IF (IERR .NE. 0) GOTO 9950

        IF (LENG .GT. 0) THEN
          SLA  = DCARD( DUMLA, LENG, IERR )
          IF (IERR .NE. 0) GOTO 9950
        ELSE
          SLA = 0.D0
        ENDIF

      ELSE
        RMLA = 0.D0
        SLA = 0.D0
      ENDIF

* Check for illogical values

      IF (RDLA .LT.   0.D0) GOTO 9940
      IF (RDLA .GT.  90.D0) GOTO 9950
      IF (RMLA .LT. 0.D0  .OR.  RMLA .GT. 60.D0) GOTO 9950
      IF ( SLA .LT. 0.D0  .OR.   SLA .GT. 60.D0) GOTO 9950

***********
* LONGITUDE
***********

      DUMLO = CARD(IBLK+1:80)
      CALL NBLANK (DUMLO, IFLAG2, N2)
      LENG = N2
      RDLO = DCARD( DUMLO, LENG, IERR )
      IF (IERR .NE. 0) GOTO 9960
      IF (LENG .GT. 0) THEN

        RMLO = DCARD( DUMLO, LENG, IERR )
        IF (IERR .NE. 0) GOTO 9960

        IF (LENG .GT. 0) THEN
          SLO  = DCARD( DUMLO, LENG, IERR )
          IF (IERR .NE. 0) GOTO 9960
        ELSE
          SLO = 0.D0
        ENDIF

      ELSE
        RMLO = 0.D0
        SLO = 0.D0
      ENDIF

* Check for illogical values

      IF (RDLO .LT.   0.D0) GOTO 9940
      IF (RDLO .GT. 360.D0) GOTO 9960
      IF (RMLO .LT. 0.D0  .OR.  RMLO .GT. 60.D0) GOTO 9960
      IF ( SLO .LT. 0.D0  .OR.   SLO .GT. 60.D0) GOTO 9960

* Calculate decimal degrees

      YPT = RDLA + RMLA/60.D0 + SLA/3600.D0
      XPT = RDLO + RMLO/60.D0 + SLO/3600.D0

* Get degrees, minutes, seconds

      CALL HMS (YPT, IDLA, IMLA, SLA)
      CALL HMS (XPT, IDLO, IMLO, SLO)

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
 9955 FORMAT (' ERROR - Illogical values for latitude',
     +        ' in the following record:', /,
     +        9X, '''', A, '''', /,
     +        '         Latitude must be between 0 and 90 degrees.', /,
     +        '         Minutes and seconds must be between 0',
     +                                                    ' and 60.', /)
      NOPT = .TRUE.
      GOTO 9000

 9960 CONTINUE
      CALL NBLANK (CARD, IFLAG1, N1)
      CALL NBLANK (CARD, IFLAG2, N2)
      WRITE (LUOUT,9965) CARD(N1:N2)
 9965 FORMAT (' ERROR - Illogical values for longitude',
     +        ' in the following record:', /,
     +        9X, '''', A, '''', /,
     +        '         Longitude must be between 0 and 360 degrees.',/,
     +        '         Minutes and seconds must be between 0',
     +                                                    ' and 60.', /)
      NOPT = .TRUE.
      GOTO 9000

 9980 CONTINUE
      CALL NBLANK (CARD, IFLAG1, N1)
      CALL NBLANK (CARD, IFLAG2, N2)
      WRITE (LUOUT,9985) CARD(N1:N2)
 9985 FORMAT (' ERROR - The following record does not have a decimal',
     +        ' point in the latitude.', /,
     +        9X, '''', A, '''', /,
     +        '         In the free format a decimal point is used',
     +        ' to determine what is', /,
     +        '         the last number in the latitude.  Please',
     +        ' correct this record', /,
     +        '         and check all of the data in this file to',
     +        ' ensure that it follows', /,
     +        '         the correct format.', /)
      NOPT = .TRUE.
      GOTO 9000

 9999 CONTINUE
      EOF = .TRUE.
      GOTO 9000
      END
