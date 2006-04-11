*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vtype1.f,v 1.1 1998/07/07 20:10:43 grogers Exp $
*  vtype1.f
*
      SUBROUTINE vTYPE1 (NAME, IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +                  XPT, YPT, EOF, NOPT)
*
* Purpose: Read a record from a file of type 1.
***********************************************
*
*  $Log: vtype1.f,v $
*  Revision 1.1  1998/07/07 20:10:43  grogers
*  PR#0, initial add of vertcon_lib
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
*
      CHARACTER*1 DOT, BLK
      PARAMETER ( DOT = '.', BLK = ' ' )
      CHARACTER*80  B80
      CHARACTER*20 B20
      PARAMETER (B20 = '                   ', B80 = B20//B20//B20//B20)
      DOUBLE PRECISION XPT, YPT, RDLA, RDLO, vDCARD
      REAL SLA, SLO, vRCARD, RMLA, RMLO
      INTEGER IFLAG1, IFLAG2, N1, N2
      INTEGER IDOT, IBLK, LENG, IERR
      INTEGER IDLA, IMLA, IDLO, IMLO
      CHARACTER*80 CARD
      CHARACTER*40 NAME
      CHARACTER*96 B96
      CHARACTER*40 DUMLA, DUMLO
      LOGICAL EOF, NOPT
      COMMON /vCURNT/ B96
      COMMON /vINOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, NSPACE(2)
      EQUIVALENCE(CARD,B96)
      DATA IFLAG1 /1/, IFLAG2 /2/
* FOR INPUT FILE OF ITYPE = 1
    1 READ (NIN,'(A80)',END=9999) CARD
* Check for blank line
      IF (CARD .EQ. B80) GOTO 1
      READ (CARD(1:40), '(A40)') NAME
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
      RDLA = vDCARD( DUMLA, LENG, IERR )
      IF (IERR .NE. 0) GOTO 9950
      IF (LENG .GT. 0) THEN
        RMLA = vRCARD( DUMLA, LENG, IERR )
        IF (IERR .NE. 0) GOTO 9950
        IF (LENG .GT. 0) THEN
          SLA  = vRCARD( DUMLA, LENG, IERR )
          IF (IERR .NE. 0) GOTO 9950
        ELSE
          SLA = 0.E0
        ENDIF
      ELSE
        RMLA = 0.E0
        SLA = 0.E0
      ENDIF
* Check for illogical values
      IF (RDLA .LT.   0.D0) GOTO 9940
      IF (RDLA .GT.  90.D0) GOTO 9950
      IF (RMLA .LT. 0.E0  .OR.  RMLA .GT. 60.E0) GOTO 9950
      IF ( SLA .LT. 0.E0  .OR.   SLA .GT. 60.E0) GOTO 9950
* LONGITUDE
      DUMLO = CARD(IBLK+1:80)
      CALL vNBLANK (DUMLO, IFLAG2, N2)
      LENG = N2
      RDLO = vDCARD( DUMLO, LENG, IERR )
      IF (IERR .NE. 0) GOTO 9960
      IF (LENG .GT. 0) THEN
        RMLO = vRCARD( DUMLO, LENG, IERR )
        IF (IERR .NE. 0) GOTO 9960
        IF (LENG .GT. 0) THEN
          SLO  = vRCARD( DUMLO, LENG, IERR )
          IF (IERR .NE. 0) GOTO 9960
        ELSE
          SLO = 0.E0
        ENDIF
      ELSE
        RMLO = 0.E0
        SLO = 0.E0
      ENDIF
* Check for illogical values
      IF (RDLO .LT.   0.D0) GOTO 9940
      IF (RDLO .GT. 360.D0) GOTO 9960
      IF (RMLO .LT. 0.E0  .OR.  RMLO .GT. 60.E0) GOTO 9960
      IF ( SLO .LT. 0.E0  .OR.   SLO .GT. 60.E0) GOTO 9960
* Calculate decimal degrees
      YPT = RDLA + DBLE(RMLA)/60.D0 + DBLE(SLA)/3600.D0
      XPT = RDLO + DBLE(RMLO)/60.D0 + DBLE(SLO)/3600.D0
* Get degrees, minutes, seconds
      CALL ANGLE (YPT, IDLA, IMLA, SLA)
      CALL ANGLE (XPT, IDLO, IMLO, SLO)
 9000 RETURN
* Error messages
 9940 CONTINUE
      CALL vNBLANK (CARD, IFLAG1, N1)
      CALL vNBLANK (CARD, IFLAG2, N2)
      WRITE (LUOUT,9945) CARD(N1:N2)
 9945 FORMAT (' ERROR - in the following record:', /,
     +        9X, '''', A, '''', /,
     +        '         Latitude and Longitudes must be positive!', /,
     +        '         Longitude is positive west.', /)
      NOPT = .TRUE.
      GOTO 9000
 9950 CONTINUE
      CALL vNBLANK (CARD, IFLAG1, N1)
      CALL vNBLANK (CARD, IFLAG2, N2)
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
      CALL vNBLANK (CARD, IFLAG1, N1)
      CALL vNBLANK (CARD, IFLAG2, N2)
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
      CALL vNBLANK (CARD, IFLAG1, N1)
      CALL vNBLANK (CARD, IFLAG2, N2)
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
