*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vaskpt.f,v 1.1 1998/07/07 20:10:29 grogers Exp $
*  vaskpt.f
*
      SUBROUTINE vASKPT (NCONV, NAME, IDLA, IMLA, SLA,
     +                  IDLO, IMLO, SLO, XPT, YPT, EOF, NOPT)
*
* Purpose: Ask for the name and location of a point
***************************************************
*
*  $Log: vaskpt.f,v $
*  Revision 1.1  1998/07/07 20:10:29  grogers
*  PR#0, initial add of vertcon_lib
*
*
      CHARACTER*40 B40
      PARAMETER (B40 = '                                        ')
      DOUBLE PRECISION XPT, YPT, RDLA, RDLO, vDCARD
      REAL SLA, SLO, RMLA, RMLO, vRCARD
      INTEGER NCONV
      INTEGER IDLA, IMLA, IDLO, IMLO
      INTEGER IFLAG1, IFLAG2, N1, N2
      INTEGER LENG, IERR, IOS
      CHARACTER*40 NAME
      CHARACTER*40 ANS, DUM
      LOGICAL EOF, NOPT
      COMMON /vINOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, NSPACE(2)
      DATA IFLAG1 /1/, IFLAG2 /2/
* LATITUDE
      IF (NCONV .EQ. 0) THEN
      WRITE (LUOUT,101) 
  101 FORMAT (15x,'INTERACTIVE PROCESSING',//'  Station Name,',
     +   ' Latitude and Longitude are entered -',//
     +   '  [station name entry is not required;',
     +   '   the position entries',/'   (i.e., latitude & longitude)',
     +   ' are identifed with generated'/,'   sequence numbers',
     +   ' when station name is not entered]'/)
        WRITE (LUOUT,110)
  110   FORMAT (' Latitudes and Longitudes may be entered',
     +          ' in three formats:', //
     +          '   (1) degrees, minutes, and decimal seconds, OR', /,
     +          '   (2) degrees, and decimal minutes OR', /,
     +          '   (3) decimal degrees.', /,
     +          '       [decimal points must be entered !]'/
     +          ' Degrees, minutes and seconds can be separated',
     +          ' by either blanks or commas.', //
     +          ' To terminate session :'/'  (1) key in ''n'' or',
     +          '''N'' for ''another computation ?'' prompt  - or -'/
     +          '  (2) press <RETURN> for latitude or longitude',
     +          ' prompt  - or -'/'  (3) enter <ctrl-c>'/)
      ENDIF
      NAME = '        '
      WRITE (LUOUT,*) ' Enter the NAME for this station',
     +                ' or press <RETURN> to skip NAME -'
      READ (LUIN,'(A40)') NAME
        WRITE (LUOUT,*) ' Enter the LATITUDE :'
        WRITE (LUOUT,*) ' '
      READ (LUIN,170,ERR=9930,IOSTAT=IOS) ANS
  170 FORMAT (A40)
      IF (ANS .EQ. B40) GOTO 9999
      DUM = ANS
      CALL vNBLANK (DUM, IFLAG2, N2)
      LENG = N2
      RDLA = vDCARD( DUM(1:N2), LENG, IERR )
      IF (IERR .NE. 0) GOTO 9950
      IF (LENG .GT. 0) THEN
        RMLA = vRCARD( DUM, LENG, IERR )
        IF (IERR .NE. 0) GOTO 9950
        IF (LENG .GT. 0) THEN
          SLA  = vRCARD( DUM, LENG, IERR )
          IF (IERR .NE. 0) GOTO 9950
        ELSE
          SLA = 0.E0
        ENDIF
      ELSE
        RMLA = 0.E0
        SLA = 0.E0
      ENDIF
      IF ( (RDLA .EQ. 0.D0)  .AND.  (RMLA .EQ. 0.E0)  .AND.
     +     (SLA .EQ. 0.E0) ) GOTO 9999
* Check for illogical values
      IF (RDLA .LT.   0.D0) GOTO 9940
      IF (RDLA .GT.  90.D0) GOTO 9950
      IF (RMLA .LT. 0.E0  .OR.  RMLA .GT. 60.E0) GOTO 9950
      IF ( SLA .LT. 0.E0  .OR.   SLA .GT. 60.E0) GOTO 9950
* LONGITUDE
      WRITE (LUOUT,*) ' Enter the LONGITUDE ',
     +                '  (longitude is positive west) :'
        WRITE (LUOUT,*) ' '
      READ (LUIN,170,ERR=9930,IOSTAT=IOS) ANS
      IF (ANS .EQ. B40) GOTO 9999
      DUM = ANS
      CALL vNBLANK (DUM, IFLAG2, N2)
      LENG = N2
      RDLO = vDCARD( DUM(1:N2), LENG, IERR )
      IF (IERR .NE. 0) GOTO 9960
      IF (LENG .GT. 0) THEN
        RMLO = vRCARD( DUM, LENG, IERR )
        IF (IERR .NE. 0) GOTO 9960
        IF (LENG .GT. 0) THEN
          SLO  = vRCARD( DUM, LENG, IERR )
          IF (IERR .NE. 0) GOTO 9960
        ELSE
          SLO = 0.E0
        ENDIF
      ELSE
        RMLO = 0.E0
        SLO = 0.E0
      ENDIF
      IF ( (RDLO .EQ. 0.D0)  .AND.  (RMLO .EQ. 0.E0)  .AND.
     +     (SLO .EQ. 0.E0) ) GOTO 9999
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
 9930 CONTINUE
      CALL vNBLANK (ANS, IFLAG1, N1)
      CALL vNBLANK (ANS, IFLAG2, N2)
      WRITE (LUOUT,9935) ANS(N1:N2)
 9935 FORMAT (' ERROR - in the answer:', /,
     +        9X, '''', A, '''', /,
     +        '         Must enter number in prescribed format!', /)
      NOPT = .TRUE.
      GOTO 9000
 9940 WRITE (LUOUT,9945)
      CALL vNBLANK (ANS, IFLAG1, N1)
      CALL vNBLANK (ANS, IFLAG2, N2)
      WRITE (LUOUT,9945) ANS(N1:N2)
 9945 FORMAT (' ERROR - in the answer:', /,
     +        9X, '''', A, '''', /,
     +        '         Latitude and Longitudes must be positive!', /,
     +        '         Longitude is positive west.', /)
      NOPT = .TRUE.
      GOTO 9000
 9950 CONTINUE
      CALL vNBLANK (ANS, IFLAG1, N1)
      CALL vNBLANK (ANS, IFLAG2, N2)
      WRITE (LUOUT,9955) ANS(N1:N2)
 9955 FORMAT (' ERROR - Illogical value for latitude in the answer:', /,
     +        '         ''', A, '''', /,
     +        '         Latitude must be between 0 and 90 degrees.', /,
     +        '         Minutes and seconds must be between 0',
     +                                                    ' and 60.', /)
      NOPT = .TRUE.
      GOTO 9000
 9960 CONTINUE
      CALL vNBLANK (ANS, IFLAG1, N1)
      CALL vNBLANK (ANS, IFLAG2, N2)
      WRITE (LUOUT,9965) ANS(N1:N2)
 9965 FORMAT (' ERROR - Illogical value for longitude in the answer:',/,
     +        '         ''', A, '''', /,
     +        '         Longitude must be between 0 and 360 degrees.',/,
     +        '         Minutes and seconds must be between 0',
     +                                                    ' and 60.', /)
      NOPT = .TRUE.
      GOTO 9000
 9999 CONTINUE
      EOF = .TRUE.
      GOTO 9000
      END
