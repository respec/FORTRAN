*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/askpt.f,v 1.1 1998/07/07 19:32:00 grogers Exp $
*  askpt.f
*
      SUBROUTINE ASKPT (NCONV, KEY, NAME, IDLA, IMLA, SLA,
     +                  IDLO, IMLO, SLO, XPT, YPT, EOF, NOPT,dsel)

* Purpose: Interactively ask for the name and location of a point
*****************************************************************
*
*  $Log: askpt.f,v $
*  Revision 1.1  1998/07/07 19:32:00  grogers
*  PR#0, initial load of nadcon_lib
*
*

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)

      DOUBLE PRECISION XPT, YPT, RDLA, RDLO, DCARD
      DOUBLE PRECISION RMLA, RMLO, SLA, SLO
      INTEGER NCONV, KEY
      INTEGER IDLA, IMLA, IDLO, IMLO
      INTEGER IFLAG1, IFLAG2, N1, N2, IOS, LENG, IERR
      CHARACTER*80 NAME
      CHARACTER*40 ANS, B40, DUM
      LOGICAL EOF, NOPT,dsel

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      DATA IFLAG1 /1/, IFLAG2 /2/
      DATA B40 /'                                        '/
      NAME = '    '
      WRITE (LUOUT,*) ' What is the NAME for this station or',
     +                ' point?'
      READ (LUIN,'(A80)') NAME

**********
* LATITUDE
**********

      IF (NCONV .EQ. 0) THEN
        WRITE (LUOUT,110)
  110   FORMAT (/, ' Latitudes and Longitudes may be entered',
     +          ' in three formats:', /,
     +          '   (1) degrees, minutes, and decimal seconds, OR', /,
     +          '   (2) degrees, and decimal minutes OR', /,
     +          '   (3) decimal degrees.', /,
     +          ' Degrees, minutes and seconds may be separated',
     +          ' by blanks or commas.', /,
     +          ' A latitude or longitude of 0 will end data',
     +          ' entry.', /)
      ENDIF
      IF (KEY .EQ. 1) THEN
	IF(dsel) then
          WRITE (LUOUT,*) ' What is its NAD 27 latitude?'
	ELSE
	  WRITE (LUOUT,*) ' What is its NAD 83 latitude?'
	END IF
        WRITE (LUOUT,*) ' '
      ELSEIF (KEY .EQ. -1) THEN
        IF (NCONV .EQ. 1) THEN
          WRITE (LUOUT,110)
        ENDIF
	IF(dsel) then
	   
           WRITE (LUOUT,*) ' What is its NAD 83 latitude?'
	ELSE
	   WRITE (LUOUT,*) ' What is its HPGN latitude?'
	END IF
           WRITE (LUOUT,*) ' '
      ENDIF
      READ (LUIN,170,ERR=9930,IOSTAT=IOS) ANS
  170 FORMAT (A40)
      IF (ANS .EQ. B40) GOTO 9999

      DUM = ANS
      CALL NBLANK (DUM, IFLAG2, N2)
      LENG = N2
      RDLA = DCARD( DUM(1:N2), LENG, IERR )
      IF (IERR .NE. 0) GOTO 9950
      IF (LENG .GT. 0) THEN

        RMLA = DCARD( DUM, LENG, IERR )
        IF (IERR .NE. 0) GOTO 9950

        IF (LENG .GT. 0) THEN
          SLA  = DCARD( DUM, LENG, IERR )
          IF (IERR .NE. 0) GOTO 9950
        ELSE
          SLA = 0.D0
        ENDIF

      ELSE
        RMLA = 0.D0
        SLA = 0.D0
      ENDIF

      IF ( (RDLA .EQ. 0.D0)  .AND.  (RMLA .EQ. 0.D0)  .AND.
     +     (SLA .EQ. 0.D0) ) GOTO 9999

* Check for illogical values

      IF (RDLA .LT.   0.D0) GOTO 9940
      IF (RDLA .GT.  90.D0) GOTO 9950
      IF (RMLA .LT. 0.D0  .OR.  RMLA .GT. 60.D0) GOTO 9950
      IF ( SLA .LT. 0.D0  .OR.   SLA .GT. 60.D0) GOTO 9950

***********
* LONGITUDE
***********

      IF (KEY .EQ. 1) THEN
        WRITE (LUOUT,*) ' '
       IF(dsel) then
        WRITE (LUOUT,*) ' What is its NAD 27 longitude?',
     +                  '  (Longitude is positive west.)'
       ELSE
        WRITE (LUOUT,*) ' What is its NAD 83 longitude?',
     +                  '  (Longitude is positive west.)'
       END IF
        WRITE (LUOUT,*) ' '
      ELSEIF (KEY .EQ. -1) THEN
        WRITE (LUOUT,*) ' '
       IF(dsel) then
        WRITE (LUOUT,*) ' What is its NAD 83 longitude?',
     +                  '  (Longitude is positive west.)'
       ELSE
        WRITE (LUOUT,*) ' What is its HPGN longitude?',
     +                  '  (Longitude is positive west.)'
       END IF
      ENDIF

      READ (LUIN,170,ERR=9930,IOSTAT=IOS) ANS
      IF (ANS .EQ. B40) GOTO 9999

      DUM = ANS
      CALL NBLANK (DUM, IFLAG2, N2)
      LENG = N2
      RDLO = DCARD( DUM(1:N2), LENG, IERR )
      IF (IERR .NE. 0) GOTO 9960
      IF (LENG .GT. 0) THEN

        RMLO = DCARD( DUM, LENG, IERR )
        IF (IERR .NE. 0) GOTO 9960

        IF (LENG .GT. 0) THEN
          SLO  = DCARD( DUM, LENG, IERR )
          IF (IERR .NE. 0) GOTO 9960
        ELSE
          SLO = 0.D0
        ENDIF

      ELSE
        RMLO = 0.D0
        SLO = 0.D0
      ENDIF

      IF ( (RDLO .EQ. 0.D0)  .AND.  (RMLO .EQ. 0.D0)  .AND.
     +     (SLO .EQ. 0.D0) ) GOTO 9999

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

 9930 CONTINUE
      CALL NBLANK (ANS, IFLAG1, N1)
      CALL NBLANK (ANS, IFLAG2, N2)
      WRITE (LUOUT,9935) ANS(N1:N2)
 9935 FORMAT (' ERROR - in the answer:', /,
     +        9X, '''', A, '''', /,
     +        '         Must enter number in prescribed format!', /)
      NOPT = .TRUE.
      GOTO 9000

 9940 CONTINUE
      CALL NBLANK (ANS, IFLAG1, N1)
      CALL NBLANK (ANS, IFLAG2, N2)
      WRITE (LUOUT,9945) ANS(N1:N2)
 9945 FORMAT (' ERROR - in the answer:', /,
     +        9X, '''', A, '''', /,
     +        '         Latitude and Longitudes must be positive!', /,
     +        '         Longitude is positive west.', /)
      NOPT = .TRUE.
      GOTO 9000

 9950 CONTINUE
      CALL NBLANK (ANS, IFLAG1, N1)
      CALL NBLANK (ANS, IFLAG2, N2)
      WRITE (LUOUT,9955) ANS(N1:N2)
 9955 FORMAT (' ERROR - Illogical value for latitude in the answer:', /,
     +        '         ''', A, '''', /,
     +        '         Latitude must be between 0 and 90 degrees.', /,
     +        '         Minutes and seconds must be between 0',
     +                                                    ' and 60.', /)
      NOPT = .TRUE.
      GOTO 9000

 9960 CONTINUE
      CALL NBLANK (ANS, IFLAG1, N1)
      CALL NBLANK (ANS, IFLAG2, N2)
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
