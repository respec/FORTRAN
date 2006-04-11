      SUBROUTINE ZRTEXT (IFLTAB, CPATH, IUNIT, IUHEAD, KUHEAD, NUHEAD,
     * LCCNTL, NLINES, ISTAT)
C
C     Retrieve Text data, writing the text to unit IUNIT
C     If LCCNTL is true, place a space before each line
C     to act as carriage control
C     NLINES is returned with the number of lines retrieved
C
C     Written by Bill Charley at HEC, 1990
C
C     INTEGER*6 IFLTAB(*), IUHEAD(*), ICHEAD, NBYTES                    H
C     INTEGER IFLTAB(*), IUHEAD(*)                                      u
      INTEGER*4 IFLTAB(*), IUHEAD(*), ICHEAD, NBYTES                    ML
      INTEGER*4 JTYPE                                                   ML
      PARAMETER (KLINE=160)
      CHARACTER CPATH*(*), CLINE*(KLINE), CLINE2*(KLINE)
      LOGICAL LCCNTL, LFOUND
C
      COMMON /WORDS/ IWORD(10)
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
CADD C.ZDSSTS                                                           H
      INCLUDE 'zdssts.h'                                                MLu
C
C
C     IF (IFLTAB(1).EQ.4) THEN                                          H
C     WRITE (MUNIT, *)'ZRTEXT:  DSS file is not version 6!'             H
C     WRITE (MUNIT, *)'Text Data is only applicable to version 6'       H
C     ISTAT = -10                                                       H
C     RETURN                                                            H
C     ENDIF                                                             H
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL ZERROR (IFLTAB, 5, 'ZRTEXT',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
C
      ISTAT = 0
      CALL CHRLNB (CPATH, NPATH)
C
C     If a debug level is on, print out information
      IF (MLEVEL.GE.7) WRITE (MUNIT, 20) IUNIT, KUHEAD, LCCNTL,         D
     * CPATH(1:NPATH)                                                   D
 20   FORMAT (T5,'----- Enter ZRTEXT  -----',/,                         D
     * T11,'Write to Unit:',I4,'  User Header Dimension:',I5,           D
     * ',  Carriage Control:',L2,/,T11,'Pathname: ',A)                  D
C
      CALL ZREADX (IFLTAB, CPATH, NBYTES, 1, N, ICHEAD, 0, N,
     * IUHEAD, KUHEAD, NUHEAD, ILBUFF, KLBUFF, NBUFF, 0, LFOUND)
C
      IF (.NOT.LFOUND) THEN
      ISTAT = -1
      GO TO 800
      ENDIF
C
      CALL ZINQIR (IFLTAB, 'TYPE', CLINE, JTYPE)
      IF (JTYPE.NE.300) GO TO 900
C
C     Some day replace this with ZRDBUF!!!!
      IF (NBUFF.GT.KLBUFF) GO TO 910
C
      N = NBUFF * IWORD(2)
C     IF (NBYTES.GT.N) THEN ???
C
C
      JPOS = 1
      IPOS = 0
      NPOS = 0
      NLINES = 0
C     If the first character is a line feed, ignore it
      CALL HOLCHR (ILBUFF, 1, 1, CLINE, 1)
      IF (CLINE(1:1).EQ.CHAR(10)) JPOS = 2
C
 100  CONTINUE
      N = NBYTES - IPOS
      IF (N.LE.0) GO TO 800
      IF (N.GT.KLINE) N = KLINE
      CALL HOLCHR (ILBUFF, IPOS+1, N, CLINE, 1)
      IPOS = IPOS + N
C
 120  CONTINUE
C     Look for a carriage return
      I = INDEX (CLINE(JPOS:N), CHAR(13))
C
      IF (I.EQ.0) THEN
      CLINE2 = CLINE(JPOS:)
      NPOS = KLINE - JPOS + 1
      JPOS = 1
      GO TO 100
      ELSE
      KPOS = JPOS + I - 2
      ENDIF
C
      NLINES = NLINES + 1
      IF (NPOS.EQ.0) THEN
      IF (KPOS.LT.JPOS) THEN
C     Write out a blank line
      WRITE (IUNIT, *) ' '
      ELSE
      IF (LCCNTL) THEN
      WRITE (IUNIT,140, ERR=920, IOSTAT=JST) CLINE(JPOS:KPOS)
 140  FORMAT (1X,A,A)
      ELSE
      WRITE (IUNIT, 160, ERR=920, IOSTAT=JST) CLINE(JPOS:KPOS)
 160  FORMAT (A,A)
      ENDIF
      ENDIF
C
      ELSE
      IF (KPOS.LT.JPOS) THEN
      IF (LCCNTL) THEN
      WRITE (IUNIT,140, ERR=920, IOSTAT=JST) CLINE2(1:NPOS)
      ELSE
      WRITE (IUNIT, 160, ERR=920, IOSTAT=JST) CLINE2(1:NPOS)
      ENDIF
      ELSE
      IF (LCCNTL) THEN
      WRITE (IUNIT,140, ERR=920, IOSTAT=JST) CLINE2(1:NPOS),
     * CLINE(JPOS:KPOS)
      ELSE
      WRITE (IUNIT, 160, ERR=920, IOSTAT=JST) CLINE2(1:NPOS),
     * CLINE(JPOS:KPOS)
      ENDIF
      ENDIF
      NPOS = 0
      ENDIF
C
      JPOS = KPOS + 3
      IF (JPOS.GT.KLINE) THEN
      JPOS = JPOS - KLINE
      GO TO 100
      ELSE
      IF (JPOS.GT.N) GO TO 800
      GO TO 120
      ENDIF
C
 800  CONTINUE
      IF (MLEVEL.GE.7) WRITE (MUNIT,820) NBUFF, NBYTES, NLINES, ISTAT   D
 820  FORMAT(T5,'----- Exit ZRTEXT, Number of data values read:',I5,/,  D
     * T11,'Number Bytes:',I6,',  Number Lines:',I5,',  Status:',I5)    D
C
      RETURN
C
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,901) JTYPE, CPATH(1:NPATH)
 901  FORMAT (/,' *****DSS*** ZRTEXT:  ERROR - Record Not Identified',
     * ' as TEXT data.',/,' Data Type:',I5,/,' Pathname: ',A)
      ISTAT = -2
      GO TO 800
C
 910  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,911) IUNIT, NBUFF, KLBUFF,
     * CPATH(1:NPATH)
 911  FORMAT (/,' *****DSS*** ZRTEXT:  ERROR - Insufficient Buffer',
     * ' Space to Read Text Data',/,' Unit:',I5,',  Buffer Required',
     * I7,'  Buffer Supplied:',I7,/,' Pathname: ',A,/)
      ISTAT = -3
      GO TO 800
C
 920  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,921) IUNIT, JST, CPATH(1:NPATH)
 921  FORMAT (/,' *****DSS*** ZRTEXT:  ERROR During Write',
     * /,' Writing to Unit:',I5,',  Error:',I5,/,
     * ' Pathname: ',A,/)
      ISTAT = -4
      GO TO 800
C
      END
