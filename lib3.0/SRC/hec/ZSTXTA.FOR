      SUBROUTINE ZSTXTA (IFLTAB, CPATH, CARRAY, NARRAY, IUHEAD,
     * NUHEAD, ISTAT)
C
C     Store text data passed as array CARRAY.
C
C     Written by Bill Charley at HEC, 1989
C
C     INTEGER*6 IFLTAB(*), IUHEAD(*), ICHEAD, NBYTES                    H
      INTEGER*4 IFLTAB(*), IUHEAD(*), ICHEAD, NBYTES                    ML
C     INTEGER IFLTAB(*), IUHEAD(*)                                      u
      CHARACTER CPATH*(*)
      CHARACTER CARRAY(NARRAY)*(*)
      LOGICAL LFOUND
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
      ISTAT = 0
      MAXBYT = KLBUFF * IWORD(2)
      CALL CHRLNB (CPATH, NPATH)
C
C     If a debug level is on, print out information
      IF (MLEVEL.GE.7) WRITE (MUNIT, 20) NARRAY, NUHEAD, CPATH(1:NPATH) D
 20   FORMAT (T5,'----- Enter ZSTXTA  -----',/,                         D
     * T11,'Number of Lines in Array:',I4,'  User Header Length:',I5,/  D
     * T11,'Pathname: ',A)                                              D
C
C
C     IF (IFLTAB(1).EQ.4) THEN                                          H
C     WRITE (MUNIT, *)'ZSTXTA:  DSS file is not version 6!'             H
C     WRITE (MUNIT, *)'Text Data is only applicable to version 6'       H
C     ISTAT = -10                                                       H
C     RETURN                                                            H
C     ENDIF                                                             H
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL ZERROR (IFLTAB, 5, 'ZXTXTA',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
C
      NBYTES = 0
      IPOS = 0
      NLINES = 0
 60   CONTINUE
      IF (NLINES.GE.NARRAY) GO TO 200
      NLINES = NLINES + 1
      CALL CHRLNB (CARRAY(NLINES), NLINE)
      IF (NLINE.EQ.0) NLINE = 1
C     Count the number of bytes to write (LF + NLINE + CR)
      NBYTES = IPOS + NLINE + 2
      IF (NBYTES.GT.MAXBYT) GO TO 910
      CALL CHRHOL (CHAR(10), 1, 1, ILBUFF, IPOS+1)
      CALL CHRHOL (CARRAY(NLINES), 1, NLINE, ILBUFF, IPOS+2)
      CALL CHRHOL (CHAR(13), 1, 1, ILBUFF, IPOS+NLINE+2)
      IPOS = NBYTES
      GO TO 60
C
 200  CONTINUE
      NVALS = ((NBYTES - 1) / IWORD(2)) + 1
      JTYPE = 300
C
      CALL ZWRITX (IFLTAB, CPATH, NPATH, NBYTES, 1, ICHEAD, 0,
     * IUHEAD, NUHEAD, ILBUFF, NVALS, JTYPE, 0, ISTAT, LFOUND)
C
C
 800  CONTINUE
      IF (MLEVEL.GE.7) WRITE (MUNIT,820) NVALS, NBYTES, NLINES, ISTAT   D
 820  FORMAT(T5,'----- Exit ZSTXTA, Number of data values ',            D
     * 'stored:',I7,/,T11,'Number Bytes:',I6,',  Number Lines:',I5,     D
     * ',  Status:',I4)                                                 D
C
      RETURN
C
C
 910  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,911) MAXBYT, CPATH(1:NPATH)
 911  FORMAT (/,' *****DSS*** ZSTXTA:  ERROR - Insufficient Buffer',
     * ' Space to Store Text Data',/',  Maximum Number',
     * 'of Bytes:',I7,/,' Pathname: ',A,/)
      ISTAT = -3
      GO TO 800
C
      END
