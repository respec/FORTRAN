      SUBROUTINE ZRRTSB (IFLTAB, JULS, ISTIME, INTL, JULSD, NSTART,
     * NLDATA, NADATA, NVALS, VALUES, JQUAL, LQBLOK, LQUAL, JCOMP,
     * LFOUND, ISTAT)
C
C     Get Regular Interval Time-Series Data Block
C     Accomplishes essentially a ZREAD
C
C     This is an internal subroutine called by ZRRTSX
C
C     Written by Bill Charley at HEC, 1989.
C
C
C     Passed arguments
C     INTEGER*6 IFLTAB(*), IADD, JQUAL(*)                               H
C     INTEGER IFLTAB(*), JQUAL(*)                                       u
      INTEGER*4 IFLTAB(*), IADD, JQUAL(*)                               ML
      INTEGER*4 JULS, INTL, JULSD, NELMS                                ML
      REAL VALUES(*)
      LOGICAL LFOUND, LQUAL, LQBLOK
C
      CHARACTER CDATE1*9, CDATE2*9
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
CADD C.ZDSSTS                                                           H
      INCLUDE 'zdssts.h'                                                MLu
C
CADD C.ZDSSDC                                                           H
      INCLUDE 'zdssdc.h'                                                MLu
C
C
C
      IF (MLEVEL.GE.9) WRITE (MUNIT,20) IFLTAB(KUNIT)                   D
 20   FORMAT (T8,'----DSS---Debug:  Enter ZRRTSB;  Unit:',I4)           D
C
C
C
C     Obtain the number of periods from the start of the stored
C     record to the current data position
      NPOS = NOPERS (INTL, 0, JULSD, 0, JULS, ISTIME) + NSTART - 1
      IF (NPOS.LT.1) GO TO 900
C
C     Find number of data to read for this record (NREAD)
C
C     End of stored record controlling
      NREAD = NLDATA - NPOS + 1
C     End of data desired controlling
      J = NVALS - NSTART + 1
      IF (J.LT.NREAD) NREAD = J
      ILIM = NSTART + NREAD - 1
C
      IF (MLEVEL.GE.9) THEN                                             D
      CALL JULDAT (JULS, 114, CDATE1, N)                                D
      CALL JULDAT (JULSD, 114, CDATE2, N)                               D
      WRITE (MUNIT,40) NSTART, NVALS, JULS, ISTIME, NLDATA, JULSD,      D
     * CDATE1, CDATE2, LQBLOK, LQUAL                                    D
 40   FORMAT (T10,'NSTART:',I5,T25,'NVALS:',I5,T40,'JULS:',I8,          D
     * T55,'ISTIME:',I6,/,T10,'NLDATA:',I5,T25,'JULSD:',I8,             D
     */,T10,'JULS: ',A,T30,'JULSD: ',A,/,                               D
     * T10, 'Quality Read: ',L1,',  Quality Requested:',L2)             D
C
      WRITE (MUNIT,45) NPOS, NLDATA, NREAD, ILIM                        D
 45   FORMAT (T10,'---ZRRTSB Calculations:  ',                          D
     *'NPOS:',I5,'  NDATA:',I5,'  NREAD:',I5,'  ILIM:',I5)              D
      ENDIF                                                             D
C
C
      IF (LFOUND) THEN
C
C     If the compression flag was set, uncompress the data
C
      IF (INFO(NPPWRD+KICOMP).GT.0) THEN
      JCOMP = INFO(NPPWRD+KICOMP)
C     Don't uncompress the data if we do not have enough room
      IF (INFO(NPPWRD+KILNDA).GT.745) GO TO 910
C     Get the compression header
      NDCH = INFO(NPPWRD+KINCHE)
      NDCH = MIN0 (NDCH, KDCH)
      CALL ZGTREC (IFLTAB, IDCH, NDCH, INFO(NPPWRD+KIACHE), .FALSE.)
C
C     Get the data block
      CALL ZGTREC (IFLTAB, BUFF(745), NADATA, INFO(NPPWRD+KIADAT),
     * .FALSE.)
C     Uncompress the data, with new data in buffer
      NELMS = -1
      CALL DUREAL(BUFF, 745, 1, NELMS, BUFF(745), NADATA, 745,
     * IDCH, NDCH, IST)
      IF (IST.NE.0) GO TO 910
C
      M = NPOS - 1
      DO 60 I=NSTART, ILIM
      M = M + 1
      VALUES(I) = BUFF(M)
 60   CONTINUE
C
      ELSE
C
C     No data compression...
C     If we don't have data quality flags, read data directly
C
      IF (.NOT.LQBLOK) THEN
C     Compute starting address of data
      IADD = INFO(NPPWRD+KIADAT) + NPOS - 1
      CALL ZGTREC (IFLTAB, VALUES(NSTART), NREAD, IADD, .FALSE.)
C
      IF (LQUAL) THEN
      DO 80 I=NSTART,ILIM
      JQUAL(I) = 0
 80   CONTINUE
      ENDIF
C
      ELSE
C
C     We have data quality flags
C     Compute starting address of data
      IADD = INFO(NPPWRD+KIADAT) + ((NPOS-1) * 2)
      IREAD = NREAD * 2
      CALL ZGTREC (IFLTAB, BUFF, IREAD, IADD, .FALSE.)
C
      DO 100 I=1,NREAD
      J = NSTART + I - 1
      K = ((I - 1) * 2) + 1
      VALUES(J) = BUFF(K)
      IF (LQUAL) THEN
      K = K + 1
      JQUAL(J) = ILBUFF(K)
      ENDIF
 100  CONTINUE
C
      ENDIF
C
      ENDIF
C
      ELSE
C     Record not found.  Fill in -902.'s
      DO 200 I=NSTART,ILIM
      VALUES(I) = -902.0
      IF (LQUAL) JQUAL(I) = 0
 200  CONTINUE
      ENDIF
C
C
C     Update the position of VALUES for the next record
      NSTART = NSTART + NREAD
C
C
 800  CONTINUE
      IF (MLEVEL.GE.12) WRITE (MUNIT,820)                               D
 820  FORMAT (T8,'----DSS---Debug:  Exit  ZRRTSB')                      D
      RETURN
C
C
C     Probably will never get here - a terse error message is ok
 900  CONTINUE
      WRITE (MUNIT,901) JULS, ISTIME
 901  FORMAT (/,' **** ERROR - ZRRTSB:  Illegal starting date/time',/,
     * ' Julian date:',I8,'  Time:',I8,/)
      ISTAT = 15
      GO TO 800
C
 910  CONTINUE
      WRITE (MUNIT,911) IST
 911  FORMAT (/,' **** ERROR - ZRRTSB:  Unable to Uncompress Data',/,
     * ' Status:',I6,/)
      ISTAT = 53
      GO TO 800
C
      END
