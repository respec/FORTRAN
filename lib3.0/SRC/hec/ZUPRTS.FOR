      SUBROUTINE ZUPRTS (IFLTAB, JULS, ISTIME, INTL, JULSD,
     * NSTART, NDATA, NVALS, VALUES, JQUAL, LQUAL, IPLAN, LERR)
C
C     Update Regular Interval Time-Series Data
C     Accomplishes essentially a ZWRITE
C
C     This is an internal subroutine called by ZSRTS and ZPTDTS
C
C     Written by Bill Charley at HEC, 1988.
C
C
C     Passed arguments
C     INTEGER*6 IFLTAB(*), IOFF, IADD, JQUAL(*)                         H
C     INTEGER IFLTAB(*), JQUAL(*)                                       u
      INTEGER*4 IFLTAB(*), IOFF, IADD, JQUAL(*)                         ML
      INTEGER*4 JULS, JULSD, INTL                                       ML
      REAL VALUES(*)
      LOGICAL LEQNER, LERR, LQUAL
C
      CHARACTER CDATE1*9, CDATE2*9
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSCZ                                                           H
      INCLUDE 'zdsscz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
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
C
C
      IF (MLEVEL.GE.9) WRITE (MUNIT,20) IFLTAB(KUNIT)                   D
 20   FORMAT (T8,'----DSS---Debug:  Enter ZUPRTS;  Unit:',I4)           D
C
      LERR = .FALSE.
C
C     This record has already been checked, and there is no
C     increase in the size of the data.
C
C     Update last written date, time, version, and program
      CALL CHRHOL (CDATE, 1, NDATEC, INFO(NPPWRD+KIDATE), 1)
      CALL CHRHOL (CTIME, 1, NTIMEC, INFO(NPPWRD+KITIME), 1)
      INFO(NPPWRD+KIVER) = INFO(NPPWRD+KIVER) + 1
      INFO(NPPWRD+KIPREC) = IPREC
      CALL CHRHOL (CPROG, 1, NPROGC, INFO(NPPWRD+KIPROG), 1)
C
C     Now store the updated information block
      CALL ZPTREC (IFLTAB, INFO, NPPWRD+NINFO,
     * IPNBIN(JPNBIN+NPPWRD+KBAINF), .TRUE.)
C
C     Read the first word of the header array to check
C     for the same time offset
C?    CALL ZGTREC (IFLTAB, IOFF, 1, INFO(NPPWRD+KIAIHE))
C?    IF ((IOFF.NE.ISOFF).AND.(MLEVEL.GE.3))
C?   * WRITE (MUNIT,50) IOFF, ISOFF
C50   FORMAT (' -----DSS--- ZSRTS:  Caution - Writing to a'
C?   * ' Record with a different time offset',/,
C?   * ' Current Offset:',I8,'  minutes;  New Offset:',I8,' minutes')
C
C     Compute the location of the data that we need to store
C
C     Obtain the number of periods from the start of the stored
C     record to the current data position
      NPOS = NOPERS (INTL, 0, JULSD, 0, JULS, ISTIME) + NSTART - 1
      IF (NPOS.LT.1) GO TO 910
C
C     Find number of data to store for this record (NSTORE)
C
C     End of stored record controling
      NSTORE = NDATA - NPOS + 1
C     End of data to store controling
      J = NVALS - NSTART + 1
      IF (J.LT.NSTORE) NSTORE = J
C
      IF (MLEVEL.GE.9) THEN                                             D
      CALL JULDAT (JULS, 114, CDATE1, N)                                D
      CALL JULDAT (JULSD, 114, CDATE2, N)                               D
      WRITE (MUNIT,40) NSTART, NVALS, JULS, ISTIME, NDATA, JULSD,       D
     * CDATE1, CDATE2, LQUAL, LTOL, IPLAN                               D
 40   FORMAT (T10,'NSTART:',I5,T25,'NVALS:',I5,T40,'JULS:',I8,          D
     * T55,'ISTIME:',I6,/,T10,'NDATA:',I5,T25,'JULSD:',I8,              D
     */,T10,'JULS: ',A,T30,'JULSD: ',A,/,                               D
     * T10, 'Quality Flag: ',L1,'   Tol: ',L1,'   Plan:',I3)            D
C
      WRITE (MUNIT,45) NPOS, NDATA, NSTORE                              D
 45   FORMAT (T10,' --- ZUPRTS Calculations:   ',                       D
     *'NPOS:',I5,'   NDATA:',I5,'   NSTORE:',I5)                        D
      ENDIF                                                             D
C
C
C     If we don't have data quality flags, store data directly
C
      IF (.NOT.LQUAL) THEN
C     Compute starting address of data to store
      IADD = INFO(NPPWRD+KIADAT) + NPOS - 1
C
C     Does IPLAN require us the read the current data first?
      IF (((IPLAN.EQ.0).OR.(IPLAN.EQ.2)).AND.(.NOT.LTOL)) THEN
C     No - just store the data
      CALL ZPTREC (IFLTAB, VALUES(NSTART), NSTORE, IADD, .FALSE.)
      ELSE
      CALL ZGTREC (IFLTAB, BUFF, NSTORE, IADD, .TRUE.)
      J = NSTART - 1
      IF (IPLAN.EQ.1) THEN
      DO 60 I=1,NSTORE
      J = J + 1
      IF (BUFF(I).EQ.-901.) BUFF(I) = VALUES(J)
 60   CONTINUE
      ELSE IF (IPLAN.EQ.4) THEN
      DO 80 I=1,NSTORE
      J = J + 1
      IF (VALUES(J).NE.-901.) BUFF(I) = VALUES(J)
 80   CONTINUE
      ELSE IF (LTOL) THEN
      DO 90 I=1,NSTORE
      J = J + 1
      IF (.NOT.(LEQNER (BUFF(I), VALUES(J), TOL))) BUFF(I) = VALUES(J)
 90   CONTINUE
      ELSE
      DO 95 I=1,NSTORE
      J = J + 1
      BUFF(I) = VALUES(J)
 95   CONTINUE
      ENDIF
      CALL ZPTREC (IFLTAB, BUFF, NSTORE, IADD, .FALSE.)
      ENDIF
C
      ELSE
C
C     We have data quality flags
C     Compute starting address of data to store
      IADD = INFO(NPPWRD+KIADAT) + ((NPOS-1) * 2)
      ISTORE = NSTORE * 2
C
C     Does IPLAN require us the read the current data first?
      IF ((IPLAN.EQ.0).OR.(IPLAN.EQ.2)) THEN
C     No - just store the data
C     Prepare the array to store
      DO 100 I=1,NSTORE
      J = NSTART + I - 1
      K = ((I - 1) * 2) + 1
      BUFF(K) = VALUES(J)
      K = K + 1
      ILBUFF(K) = JQUAL(J)
 100  CONTINUE
      CALL ZPTREC (IFLTAB, BUFF, ISTORE, IADD, .FALSE.)
C
      ELSE
      CALL ZGTREC (IFLTAB, BUFF, ISTORE, IADD, .TRUE.)
C
      IF (IPLAN.EQ.1) THEN
      DO 120 I=1,NSTORE
      J = NSTART + I - 1
      K = ((I - 1) * 2) + 1
      IF (BUFF(K).EQ.-901.) THEN
      BUFF(K) = VALUES(J)
      K = K + 1
      ILBUFF(K) = JQUAL(J)
      ENDIF
 120  CONTINUE
C
      ELSE
      DO 140 I=1,NSTORE
      J = NSTART + I - 1
      K = ((I - 1) * 2) + 1
      IF (VALUES(J).NE.-901.) THEN
      BUFF(K) = VALUES(J)
      K = K + 1
      ILBUFF(K) = JQUAL(J)
      ENDIF
 140  CONTINUE
      ENDIF
C
      CALL ZPTREC (IFLTAB, BUFF, ISTORE, IADD, .FALSE.)
      ENDIF
C
      ENDIF
C
C
C     Update the position of VALUES for the next record
      NSTART = NSTART + NSTORE
C
C     Update the file's last written date, if not already done
      IF (IFLTAB(KFILEW).EQ.1) THEN
      CALL CHRHOL (CDATE, 1, NDATEC, IFLTAB(KLWDAT), 1)
      CALL CHRHOL (CTIME, 1, NTIMEC, IFLTAB(KLWTIM), 1)
      IFLTAB(KFILEW) = 0
      IADD = 1
      CALL ZPTREC (IFLTAB, IFLTAB(KPERM), NPERM, IADD, .FALSE.)
      ENDIF
C
 800  CONTINUE
      IF (MLEVEL.GE.12) WRITE (MUNIT,820)                               D
 820  FORMAT (T8,'----DSS---Debug:  Exit  ZUPRTS')                      D
      RETURN
C
C
C     Probably will never get here - a terse error message is ok
C     (This error will be picked up by ZMOVBK!)
 910  CONTINUE
      WRITE (MUNIT,911) JULS, ISTIME
 911  FORMAT (/,' ****ERROR - ZUPRTS:  Illegal starting date/time',/,
     * ' Julian date:',I8,'  Time:',I8,/)
      LERR = .TRUE.
      GO TO 800
C
      END
