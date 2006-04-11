      SUBROUTINE ZMOVBK ( VALUES, IQUAL, LQVALS, LQBLOK, NSTART, NVALS,
     * JULS, ISTIME, INTL, BLOCK, IBLOCK, NDATA, JULSD, ISTAT, IFLAG)
C
C     INTERNAL SUBROUTINE FOR ZPTDTS AND ZGTDTS
C     GIVEN A BUFFER OF INFO, WHICH STARTS AT PERIOD NSTART,
C     AND ENDS AT NVALS, WITH THE TIME FOR PERIOD 1 OF JULS
C     DAYS AND ISTIME MINUTES.
C
C     PUT INFO IN ARRAY BLOCK AND UP TO MAX OF NDATA, WITH
C     TIME FOR LOCATION 1 OF JULSD.  INTERVAL OF VALUES
C     IS INTL IN MINUTES.
C
C     VALUES is the array passed back to the users routine
C     BLOCK is the array read or written to the DSS file
C
C     IF IFLAG = 1, MOVE VALUES INTO BLOCK
C     IF IFLAG = -1, UPDATE VALUES INTO BLOCK
C
C
      REAL VALUES(*), BLOCK(*)
C     INTEGER*6 IQUAL(*), IBLOCK(*)                                     H
      INTEGER*4 IQUAL(*), IBLOCK(*)                                     ML
C     INTEGER IQUAL(*), IBLOCK(*)                                       u
      INTEGER*4 JULS, INTL, JULSD                                       ML
C
      CHARACTER CDATE1*9, CDATE2*9
      LOGICAL LQVALS, LQBLOK, LEQNER
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
C
C
      ISTAT = 0
      IF (MLEVEL.GE.9) THEN                                             D
      CALL JULDAT (JULS, 114, CDATE1, N)                                D
      CALL JULDAT (JULSD, 114, CDATE2, N)                               D
      WRITE (MUNIT,10) NSTART, NVALS, JULS, ISTIME, NDATA, JULSD,       D
     * CDATE1, CDATE2, LQVALS, LQBLOK                                   D
 10   FORMAT (T10,'----- ENTERING ZMOVBK ',                             D
     */,T5,'NSTART:',I5,T20,'NVALS:',I5,/,T5,'JULS:',I5,                D
     *T20,'ISTIME:',I5,/,T5,'NDATA:',I5,T20,'JULSD:',I5,                D
     */,T5,'JULS:',A,T25,'JULSD:',A,/,T5,                               D
     * 'Value Quality Flag: ',L1,',  Block Qualtiy Flag: ',L1)          D
      ENDIF                                                             D
C
C
C     GET THE NUMBER OF PERIODS FROM START OF BLOCK TO VALUES
      NPOS = NOPERS (INTL, 0, JULSD, 0, JULS, ISTIME) + NSTART - 1
      IF (NPOS.LT.1) GO TO 910
C
C     Find Upper Limit
C
      NSTORE = NDATA - NPOS + 1
      J = NVALS - NSTART + 1
      IF (J.LT.NSTORE) NSTORE = J
      ILIM = NSTART + NSTORE - 1
C
      IF  (MLEVEL.GE.9) WRITE (MUNIT,25) NPOS, NDATA, NSTORE, ILIM      D
 25   FORMAT (T10,' --- ZMOVBK CALCULATIONS --',                        D
     */,T5,'NPOS:',I5,T20,'NDATA:',I5,                                  D
     */,T5,'NSTORE:',I5,T20,'ILIM:',I5)                                 D
C
C     Now move block
C     Prepare block to be stored (called by ZSRTS)
C
C     Is data quality used?
      IF ((.NOT.LQVALS).AND.(.NOT.LQBLOK)) THEN
      M = NPOS - 1
C
      DO 100 I=NSTART,ILIM
      M = M + 1
      IF (IFLAG.LT.0) THEN
      IF (IFLAG.EQ.-1) THEN
      IF ((BLOCK(M).GT.-900.0).OR.(BLOCK(M).LT.-910.0)) GO TO 100
      ELSE IF (IFLAG.EQ.-4) THEN
      IF ((VALUES(I).LT.-900.0).AND.(VALUES(I).GT.-910.0)) GO TO 100
      ENDIF
      ENDIF
      IF (LTOL) THEN
      IF (.NOT.(LEQNER (BLOCK(M), VALUES(I), TOL))) BLOCK(M) = VALUES(I)
      ELSE
      BLOCK(M) = VALUES(I)
      ENDIF
 100  CONTINUE
C
      ELSE
C
C     Data quality used
      M = ((NPOS - 1) * 2) - 1
      DO 150 I=NSTART,ILIM
      M = M + 2
      IF (IFLAG.LT.0) THEN
      IF (IFLAG.EQ.-1) THEN
      IF ((BLOCK(M).GT.-900.0).OR.(BLOCK(M).LT.-910.0)) GO TO 150
      ELSE IF (IFLAG.EQ.-4) THEN
      IF ((VALUES(I).LT.-900.0).AND.(VALUES(I).GT.-910.0)) GO TO 150
      ENDIF
      ENDIF
      IF (LTOL) THEN
      IF (.NOT.(LEQNER (BLOCK(M), VALUES(I), TOL))) BLOCK(M) = VALUES(I)
      ELSE
      BLOCK(M) = VALUES(I)
      ENDIF
      IF (LQVALS) THEN
      IBLOCK(M+1) = IQUAL(I)
      ENDIF
 150  CONTINUE
C
      ENDIF
C
C
C     Update pointers
      NSTART = ILIM + 1
      RETURN
C
C     ------ERROR CONDITIONS-------
C
 910  CONTINUE
      CALL JULDAT (JULS, 114, CDATE1, N)
      CALL JULDAT (JULSD, 114, CDATE2, N)
      WRITE (MUNIT, 911) JULS, ISTIME, NPOS, JULSD, CDATE1, CDATE2
 911  FORMAT (//,' ***** ERROR - ZMOVBK - Illegal Starting Date',
     +4(2X,I6)/,' Start Date: ',A,'  Block Date: ',A/)
      ISTAT = 21
      RETURN
C
      END
