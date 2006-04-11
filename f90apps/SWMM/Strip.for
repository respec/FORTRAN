      SUBROUTINE STRIP(N55)
C=======================================================================
C     THIS SUBROUTINE STRIPS COMMENT LINES FROM THE SWMM INPUT FILE
C     Note to Vax Programmers: An output format error will occur
C                              unless special provisions are made by the
C                              VAX operator.  An alternative is to change
C                              all occurrences of 230 to 120 in the
C                              STRIP subroutine.
C     Changed to allow 400 characters per line
C     Change in message to screen, WCH, 12/9/94.
C=======================================================================
      INCLUDE 'TAPES.INC'
      CHARACTER ICODE*1,JCODE*1,ISTAR*1,KCODE*400,DOLL*1,BLANK*1,LEND*10
      CHARACTER OUTSTR*402,SQUO*1
      DATA ISTAR/'*'/,DOLL/'$'/,BLANK/' '/,LEND/'''  ''ENDPR'/,
     ASQUO/''''/
      LOGICAL EXTRAN,RUNOFF,TRANSPORT,STATS
C=======================================================================
      WRITE(*,999)
      EXTRAN = .FALSE.
      RUNOFF = .FALSE.
	TRANSPORT = .FALSE.
	STATS  = .FALSE.
   10 CONTINUE
      READ(N55,1,END=101,ERR=101) ICODE,JCODE,KCODE
      IF(ICODE.EQ.ISTAR) GO TO 100
      IF (ICODE.EQ.DOLL) THEN
      IF (JCODE.EQ.'E'.and.KCODE(1:3).EQ.'XTR') EXTRAN = .TRUE.
      IF (JCODE.EQ.'R'.AND.KCODE(1:3).EQ.'UNO') RUNOFF = .TRUE.
	IF (JCODE.EQ.'S'.AND.KCODE(1:3).EQ.'TAT') STATS = .TRUE.
	IF (JCODE.EQ.'T'.AND.KCODE(1:3).EQ.'RAN') TRANSPORT = .TRUE.
      ENDIF
cim  further process KCODE to strip stuff after any additional asterisks
cim
      locast = index(kCODE,istar)
      locast =locast-1
      if(locast.le.0) THEN
        Locast = len(kCODE)
      ELSE
        KCODE = KCODE(1:LOCAST)
      ENDIF
CIM STRIP LEADING BLANKS FROM KCODE
      DO I=1,LOCAST
      IF (KCODE(I:I).NE.BLANK) GO TO 11
      ENDDO
C  ALL BLANK
      LOCAST = 1
      GO TO 20
   11 CONTINUE
C***WCH, 9/27/99.  OMIT THIS TASK TO SEE IF FORMATTED READS NOW WORK.
C      KCODE = KCODE(I:LOCAST)
C      LOCAST = LOCAST - I + 1
CIM FIND LAST BLANK TO THE LEFT OF LOCAST
CIM AND REMOVE TRAILING BLANKS
   12 IF (KCODE(LOCAST:LOCAST) .NE. BLANK) GO TO 13
	LOCAST=LOCAST - 1
	IF(LOCAST.GT.0) GO TO 12
C     ALLBLANKS
      LOCAST = 1
      GO TO 20
   13 CONTINUE
CIM  10/98  PAD KCODE WITH 10 ZEROS
cim  only lines with optional inputs
c  extran lines with optional inputs include B0, BA, BB, B1, D1, G1
      IF (EXTRAN.and.ICODE.EQ.'B'.AND.JCODE.EQ.'0') GO TO 15
      IF (EXTRAN.and.ICODE.EQ.'B'.AND.JCODE.EQ.'A') GO TO 15
      IF (EXTRAN.and.ICODE.EQ.'B'.AND.JCODE.EQ.'B') GO TO 15
      IF (EXTRAN.and.ICODE.EQ.'B'.AND.JCODE.EQ.'1') GO TO 15
      IF (EXTRAN.and.ICODE.EQ.'D'.AND.JCODE.EQ.'1') GO TO 15
      IF (EXTRAN.AND.ICODE.EQ.'E'.AND.JCODE.EQ.'1') GO TO 15
      IF (EXTRAN.and.ICODE.EQ.'G'.AND.JCODE.EQ.'1') GO TO 15
	IF (EXTRAN.AND.ICODE.EQ.'H'.AND.JCODE.EQ.'1') GO TO 15
c  RUNOFF lines with optional data input B1, B2
      IF (RUNOFF.and.ICODE.EQ.'B'.AND.JCODE.EQ.'1') GO TO 15
      IF (RUNOFF.and.ICODE.EQ.'B'.AND.JCODE.EQ.'2') GO TO 15
C  New Stats line with optional data input on A1 line
      IF (STATS.AND.ICODE.EQ.'A'.and.JCODE.EQ.'1') go to 15
      GO TO 20
   15 CONTINUE
      DO I = 1,10
      KCODE = KCODE(1:LOCAST) // BLANK
      LOCAST = LOCAST + 1
	IF (LOCAST.GT.400) go to 17
      KCODE = KCODE(1:LOCAST) // '0'
      LOCAST = LOCAST + 1
	IF (LOCAST.GT.400) go to 17
      ENDDO
	go to 20
   17	WRITE(N6,8000) KCODE
 8000 FORMAT(/,'  ERROR >>> INPUT LINE LONGER THAN 400 CHARACTERS.',/,
     112X,'YOU MAY NEED TO INSERT ASTERISK TO IDENTIFY START',
     2' OF COMMENT FIELD.',/,12X,'BAD LINE FOLLOWS.',//,12X,A400)
	STOP 'INPUT LINE TOO LONG'
cim end
   20 CONTINUE
      IF ((ICODE.EQ.' ').AND.(JCODE.EQ.' ').AND.(LOCAST.EQ.1)) GO TO 10
      IF (ICODE.EQ.DOLL) THEN
        OUTSTR = SQUO // ICODE // SQUO // BLANK // JCODE
        OUTSTR = OUTSTR(1:5) // KCODE(1:LOCAST)
        LOCAST = LOCAST + 5
      ELSE
        IF(ICODE.EQ.' ') THEN
        OUTSTR = ICODE // JCODE // KCODE(1:LOCAST)
        LOCAST = LOCAST + 2
        ELSE
        OUTSTR = SQUO // ICODE // JCODE // SQUO
        OUTSTR = OUTSTR(1:4) // ' '
        OUTSTR = OUTSTR(1:5) // KCODE(1:LOCAST)
        LOCAST = LOCAST + 5
       ENDIF
      ENDIF
      WRITE(N5,7000) (OUTSTR(I:I),I=1,LOCAST)
 7000 FORMAT(1X,402A1)
 2000 FORMAT(2A1,A400)
C     IF(ICODE.EQ.DOLL) THEN
C                       WRITE(N6,2) ICODE,JCODE,KCODE(1:locast)
C                       ELSE IF(ICODE.EQ.BLANK) THEN
C                            WRITE(N6,5) ICODE,JCODE,KCODE(1:locast)
C                            ELSE
C                            WRITE(N6,4) ICODE,JCODE,KCODE(1:locast)
C                            ENDIF
100   GO TO 10
101   WRITE(N5,6) LEND
	WRITE(N5,*) '$ENDPROGRAM'
      CLOSE (N55)
      REWIND N5
1     FORMAT(2A1,A400)
2     FORMAT('''',A1,'''',1X,A1,A400)
3     FORMAT(A400)
4     FORMAT('''',2A1,''' ',A400)
5     FORMAT(2A1,A400)
6     FORMAT(A10)
C#### WCH, 12/9/94.  REMOVE "...process is much faster..." stuff.
999   FORMAT(/,' Reading the input file and deleting comment lines.')
C####     +       ' This process is much faster when a RAM drive is used.',/)
      RETURN
      END
