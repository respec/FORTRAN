      SUBROUTINE PLDFUN (ISTAT)
C
C        SUBROUTINE 'PLDFUN' LOADS THE GIVEN FUNCTIONS INTO
C        THE FUNCTION FILE.  THE SUBROUTINE HAS BEEN DESIGNED
C        TO ACCESS THE FUNCTION FILE, READ THE FUNCTIONS,AND
C        THEN CUT LOOSE FROM THE FILE.  THIS IS DONE TO SAVE
C        TIME.  TO INSURE THAT TWO USERS DON'T TRY TO ACCESS
C        THE FILE AT THE SAME TIME, THEREBY KILLING THE PRO-
C        GRAM WITH AN ERROR, THE SUBROUTINE HAS A BUILT-IN
C        WAITING MECHANISM.  IF AN 'OPEN' STATEMENT TO THE
C        FILE IS UNSUCCESSFUL, THE SUBROUTINE SIMPLY WAITS
C        AND TRIES AGAIN.
C
C
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
C
      CHARACTER*1 CEQUAL
      LOGICAL LFIRST,LEXIST
C
C
C
      CEQUAL = '='
      DATA LFIRST/.TRUE./
C   ***
C   ***
C   ** IF ON THE FIRST TRIP THROUGH THE SUBROUTINE, FIND OUT THE
C   ** NAME OF THE FILE, AND SAVE THAT NAME IN A VARIABLE.
C   ** ELSE, IF NOT THE FIRST TIME THROUGH THE SUBROUTINE, OPEN
C   ** A LINE TO THE FILE WITH THE NAME THAT WAS STORED IN THE
C   ** VARIABLE.
C   ***
      IF (IFUN.EQ.-1) RETURN
      INQUIRE (FILE=CFUNFL,EXIST=LEXIST)
      IF (.NOT.LEXIST) RETURN
      IF (LFIRST) THEN
 504   OPEN(UNIT=IFUN,FILE=CFUNFL,STATUS='OLD',ERR=503,IOSTAT=ISTAT)
 503   IF (ISTAT .NE. 0) THEN
C       IF (ISTAT .EQ. 77) THEN                                         H
C        CALL WAITS (2.0)                                               H
C        IF (ICOUNT.EQ.0)WRITE(IDSP,*)'THE FILE IS IN USE => ',CFUNFL   H
C        ICOUNT = ICOUNT + 1                                            H
C        IF (ICOUNT .GT. 10) THEN                                       H
C          WRITE(IDSP,505) CFUNFL                                       H
C          GOTO 504                                                     H
C        ENDIF                                                          H
C       ELSE                                                            H
         WRITE(IDSP,505) CFUNFL
 505     FORMAT('**  WARNING:  NOT ABLE TO ACCESS FILE ', A)
         CLOSE (UNIT=IFUN)
         RETURN
C       END IF                                                          H
       END IF
       LFIRST = .FALSE.
       ELSE
        ICOUNT = 0
 600    OPEN(UNIT=IFUN,FILE=CFUNFL,STATUS='OLD',ERR=601,IOSTAT=ISTAT2)
 601    IF (ISTAT2 .NE. 0) THEN
C        IF (ISTAT2 .EQ. 77) THEN                                       H
C         CALL WAITS (2.0)                                              H
C         IF (ICOUNT.EQ.0) WRITE (IDSP,*)'FILE IS IN USE => ',CFUNFL    H
C         ICOUNT = ICOUNT+1                                             H
C         IF (ICOUNT .GT. 10) THEN                                      H
C           WRITE(IDSP,602) CFUNFL                                      H
C           GOTO 600                                                    H
C         END IF                                                        H
C        ELSE                                                           H
          WRITE(IDSP,602) CFUNFL
 602      FORMAT('**  WARNING:  NOT ABLE TO ACCESS FILE ',A)
          CLOSE (UNIT=IFUN)
          RETURN
C        END IF                                                         H
        END IF
      END IF
      REWIND IFUN
      READ (IFUN,1,END=30) CLINE
    1 FORMAT (A)
      I = NINDX(CLINE,CSPL(2:2))
      IF (I .LE. 0) THEN
      WRITE (IDSP,2)
    2 FORMAT(' THE BLANK CHARACTER MAY NOT BE THE FUNCTION CHARACTER')
      STOP
      END IF
      CSPL(4:4) = CLINE(I:I)
      ILOOP = 0
   10 CONTINUE
       READ (IFUN,1,END=30) CLINE
       I = NINDX(CLINE,CSPL(2:2))
       IF (I .LE. 0) THEN
        WRITE (IDSP,11)
   11   FORMAT(' THE BLANK CHARACTER MAY NOT BE A DEFINED FUNCTION KEY')
        READ (IFUN,1,END=30) CLINE
        GO TO 10
       END IF
       IF (CLINE(I:I) .EQ. CEQUAL) THEN
        WRITE (IDSP,14) CEQUAL
   14   FORMAT(' THE ',A,' CHARACTER MAY NOT BE A DEFINED FUNCTION KEY')
        READ (IFUN,1,END=30) CLINE
        GO TO 10
       END IF
C   ***
C   ***
C   ** MUST USE 'ISCAN' BECAUSE CSPL IS MORE THAN ONE CHARACTER LONG
C   ***
       J = ISCAN (CSPL,1,4,CLINE,I,1,L)
       IF (J .EQ. 0) GO TO 16
       WRITE (IDSP,14) CSPL(J:J)
       READ (IFUN,1,END=30) CLINE
       GO TO 10
   16  CONTINUE
       ILOOP = ILOOP + 1
       CKEY(ILOOP) = CLINE(I:I)
       L = NINDX(CLINE((I+1):132),CSPL(2:2))
       IF (L .LE. 0) THEN
  17    IKEY(ILOOP) = 0
        GO TO 19
       END IF
       L = L + I
       I = INDEX(CLINE(L:132), CSPL(2:2))
       IF (I .LE. 0) THEN
         IKEY(ILOOP) = 0
         GO TO 19
       END IF
       IKEY(ILOOP) = INTGR(CLINE,L,(I-1),ISTAT)
       IF (ISTAT .NE. 0) IKEY(ILOOP) = 0
 19    READ(IFUN,1,END = 30) CLINE
       CFUNCT(ILOOP)(1:IFUNLN) = CLINE(1:IFUNLN)
       NKEY = ILOOP
       IF (IKEY(ILOOP) .LE. 0)
     &    IKEY(ILOOP) = NINDXR(CLINE,CSPL(2:2))
 20   CONTINUE
      IF ( IKEY(ILOOP) .GT. IFUNLN) THEN
         WRITE(IDSP,22) CKEY(ILOOP), IFUNLN
 22      FORMAT(' FUNCTION DEFINITION FOR ', A,
     &          ' MUST NOT BE GREATER THAN', I3, ' CHARACTERS')
         ILOOP = ILOOP - 1
      ENDIF
      IF (ILOOP .GE. MAXFUN) GOTO 25
      GOTO 10
 25   READ(IFUN,1,END=30) CLINE
      WRITE (IDSP,21) MAXFUN
   21 FORMAT (' NOT MORE THAN',I4,' FUNCTION KEYS MAY BE DEFINED')
   30 CONTINUE
C   ***
C   ***
C   ** THIS LINE SEVERES THE TIE BETWEEN THE PROGRAM
C   ** AND THE FILE IFUN.
C   ***
      CLOSE (UNIT=IFUN)
      RETURN
      END
