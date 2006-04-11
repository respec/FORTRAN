C
C
C
      SUBROUTINE   GETKEY
     I                    (GROUP,CODE)
C
C     + + + PURPOSE + + +
C     *** PRIME/VAX SPECIFIC ***
C     To wait for keyboad interrupt by performing queue I/O and
C     assign function code to each key inerrupt
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GROUP,CODE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GROUP  - key group number:
C              =0 for failure
C              =1 printable character
C              =2 unprintable keys
C              =3 arrow keys
C                 CODE=1,2,3,4 for UP,DOWN,RIGHT,LEFT
C              =4 function/keypad keys
C                 CODE=0,1,2,3,4,5,6,7,8,9 for KP0-9
C                 CODE=10,11,12,13 for ENTER,".","-",","
C                 CODE=21,22,23,24 for PF1 PF2 PF3 PF4
C                 For EMIFE utility: CODE=1  HELP
C                                    CODE=2  CMDS
C                                    CODE=3  NEXT
C                                    CODE=4  BACK
C                                    CODE=5  END
C                                    CODE=6  EXIT
C     CODE   - ASCII code or function/keypad key number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*2   KEY,ESCAPE,ARROW,KEYPAD
      INTEGER     ICHR,CRFLG
      CHARACTER*1 ACHAR(1)
C
C     + + + INTRINSICS + + +
      INTRINSIC   ICHAR, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL    QCHR
C
C     + + + END SPECIFICATIONS + + +
C
      ESCAPE= 27
      ARROW = 91
      KEYPAD= 79
 50   CONTINUE
CPRH        CALL C1IN$(KEY,.FALSE.)
        CALL QCHR (ACHAR,ICHR)
        IF (ICHR.EQ.13) THEN
C         carriage return typed
          CRFLG= 1
        ELSE
C         something else
          CRFLG= 0
        END IF
        KEY = ICHAR(ACHAR(1))
        KEY = MOD(KEY,128)
        IF (KEY .NE. ESCAPE) THEN
          IF (KEY .LT. 32 .OR. KEY .GT. 126) THEN
            GROUP= 2
            IF (KEY.EQ.10) KEY= 13
          ELSE IF (KEY.EQ.32 .AND. CRFLG.EQ.1) THEN
            GROUP= 2
            KEY  = 13
          ELSE
            GROUP= 1
          END IF
          GO TO 999
        END IF
C
C       check second char of an escape sequence
C
 100    CONTINUE
CPRH          CALL C1IN$(KEY,.FALSE.)
          CALL QCHR (ACHAR,ICHR)
          KEY = ICHAR(ACHAR(1))
          KEY = MOD(KEY,128)
        IF (KEY .EQ. ESCAPE) GO TO 100
C
C       check for arrow key
C
        IF (KEY .NE. ARROW) GO TO 200
CPRH        CALL C1IN$(KEY,.FALSE.)
        CALL QCHR (ACHAR,ICHR)
        KEY = ICHAR(ACHAR(1))
        KEY = MOD(KEY,128)
        IF (KEY .EQ. ESCAPE) GO TO 100
        IF (KEY .GT. 64 .AND. KEY .LT. 69) THEN
          GROUP = 3
          KEY = KEY - 64
          GO TO 999
        ELSE
          GO TO 100
        END IF
C
C       check for keypad interrupt
C
 200    CONTINUE
      IF (KEY .NE. KEYPAD) GO TO 50
C
CPRH      CALL C1IN$(KEY,.FALSE.)
      CALL QCHR (ACHAR,ICHR)
      KEY = ICHAR(ACHAR(1))
      KEY = MOD(KEY,128)
      IF (KEY .EQ. ESCAPE) GO TO 100
      IF (KEY .GT. 111. AND. KEY .LT. 122) THEN
        KEY = KEY - 112
      ELSE IF (KEY .GT. 107 .AND. KEY .LT. 111) THEN
        KEY = KEY - 97
      ELSE IF (KEY .GT. 79 .AND. KEY .LT. 84) THEN
        KEY = KEY - 79
      ELSE IF (KEY .EQ. 77) THEN
        KEY = 10
      END IF
      GROUP = 4
      GO TO 999
C
 999  CONTINUE
      CODE = KEY
C     WRITE (*,*) 'GROUP,CODE ',GROUP,CODE
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCPRBN
     I                    (LEN,RMFLG,CRFLG,STR)
C
C     + + + PURPOSE + + +
C     writes out output string buffer
C     ***** VAX SPECIFIC ******
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,RMFLG,CRFLG
      CHARACTER*1 STR(256)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - length of string to write
C     RMFLG  - relative movement flag
C     CRFLG  - carriage return/line feed flag
C     STR    - string to write(or store in buffer)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,ATCRFG
      SAVE        ATCRFG
      CHARACTER*5 FBUFF(3)
C
C     + + + DATA INITIALIZATIONS + + +
      DATA ATCRFG/1/
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (I4,'A')
2010  FORMAT ('(1H+,')
2020  FORMAT ('(1X ,')
2030  FORMAT ('1,$) ')
2040  FORMAT ('1  ) ')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (RMFLG.EQ.1.OR.ATCRFG.EQ.0) THEN
C       write from where we are
        WRITE (FBUFF(1),2010)
      ELSE
C       write from beginning of next rec
        WRITE (FBUFF(1),2020)
      END IF
C
      WRITE (FBUFF(2),2000) LEN
C
      IF (CRFLG.EQ.0) THEN
C       write with no cr/lf
        WRITE (FBUFF(3),2030)
        ATCRFG= 0
      ELSE
C       with cr/lf
        WRITE (FBUFF(3),2040)
        IF (RMFLG.NE.2) THEN
C         really at carriage return
          ATCRFG= 1
        ELSE
C         fake cr for color
          ATCRFG= 0
        END IF
      END IF
C
      WRITE (*,FBUFF) (STR(I),I=1,LEN)
C
      RETURN
      END
C
C
C
      SUBROUTINE   C1IN
     I                  (I2CHAR)
C
C     + + + PURPOSE + + +
C     gets a single character from a terminal
C     based on RPA routine TTREAD
C     *** VAX SPECIFIC ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*2   I2CHAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     I2CHAR - ASCII integer code for character
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   '($SSDEF)/NOLIST'
      INCLUDE   '($IODEF)/NOLIST'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*2    IFIRST, TTCHAN
      SAVE         IFIRST, TTCHAN
      INTEGER*2    IOSB(4), QIOMSK
      INTEGER*4    IDS, NOTERM(2), EFLG, CRLF, LEN
      CHARACTER*1  CVAL
C
C     + + + FUNCTIONS + + +
      INTEGER*4    SYS$ASSIGN, SYS$QIOW
C
C     + + + INTRINSICS + + +
      INTRINSIC    ICHAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA         IFIRST /0/
      DATA         NOTERM /0,0/
C
C     + + + END SPECIFICATIONS + + +
C
C     initialization
      EFLG= 0
C     define IO mask for reading from the terminal
      QIOMSK= IO$_TTYREADALL
C
C     assign and attach to SYS$INPUT device
      IF (IFIRST.EQ.0) THEN
        IDS= SYS$ASSIGN ('SYS$INPUT', TTCHAN, , )
        IF (IDS.NE.1) THEN
          EFLG= 1
        ELSE
C         reset initialization flag
          IFIRST= 1
        END IF
      END IF
C
      IF (EFLG.EQ.0) THEN
C       read STRING from SYS$INPUT
        LEN= 1
        IDS= SYS$QIOW( , %VAL(TTCHAN), %VAL(QIOMSK), IOSB, , ,
     1       %REF(CVAL), %VAL(LEN), , %REF(NOTERM) , , )
C
C       check for satisfactory completion
        IF (IDS.EQ.SS$_NORMAL.AND.IOSB(1).EQ.1) THEN
          IF (IOSB(2).EQ.1) THEN
C           OK, get integer value for character
            I2CHAR= ICHAR(CVAL)
          ELSE
C           get value of terminator
            I2CHAR= IOSB(3)
          END IF
        ELSE
C         QIO directive failed
          EFLG= 2
        END IF
      END IF
C
      IF (EFLG.NE.0) THEN
        WRITE (*,*) 'FATAL ERROR ON VAX CHARACTER READ:',EFLG,IDS
        IF (EFLG.EQ.2) WRITE (*,*) ' IOSB:',IOSB
        STOP
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   C1INT
     O                  (I2CHAR)
C
C     + + + PURPOSE + + +
C     get a single character with no carriage return, time out if not there
C     *** PRIME/VAX SPECIFIC ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*2 I2CHAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     I2CHAR - integer*2 equivalent of keyboard response
C
C     + + + END SPECIFICATIONS + + +
C
C     otg equivalent of dg routine not yet written, return no keystroke
      I2CHAR= -1
C
      RETURN
      END
