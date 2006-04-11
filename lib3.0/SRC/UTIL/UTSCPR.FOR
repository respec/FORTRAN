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
C     prints a string to the terminal with no cr/lf
C     *** PRIME/PC SPECIFIC ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,RMFLG,CRFLG
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - len of string to write (characters)
C     RMFLG  - relative movement flag
C     CRFLG  - carriage return flag 0
C              0 - no
C              1 - yes
C     STR    - characters to write
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*2   ISTR(128),I
      CHARACTER*1 CBUF1(2)
      CHARACTER*2 CBUF2
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CBUF1,CBUF2)
C
C     + + + FUNCTIONS + + +
      INTRINSIC  MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   TNOUA
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A2)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (256A1)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CRFLG.EQ.0.OR.RMFLG.EQ.1) THEN
C       use prime system routine with no cr/lf
        DO 10 I= 2,LEN,2
          CBUF1(1)= STR(I-1)
          CBUF1(2)= STR(I)
          READ (CBUF2,1000) ISTR(I/2)
 10     CONTINUE
C
        IF (MOD(LEN,2).EQ.1) THEN
C         also 1 position left over
          CBUF1(1)= STR(LEN)
          CBUF1(2)= ''
          I= LEN+ 1
          READ (CBUF2,1000) ISTR(I/2)
          ISTR(I/2)= ISTR(I/2)- 32
        ELSE
          I= LEN
        END IF
C
        CALL TNOUA (ISTR,I)
C
        IF (CRFLG.EQ.1) THEN
C         needed special start for relative movement, now cr/lf
          WRITE (*,2000)
        END IF
      ELSE
C       just write the string with cr/lf
        WRITE (*,2000) (STR(I),I=1,LEN)
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
C     *** prime specific ***
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
