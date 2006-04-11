C
C
C
      SUBROUTINE   GETKEY
     O                    (GROUP,CODE)
C
C     + + + PURPOSE + + +
C     *** DG SPECIFIC ***
C     assign function code to each key interrupt
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
C                      5 - home
C                      6 - end
C                      7 - page up
C                      8 - page down
C                     10 - insert
C              =4 function keys
C     CODE   - ASCII code or function key number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*2   KEY,ESCAPE,ARROW,KEYPAD
      INTEGER     ICHR,CRFLG,F10FLG,I,ROW,COL
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
      GROUP = 0
      CODE  = 0
C
      ESCAPE= 27
      ARROW = 91
      KEYPAD= 79
C
 50   CONTINUE
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
 100    CONTINUE
          CALL QCHR (ACHAR,ICHR)
          KEY = ICHAR(ACHAR(1))
          KEY = MOD(KEY,128)
        IF (KEY .EQ. ESCAPE) GO TO 100
C
C       check for arrow/bracket key
        IF (KEY .NE. ARROW) GO TO 200
        CALL QCHR (ACHAR,ICHR)
        KEY = ICHAR(ACHAR(1))
        KEY = MOD(KEY,128)
        IF (KEY .EQ. ESCAPE) GO TO 100
        IF (KEY .EQ. 77) THEN
C         row col from mouse follows
          I = 0
 110      CONTINUE
            CALL QCHR (ACHAR,ICHR)
            KEY = ICHAR(ACHAR(1))
            KEY = MOD(KEY,128)
            IF (KEY .GT. 32) THEN
              I= I+ 1
              IF (I .EQ. 1) THEN
                COL= KEY- 32
              ELSE
                ROW= KEY- 32
              END IF
            END IF
          IF (I .LT. 2) GO TO 110
          WRITE(99,*) 'UTSCDG:GETKEY:ROWCOL',ROW,COL
          GROUP= 7
          CODE = (ROW*1000)+COL
        ELSE IF (KEY .GT. 64 .AND. KEY .LT. 69) THEN
C         arrow key
          GROUP= 3
          KEY  = KEY - 64
          GO TO 999
        ELSE IF (KEY .EQ. 53) THEN
C         page up
          GROUP= 3
          CODE = 7
          CALL QCHR (ACHAR,ICHR)
        ELSE IF (KEY .EQ. 54) THEN
C         page down
          GROUP= 3
          CODE = 8
          CALL QCHR (ACHAR,ICHR)
        ELSE IF (KEY .EQ. 72) THEN
C         home
          GROUP= 3
          CODE = 5
        ELSE IF (KEY .EQ. 49 .OR. KEY .EQ. 50) THEN
C         function key? might be, check next key
          IF (KEY.EQ.49) THEN
C           function keys 1 - 8
            F10FLG= 0
          ELSE
C           function keys 9 - 12
            F10FLG= 1
          END IF
          CALL QCHR (ACHAR,ICHR)
          KEY = ICHAR(ACHAR(1))
          KEY = MOD(KEY,128)
          IF (F10FLG.EQ.0) THEN
C           which function key, 1 - 8?
            IF (KEY .EQ. 49) THEN
C             f1
              GROUP= 4
              CODE = 1
            ELSE IF (KEY .EQ. 50) THEN
C             f2
              GROUP= 4
              CODE = 2
            ELSE IF (KEY .EQ. 51) THEN
C             f3
              GROUP= 4
              CODE = 3
            ELSE IF (KEY .EQ. 52) THEN
C             f4
              GROUP= 4
              CODE = 4
            ELSE IF (KEY .EQ. 53) THEN
C             f5
              GROUP= 4
              CODE = 5
            ELSE IF (KEY .EQ. 55) THEN
C             f6
              GROUP= 4
              CODE = 6
            ELSE IF (KEY .EQ. 56) THEN
C             f7
              GROUP= 4
              CODE = 7
            ELSE IF (KEY .EQ. 57) THEN
C             f8
              GROUP= 4
              CODE = 8
            END IF
          ELSE
C           which function key, 9 - 12?
            IF (KEY .EQ. 48) THEN
C             f9
              GROUP= 4
              CODE = 9
            ELSE IF (KEY .EQ. 49) THEN
C             f10
              GROUP= 4
              CODE = 10
            ELSE IF (KEY .EQ. 51) THEN
C             f11
              GROUP= 4
              CODE = 11
            ELSE IF (KEY .EQ. 52) THEN
C             f12
              GROUP= 4
              CODE = 12
            END IF
          END IF
          IF (GROUP .EQ. 4) THEN
C           strip off trailing character
            CALL QCHR (ACHAR,ICHR)
          END IF
        END IF
        IF (GROUP .NE. 0) GOTO 9999
        GO TO 100
C
C       check for keypad interrupt
C
 200    CONTINUE
      IF (KEY .NE. KEYPAD) GO TO 50
C
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
9999  CONTINUE
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
C     *** UNIX SPECIFIC ***
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
      INTEGER   I,J
      CHARACTER*132 LBUF
C
C     + + + INTRINSICS + + +
      INTRINSIC CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL  TTYPUT
C
C     + + + FORMATS + + +
2000  FORMAT (132A1)
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE(LBUF,2000) (STR(I),I=1,LEN)
      CALL TTYPUT(LBUF(1:LEN))
C
      IF (CRFLG.EQ.1) THEN
        CALL TTYPUT(CHAR(13))
        CALL TTYPUT(CHAR(10))
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   C1IN
     O                  (I2CHAR)
C
C     + + + PURPOSE + + +
C     get a single character with no carriage return
C     *** DG specific ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*2 I2CHAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     I2CHAR - integer*2 equivalent of keyboard response
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I
      CHARACTER*1 CHAR
C
C     + + + FUNCTIONS + + +
      INTEGER     TTYKEY
C
C     + + + INTRINSICS + + +
      INTRINSIC   ICHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    TTYKEY
C
C     + + + END SPECIFICATIONS + + +
C
C     call POSIX specific function to get character
      I= TTYKEY(CHAR)
      I2CHAR= ICHAR(CHAR)
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
C     *** DG specific ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*2 I2CHAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     I2CHAR - integer*2 equivalent of keyboard response
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I
      CHARACTER*1 CHAR
C
C     + + + FUNCTIONS + + +
      INTEGER     TTYTIM
C
C     + + + INTRINSICS + + +
      INTRINSIC   ICHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    TTYTIM
C
C     + + + END SPECIFICATIONS + + +
C
C     call POSIX specific function to get character
      I= TTYTIM(CHAR)
      I2CHAR= ICHAR(CHAR)
C
      RETURN
      END
