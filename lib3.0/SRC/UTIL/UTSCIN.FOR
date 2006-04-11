C
C
C
      SUBROUTINE   GETKEY
     O                    (GROUP,CODE)
C
C     + + + PURPOSE + + +
C     catch keyboard interrupt - interactor
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
C              =7 Mouse movement to field in CODE
C     CODE   - ASCII code or function/keypad key number or field
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ICHR,ROW,COL
      CHARACTER*1 DUMCHR
C
C     + + + INTRINSICS + + +
      INTRINSIC   ICHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    QCHR, InfoInput,IMouseCursorHide, IMouseCursorShow
C
C     + + + FUNCTIONS + + +
      INTEGER     InfoInput
C
C     + + + END SPECIFICATIONS + + +
C
C     be sure mouse cursor available
      CALL IMouseCursorShow
C     get a character
      CALL QCHR(DUMCHR,ICHR)
C     be hide mouse cursor available
      CALL IMouseCursorHide
      GROUP= 0
      CODE = 0
C
      IF (ICHR > 0 .AND. ICHR < 127)  THEN
C       a standard character
        CODE= ICHR
        IF (ICHR .GE. 32) THEN
C         a printing character
          GROUP= 1
        ELSE
C         non-printing character
          GROUP= 2
        END IF
      ELSE IF (ICHR >= 171 .AND. ICHR <= 190) THEN
C       function key
        GROUP= 4
        CODE = ICHR - 170
      ELSE IF (ICHR >= 127 .AND. ICHR <= 170) THEN
C       cursor movement key
        GROUP = 3
        IF (ICHR .EQ. 128) THEN
C         up arrow
          CODE = 1
        ELSE IF (ICHR .EQ. 129) THEN
C         down arrow
          CODE = 2
        ELSE IF (ICHR .EQ. 130) THEN
C         right arrow
          CODE = 3
        ELSE IF (ICHR .EQ. 131) THEN
C         left arrow
          CODE = 4
        ELSE IF (ICHR .EQ. 140) THEN
C         home
          CODE = 5
        ELSE IF (ICHR .EQ. 141) THEN
C         end
          CODE = 6
        ELSE IF (ICHR .EQ. 132) THEN
C         pg up
          CODE = 7
        ELSE IF (ICHR .EQ. 133) THEN
C         pg dn
          CODE = 8
        ELSE IF (ICHR .EQ. 143) THEN
C         del
          CODE = 9
        ELSE IF (ICHR .EQ. 142) THEN
C         ins
          CODE = 10
        ELSE
          WRITE(99,*) 'GETKEYA',ICHR,GROUP,CODE
          GROUP= 0
          CODE = 0
        END IF
      ELSE IF (ICHR .EQ. 251) THEN
C       mouse
        ROW = InfoInput(63)
        COL = InfoInput(62)
        GROUP= 7
        CODE = (ROW*1000)+COL
        WRITE(99,*) 'GETKEY:mouse move:',ROW,COL,GROUP,CODE
      ELSE
        WRITE(99,*) 'GETKEYB',ICHR
      END IF
C
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
C     prints a string to the terminal
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,RMFLG,CRFLG
      CHARACTER*1 STR(255)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - len of string to write (characters)
C     RMFLG  - relative movement flag 0-no,1-yes
C     CRFLG  - carriage return flag 0-no,1-yes
C     STR    - characters to write
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'COLOR.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,INIT,CFORE,CBACK,CSET,IDIS
      CHARACTER*1   CR
      CHARACTER*255 LSTR
C
C     + + + SAVES + + +
      SAVE        INIT,CFORE,CBACK,IDIS
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR
C
C     + + + FUNCTIONS + + +
      INTEGER     InfoHardware, InfoScreen
C
C     + + + EXTERNALS + + +
      EXTERNAL    IOutString, ITextColourN, IGrInit
      EXTERNAL    InfoHardware, InfoScreen, IScreenModeN, IScreenMode
C
C     + + + DATA INITIALIZATIONS + + +
      DATA INIT/0/
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(255A1)
C
C     + + + END SPECIFICATIONS + + +
C
C     WRITE(99,*) 'SCPRBN:',LEN,RMFLG,CRFLG
      IF (INIT .EQ. 0) THEN
C       initialize output screen
        CALL IScreenInit(' ')
        IDIS= InfoHardware(10)
        CALL IGrInit(' ',1024,768,16)
C       new windows will be popup
        CALL IWinAction('P')
C       back to text
        IF (IDIS .EQ. 454) THEN
C         windows
          CALL IScreenModeN(0)
        ELSE
          CALL IScreenMode('T',80,24,16)
        END IF
        WRITE(99,*) 'SCPRBN:init:',IDIS,InfoScreen(1),InfoScreen(6)
C       text window
        CALL IWinOpen(1,1,80,24)
        CFORE= FORE
        CBACK= BACK
        CSET = 1
        INIT = 1
      ELSE IF (FORE.NE.CFORE .OR. BACK.NE.CBACK) THEN
C       change color
        CSET = 1
      ELSE
C       same color
        CSET = 0
      END IF
      IF (CSET .EQ. 1) THEN
C       change color with interactor
        CALL ITextColourN(FORE,BACK)
        CFORE= FORE
        CBACK= BACK
      END IF
      WRITE(LSTR,2000) (STR(I),I=1,LEN)
      CALL IOutString (LSTR(1:LEN))
      IF (CRFLG .EQ. 1) THEN
        CR= CHAR(13)
        CALL IOutString(CR)
      END IF
C
      !CALL FLUSH(6)
C
      RETURN
      END
C
C
C
      SUBROUTINE   C1IN
     O                 (I2CHAR)
C
C     + + + PURPOSE + + +
C     get a single character
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*2   I2CHAR
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ICHR
C
C     + + + EXTERNALS + + +
      EXTERNAL    EHIN, EHOUT, InKeyEvent
C
C     + + + END SPECIFICATIONS + + +
C
C     set up for Expert Help
      !CALL EHIN
C
      CALL InKeyEvent(ICHR)
      I2CHAR= ICHR
C
C     reset from Expert Help
      !CALL EHOUT
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
C     *** lahey specific ***
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
C
C
C
      SUBROUTINE   OSDIRL
     I                   (WRKDIR,IFNAME,
     O                    UMFNMS,UMNFIL)
C
C     + + + PURPOSE + + +
C     dummy for link
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*64 WRKDIR,IFNAME,UMFNMS(*)
      INTEGER      UMNFIL
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
      CHARACTER*120 SYSCMD
      CHARACTER*12  FNAME
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A12)
C
C     + + + END SPECIFCATIONS + + +
C
      SYSCMD= 'DIR'
C
      I= LEN_TRIM(WRKDIR)
      IF (I .GT. 0) THEN
        SYSCMD= SYSCMD(1:4) // ' ' // WRKDIR(1:I)
      END IF
C
      I= LEN_TRIM(IFNAME)
      IF (I .GT. 0) THEN
        SYSCMD= SYSCMD(1:LEN_TRIM(SYSCMD)) // ' ' // IFNAME(1:I)
      END IF
C
      SYSCMD= SYSCMD(1:LEN_TRIM(SYSCMD)) // ' >DIR.LIS /B'
C
      CALL SYSTEM(SYSCMD)
C
      UMNFIL = 0
      OPEN(UNIT=100,FILE='DIR.LIS',STATUS='OLD',ERR=30)
 10   CONTINUE
        READ(100,1000,END=20) FNAME
        UMNFIL = UMNFIL+ 1
        UMFNMS(UMNFIL)= FNAME
      GO TO 10
 20   CONTINUE
      CLOSE(UNIT=100,STATUS='DELETE')
      GO TO 40
 30   CONTINUE
        WRITE(99,*) 'OSDIRL:error opening DIR.LIS',SYSCMD
        UMNFIL = UMNFIL+ 1
        UMFNMS(UMNFIL)= IFNAME(1:I)
 40   CONTINUE
C
      RETURN
      END
