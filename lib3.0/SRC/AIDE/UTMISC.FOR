C
C
C
      SUBROUTINE   ZGTKEY
     M                    (GROUP,CODE)
C
C     + + + PURPOSE + + +
C     calls system specific routine to get keyboard response
C     and performs macro/input log functions
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GROUP,CODE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GROUP  - key group number:
C              =1 printable character
C              =2 unprintable character
C              =3 arrow key
C                 CODE= 1 - up,  2 - down,  3 - right, 4 - left, 5 - home,
C                       6 - end, 7 - pg up, 8 - pg dn, 9 - del,  10 - ins
C              =4 function keys
C     CODE   - ASCII code, arrow key code or function key number
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cterif.inc'
      INCLUDE 'zcntrl.inc'
      INCLUDE 'cscren.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I0,I1,I80,J,MESSFL,SCLU,SGRP(2),ILEN,
     1             OLEN,JLEN,LOGFLG,ILOGFL,IPAUSE,IESC,LOGIFL(3),LOGCNT,
     2             CRLF,LOGMAX,ERRFLG,WINFG,
     4             SCRCNT,IBACK,DONFG,DONLOG,IOS
      CHARACTER*1  INBUFF(80),OBUFF(80),CTMP(1)
      CHARACTER*21 WNDNAM
      CHARACTER*64 LGFLNM
      LOGICAL      LOPEN
C
C     + + + SAVES + + +
      SAVE         MESSFL,SCLU,ILOGFL,IESC,ILEN,OLEN,
     1             LOGFLG,LOGIFL,LOGCNT,
     2             INBUFF,OBUFF,WINFG,SCRCNT
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (TBUFF,INBUFF)
      CHARACTER*78 TBUFF
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR, CHRINT
C
C     + + + INTRINSICS + + +
      INTRINSIC    CHAR, ICHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL     LENSTR, CHRINT, GETKEY, ANPRGT, MACWIN, ZIPC
      EXTERNAL     CHRDEL, QFCLOS, SCCUMV, ZWRMN1, ZBEEP
      EXTERNAL     ZWRSCR, ZWRVDR, SCPRBF
      EXTERNAL     INTCHR, GETFUN, CARVAR, ZTUTOR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA INBUFF,OBUFF/80*' ',80*' '/
      DATA I0,I1,I80,ILEN,IBACK,OLEN
     1     /0, 1, 80, 0,   60,   0/
      DATA LOGCNT,LOGMAX,LOGFLG,WINFG,SCRCNT
     1     /0,    3,     -1,    0,    0/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (80A1)
C
C     + + + END SPECIFICATIONS + + +
C
      BLNK= ' '
      ERRFLG= 0
C
      IF (LOGFLG .LE. 0) THEN
C       get character for indicating log file
        I= 6
        CALL ANPRGT (I,ILOGFL)
C       get character for indicating escape
        I= 7
        CALL ANPRGT (I,IESC)
C       get character for indicating pause
        I= 8
        CALL ANPRGT (I,IPAUSE)
C       get message file unit number
        I= -1
        CALL ANPRGT (I,MESSFL)
        SCLU= 4
C       is the log file open for use?
        INQUIRE (UNIT=LOGFL,OPENED=LOPEN)
        IF (LOPEN.EQV..TRUE.) THEN
C         yes, its open
          LOGFLG= 1
          WRITE(99,*) '* output log file is open on',LOGFL
        ELSE
C         no log file in use
          LOGFLG= 0
          WRITE(99,*) '* no output log file is open on',LOGFL
        END IF
      ELSE IF (LOGFL .EQ. 0) THEN
C       no output log file, turn the log flag off
        LOGFLG= 0
        WRITE(99,*) '* no output log file is open 2'
      END IF
C
      IF (TERIFL.NE.TEMPFL .AND. LOGCNT.EQ.0) THEN
C       log file was opened externally, set to first LOGIFL
        LOGCNT= LOGCNT+ 1
        LOGIFL(LOGCNT)= TERIFL
        WRITE(99,*) '* using external input log file',TERIFL
      END IF
      DONLOG= 0
C     loop to get input
 10   CONTINUE
        IF (DONLOG .EQ. 1) THEN
C         end of input from log file
          IF (TERIFL .EQ. TEMPFL) THEN
C           at end of input file from terminal
            STOP '*** FATAL ERROR - END OF INPUT FILE FROM KEYBOARD'
          ELSE
C           flush input buffer
            ILEN= 0
C           close current input log file
            WRITE(99,*) '* close input log file',LOGCNT,LOGIFL(LOGCNT)
            J= 0
            CALL QFCLOS (LOGIFL(LOGCNT),J)
C           decrement number of log files open
            LOGCNT= LOGCNT- 1
C
            IF (LOGCNT .EQ. 0) THEN
C             end of all log files, signal read from terminal
              TERIFL = TEMPFL
              WNDNAM = 'Macro end'
              SGRP(1)= 5
              SGRP(2)= 6
              CALL MACWIN (MESSFL,SCLU,SGRP,ZITYPE,WNDNAM)
C             set window overwrote part of screen flag to yes
              WINFG= 1
              WRITE(99,*) '* back to keyboard input'
            ELSE
C             back to log file
              TERIFL= LOGIFL(LOGCNT)
              WRITE(99,*) '* use input log file',LOGCNT,LOGIFL(LOGCNT)
            END IF
          END IF
C
          IF (TERIFL .EQ. TEMPFL) THEN
          END IF
C         not at end of log file
          DONLOG= 0
        END IF
C       time to get input
        IF (ERRFLG .LT. 0) THEN
C         returned here from bad log file name, reset and try again
          ILEN  = 0
        END IF
        ERRFLG= 0
        IF (ILEN .EQ. 0) THEN
C         nothing in the type ahead buffer, read from user now
          IF (TERIFL .NE. TEMPFL) THEN
C           read a string from log file
            READ (TERIFL,1000,END=20) (INBUFF(J),J=1,I80)
            ILEN= LENSTR(I80,INBUFF)
            GOTO 22
 20         CONTINUE
C             nothing more in log file
              DONLOG= 1
              ILEN  = 0
C             force more input
              ERRFLG= 1
 22         CONTINUE
          ELSE
C           get keyboard input from system specific call
            ILEN= 1
            CALL GETKEY (GROUP,CODE)
            IF (GROUP.EQ.1 .AND. CODE.EQ.IESC) THEN
C             alternative escape
              GROUP= 2
              CODE = 27
            END IF
            IF (CODE .EQ. ILOGFL) THEN
C             a log file has been specificd
C             clear current highlight
              IF (ZDTYP .EQ. 4) THEN
C               parm2 field highlight
                J= NMHDRW+ CROW
              ELSE
C               other highlight
                J= ZHLLIN
              END IF
              IF (J .GT. 0) THEN
C               highlight available to clear, do it
                CALL ZWRSCR(ZMNTXT(J)(ZHLCOL:ZHLCOL+ZHLLEN-1),
     I                      ZHLLIN+1,ZHLCOL+1)
              END IF
              WNDNAM= 'Macro name'
              SGRP(1)= 8
              SGRP(2)= -1
              CALL MACWIN (MESSFL,SCLU,SGRP,ZITYPE,WNDNAM)
C             move cursor to first position
              I= 4
              J= 11
              CALL SCCUMV (I,J)
C             use dummy write to get cursor to move
              CRLF= 2
              JLEN= 0
              CTMP(1)= ' '
              CALL SCPRBF (JLEN,JLEN,CRLF,CTMP)
C             read name of log file a character at a time from keyboard
              DONFG= 0
 25           CONTINUE
                CALL GETKEY (GROUP,CODE)
                IF (GROUP.EQ.1 .AND. CODE.EQ.IESC) THEN
C                 alternative escape
                  GROUP= 2
                  CODE = 27
                END IF
                IF (GROUP .EQ. 1) THEN
C                 printable character
                  ILEN= ILEN+ 1
                  INBUFF(ILEN)= CHAR(CODE)
C                 be sure cursor in right spot
                  CALL SCCUMV (I,J)
C                 echo character
                  CRLF= 1
                  JLEN= 1
                  CALL SCPRBF (JLEN,I1,CRLF,INBUFF(ILEN))
C                 move cursor
                  J= J+ 1
                  CALL SCCUMV (I,J)
                ELSE IF (GROUP.EQ.2 .AND. CODE.EQ.8) THEN
C                 backspace
                  IF (ILEN .GT. 1) THEN
C                   move cursor back
                    J= J- 1
                    CALL SCCUMV (I,J)
                    INBUFF(ILEN)= BLNK
C                   echo highlighted blank
                    CALL ZWRVDR(INBUFF(ILEN),I,J)
                    ILEN= ILEN- 1
                  ELSE
C                   cant go back further
                    CALL ZBEEP
                  END IF
C                 be sure cursor in right spot
                  CALL SCCUMV (I,J)
                ELSE IF (GROUP.EQ.2 .AND. CODE .EQ. 27) THEN
C                 escape
                  DONFG = 1
                  ILEN  = 0
                  WINFG = 1
                  ERRFLG= -1
                ELSE IF (GROUP.EQ.2 .AND. CODE .EQ. 13) THEN
C                 enter, try name
                  DONFG= 1
                END IF
              IF (DONFG .EQ. 0) GO TO 25
C
              IF (ILEN .GT. 0) THEN
C               reset code for new logfile
                CODE = ILOGFL
                GROUP= 1
              END IF
C             rehighlight current field
              IF (ZDTYP .EQ. 4) THEN
C               parm2 field highlight
                J= NMHDRW+ CROW
              ELSE
C               other highlight
                J= ZHLLIN
              END IF
              IF (J .GT. 0) THEN
C               highlight available to relight, do it
                CALL ZWRVDR(ZMNTXT(J)(ZHLCOL:ZHLCOL+ZHLLEN-1),
     I                                ZHLLIN+1,ZHLCOL+1)
              END IF
            ELSE
C             normal keyboard input
              ILEN= 0
            END IF
          END IF
C
          IF (WINFG.GT.0) THEN
C           rewrite top part of screen
            IF (ZDTYP .EQ. 4) THEN
C             special rewrite for parm2 screen
              CALL ZWRMN1
            ELSE
C             just rewrite first five lines of menu text
              DO 30 I= 1,5
                CALL ZWRSCR (ZMNTXT(I),I+1,2)
 30           CONTINUE
            END IF
C           reset window overwrote part of screen flag to no
            WINFG= 0
          END IF
        END IF
C
        IF (TERIFL.NE.TEMPFL .AND. DONLOG.EQ.0) THEN
C         convert log file input to group/code
          IF (INBUFF(1).EQ.'#') THEN
C           unprintable, need to convert
            GROUP= CHRINT(I1,INBUFF(2))
            IF (GROUP .LT. 5) THEN
C             not pause or tutor display, continue to read input
              J= 2
              CODE = CHRINT(J,INBUFF(3))
              CALL ZIPC (ILEN,BLNK,INBUFF)
              ILEN = 0
            ELSE
C             set code to not be processed
              CODE= -1
            END IF
          ELSE
C           standard text, take next character
            GROUP= 1
            CODE = ICHAR(INBUFF(1))
            IF (CODE.NE.ILOGFL .AND. ILEN.GT.0) THEN
C             remove a character from buffer
              CALL CHRDEL (ILEN,I1,INBUFF)
              ILEN= ILEN- 1
            END IF
          END IF
        END IF
C
        IF (CODE.EQ.ILOGFL .AND. DONLOG.EQ.0) THEN
C         input now to come from a log file
          IF (LOGCNT .GE. LOGMAX) THEN
C           log files are all busy right now, try again later please
            WRITE (99,*) '* sorry, too many input log files open',LOGCNT
            WNDNAM= 'Macro opening problem'
            SGRP(1)= 9
            SGRP(2)= 7
            CALL MACWIN (MESSFL,SCLU,SGRP,ZITYPE,WNDNAM)
C           set window overwrote part of screen flag to yes
            WINFG= 1
          ELSE
C           go ahead try to start using a new log file
            LOGCNT= LOGCNT+ 1
            IF (ILEN .GT. 0) THEN
C             delete leading @ sign
              CALL CHRDEL (ILEN,I1,INBUFF)
              JLEN= ILEN- 1
            ELSE
              JLEN= 0
            END IF
            IF (JLEN .GT. 0) THEN
C             log file name was entered
              WRITE (99,*) '* open input logfile, name length is',JLEN
              J= 64
              CALL CARVAR (JLEN,INBUFF,J,LGFLNM)
              CALL GETFUN (I1,LOGIFL(LOGCNT))
              OPEN (UNIT=LOGIFL(LOGCNT),FILE=LGFLNM,STATUS='OLD',
     1              IOSTAT=IOS,ERR=50)
              WRITE (99,*) '  ok open of logfile',LOGCNT
              WRITE (99,*) '  on unit           ',LOGIFL(LOGCNT)
              WRITE (99,*) '  other units       ',TERIFL,TEMPFL,LOGFL
              GO TO 60
C
 50           CONTINUE
C               get here on log file open error
                WRITE (99,*) '** log file open error',IOS
                ERRFLG= -1
C
 60           CONTINUE
            ELSE
C             blank name after @
              WRITE (99,*) '** no name of log file to open'
              ERRFLG= -1
            END IF
C
            IF (ERRFLG .EQ. 0) THEN
C             ok open of log file
              TERIFL= LOGIFL(LOGCNT)
C             read first record of log file
              READ (TERIFL,1000,END=65) (INBUFF(J),J=1,I80)
              ILEN= LENSTR(I80,INBUFF)
              GOTO 68
 65           CONTINUE
C               nothing in log file
                DONLOG= 1
                ILEN  = 0
 68           CONTINUE
C             force more input
              ERRFLG= 1
            ELSE
C             problem opening log file
              WNDNAM= 'Macro opening problem'
              SGRP(1)= 9
              SGRP(2)= 0
              CALL MACWIN (MESSFL,SCLU,SGRP,ZITYPE,WNDNAM)
C             dont add log file
              LOGCNT= LOGCNT- 1
            END IF
C           set window overwrote part of screen flag to yes
            WINFG= 1
          END IF
        END IF
C
        IF ((GROUP.EQ.5 .OR. GROUP.EQ.6) .AND. DONLOG.EQ.0) THEN
C         tutorial
          CALL ZTUTOR(TERIFL,IBACK,
     M                GROUP,CODE,SCRCNT,ERRFLG,DONLOG,ILEN,INBUFF)
        ELSE
C         reset number of screens to back up if not a tutor directive
          SCRCNT= 0
        END IF
C
C       need to read more?
      IF (ERRFLG .NE. 0) GO TO 10
C
C     LRNFG= 0
C     IF (INBUFF(1).EQ.LEARN) THEN
C       learn flag found
C       IF (TERIFL.NE.TEMPFL) THEN
C         LRNFG= 1
C         write a bell to tell user to type
C         CBELL(1)= CHAR(7)
C         CALL SCPRST(I1,CBELL)
C         read from user
C         READ (TEMPFL,1000) (INBUFF(J),J=1,I80)
C         ILEN= LENSTR(I80,INBUFF)
C       ELSE
C         LRNFG= 2
C         CALL CHRDEL (LEN,I1,INBUFF)
C         IF (LOGFLG.NE.0) WRITE (LOGFL,1000) LEARN
C       END IF
C     END IF
C
      IF (LOGFLG .NE. 0) THEN
C       output log file in use, put input to output log file with buffer
        IF (GROUP .EQ. 1) THEN
C         standard text
          IF (CODE .NE. ILOGFL) THEN
C           character is not @, ok to put to log file
            OLEN= OLEN+ 1
            OBUFF(OLEN)= CHAR(CODE)
            IF (OLEN .GT. 79) THEN
C             buffer full, write to log file
              WRITE (LOGFL,1000) (OBUFF(I),I=1,OLEN)
C             clear out buffer
              CALL ZIPC (ILEN,BLNK,OBUFF)
              OLEN= 0
            END IF
          END IF
        ELSE
C         unprintable character, convert to group/code
          IF (OLEN .GT. 0) THEN
C           output existing buffer
            WRITE (LOGFL,1000) (OBUFF(I),I=1,OLEN)
C           clear out buffer
            CALL ZIPC (OLEN,BLNK,OBUFF)
          END IF
          OBUFF(1)= '#'
          CALL INTCHR (GROUP,I1,I0,
     1                 J,OBUFF(2))
          IF (GROUP .EQ. 7) THEN
            I= 5
          ELSE
            I= 2
          END IF
          CALL INTCHR (CODE,I,I0,
     1                 J,OBUFF(3))
          IF (OBUFF(3) .EQ. BLNK) THEN
C           get rid of blank
            OBUFF(3)= '0'
          END IF
          OLEN= I+ 2
          WRITE (LOGFL,1000) (OBUFF(I),I=1,OLEN)
C         clear out buffer
          CALL ZIPC (OLEN,BLNK,OBUFF)
          OLEN= 0
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MACWIN
     I                   (MESSFL,SCLU,SGRP,ITYPE,WNDNAM)
C
C     + + + PURPOSE + + +
C     perform pull down window tasks related to macros
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,SCLU,SGRP(2),ITYPE
      CHARACTER*(*) WNDNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - clustern number on message file
C     SGRP   - array of group numbers for text to appear in window
C     ITYPE  - type of line to draw for window
C     WNDNAM - window name
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,ILIN,ICOL,NLIN,NCOL,ILEN
      CHARACTER*64 TBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TBUF1,TBUFF)
      CHARACTER*1  TBUF1(64)
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZDRWDW, ZWRSCR, ZWRVDR, GETTXT
C
C     + + + END SPECIFICATIONS + + +
C
C     draw pull down window
      ILIN= 2
      ICOL= 10
      NLIN= 4
      NCOL= 66
      CALL ZDRWDW (ITYPE,WNDNAM,ILIN,ICOL,NLIN,NCOL,' ')
      DO 10 I= 1,2
C       display two lines of text
        TBUFF= ' '
        IF (SGRP(I).GT.0) THEN
C         get text off message file
          ILEN= 64 
          CALL GETTXT (MESSFL,SCLU,SGRP(I),ILEN,TBUF1)
        END IF
        IF (SGRP(I).LT.0) THEN
C         write this line in inverse video
          CALL ZWRVDR (TBUFF,ILIN+I,ICOL+1)
        ELSE
C         normal display
          CALL ZWRSCR (TBUFF,ILIN+I,ICOL+1)
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ARRINP
     I                    (MESSFL, GROUP, NVAL,
     I                     OBUFF)
C
C     + + + PURPOSE + + +
C     This routine gets a set of real numbers from the user from the
C     terminal.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, GROUP, NVAL
      REAL      OBUFF(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     GROUP  - group number in the message file
C     NVAL   - number of values to get
C     OBUFF  - array of the values
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cterif.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   BVFLG, QNUM, I
C
C     + + + EXTERNALS + + +
      EXTERNAL   PRNTXT
C
C     + + + END SPECIFICATIONS + + +
C
 20   CONTINUE
        BVFLG = 0
C       fill array ( enter values seperated by commas or blanks.)
        QNUM = 12
        CALL PRNTXT (MESSFL,GROUP,QNUM)
        READ (TERIFL,*,ERR=25) (OBUFF(I),I=1,NVAL)
        IF (LOGFL.GT.0) WRITE (LOGFL,*) (OBUFF(I),I=1,NVAL)
C       input complete.
        QNUM = 14
        CALL PRNTXT (MESSFL,GROUP,QNUM)
        GO TO 30
 25     CONTINUE
C         bad input value, try again from start.
          BVFLG = 1
          QNUM = 13
          CALL PRNTXT (MESSFL,GROUP,QNUM)
 30     CONTINUE
      IF (BVFLG .EQ. 1) GO TO 20
C
      RETURN
      END
C
C
C
      SUBROUTINE   TRMINL
     I                    (TRMI, TEMP, LOG)
C
C     + + + PURPOSE  + + +
C     This routine set Fortran unit numbers for the log file and the
C     terminal.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TRMI, TEMP, LOG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TRMI   - Fortran unit number for terminal input
C     TEMP   - Fortran unit number of temporary file for terminal input
C     LOG    - Fortran unit number of log file of user responses
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cterif.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      TERIFL = TRMI
      TEMPFL = TEMP
      LOGFL  = LOG
C
      RETURN
      END
