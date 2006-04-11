C     zutilx.f 2.1 9/4/91
C
C
C
      SUBROUTINE   ZB2ON
C
C     + + + PURPOSE + + +
C     turn on the middle box, move upper text to make it fill whats
C     available
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    LMXSCB
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZUNCNT, ZCNTER, ZDRWSC, ZWRTMN, ZSTCMA, ZWRMN1
C
C     + + + END SPECIFICATIONS + + +
C
C     set local version of max screen buffer to remove
C     any chance of parameter MXSCBF being modified
      LMXSCB= MXSCBF
C
      IF (ZMNCSL.EQ.1 .AND. ZDTYP.LE.2) THEN
C       uncenter existing text
        CALL ZUNCNT (ZCLEN,ZCWID,ZHLLEN,LMXSCB,
     M               ZMNNLI,ZMNTXT,ZMNLEN,ZHLLIN,ZHLCOL)
      END IF
C     turn off quiet
      ZQUFG= 0
C     may turn back on later
      CALL ZSTCMA (20,1)
C     resize boxes
      ZB1N= 10
      ZB2N= 4
      IF (ZMNCSL.EQ.1 .AND. ZDTYP.LE.2) THEN
C       try to center again
        CALL ZCNTER (ZB1N,ZHLLEN,
     M               ZMNNLI,ZMNTXT,ZMNLEN,ZHLLIN,ZHLCOL,
     O               ZCLEN,ZCWID)
      END IF
      IF (ZDTYP.NE.0) THEN
C       redraw boxes (if not at very beginning)
        CALL ZDRWSC
        IF (ZDTYP.NE.4) THEN
C         refill in text
          CALL ZWRTMN (ZB1F,ZB1N,1,ZMNTXT(ZMNCSL),
     M                 ZMNLEN(ZMNCSL))
        ELSE
C         rewrite 2-d data screen
          CALL ZWRMN1
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZCOLOR
     I                   (ITYPE,BORD,FCOL,BCOL,RFCOL,RBCOL)
C
C     + + + PURPOSE + + +
C     control screen colors in normal and reverse video
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ITYPE,BORD,FCOL,BCOL,RFCOL,RBCOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITYPE  - defines terminal type
C     FCOL   - foreground text color
C     BCOL   - background text color
C     RFCOL  - foreground reverse video color
C     RBCOL  - background reverse video color
C     BORD   - border color code
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      ZNFCOL= 37
      ZNBCOL= 40
      ZRFCOL= 30
      ZRBCOL= 47
      ZBORD = BORD
      IF (ITYPE .NE. 0) ZITYPE = ITYPE
      IF (ITYPE .EQ. 2) THEN
        ZNFCOL= FCOL
        ZNBCOL= BCOL
        ZRFCOL= RFCOL
        ZRBCOL= RBCOL
        ZBORD = BORD
      END IF
C
      RETURN
      END
C
C
C
      LOGICAL FUNCTION   ZCKINT
     I                          (STRING,
     O                           IVAL)
C
C     + + + PURPOSE + + +
C     validate an integer character string
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER STRING*(*)
      INTEGER   IVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STRING  - string containing integer
C     IVAL    - converted integer
C
C     + + + COMMON BLOCKS + + +
C     numeric constants
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,K
      INTEGER   ERR
      CHARACTER*1 STRIN1(78)
C
C     + + + FUNCTIONS + + +
      INTEGER    ZLNTXT,CRINTE
C
C     + + + INTRINSICS + + +
      INTRINSIC  LEN
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZLNTXT, CRINTE, CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
C     subroutine CRINTE requires a right-justified string
      I = LEN(STRING)
      J = ZLNTXT(STRING)
      K = I
      IF (J .NE. K) THEN
 20      CONTINUE
         IF (J .GT. 0) THEN
            STRING(K:K) = STRING(J:J)
            K = K - 1
            J = J - 1
            GO TO 20
         END IF
         STRING(1:K) = ' '
      END IF
C
      CALL CVARAR (I,STRING,I,STRIN1)
C     use CRINTE to determine if there is an error
      ZCKINT = .TRUE.
      ERR = -1
      IVAL = CRINTE(ERR,I,STRIN1)
      IF (IVAL .EQ. ERR) THEN
C        CRINTE returned -1, but the string may have been '-1'!
         ERR = 0
         IVAL = CRINTE(ERR,I,STRIN1)
         IF (IVAL .EQ. ERR) THEN
C           the string couldn't have been both '-1' and '0'!
            ZCKINT = .FALSE.
         END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZDELET
     I                    (FILE)
C
C     + + + PURPOSE + + +
C     delete a file using FORTRAN OPEN statement
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER FILE*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FILE   - name of file to be deleted
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,LUN
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETFUN, QFCLOS
C
C     + + + END SPECIFICATIONS + + +
C
      I= 1
      CALL GETFUN (I,LUN)
      OPEN (UNIT=LUN, FILE=FILE, STATUS='OLD',ERR=999)
C     close with delete opion on (I=1)
      CALL QFCLOS (LUN,I)
C     skip to here if open fails
 999  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZDRWDW
     I                    (ITYPE,WNDONM,LIN1,COL1,NLIN,NCOL,APPNAM)
C
C     + + + PURPOSE + + +
C     draw a window on the monitor screen for given upper left
C     corner coordinates and window size
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ITYPE,LIN1,COL1,NLIN,NCOL
      CHARACTER WNDONM*(*),APPNAM*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITYPE  - window type: =1 horizontal/vertical dashes
C                           =2 IBM PC line graphics
C                           =3 VT100 line graphics
C     WNDONM - name to appear at top of window
C     LIN1   - upper left corner line number
C     COL1   - upper left corner column number
C     NLIN   - number of lines of window
C     NCOL   - number of columns of window
C     APPNAM - application name
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,K,WNCOL,SCOL
      CHARACTER STRING*80
      CHARACTER ESC*1,H*1,V*1,SO*1,UL*1,UR*1,LL*1,LR*1
C
C     + + + FUNCTIONS + + +
      INTEGER    ZLNTXT
C
C     + + + INTRINSICS + + +
      INTRINSIC  CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZLNTXT, ZWRSCR, ZFMTWR
C
C     + + + END SPECIFICATIONS + + +
C
C     set graphics mode
      IF (ITYPE .EQ. 3) THEN
C       VT100 line graphics
        ESC= CHAR(27)
        H  = CHAR(113)
        V  = CHAR(120)
        SO = CHAR(15)
        UL = CHAR(108)
        UR = CHAR(107)
        LL = CHAR(109)
        LR = CHAR(106)
        CALL ZFMTWR(ESC//SO//'(0')
      ELSE IF (ITYPE .EQ. 2) THEN
C       IBM PC line graphics
        H  = CHAR(196)
        V  = CHAR(179)
        UL = CHAR(218)
        UR = CHAR(191)
        LL = CHAR(192)
        LR = CHAR(217)
      ELSE
C       horizontal/vertical dashes
        H  = '-'
        V  = '|'
        UL = '+'
        UR = '+'
        LL = '+'
        LR = '+'
      END IF
C
C     write window
      STRING(1:1) = UL
      DO 100 I = 2, NCOL-1
        STRING(I:I) = H
 100  CONTINUE
      STRING(NCOL:NCOL) = UR
      CALL ZWRSCR(STRING(1:NCOL),LIN1,COL1)
      J = COL1 + NCOL - 1
      K = LIN1 + NLIN - 1
      DO 200 I = LIN1+1, K-1
        CALL ZWRSCR(V,I,COL1)
        CALL ZWRSCR(V,I,J)
 200  CONTINUE
      STRING(1:1) = LL
      STRING(NCOL:NCOL) = LR
      CALL ZWRSCR(STRING(1:NCOL),K,COL1)
C
      IF (ITYPE .EQ. 3) THEN
C       exit vt100 graphics mode
        CALL ZFMTWR(ESC//SO//'(B')
      END IF
C     see if window name exists
      J= ZLNTXT(WNDONM)
      IF (J.GT.0) THEN
C       put window name in top left of border
        CALL ZWRSCR (WNDONM(1:J),LIN1,COL1+2)
C       determine where name on right side could start
        WNCOL= COL1+ J+ 4
      ELSE
C       no screen name, plenty of room for name on right side
        WNCOL= COL1+ 2
      END IF
      J= ZLNTXT(APPNAM)
      IF (J.GT.0) THEN
C       put application name in top right of border
C       determine where name needs to start (right justified)
        SCOL= NCOL- J- 1
        IF (SCOL.LT.WNCOL) THEN
C         name needs to be truncated
          J   = NCOL- WNCOL- 1
          SCOL= WNCOL
        END IF
        CALL ZWRSCR (APPNAM(1:J),LIN1,SCOL)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZDRWSC
C
C     + + + PURPOSE + + +
C     draw EMIFE screen for user interaction.  The EMIFE screen
C     includes data window, message window, and command list.
C     (instructions are not written here)
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
      INCLUDE 'cscren.inc'
      INCLUDE 'cqrsp.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    QFLG,I,LFLD
C
C     + + + EXTERNALS + + +
      EXTERNAL   SCCLAL, ZDRWDW, ZSTWRT, ZWRHLP, ZLIMIT, ZSTCMA
      EXTERNAL   ZWRTCM, ZXPAD
C
C     + + + END SPECIFICATIONS + + +
C
C     clear screen
      CALL SCCLAL
C     always display commands
      ZCMDST= 1
C     determine field for help
      IF (ZDTYP.GE.3) THEN
C       use current data field
        LFLD= CFLD
      ELSE IF (ZDTYP.EQ.2) THEN
C       use current menu option
        LFLD= DANS
      ELSE
C       just use 1st field
        LFLD= 1
      END IF
C
      IF (ZWN2ID.EQ.8 .AND. ZDTYP.LE.2) THEN
C       limits not possible, try help
        ZWN2ID= 7
      END IF
      IF (ZWN2ID.EQ.7 .AND. GPTR.EQ.0 .AND. HPTR(LFLD).EQ.0) THEN
C       no help, set to quiet mode
        ZQUFG = 1
C       no middle window id
        ZWN2ID= 0
C       quiet option now not available
        CALL ZSTCMA (20,0)
C       resize boxes
        ZB1N= ZB1N+ ZB2N+ 2
        ZB2N= 0
      END IF
C
C     draw menu window with screen name
      CALL ZDRWDW(ZITYPE,ZSCNAM,ZB1F-1,1,ZB1N+2,80,ZAPNAM)
C
      QFLG= 1
      IF (ZWN2ID.EQ.6) THEN
C       display status
        ZWN2ID= 0
        CALL ZSTWRT (QFLG)
      ELSE IF (ZWN2ID.EQ.7) THEN
C       display help
        ZWN2ID=0
        I= 7
        CALL ZWRHLP (ZMESFL,GPTR,HPTR(LFLD),I,QFLG)
        ZWN2ID= 7
      ELSE IF (ZWN2ID.EQ.8) THEN
C       display limits
        ZWN2ID= 0
        CALL ZLIMIT
      ELSE IF (ZWN2ID.EQ.9) THEN
C       display tutor
        ZWN2ID= 0
        I= 9
        CALL ZWRHLP (ZMESFL,GPTR,HPTR(LFLD),I,QFLG)
        ZWN2ID= I
      ELSE IF (ZWN2ID.EQ.10) THEN
C       display commands
        ZWN2ID= 0
        CALL ZWRTCM (QFLG)
      ELSE IF (ZWN2ID.EQ.11) THEN
C       display scratch pad
        ZWN2ID= 0
        CALL ZXPAD (QFLG)
      END IF
C     reset error and instruction
      ZERR  = 0
      ZWN3ID= 0
      ZGP3  = 0
C
      IF (ZMNCSL+ZB1N.LE.ZMNNLI) THEN
C       page down available
        CALL ZSTCMA(14,1)
      ELSE
C       page down not available
        CALL ZSTCMA(14,0)
      END IF
C
      IF (ZB1N.GE.ZMNNLI) THEN
C       always start at top and display everything
        CALL ZSTCMA(15,0)
        ZMNCSL= 1
      END IF
C
      IF (ZSTNLN.GT.0) THEN
C       system status available
        CALL ZSTCMA(7,1)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZEMSTP
C
C     + + + PURPOSE + + +
C     save xpad to a file
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,FUN,PTHLEN
      CHARACTER*8  AIDPTH
      CHARACTER*12 IFLNAM
      CHARACTER*52 PTHNAM
      CHARACTER*64 FILNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETFUN, QFCLOS, GETPTH, BLDFNM
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (A78)
C
C     + + + END SPECIFICATIONS + + +
C
      I= 1
      CALL GETFUN(I,FUN)
C     first get path for file
      INCLUDE 'faidep.inc'
      CALL GETPTH (AIDPTH,
     O             PTHNAM,PTHLEN)
      IFLNAM= 'XPAD.DAT'
      CALL BLDFNM (PTHLEN,PTHNAM,IFLNAM,
     O             FILNAM)
      OPEN (UNIT=FUN,FILE=FILNAM,STATUS='OLD',ERR=10)
      GO TO 20
 10   CONTINUE
        OPEN(UNIT=FUN,FILE=FILNAM,STATUS='NEW')
 20   CONTINUE
C     xpad file open
 30   CONTINUE
C       write records to xpad file
        WRITE(FUN,2000) ZXPTXT(I)
        I= I+ 1
      IF (I.LE.10) GO TO 30
C     close the xpad file
      I= 0
      CALL QFCLOS(FUN,I)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZGTRET
     O                   (IRET)
C
C     + + + PURPOSE + + +
C     get return code from last screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IRET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IRET   - return code from last screen
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IRET= ZRET
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   ZLNSTR
     I                          (STRING)
C
C     + + + PURPOSE + + +
C     determine length of first word in a character string
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER STRING*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STRING - character string
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,L
      CHARACTER NULL*1
C
C     + + + INTRINSICS + + +
      INTRINSIC   LEN, CHAR
C
C     + + + END SPECIFICATIONS + + +
C
      NULL= CHAR(0)
      L   = LEN(STRING)
      I   = 1
      IF (L.GT.0) THEN
C       string is not all blanks
 10     CONTINUE
C         look until blank or null is found
          IF (STRING(I:I).EQ.' ' .OR. STRING(I:I).EQ.NULL) THEN
C           found one
            L= I- 1
          ELSE
C           check next one
            I= I+ 1
          END IF
        IF (I.LE.L) GO TO 10
      END IF
      I= I- 1
C
      ZLNSTR = I
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZMNSST
C
C     + + + PURPOSE + + +
C     set menu save flag
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      ZMNSAV= 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZSTADD
     I                   (STPOS,STTXT)
C
C     + + + PURPOSE + + +
C     add to system status buffer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      STPOS
      CHARACTER*78 STTXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STPOS  - position in status buffer to add
C     STTXT  - text to put in status buffer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    STDIFG
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZSTADQ
C
C     + + + END SPECIFICATIONS + + +
C
      STDIFG= 1
      CALL ZSTADQ (STPOS,STTXT,STDIFG)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZSTADQ
     I                   (STPOS,STTXT,STDIFG)
C
C     + + + PURPOSE + + +
C     add to system status buffer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      STPOS,STDIFG
      CHARACTER*78 STTXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STPOS  - position in status buffer to add
C     STTXT  - text to put in status buffer
C     STDIFG - status display flag (0 - dont display, 1 - display now)
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   QFLG
C
C     + + + FUNCTIONS + + +
      INTEGER   ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZLNTXT, ZSTWRT, ZSTCMA
C
C     + + + END SPECIFICATIONS + + +
C
      IF (STPOS.LE.10) THEN
C       ok to add
        ZSTTXT(STPOS)= STTXT
        ZSTLEN(STPOS)= ZLNTXT(STTXT)
C
        IF (STPOS.GT.ZSTNLN .AND. ZSTLEN(STPOS).GT.0) THEN
C         new rec of text at end
          ZSTNLN= STPOS
        ELSE IF (STPOS.EQ.ZSTNLN .AND. ZSTLEN(STPOS).EQ.0) THEN
C         delete last record
          IF (ZSTNLN.GT.1) THEN
C           loop to delete unused records
 10         CONTINUE
              ZSTNLN= ZSTNLN- 1
            IF (ZSTLEN(ZSTNLN).EQ.0.AND. ZSTNLN.GT.1) GO TO 10
          END IF
          IF (ZSTLEN(ZSTNLN).EQ.0) THEN
C           no status left
            CALL ZSTCMA(7,0)
            ZSTNLN= 0
          END IF
        END IF
C       update current line to show change at bottom of box
        ZSTCSL= STPOS- 3
        IF (ZSTCSL.LT.1) THEN
C         show whole small box
          ZSTCSL= 1
        END IF
        IF (STDIFG.EQ.1) THEN
C         display revised status
          QFLG= 1
          CALL ZSTWRT (QFLG)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZSTORE
     I                   (VALUE,DTYP,FMT,POS,
     O                    STRING)
C
C     + + + PURPOSE + + +
C     store data in a buffer in specific format
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   POS
      CHARACTER VALUE*(*),DTYP,FMT*(*),STRING*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VALUE  - data value in text form
C     DTYP   - data type (I,R,D,C)
C     FMT    - output format
C     POS    - starting byte in buffer to receive data
C     STRING - buffer to store data
C
C     + + + COMMON BLOCKS + + +
C     numeric constants
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L,M,N,IVAL
      REAL      RVAL,RERR
      CHARACTER CVAL*80
      DOUBLE PRECISION DVAL,DERR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (STR1,CVAL)
      CHARACTER*1  STR1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER    ZLNTXT
      REAL       CHRDEC
      DOUBLE PRECISION   CHRDPR
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZLNTXT, CHRDEC, CHRDPR
C
C     + + + DATA INITIALIZATIONS + + +
Chnb      DATA RERR,DERR/-1.0E29,-1.0E29/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(I10)
C
C     + + + END SPECIFICATIONS + + +
C
      RERR = -R0MAX
      DERR = -D0MAX
      CVAL= VALUE
      M   = POS / 1000
      N   = POS - 1000 * M
      IF (DTYP .EQ. 'I') THEN
C       integer
        READ (VALUE,1000,ERR=999) IVAL
        WRITE (STRING(M:N),FMT,ERR=999) IVAL
      ELSE IF (DTYP .EQ. 'R') THEN
C       real
        L= ZLNTXT(VALUE)
        RVAL= CHRDEC(L,STR1)
        IF (RVAL.LE.RERR) GO TO 999
        WRITE (STRING(M:N),FMT,ERR=999) RVAL
      ELSE IF (DTYP .EQ. 'D') THEN
C       double precision
        L= ZLNTXT(VALUE)
        DVAL= CHRDPR(L,STR1)
        IF (DVAL.LE.DERR) GO TO 999
        WRITE (STRING(M:N),FMT,ERR= 999) DVAL
      ELSE
C       character
        L = N - M + 1
        STRING(M:N) = VALUE(1:L)
      END IF
C
 999  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZSTWRT
     I                    (QFLG)
C
C     + + + PURPOSE + + +
C     display system status
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   QFLG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     QFLG   - quick flag (0- stay in status mode if more than 4 lines,
C                          1- just display current 4 lines)
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IGKY,I,J,LWNDID,LGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZDRWDW,ZWRTB3,ZWRTB2,SCCUMV,ZGTKEY,ZCMDIS,ZB2ON
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ZQUFG.EQ.1) THEN
C       turn on middle box
        CALL ZB2ON
      END IF
      IF (ZWN2ID.NE.6) THEN
C       set window id
        ZWN2ID= 6
C       draw the status box
        CALL ZDRWDW(ZITYPE,ZWNNAM(ZWN2ID),ZB2F-1,1,ZB2N+2,80,' ')
      END IF
C     loop to write all or part of status
 10   CONTINUE
        IF (QFLG.EQ.0) THEN
C         more than a page?
          IF (ZSTNLN-ZSTCSL.GE.ZB2N) THEN
C           more status available
            IF (ZSTCSL.GT.1) THEN
C             not at top, may review also
              LGRP= 73
            ELSE
C             only below
              LGRP= 71
            END IF
          ELSE IF (ZSTCSL.GT.1) THEN
C           none below, may review above
            LGRP= 72
          ELSE
C           no more pages of status
            LGRP= 0
          END IF
        ELSE
C         just display one page
          LGRP= 0
        END IF
C       write out the status text
        CALL ZWRTB2 (ZSTTXT(ZSTCSL),ZSTLEN(ZSTCSL))
        IF (LGRP.GT.0) THEN
C         erase valid commands
          I= -1
          CALL ZCMDIS(I,
     O                J)
C         user needs to tell what to do
          IGKY= 1
C         assume no error
          ZERR  = 0
          ZESCST= 0
C         id for instruction box
          LWNDID= 12
 20       CONTINUE
            IF (IGKY.EQ.0) THEN
C             error box instead of instruction
              LWNDID= 13
            END IF
C           display instruction
            CALL ZWRTB3(LWNDID,LGRP)
C           wait for keyboard interrupt
            CALL SCCUMV(ZCRLIN,ZCRCOL)
            CALL ZGTKEY(I,J)
C           assume invalid key
            IGKY= 0
            IF (I .EQ. 4 .AND. J .EQ. 3) THEN
C             f3, all done
              LGRP= 0
              IGKY= 1
              ZERR= 0
              ZESCST= 0
            ELSE IF (I.EQ.2 .AND. J.EQ.27) THEN
C             escape, all done
              LGRP= 0
              IGKY= 1
              ZERR= 0
              ZESCST= 0
            ELSE IF (I.EQ.3) THEN
C             arrow or page
              IF (J.EQ.1 .AND. ZSTCSL.GT.1) THEN
C               up a line
                IGKY  = 1
                ZSTCSL= ZSTCSL- 1
              ELSE IF (J.EQ.2 .AND. ZSTCSL+ZB2N.LE.ZSTNLN) THEN
C               down a line
                IGKY  = 1
                ZSTCSL= ZSTCSL+ 1
              ELSE IF (J.EQ.7 .AND. ZSTCSL.GT.1) THEN
C               up a page
                IGKY  = 1
                ZSTCSL= ZSTCSL- ZB2N
                IF (ZSTCSL.LT.1) ZSTCSL= 1
              ELSE IF (J.EQ.8 .AND. ZSTCSL+ZB2N.LE.ZSTNLN) THEN
C               down a page
                IGKY  = 1
                ZSTCSL= ZSTCSL+ ZB2N
                IF (ZSTCSL+ZB2N.GT.ZSTNLN) THEN
C                 dont go past bottom
                  ZSTCSL= ZSTNLN- ZB2N+ 1
                END IF
              END IF
            END IF
          IF (IGKY.EQ.0) GO TO 20
        END IF
      IF (LGRP.NE.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZUNCNT
     I                    (ZCLEN,ZCWID,ZHLLEN,LMXSCB,
     M                     ZMNNLI,ZMNTXT,ZMNLEN,ZHLLIN,ZHLCOL)
C
C     + + + PURPOSE + + +
C     uncenter menu text
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      ZCLEN,ZCWID,ZHLLEN,LMXSCB,
     $             ZMNNLI,ZMNLEN(LMXSCB),ZHLLIN,ZHLCOL
      CHARACTER*78 ZMNTXT(LMXSCB)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ZCLEN  - number of lines moved to center
C     ZCWID  - number of characters moved to center
C     ZHLLEN - length of highlighted field
C     LMXSCB - max number of lines in screen text buffer
C     ZMNNLI - number of lines in menu
C     ZMNTXT - text of menu
C     ZMNLEN - length of text lines
C     ZHLLIN - highlighted line number
C     ZHLCOL - highlighted column number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,ILEN
      CHARACTER*78 ZDMTXT
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZLNTXT
C
C     + + + END SPECIFICATIONS + + +
C
C     adding to existing text which is now displayed, uncenter
      DO 10 I= ZCLEN+1,ZMNNLI
        IF (ZMNLEN(I).GT.0) THEN
C         text to move
          ZDMTXT= ZMNTXT(I)(ZCWID+1:ZMNLEN(I))
        ELSE
C         move a blank line
          ZDMTXT= ' '
        END IF
        ZMNTXT(I-ZCLEN)= ZDMTXT
        ILEN= ZLNTXT(ZMNTXT(I-ZCLEN))
        IF (ILEN .GT. ZMNLEN(I-ZCLEN)) THEN
C         write more than currently on screen
          ZMNLEN(I-ZCLEN)= ILEN
        END IF
 10   CONTINUE
      IF (ZCLEN.GT.0) THEN
C       clear last lines
        DO 20 I= ZMNNLI-ZCLEN+1,ZMNNLI
C         length with be converted when written
          ZMNTXT(I)= ' '
 20     CONTINUE
C       change the number of text lines
        ZMNNLI= ZMNNLI- ZCLEN
      END IF
      IF (ZHLLEN.GT.0) THEN
C       move highlight back
        ZHLLIN= ZHLLIN- ZCLEN
        ZHLCOL= ZHLCOL- ZCWID
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZWRTCM
     I                    (QFLG)
C
C     + + + PURPOSE + + +
C     display a list of command descriptions
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   QFLG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     QFLG   - quick flag (0- stay in commands mode if more than 4 lines,
C                          1- just display current 4 lines)
C
C     + + + COMMON BLOCKS + + +
C     contains control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    LWNDID,IGKY,I,J,L,CMDCNT,CMDPTR(26),CMDPOS,
     1           LGRP,TGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZDRWDW, GETTXT, ZB2ON
      EXTERNAL   ZWRTB2, ZWRTB3, ZGTKEY, SCCUMV, ZCMDIS
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ZQUFG.EQ.1) THEN
C       turn off quiet
        CALL ZB2ON
      END IF
      IF (ZWN2ID.NE.10) THEN
C       set window id
        ZWN2ID= 10
C       draw the command window
        CALL ZDRWDW(ZITYPE,ZWNNAM(ZWN2ID),ZB2F-1,1,ZB2N+2,80,' ')
      END IF
C
C     how many commands are available
      CMDCNT= 0
      CMDPOS= 1
      I     = 0
10    CONTINUE
        I= I+ 1
        IF (ZCMDAV(I).GT.0) THEN
C         this command is available
          CMDCNT= CMDCNT+ 1
          CMDPTR(CMDCNT)= I
        END IF
      IF (I.LT.26) GO TO 10
C
      I= 0
20    CONTINUE
        IF (QFLG.EQ.0) THEN
C         how may commands
          IF (CMDCNT-CMDPOS.GE.ZB2N) THEN
C           more commands available
            IF (CMDPOS.GT.1) THEN
C             not at top, may review also
              LGRP= 73
            ELSE
C             only below
              LGRP= 71
            END IF
          ELSE IF (CMDPOS.GT.1) THEN
C           none below, may review above
            LGRP= 72
          ELSE
C           only one page of commands
            LGRP= 0
          END IF
        ELSE
C         only one page of commands
          LGRP= 0
        END IF
C       fill in command window
        DO 30 I= 1,4
          J= I+ CMDPOS- 1
          IF (J.LE.CMDCNT) THEN
C           command is available, get definition
            TGRP= CMDPTR(J)+ 10
            L   = 78
            CALL GETTXT (ZMESFL,ZSCLU,TGRP,
     M                   L,
     O                   ZHLTX1(1,I))
            ZHLPLN(I)= L
          ELSE
C           no more commands  to write
            ZHLTXT(I)= ' '
            ZHLPLN(I)= 0
          END IF
  30    CONTINUE
C
C       write a page of commands
        CALL ZWRTB2 (ZHLTXT,ZHLPLN)
        IF (LGRP.GT.0) THEN
C         erase valid commands
          I= -1
          CALL ZCMDIS(I,
     O                J)
C         user needs to tell what to do
          IGKY= 1
C         assume no error
          ZERR  = 0
          ZESCST= 0
C         instruction id
          LWNDID= 12
 40       CONTINUE
            IF (IGKY.EQ.0) THEN
C             error box instead of instruction
              LWNDID= 13
            END IF
C           display instruction
            CALL ZWRTB3 (LWNDID,LGRP)
C           wait for keyboard interrupt
            CALL SCCUMV(ZCRLIN,ZCRCOL)
            CALL ZGTKEY(I,J)
C           assume invalid key
            IGKY= 0
            IF (I .EQ. 4 .AND. J .EQ. 3) THEN
C             f3, all done
              LGRP= 0
              IGKY= 1
              ZERR= 0
              ZESCST= 0
            ELSE IF (I.EQ.2 .AND. J.EQ.27) THEN
C             escape, all done
              LGRP= 0
              IGKY= 1
              ZERR= 0
              ZESCST= 0
            ELSE IF (I.EQ.3) THEN
C             arrow or page
              IF (J.EQ.1 .AND. CMDPOS.GT.1) THEN
C               up a line
                IGKY  = 1
                CMDPOS= CMDPOS- 1
              ELSE IF (J.EQ.2 .AND. CMDPOS+ZB2N.LE.CMDCNT) THEN
C               down a line
                IGKY  = 1
                CMDPOS= CMDPOS+ 1
              ELSE IF (J.EQ.7 .AND. CMDPOS.GT.1) THEN
C               up a page
                IGKY  = 1
                CMDPOS= CMDPOS- ZB2N
                IF (CMDPOS.LT.1) CMDPOS= 1
              ELSE IF (J.EQ.8 .AND. CMDPOS+ZB2N.LE.CMDCNT) THEN
C               down a page
                IGKY  = 1
                CMDPOS= CMDPOS+ ZB2N
                IF (CMDPOS+ZB2N.GT.CMDCNT) THEN
C                 dont go past bottom
                  CMDPOS= CMDCNT- ZB2N+ 1
                END IF
              END IF
            END IF
          IF (IGKY.EQ.0) GO TO 40
        END IF
      IF (LGRP.NE.0) GO TO 20
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZWRTMN
     I                   (FROW,NROW,FLAG,ZTXT,
     M                    ZLEN)
C
C     + + + PURPOSE + + +
C     write menu to menu window
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FROW,NROW,FLAG,ZLEN(NROW)
      CHARACTER*78 ZTXT(NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FROW   - starting line number to write
C     NROW   - number of lines to write
C     FLAG   - flag to write highlight: 0 - no highlight,
C                                       1 - write highlight
C     ZTXT   - text to write
C     ZLEN   - length of each text line
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C     option field parameters
      INCLUDE 'czoptn.inc'
C     hidden field parameters
      INCLUDE 'czhide.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,LLEN,CMPTYP,LINE,COL,
     1             HIDRWL,HIDRWH,HIDCLL,HIDCLH
      CHARACTER*78 CBLNK
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZLNTXT, ZWRSCR, ZWRVDR, SCCUMV, ANPRGT
      EXTERNAL     WDPTSP, WOPWDS
C
C     + + + END SPECIFICATIONS + + +
C
      CBLNK= ' '
C
C     determine computer type
      I= 1
      CALL ANPRGT (I,CMPTYP)
C
      DO 100 I = 1, NROW
        J = FROW+ I- 1
        IF (CMPTYP.EQ.1) THEN
C         fill in full length for color on PC
          LLEN= 78
        ELSE
C         just use current length
          LLEN= ZLEN(I)
        END IF
        IF (LLEN.GT.0) THEN
C         something to write
          CALL ZWRSCR(ZTXT(I)(1:LLEN),J,2)
        END IF
        ZLEN(I)= ZLNTXT(ZTXT(I))
 100  CONTINUE
C
      IF (FLAG.EQ.1) THEN
C       data window, may need to do some special stuff for option fields
        DO 125 I= 1,MXFLD
C         see if any boxes need to be drawn next to option fields
          IF (OPBOX(I).GT.0) THEN
C           need box next to this field
            CALL WDPTSP (OPBOX(I),
     O                   LINE,COL)
C           check for duplicating inverse video
            IF (LINE.NE.ZHLLIN .OR.
     1          (COL.LT.ZHLCOL .OR. COL.GT.ZHLCOL+ZHLLEN-1)) THEN
C             use inverse video to simulate box
              CALL ZWRVDR (ZMNTXT(LINE)(COL:COL),LINE+1,COL+1)
            END IF
          END IF
 125    CONTINUE
C
        IF (NUMHID.GT.0) THEN
C         hidden fields exist, may need to hide them
          DO 150 I= 1,NUMHID
            IF (HIDFLG(I).EQ.1) THEN
C             this field is currently hidden
              CALL WOPWDS (HIDBOX(I),
     O                     HIDRWL,HIDCLL,HIDRWH,HIDCLH)
              DO 140 J= HIDRWL,HIDRWH
C               blank out area on screen
                CALL ZWRSCR (CBLNK(HIDCLL:HIDCLH),J+1,HIDCLL+1)
 140          CONTINUE
            END IF
 150      CONTINUE
        END IF
      END IF
C
      IF (FLAG .NE. 0 .AND. ZHLLIN .GT. 0) THEN
C       write in reverse video
        J = ZHLLIN + 1
        I = ZHLCOL + ZHLLEN - 1
        CALL ZWRVDR (ZMNTXT(ZHLLIN)(ZHLCOL:I),J,ZHLCOL+1)
      END IF
      CALL SCCUMV(ZCRLIN,ZCRCOL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZWTREA
     I                    (RVAL,
     O                     STRING)
C
C     + + + PURPOSE + + +
C     convert floating point number to numeric string
C
C     + + + DUMMY ARGUMENTS + + +
      REAL      RVAL
      CHARACTER STRING*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RVAL   - floating point number
C     STRING - numeric string
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,L,N
      CHARACTER FMT*7
C
C     + + + FUNCTIONS + + +
      INTEGER    ZLNTXT
C
C     + + + INTRINSICS + + +
      INTRINSIC  ALOG10, IABS, ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZLNTXT, ZLJUST
C
C     + + + END SPECIFICATIONS + + +
C
      IF ((RVAL-0) .LT. 1.0E-5) THEN
        STRING = '0'
      ELSE
        N = ALOG10(ABS(RVAL))
        IF (N .GT. 6 .OR. N .LT. -4) THEN
          WRITE (STRING,'(E10.4)') RVAL
        ELSE
          FMT = '(F10. )'
          IF (N .GE. 0) THEN
            L = 7 - N
          ELSE
            L = IABS(N) + 4
          END IF
          WRITE (FMT(6:6),'(I1)') L
          WRITE (STRING,FMT) RVAL
          I= 0
 100      CONTINUE
            L = ZLNTXT(STRING)
            IF (STRING(L:L) .EQ. '0') THEN
              STRING(L:L) = ' '
              I= 0
            ELSE
              I= 1
            END IF
          IF (I.EQ.0) GO TO 100
        END IF
      END IF
      CALL ZLJUST(STRING)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZWNSET
     I                   (WNNAME)
C
C     + + + PURPOSE + + +
C     set window name for a screen
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER WNNAME*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WNNAME - name to be displayed in upper left of screen
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + INTRINSICS + + +
      INTRINSIC  LEN
C
C     + + + END SPECIFICATIONS + + +
C
      ZSCNAM= ' '
      I= LEN(WNNAME)
      IF (I.GT.48) I= 48
      ZSCNAM= WNNAME(1:I)
      ZWNFLG= 1
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   ZCMDON
     I                         (ICMD)
C
C     + + + PURPOSE + + +
C     returns whether or not a command is available
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ICMD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ICMD   - command number being checked
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ZCMDAV(ICMD).EQ.1) THEN
C       command is available
        I= 1
      ELSE
C       command is not available
        I= 0
      END IF
C
      ZCMDON= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZCKREA
     I                    (RMIN,RMAX,RVAL,
     O                     RCHK)
C
C     + + + PURPOSE + + +
C     This routine checks the real RVAL against the minimum (RMIN)
C     and maximum (RMAX) values.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RCHK
      REAL      RMIN,RMAX,RVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RMIN   - minimum allowable value, -999. if there is no minimum
C     RMAX   - maximum allowable value, -999. if there is no maximum
C     RVAL   - value to be checked
C     RCHK   - indicator flag for valid RVAL
C              0 - invalid RVAL
C              1 - valid RVAL
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MINCHK,MAXCHK
      REAL      FACT
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + END SPECIFICATIONS + + +
C
      RCHK= 0
      FACT= RP1MIN
C
      MINCHK= 0
      IF      ((ABS(RMIN+999.0) .LT. (RP1MIN - 1.0))
     1  .OR.  ((RVAL.GE.0.0) .AND. (RVAL.GE.RMIN/FACT))
     2  .OR.  ((RVAL.LT.0.0) .AND. (RVAL.GT.RMIN*FACT)))
     3        MINCHK= 1
C
      MAXCHK= 0
      IF      ((ABS(RMAX+999.0) .LT. (RP1MIN - 1.0))
     1  .OR.  ((RVAL.GE.0.0) .AND. (RVAL.LE.RMAX*FACT))
     2  .OR.  ((RVAL.LT.0.0) .AND. (RVAL.LT.RMAX/FACT)))
     3        MAXCHK= 1
C
      IF (MINCHK.EQ.1 .AND. MAXCHK.EQ.1)  RCHK= 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZANSET
     I                   (APPNAM)
C
C     + + + PURPOSE + + +
C     set application name to appear in upper right corner of screens
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER APPNAM*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     APPNAM - application name
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + INTRINSICS + + +
      INTRINSIC  LEN
C
C     + + + END SPECIFICATIONS + + +
C
      ZAPNAM= ' '
      I= LEN(APPNAM)
      IF (I.GT.78) I= 78
      ZAPNAM= APPNAM(1:I)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZWNSOP
     I                   (CNUM,CVAL)
C
C     + + + PURPOSE + + +
C     Set variable text strings which will fill in
C     screen names in the defined locations.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNUM
      CHARACTER*8 CVAL(CNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNUM   - number of variable strings to be filled in screen name
C     CVAL   - array of text strings to fill in screen name
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,CNUM
        ZSCNOP(I)= CVAL(I)
 10   CONTINUE
C
      RETURN
      END
