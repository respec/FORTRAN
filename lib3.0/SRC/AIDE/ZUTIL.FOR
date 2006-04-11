C     zutil.f 2.1 9/4/91
C
C
C
      SUBROUTINE   ZEMIFE
     I                   (MESSFL,ZTUTDS,ZTUTGN)
C
C     + + + PURPOSE + + +
C     retrieve control parameters from users TERM.DAT file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,ZTUTDS,ZTUTGN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for WDM file
C     ZTUTDS - screen cluster number for tutorial
C     ZTUTGN - screen group number for tutorial
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
      INCLUDE 'cqrsp.inc'
      INCLUDE 'cscren.inc'
      INCLUDE 'czhide.inc'
      INCLUDE 'czoptn.inc'
      INCLUDE 'czglvl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I0,ILEN,BORD,F1,B1,F2,B2,ITYPE,CMPTYP,TRMTYP,
     $          SGRP,IPARM,FUN,PTHLEN,LMXFLD
      CHARACTER*8  AIDPTH
      CHARACTER*12 IFLNAM
      CHARACTER*52 PTHNAM
      CHARACTER*64 FILNAM
C
C     + + + FUNCTIONS + + +
      INTEGER     ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL   ANPRGT, COLSET, ZCOLOR, ZIPI, GETTXT, GETFUN
      EXTERNAL   QFCLOS, ZLNTXT, ZSTCMA, GETPTH, BLDFNM
C
C     + + + INPUT FORMATS + + +
1000  FORMAT (A78)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
C     save message file and DSN for EMIFE utils
      ZMESFL= MESSFL
      ZSCLU = 4
C     set local version of max fields to remove
C     any chance of parameter MXFLD being modified
      LMXFLD= MXFLD
C
C     set colors read from configuration file
      IPARM= 30
      CALL ANPRGT (IPARM,F1)
      IPARM= 34
      CALL ANPRGT (IPARM,B1)
C     set inverse video colors
      F2= B1
      B2= F1
      CALL COLSET (F1,B1)
      IPARM= 1
      CALL ANPRGT (IPARM,CMPTYP)
      IPARM= 2
      CALL ANPRGT (IPARM,TRMTYP)
      IF (CMPTYP.EQ.1) THEN
C       PC
        ITYPE= 2
      ELSE IF (TRMTYP.EQ.2) THEN
C       Prime or Vax
C       VT100 with graphics
        ITYPE= 3
      ELSE
C       dumb ANSI terminal
        ITYPE= 1
      END IF
      BORD= 1
      CALL ZCOLOR(ITYPE,BORD,F1,B1,F2,B2)
C
C     init window names
      SGRP= 1
      ILEN= 104
      CALL GETTXT (MESSFL,ZSCLU,SGRP,ILEN,ZWNAM1)
C     initialize application window name
      ZAPNAM = ' '
C     initialize screen name and variable text to fill in screen name
      ZSCNAM = ' '
      DO 3 I= 1,6
        ZSCNOP(I)= ' '
 3    CONTINUE
      ZWNFLG = 0
C     no middle window to start
      ZWN2ID= 0
      ZQUFG = 1
      ZB1F  = 2
      ZB2F  = 14
      ZB3F  = 20
      ZB1N  = 16
      ZB2N  = 0
      ZB3N  = 3
C     quiet not available
      CALL ZSTCMA(20,0)
C
C     init command names
      SGRP= 2
      ILEN= 120
      CALL GETTXT (MESSFL,ZSCLU,SGRP,ILEN,ZCMDN1)
      SGRP= 3
      ILEN= 36
      CALL GETTXT (MESSFL,ZSCLU,SGRP,ILEN,ZCMDN1(1,21))
C
C     initialize function keys from TERM.DAT
      DO 5 I= 1,10
        IPARM= I+ 90
        CALL ANPRGT(IPARM,ZFKEY(I))
 5    CONTINUE
C     F3 reserved for ESC
      ZFKEY(3)= 0
C     F10 not available
CPRH      ZFKEY(10)= 0
C     initialize available commands
      I= 26
      CALL ZIPI (I,I0,ZCMDAV)
C     init more stuff
      ZESCST= 0
      ZTUTPS(1)= ZTUTDS
      ZTUTPS(2)= ZTUTGN
      IF (ZTUTPS(1).GT.0) THEN
C       tutor available
        CALL ZSTCMA(5,1)
      END IF
C     init commands to be displayed
      ZCMDST= 1
C     'next' command always available
      CALL ZSTCMA(3,1)
C     'cmnd' command always available
      CALL ZSTCMA(2,1)
C     'xpad' command always available
      CALL ZSTCMA(21,1)
C     set number of lines to initialize
      ZMNNLI = MXSCBF
      ZMNLNI = 0
      ZWLDFL = 0
      WINFLG = 0
      DO 7 I = 1, ZMNNLI
C       initialize main window text
        ZMNTXT(I) = ' '
        ZMNLEN(I) = 0
 7    CONTINUE
C     no data type to start
      ZDTYP = 0
C     dont save menu
      ZMNSAV= 0
C     init special flags
      RSINIT= 0
      QUINIT= 0
      SPINIT= 0
      HPINIT= 0
C     no hidden fields
      NUMHID= 0
C     no option field box highlights
      CALL ZIPI (LMXFLD,I0,OPBOX)
C     no system status
      ZSTNLN= 0
      ZSTCSL= 1
      DO 10 I= 1,10
        ZSTTXT(I)= ' '
        ZXPTXT(I)= ' '
 10   CONTINUE
C     xpad
      ZXPLIN= 1
      ZXPCOL= 1
      ZXPNLN= 0
      ZXPCSL= 1
      I     = 1
C     open xpad file
      CALL GETFUN(I,FUN)
C     first get path for file
      INCLUDE 'faidep.inc'
      CALL GETPTH (AIDPTH,
     O             PTHNAM,PTHLEN)
      IFLNAM= 'XPAD.DAT'
      CALL BLDFNM (PTHLEN,PTHNAM,IFLNAM,
     O             FILNAM)
      OPEN (UNIT=FUN,FILE=FILNAM,STATUS='OLD',ERR=30)
 20   CONTINUE
C       read records from xpad file
        READ(FUN,1000,ERR=30,END=30) ZXPTXT(ZXPNLN+1)
        ZXPNLN= ZXPNLN+ 1
        ZXPLEN(ZXPNLN)= ZLNTXT(ZXPTXT(ZXPNLN))
        IF (ZXPLEN(ZXPNLN).GT.0) THEN
C         default place to type on first empty record
          ZXPLIN= ZXPNLN+ 1
        END IF
      IF (ZXPNLN.LT.10) GO TO 20
 30   CONTINUE
C     close the xpad file
      I= 0
      CALL QFCLOS(FUN,I)
C     jump to here if no existing xpad file
      IF (ZXPLIN.GT.10) THEN
C       no empty records, at end of last record
        ZXPLIN= 10
        ZXPCOL= ZXPLEN(ZXPLIN)+ 1
        IF (ZXPCOL.GT.78) ZXPCOL= 78
      END IF
C     always allow 10 records
      ZXPNLN= 10
C     first line to display
      ZXPCSL= ZXPLIN- 3
      IF (ZXPCSL.LT.1) ZXPCSL= 1
C
C     nothing in global values buffer
      ZGLCNT= 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZGTKYD
     I                    (IGRP,LFLD,
     O                     GROUP,CODE)
C
C     + + + PURPOSE + + +
C     get a key stroke for the data screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IGRP,LFLD,GROUP,CODE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IGRP   - group of standard instructions
C     LFLD   - current field
C     GROUP  - key group number
C     CODE   - ASCII code for key
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
      INCLUDE 'cscren.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,GETKEY,CMDCOD,CMDFG,EGRP,WNDID,LMOV,MROW,MCOL,MCMD
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZGTKEY, ZWRTB3, ZCMDEX, ZCMDIS, ZWRSCR, ZWRVDR, ZCMMSE
C
C     + + + END SPECIFICATIONS + + +
C
C     assume dont move any lines
      LMOV= 0
C
 10   CONTINUE
        I= 0
C       write valid commands
        CALL ZCMDIS (I,
     O               J)
C
        IF (IGRP.NE.85 .OR. WINFLG.EQ.0) THEN
C         display instructions (dont do when window is first turned on)
          WNDID= 12
          CALL ZWRTB3 (WNDID,IGRP)
        END IF
C       get a key stroke
        CALL ZGTKEY (GROUP,CODE)
C       assume we dont want any more
        GETKEY= 0
        CMDFG = 0
C
        IF (GROUP.EQ.4) THEN
C         function key, is it defined?
          IF (CODE.EQ.3) THEN
C           want command mode
            CMDFG = 1
C           unknown command
            CMDCOD= 0
          ELSE IF (ZFKEY(CODE).EQ.0) THEN
C           undefined function key
            WNDID= 13
            EGRP = 51
            CALL ZWRTB3 (WNDID,EGRP)
            GETKEY= 1
          ELSE
C           defined function key, want command mode
            CMDFG = 1
            CMDCOD= ZFKEY(CODE)
          END IF
        ELSE IF (GROUP.EQ.3) THEN
C         cursor movement
          IF (ZDTYP.EQ.1) THEN
C           move within text screen
            IF (CODE.EQ.7 .AND. ZCMDAV(15).EQ.1) THEN
C             up page
              CMDFG = 1
              CMDCOD= 15
            ELSE IF (CODE.EQ.8 .AND. ZCMDAV(14).EQ.1) THEN
C             down page
              CMDFG = 1
              CMDCOD= 14
            ELSE IF (CODE.EQ.5 .AND. ZCMDAV(15).EQ.1) THEN
C             home
              LMOV  = -ZMNCSL+ 1
              CMDFG = 1
              CMDCOD= 15
            ELSE IF (CODE.EQ.6 .AND. ZCMDAV(14).EQ.1) THEN
C             end of text
              LMOV  = ZMNNLI- ZB1N- ZMNCSL+ 1
              CMDFG = 1
              CMDCOD= 14
            ELSE IF (CODE.EQ.1 .AND. ZCMDAV(15).EQ.1) THEN
C             up one line
              LMOV  = -1
              CMDFG = 1
              CMDCOD= 15
            ELSE IF (CODE.EQ.2 .AND. ZCMDAV(14).EQ.1) THEN
C             down one line
              LMOV  = 1
              CMDFG = 1
              CMDCOD= 14
            ELSE
C             undefined cursor movement
              WNDID= 13
              EGRP = 51
              CALL ZWRTB3 (WNDID,EGRP)
              GETKEY= 1
            END IF
          END IF
        ELSE IF (GROUP.EQ.2) THEN
C         unprintable character
          IF (CODE.EQ.27) THEN
C           escape
            IF (ZESCST.EQ.1) THEN
C             clear error/warning message
              ZESCST= 0
            ELSE IF (WINFLG.EQ.0) THEN
C             go to command mode
              CMDFG = 1
              CMDCOD= 0
            END IF
          END IF
        ELSE IF (GROUP .EQ. 7) THEN
C         mouse movement
          MROW = CODE/1000
          MCOL = MOD(CODE,1000)
          IF (MROW .EQ. 24) THEN
            CALL ZCMMSE(MCOL,
     O                  MCMD)
          ELSE
            MCMD= 0
          END IF
C          WRITE(99,*) 'ZGTKYD:',GROUP,CODE,MROW,MCOL,MCMD
          IF (MCMD .GT. 0) THEN
            CMDCOD= MCMD
            GETKEY= 0
            CMDFG = 1
          END IF
        ELSE
C         printable character
          IF (CODE.EQ.63) THEN
C           question mark, get some help
            CMDFG = 1
            CMDCOD= 1
          ELSE IF (FPROT(LFLD).EQ.2 .AND. ZDTYP.GE.3) THEN
C           cant change protected field on parm1 or parm2 screen
            WNDID= 13
            EGRP = 88
            CALL ZWRTB3 (WNDID,EGRP)
            GETKEY= 1
          END IF
        END IF
C
C        WRITE(99,*) 'ZGTKYD:',CMDFG,LFLD,GETKEY,LMOV
        IF (CMDFG.EQ.1) THEN
C         command mode time
          I = ZHLCOL + ZHLLEN - 1
          IF (ZHLLIN.GT.0) THEN
C           turn off highlight
            IF (ZDTYP.NE.4) THEN
C             use exact line
              J= ZHLLIN
            ELSE
C             use offset line
              J= CROW+ NMHDRW
            END IF
            CALL ZWRSCR(ZMNTXT(J)(ZHLCOL:I),ZHLLIN+1,ZHLCOL+1)
          END IF
          CALL ZCMDEX (CMDCOD,LFLD,GETKEY,LMOV)
          IF (ZHLLIN.GT.0) THEN
C           rehighlight current field
            IF (ZDTYP.NE.4) THEN
C             use exact line
              J= ZHLLIN
            ELSE
C             use offset line
              J= CROW+ NMHDRW
              IF (CMDCOD.EQ.15) THEN
C               page up set, may need to set GROUP/CODE
                GROUP= 3
                CODE = 7
              ELSE IF (CMDCOD.EQ.14) THEN
C               page down set, may need to set GROUP/CODE
                GROUP= 3
                CODE = 8
              END IF
            END IF
            CALL ZWRVDR (ZMNTXT(J)(ZHLCOL:I),ZHLLIN+1,ZHLCOL+1)
          END IF
        END IF
C       clear escape status
        ZESCST= 0
      IF (GETKEY.EQ.1) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZCMDEX
     M                    (CMDCOD,CFLD,GETKEY,LMOV)
C
C     + + + PURPOSE + + +
C     handle command input and execution
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CMDCOD,CFLD,GETKEY,LMOV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CMDCOD - command code
C     CFLD   - current field
C     GETKEY - get a key flag (0- no, 1- yes)
C     LMOV   - relative number of lines to move
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,L,GROUP,CODE,LGRP,WNDID,CMDGUD,CURCMD,
     1            NANS,LANS,ULEN,TLEN,QFLG,CMDVAL
      CHARACTER*1 CHCMD(1)
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZWRTB3, ZGTKEY, ZWRHLP, CHKANQ, ZCMDIS, ZWRTMN
      EXTERNAL    ZWRTCM, ZXPAD, ZQUIET, ZSTWRT, ZWINDO, ZSTCMA, ZLIMIT
C
C     + + + END SPECIFICATIONS + + +
C
      QFLG= 0
C
C     set current command to next
      CURCMD= 1
      IF (ZCMDAV(1).NE.0) CURCMD= CURCMD+1
C      IF (ZCMDAV(2).NE.0) CURCMD= CURCMD+1
C      WRITE(99,*) 'ZCMDEX:',CMDCOD
 10   CONTINUE
        IF (CMDCOD.EQ.0) THEN
 20       CONTINUE
C           write instruction, get a command
            WNDID= 12
            LGRP = 61
            CALL ZWRTB3 (WNDID,LGRP)
            CMDGUD= 1
C           highlight current command
            CALL ZCMDIS (CURCMD,
     O                   CMDVAL)
C           get a key
            CALL ZGTKEY (GROUP,CODE)
C           assume no error
            ZERR  = 0
            IF (GROUP.EQ.2 .AND. CODE.EQ.27) THEN
C             escape back to data
              CMDCOD= 0
            ELSE IF (GROUP.EQ.4 .AND. CODE.EQ.3) THEN
C             f3 back to data
              CMDCOD= 0
            ELSE IF (GROUP.EQ.1 .AND. CODE.NE.63) THEN
C             get the command name
              NANS= 26
              LANS= 6
              ULEN= 1
              TLEN= NANS* LANS
              CHCMD(1)= CHAR(CODE)
              CALL CHKANQ(NANS,LANS,ULEN,TLEN,ZCMDN1,
     M                    CHCMD,
     O                    CMDCOD,L)
            ELSE IF (GROUP.EQ.3 .AND. CODE.EQ.4) THEN
C             try moving left
              CMDGUD= 0
              IF (CURCMD.EQ.1) THEN
C               at leftmost command, move to rightmost
                CURCMD= ZCMDMX
              ELSE
C               ok to move
                CURCMD= CURCMD- 1
              END IF
            ELSE IF (GROUP.EQ.3 .AND. CODE.EQ.3) THEN
C             try moving right
              CMDGUD= 0
              IF (CURCMD.EQ.ZCMDMX) THEN
C               at rightmost command, move to leftmost
                CURCMD= 1
              ELSE
C               ok to move
                CURCMD= CURCMD+ 1
              END IF
            ELSE IF (GROUP.EQ.2 .AND. CODE.EQ.13) THEN
C             carriage return, get command code from CMDVAL
              CMDCOD= CMDVAL
            ELSE
C             everything else is an error
              CMDGUD= 0
              LGRP  = 64
              WNDID = 13
              CALL ZWRTB3 (WNDID,LGRP)
            END IF
          IF (CMDGUD.EQ.0) GO TO 20
        END IF
C
C       assume need another keystroke
        GETKEY= 1
C       set current command to undefined
        CURCMD= 0
        CALL ZCMDIS (CURCMD,
     O               CMDVAL)
C
C        WRITE(99,*) 'ZCMDE2:',CMDCOD,ZCMDAV(CMDCOD)
        IF (CMDCOD.EQ.0) THEN
C         no more commands needed
          CMDCOD= -1
        ELSE
C         execute a command
          IF (ZCMDAV(CMDCOD).EQ.0) THEN
C           invalid command in this context
            WNDID = 13
            LGRP  = 52
            CALL ZWRTB3 (WNDID,LGRP)
            CMDCOD= -1
          ELSE
C           do the command
            GO TO (110,120,130,140,150,160,170,180,190,200,210,
     1             220,230,240,250,260,270,280,290,300,310), CMDCOD
C
 110        CONTINUE
C             help
              WNDID= 7
              CALL ZWRHLP (ZMESFL,GPTR,HPTR(CFLD),WNDID,QFLG)
              GO TO 400
C
 120        CONTINUE
C             commands
              CALL ZWRTCM (QFLG)
              GO TO 400
C
 130        CONTINUE
C             next
              ZRET  = 1
              GETKEY= 0
              GO TO 400
C
 140        CONTINUE
C             previous
              ZRET  = 2
              GETKEY= 0
              GO TO 400
C
 150        CONTINUE
C             tutor
              GO TO 400
C
 160        CONTINUE
C             limits
              ZWN2ID= -1
              CALL ZLIMIT
              GO TO 400
C
 170        CONTINUE
C             status
              CALL ZSTWRT (QFLG)
              GO TO 400
C
 180        CONTINUE
C             oops
              ZRET  = -1
              GETKEY= 0
              GO TO 400
C
 190        CONTINUE
C             window
              CALL ZWINDO
              GO TO 400
C
 200        CONTINUE
C             move
              GO TO 400
C
 210        CONTINUE
C             view
              GO TO 400
C
 220        CONTINUE
C             begin
              ZRET  = 3
              GETKEY= 0
              GO TO 400
C
 230        CONTINUE
C             go to
              ZRET  = 4
              GETKEY= 0
              GO TO 400
C
 240        CONTINUE
C             down page
              IF (ZDTYP.NE.4) THEN
C               paging for 2-d data handled elsewhere
                IF (LMOV.EQ.0) THEN
C                 figure out how far down to move
                  LMOV= ZB1N
                END IF
                IF (LMOV+ZMNCSL+ZB1N-1.GE.ZMNNLI) THEN
C                 cant move that far
                  LMOV= ZMNNLI- ZMNCSL- ZB1N+ 1
C                 page down not available
                  CALL ZSTCMA(14,0)
                END IF
C               page up available
                CALL ZSTCMA(15,1)
              END IF
              GETKEY= 0
              GO TO 400
C
 250        CONTINUE
C             up page
              IF (ZDTYP.NE.4) THEN
C               paging for 2-d data handled elsewhere
                IF (LMOV.EQ.0) THEN
C                 figure out how far to move up
                  LMOV= -ZB1N
                END IF
                IF (LMOV+ZMNCSL.LE.1) THEN
C                 cant move that far
                  LMOV= -ZMNCSL+ 1
C                 page up not available
                  CALL ZSTCMA(15,0)
                END IF
C               page down available
                CALL ZSTCMA(14,1)
              END IF
              GETKEY= 0
              GO TO 400
C
 260        CONTINUE
C             interupt
              ZRET  = 7
              GETKEY= 0
              GO TO 400
C
 270        CONTINUE
C             exit
              ZRET  = 5
              GETKEY= 0
              GO TO 400
C
 280        CONTINUE
C             abort
              ZRET  = 6
              GETKEY= 0
              GO TO 400
C
 290        CONTINUE
C             keys
              GO TO 400
C
 300        CONTINUE
C             turn quiet on
              CALL ZQUIET
              GO TO 400
C
 310        CONTINUE
C             xpad
              CALL ZXPAD (QFLG)
              GO TO 400
C
 400        CONTINUE
C           never need another command
CPRH        CMDCOD= -1
          END IF
        END IF
C
        IF (LMOV.NE.0) THEN
C         rewrite screen
          ZMNCSL= ZMNCSL+ LMOV
          DO 245 I= ZMNCSL,ZMNCSL+ZB1N-1
C           check for longer lengths in current display
            L= ZMNLEN(I-LMOV)
            IF (L.GT.ZMNLEN(I)) THEN
C             current longer
              ZMNLEN(I)= L
            END IF
 245      CONTINUE
C         write a page of text
          CALL ZWRTMN (ZB1F,ZB1N,1,ZMNTXT(ZMNCSL),
     M                 ZMNLEN(ZMNCSL))
C         movement complete
          LMOV= 0
        END IF
C
      IF (GETKEY.EQ.1 .AND. CMDCOD.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZXPAD
     I                   (QFLG)
C
C     + + + PURPOSE + + +
C     display and modify scratch pad
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   QFLG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     QFLG   - quick flag (0- stay in status mode if more than 4 lines,
C                          1- just display current 4 lines)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,LWNDID,LGRP,IGKY,LLIN,LCOL,INSFG
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZB2ON, ZWRTB2, ZCMDIS, ZWRTB3, SCCUMV, ZGTKEY, ZDRWDW
      EXTERNAL    ZWRVDR, ZWRSCR
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ZQUFG.EQ.1) THEN
C       turn on middle box
        CALL ZB2ON
      END IF
      IF (ZWN2ID.NE.11) THEN
C       set window id
        ZWN2ID= 11
C       draw the xpad box
        CALL ZDRWDW(ZITYPE,ZWNNAM(ZWN2ID),ZB2F-1,1,ZB2N+2,80,' ')
      END IF
C     loop to write all or part of xpad
 10   CONTINUE
        IF (QFLG.EQ.0) THEN
C         how many lines?
          IF (ZXPNLN.GT.ZXPLIN) THEN
C           more xpad available
            IF (ZXPLIN.GT.1) THEN
C             not at top, may review also
              LGRP= 93
            ELSE
C             only below
              LGRP= 91
            END IF
          ELSE IF (ZXPLIN.GT.1) THEN
C           none below, may review above
            LGRP= 92
          ELSE
C           no more pages of xpad, may modify
            LGRP= 94
          END IF
        ELSE
C         show current 4 lines
          LGRP= 0
        END IF
C       write out the xpad text
        CALL ZWRTB2(ZXPTXT(ZXPCSL),ZXPLEN(ZXPCSL))
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
C         insert mode off
          INSFG= 0
 20       CONTINUE
            IF (IGKY.EQ.-1) THEN
C             error box instead of instruction
              LWNDID= 13
            END IF
C           display instruction
            CALL ZWRTB3(LWNDID,LGRP)
C           display where to type as inverse video
            LLIN= ZB2F+ ZXPLIN- ZXPCSL
            LCOL= ZXPCOL+ 1
            CALL ZWRVDR (ZXPTXT(ZXPLIN)(ZXPCOL:ZXPCOL),LLIN,LCOL)
C           wait for keyboard interrupt
            CALL SCCUMV(ZCRLIN,ZCRCOL)
            CALL ZGTKEY(I,J)
C           remove highlight
            CALL ZWRSCR(ZXPTXT(ZXPLIN)(ZXPCOL:ZXPCOL),LLIN,LCOL)
C           assume invalid key
            IGKY= 0
            IF (I.EQ.4 .AND. J.EQ.3) THEN
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
C             cursor movement
              INSFG= 0
              IF (J.EQ.1 .AND. ZXPLIN.GT.1) THEN
C               up a line
                ZXPLIN= ZXPLIN- 1
                IF (ZXPLIN.LT.ZXPCSL) THEN
C                 off page at top, move back
                  ZXPCSL= ZXPCSL- 1
C                 rewrite screen
                  IGKY  = 1
                END IF
              ELSE IF (J.EQ.2 .AND. ZXPLIN.LT.ZXPNLN) THEN
C               down a line
                ZXPLIN= ZXPLIN+ 1
                IF (ZXPLIN.GE.ZXPCSL+ZB2N) THEN
C                 off page at bottom, move down
                  ZXPCSL= ZXPCSL+ 1
C                 rewrite screen
                  IGKY  = 1
                END IF
              ELSE IF (J.EQ.7 .AND. ZXPLIN.GT.1) THEN
C               up a page
                IGKY  = 1
                ZXPCSL= ZXPCSL- ZB2N
                IF (ZXPCSL.LT.1) THEN
C                 dont go past top
                  ZXPCSL= 1
                END IF
                ZXPLIN= ZXPLIN- ZB2N
                IF (ZXPLIN.LT.1) THEN
C                 dont go past top
                  ZXPLIN= 1
                END IF
              ELSE IF (J.EQ.8 .AND. ZXPLIN.LT.ZXPNLN) THEN
C               down a page
                IGKY  = 1
                ZXPCSL= ZXPCSL+ ZB2N
                IF (ZXPCSL+ZB2N.GT.ZXPNLN) THEN
C                 dont go past bottom
                  ZXPCSL= ZXPNLN- ZB2N+ 1
                END IF
                ZXPLIN= ZXPLIN+ ZB2N
                IF (ZXPLIN.GT.ZXPNLN) THEN
C                 dont go past bottom
                  ZXPLIN= ZXPNLN
                END IF
              ELSE IF (J.EQ.3 .AND. ZXPCOL.LT.78) THEN
C               move right
                ZXPCOL= ZXPCOL+ 1
              ELSE IF (J.EQ.4 .AND. ZXPCOL.GT.1) THEN
C               move left
                ZXPCOL= ZXPCOL- 1
              ELSE IF (J.EQ.5) THEN
C               home
                ZXPCOL= 1
              ELSE IF (J.EQ.6) THEN
C               end
                ZXPCOL= ZXPLEN(ZXPLIN)+ 1
              ELSE IF (J.EQ.9) THEN
C               delete
                ZXPTXT(ZXPLIN)(ZXPCOL:78)= ZXPTXT(ZXPLIN)(ZXPCOL+1:78)
                CALL ZWRSCR(ZXPTXT(ZXPLIN)(ZXPCOL:78),LLIN,LCOL)
              ELSE IF (J.EQ.10) THEN
C               insert
                INSFG= 1
              END IF
            ELSE IF (I.EQ.1) THEN
              IF (INSFG.EQ.0) THEN
C               replace character
                ZXPTX1(ZXPCOL,ZXPLIN)= CHAR(J)
C               write on screen
                CALL ZWRSCR(ZXPTX1(ZXPCOL,ZXPLIN),LLIN,LCOL)
              ELSE
C               insert character
                ZXPTXT(ZXPLIN)(ZXPCOL:78)=
     1            CHAR(J)//ZXPTXT(ZXPLIN)(ZXPCOL+1:77)
C               write on screen
                CALL ZWRSCR(ZXPTXT(ZXPLIN)(ZXPCOL:78),LLIN,LCOL)
              END IF
              IF (ZXPCOL.LT.78) THEN
C               move to next character position
                ZXPCOL= ZXPCOL+ 1
                IF (ZXPCOL.GT.ZXPLEN(ZXPLIN)) THEN
C                 record is now longer
                  ZXPLEN(ZXPLIN)= ZXPCOL
                END IF
                IF (ZXPCOL.GT.ZB2LEN(LLIN-ZB2F+1)) THEN
C                 line on screen is now longer
                  ZB2LEN(LLIN-ZB2F+1)= ZXPCOL
                END IF
              ELSE IF (ZXPLIN.LT.10) THEN
C               ok to go to next record
                ZXPLIN= ZXPLIN+ 1
                ZXPCOL= 1
                IF (ZXPCSL+ZB2N.GE.ZXPLIN) THEN
C                 move current start line
                  ZXPCSL= ZXPCSL+ 1
                  IGKY= 1
                END IF
              END IF
            ELSE IF (I.EQ.2 .AND. J.EQ.13 .AND. ZXPLIN.LT.10) THEN
C             return
              ZXPLIN= ZXPLIN+ 1
              IF (ZXPLIN.GE.ZXPCSL+ZB2N) THEN
C               off page at bottom, move down
                ZXPCSL= ZXPCSL+ 1
C               rewrite screen
                IGKY  = 1
              END IF
              ZXPCOL= 1
            ELSE IF (I.EQ.2 .AND. J.EQ.8 .AND. ZXPCOL.GT.1) THEN
C             backspace
              ZXPTXT(ZXPLIN)(ZXPCOL-1:78)= ZXPTXT(ZXPLIN)(ZXPCOL:78)
              ZXPCOL= ZXPCOL- 1
              LCOL  = LCOL- 1
              CALL ZWRSCR(ZXPTXT(ZXPLIN)(ZXPCOL:78),LLIN,LCOL)
            ELSE
C             unknown character
              IGKY= -1
            END IF
          IF (IGKY.LE.0) GO TO 20
        END IF
      IF (LGRP.NE.0) GO TO 10
      RETURN
      END
C
C
C
      SUBROUTINE   ZCMDIS
     I                   (HCMD,
     O                    CMDVAL)
C
C     + + + PURPOSE + + +
C     display current active commands and highlight default (if exists)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   HCMD,CMDVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     HCMD   - command to be highlighted, -1 for no command display
C     CMDVAL - absolute command value of current command
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
      INCLUDE 'color.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K,L,POS,LCMDST,CMDISP(26)
      CHARACTER*2  BLNK2
      CHARACTER*4  STRING
      CHARACTER*80 BLNK80
C
C     + + + SAVES + + +
      SAVE        POS
C
C     + + + FUNCTIONS + + +
      INTEGER     ZLNTXT
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZLNTXT, ZWRSCR, ZWRVDR, SCCUMV, COLSET, ZIPI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA POS/0/
C
C     + + + END SPECIFICATIONS + + +
C
      POS   = 1
      BLNK2 = ' '
      BLNK80= ' '
      I     = 26
      K     = 0
      CALL ZIPI (I,K,CMDISP)
      LCMDST= ZCMDST
      CMDVAL= 0
      IF (HCMD.GE.0 .AND. (HCMD.GT.0 .OR. ZCMDST.GT.0)) THEN
C       write currently valid system commands
        ZCMDMX= 0
        LCMDST= 0
        I= 0
 10     CONTINUE
          I= I+ 1
          IF (ZCMDAV(I).GT.0) THEN
C           command available, assigned to a function key?
            CMDISP(I)= 1
            J= 0
 20         CONTINUE
              J= J+ 1
              IF (ZFKEY(J).EQ.I) THEN
C               command assigned to function key, display both
                CMDISP(I)= 0
                K= K+ 1
                L= ZLNTXT(ZCMDNA(I))
                IF (K.EQ.HCMD) THEN
C                 highlight command
                  CALL ZWRVDR (ZCMDNA(I)(1:L),24,POS)
C                 absolute command value of current command
                  CMDVAL= I
                ELSE IF (HCMD.GT.0) THEN
C                 in command select mode, highlight 1st characters
                  CALL COLSET (FRD,BKS)
                  CALL ZWRSCR (ZCMDNA(I)(1:1),24,POS)
                  CALL COLSET (FRS,BKS)
                  CALL ZWRSCR (ZCMDNA(I)(2:L),24,POS+1)
                ELSE
C                 normal display
                  CALL COLSET (FRS,BKS)
                  CALL ZWRSCR (ZCMDNA(I)(1:L),24,POS)
                END IF
                POS   = POS+ L
C               keep track of valid commands
                ZCMDMX= ZCMDMX+ 1
                STRING(1:2)= ':F'
                L= 3
                IF (J.LE.9) THEN
C                 funcion keys 1 - 9
                  STRING(3:L) = CHAR(J+48)
                ELSE
C                 special case for function key 10
                  L= 4
                  STRING(3:L) = '10'
                END IF
                IF (HCMD.EQ.0) THEN
C                 no command highlighted, highlight function keys
                  CALL ZWRSCR (STRING(1:1),24,POS)
                  CALL ZWRVDR (STRING(2:L),24,POS+1)
                ELSE
C                 normal display
                  CALL ZWRSCR (STRING(1:L),24,POS)
                END IF
                POS= POS+ L
                CALL ZWRSCR (BLNK2,24,POS)
                POS= POS+ 2
                J  = 10
              END IF
            IF (J.LT.10) GO TO 20
          END IF
        IF (POS.LT.71 .AND. I.LT.26) GO TO 10
        I= 0
 30     CONTINUE
          I= I+ 1
          IF (CMDISP(I).GT.0) THEN
C           command available but not yet displayed
            K= K+ 1
            L= ZLNTXT(ZCMDNA(I))
            IF (K.EQ.HCMD) THEN
C             highlight command
              CALL ZWRVDR (ZCMDNA(I)(1:L),24,POS)
C             absolute command value of current command
              CMDVAL= I
            ELSE IF (HCMD.GT.0) THEN
C             in command select mode, highlight 1st characters
              CALL COLSET (FRD,BKS)
              CALL ZWRSCR (ZCMDNA(I)(1:1),24,POS)
              CALL COLSET (FRS,BKS)
              CALL ZWRSCR (ZCMDNA(I)(2:L),24,POS+1)
            ELSE
C             normal display
              CALL COLSET (FRS,BKS)
              CALL ZWRSCR (ZCMDNA(I)(1:L),24,POS)
            END IF
            POS   = POS+ L
C           keep track of valid commands
            ZCMDMX= ZCMDMX+ 1
            IF (POS.LT.79) THEN
C             ok to put blanks
              CALL ZWRSCR (BLNK2,24,POS)
              POS= POS+ 2
            END IF
          END IF
        IF (POS.LT.76 .AND. I.LT.26) GO TO 30
        IF (HCMD.GT.0) THEN
C         set to display next time
          LCMDST= 1
        END IF
      ELSE IF (HCMD.EQ.-1) THEN
C       set to display next time
        LCMDST= 1
      END IF
C
C     rewrite rest of command line
      IF (ZCMDST.GT.0 .OR. HCMD.LT.0) THEN
C       clear to end of old length
        I= 80-POS+ 1
        IF (I.GT.0) THEN
C         something to clear
          CALL ZWRSCR (BLNK80(1:I),24,POS)
        END IF
      END IF
      ZCMDST= LCMDST
C     restore cursor position
      CALL SCCUMV(ZCRLIN,ZCRCOL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZVERIF
     I                    (DATTYP,RMIN,RMAX,VALID,INVAL,FLEN,IWRT,
     M                     VALUE,IERR)
C
C     + + + PURPOSE + + +
C     validate a data value field
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   VALID,INVAL,FLEN,IERR,IWRT
      REAL      RMIN,RMAX
      CHARACTER DATTYP,VALUE*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATTYP - data type (I,R,D,C)
C     RMIN   - minimum numeric value
C     RMAX   - maximum numeric value
C     VALID  - pointer to valid values
C     INVAL  - pointer to invalid values
C     FLEN   - field length
C     IWRT   - indicates whether or not to write error message
C     VALUE  - data value to be validate
C     IERR   - error code (non-zero value means problems)
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C
C     numeric constants
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,K,L,ILST,IVAL,RCHK,IPOS,ILEN,WNDID,LGRP,
     1          NANS,TRESP,CRESP
C    2          ,USRLEV,PRMIND
      REAL      RVAL,R,RERR
      CHARACTER CVAL*78
      CHARACTER*1 STRIN1(MXRSLN)
      CHARACTER*78 STRING
      DOUBLE PRECISION DVAL,D,DERR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CVAL1,CVAL)
      CHARACTER*1  CVAL1(78)
C
C     + + + FUNCTIONS + + +
      INTEGER    ZLNSTR, ZLNTXT
      REAL       CHRDEC
      LOGICAL    ZCKINT
      DOUBLE PRECISION   CHRDPR
C
C     + + + INTRINSICS + + +
      INTRINSIC  INDEX
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZLNSTR, ZLNTXT, CHRDEC, CHRDPR, ZCKINT, CARVAR, QUPCAS
      EXTERNAL   ZLJUST, ZWRTB3, ZCKREA, WMSPIS, CHKANQ, GGTGLV
C
C     + + + DATA INITIALIZATIONS + + +
Chnb      DATA RERR,DERR/-1.0E29,-1.0E29/
C
C     + + + END SPECIFICATIONS + + +
C
      RERR = -R0MAX
      DERR = -D0MAX
      WNDID = 13
C
      IF (ZLNTXT(VALUE) .GE. 1) THEN
C       assume correct
        IERR= 0
C       value to check
        CVAL = VALUE
        CALL ZLJUST(CVAL)
        L = ZLNTXT(CVAL)
        IF (DATTYP .EQ. 'I') THEN
C         integer
          IF (L .NE. ZLNSTR(CVAL)) THEN
C           not a number, has blanks
            IERR= 1
          ELSE IF (INDEX(CVAL,',') .GT. 0) THEN
C           not a number, has comma
            IERR= 1
          ELSE IF (.NOT. ZCKINT(CVAL,I)) THEN
C           not an integer
            IERR= 1
          ELSE
C           looks good
            RVAL = I
          END IF
        ELSE IF (DATTYP .EQ. 'R') THEN
C         real
          IF (L .NE. ZLNSTR(CVAL)) THEN
C           not a number, has blanks
            IERR= 1
          ELSE IF (INDEX(CVAL,',') .GT. 0) THEN
C           not a number, has comma
            IERR= 1
          ELSE
C           looks good, need to be sure
            RVAL= CHRDEC(L,CVAL1)
            IF (RVAL.LE.RERR) THEN
C             whoops, didnt make it
              IERR= 1
            END IF
          END IF
        ELSE IF (DATTYP .EQ. 'D') THEN
C         double precision
          IF (L .NE. ZLNSTR(CVAL)) THEN
C           not a number, has blanks
            IERR= 1
          ELSE IF (INDEX(CVAL,',') .GT. 0) THEN
C           not a number, has comma
            IERR= 1
          ELSE
C           looks good, need to be sure
            DVAL= CHRDPR(L,CVAL1)
            IF (DVAL.LE.DERR) THEN
C             whoops, didnt make it
              IERR= 1
            END IF
          END IF
        ELSE
C         character, make no number check
        END IF
C
C       check data range
        IF (IERR .EQ. 0) THEN
C         range reasonable
          IF (DATTYP .NE. 'C') THEN
C           check integers, reals, or double precisions
            IF (DATTYP .EQ. 'D') RVAL= DVAL
            CALL ZCKREA (RMIN,RMAX,RVAL,
     O                   RCHK)
            IF (RCHK.NE.1) THEN
C             not in range
              IERR= 1
            END IF
          END IF
        END IF
C
        IF (DATTYP.EQ.'C') THEN
C         convert string entered to upper case for checking
          CALL QUPCAS (L,
     M                 CVAL1)
        END IF
C
        DO 200 K= 1, 2
          STRING= ' '
          IF (K .EQ. 1) THEN
C           determine valid pointers
            CALL WMSPIS (VALID,
     O                   IPOS,ILEN)
          ELSE
C           determine invalid pointers
            CALL WMSPIS (INVAL,
     O                   IPOS,ILEN)
          END IF
          IF (IERR.EQ.0 .AND. ILEN.GT.0) THEN
C           assume not last time thru
            ILST  = 0
            I = 1
            DO 130 J = IPOS, IPOS+ILEN-1
              STRIN1(I) = RSPSTR(J)
              I = I + 1
 130        CONTINUE
            IF (STRIN1(1) .EQ. '?') THEN
C             global values, get them
              I= 0
              CALL GGTGLV(FLEN,I,
     M                    STRIN1,
     O                    ILEN)
            END IF
C
            NANS= ILEN/FLEN
            IF (K.EQ.1 .AND. DATTYP.EQ.'C') THEN
C             do CHKANQ on input string, complete response not required
              CALL CHKANQ (NANS,FLEN,L,ILEN,STRIN1,
     M                     CVAL1,
     O                     TRESP,CRESP)
              IF (TRESP.GT.0 .AND. TRESP.LE.NANS .AND. CRESP.EQ.0) THEN
C               valid response, no conflicts
                IPOS= (TRESP-1)*FLEN+ 1
Chnb                WRITE (VALUE,2000) (STRIN1(I),I=IPOS,IPOS+FLEN-1)
                I = 1
                DO 135, J = IPOS, IPOS+FLEN-1
                  VALUE(I:I) = STRIN1(J)
                  I = I + 1
 135            CONTINUE
              ELSE
C               problems, set to 'none'
                IERR = 1
                VALUE= 'none'
              END IF
            ELSE
C             do checking requiring complete response
C             (valid or invalid limited to 80 character total length)
              I= 0
 150          CONTINUE
C               assume string is ok
                IERR= 0
C               more to convert?
                IF (ILST .EQ. 0) THEN
C                 you bet there is
                  CALL CARVAR (FLEN,STRIN1(I*FLEN+1),FLEN,STRING)
                  CALL ZLJUST(STRING)
                END IF
                I = I+ 1
                IF (I.EQ.NANS) THEN
C                 last time thru
                  ILST= 1
                END IF
C
                IF (DATTYP .EQ. 'I') THEN
C                 integer
                  IF (ZCKINT(STRING(1:FLEN),IVAL)) THEN
C                   converts ok to integer
                    R= IVAL
                    IF ((R-RVAL) .GT. 1.0E-5) THEN
C                     value in list not what we are looking for
                      IERR= 1
                    END IF
                  ELSE
C                   conversion failed for value in file
                    WRITE (*,*) 'BAD INTEGER (IN)VALID STRING'
                  END IF
                ELSE IF (DATTYP .EQ. 'R') THEN
C                 real
                  R= CHRDEC(FLEN,STRIN1)
                  IF (R.LE.RERR) THEN
C                   conversion failed for value in file
                    WRITE (*,*) 'BAD REAL (IN)VALID STRING'
                  ELSE IF ((R-RVAL) .GT. 1.0E-5) THEN
C                   value in valid list not what we are looking for
                    IERR= 1
                  END IF
                ELSE IF (DATTYP .EQ. 'D') THEN
C                 double precision
                  D= CHRDPR(FLEN,STRIN1)
                  IF (D.LE.DERR) THEN
C                   conversion failed for value in file
                    WRITE (*,*) 'BAD DOUBLE PRECISION (IN)VALID STRING'
                  ELSE IF ((D-DVAL) .GT. 1.0E-5) THEN
C                   value in valid list not what we are looking for
                    IERR= 1
                  END IF
                ELSE
C                 character type (invalid checking only)
                  IF (CVAL(1:L) .NE. STRING(1:FLEN)) THEN
C                   value in list what we are looking for
                    IERR = 1
                  END IF
                END IF
C
                IF (K .EQ. 2) THEN
C                 looking for invalid
                  IF (IERR .EQ. 0) THEN
C                   found a match, problem
                    IERR= 1
                    ILST= 1
                  ELSE IF (ILST.EQ.1) THEN
C                   last one and havent found a match, okay
                    IERR= 0
                  END IF
                END IF
C
C               more to convert?
C               IF (ILST .EQ. 0) THEN
C                 you bet there is
C                 TSTR  = STRING(I*FLEN+1:80)
C                 STRING= TSTR
C                 CALL ZLJUST(STRING)
C               END IF
              IF (IERR .EQ. 1 .AND. ILST .EQ. 0) GO TO 150
            END IF
          END IF
 200    CONTINUE
C
        IF (IERR.EQ.1 .AND. IWRT.EQ.1) THEN
C         invalid input data
          LGRP= 75
          CALL ZWRTB3 (WNDID,LGRP)
        END IF
      ELSE IF (VALID.EQ.0 .AND. INVAL.EQ.0) THEN
C       null field is ok
        IERR = 0
      ELSE
C       null field with valids, let user know its a problem
        IERR = 1
        IF (IWRT.EQ.1) THEN
C         show user error message
          LGRP = 76
          CALL ZWRTB3 (WNDID,LGRP)
C         put 'none' in field
          VALUE= 'none'
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZWRTB2
     I                    (ZB2TXT,ZBXLEN)
C
C     + + + PURPOSE + + +
C     write a message to middle box from specified text
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      ZBXLEN(4)
      CHARACTER*78 ZB2TXT(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ZB2TXT - text to write
C     ZBXLEN - length of each new line to write
C
C     + + + COMMON BLOCKS + + +
C     contains control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZWRTMN
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,ZB2N
        IF (ZBXLEN(I).GT.ZB2LEN(I)) THEN
C         new line longer than old
          ZB2LEN(I)= ZBXLEN(I)
        END IF
 10   CONTINUE
C     write the box
      CALL ZWRTMN (ZB2F,ZB2N,0,ZB2TXT,
     M             ZB2LEN)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZWRTB3
     I                    (WNDID,LGRP)
C
C     + + + PURPOSE + + +
C     write a message to bottom box from WDM file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WNDID,LGRP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WNDID  - window id number
C     LGRP   - group to get message from
C
C     + + + COMMON BLOCKS + + +
C     contains control parameters
      INCLUDE 'zcntrl.inc'
      INCLUDE 'color.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,OLEN,WNDWRT,INITFG
      CHARACTER*1 OBUFF(78)
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZWRTMN, ZDRWDW, WMSGTT, SCCUMV, COLSET
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (78A1)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (WNDID.GE.13) THEN
C       error/warning condition text
        ZESCST= 1
        ZERR  = 1
        WNDWRT= 1
C       set to error colors
        CALL COLSET (FRE,BKE)
      ELSE IF (WNDID.EQ.12.AND.ZERR.EQ.0) THEN
C       instruction text with no error condition
        WNDWRT= 1
      ELSE
C       dont write this text
        WNDWRT= 0
      END IF
C
      IF (WNDWRT.EQ.1) THEN
C       time to write all/part of bottom window
        IF (WNDID.NE.ZWN3ID) THEN
C         redraw bottom window
          CALL ZDRWDW(ZITYPE,ZWNNAM(WNDID),ZB3F-1,1,ZB3N+2,80,' ')
          ZWN3ID= WNDID
        END IF
C
        IF (ZGP3.NE.LGRP) THEN
C         write text
          INITFG= 1
          DO 10 I = 1, 3
C           text from wdm file
            OLEN= 78
            CALL WMSGTT (ZMESFL,ZSCLU,LGRP,INITFG,
     M                   OLEN,
     O                   OBUFF,J)
            INITFG= 0
            IF (OLEN.GT.0) THEN
C             not a blank line
              WRITE (ZMSTXT(I),2000) (OBUFF(J),J=1,OLEN)
            ELSE
              ZMSTXT(I)= ' '
            END IF
            J = I + 19
            IF (OLEN .GT. ZMSLEN(I)) THEN
C             write more than currently on screen
              ZMSLEN(I)= OLEN
            END IF
 10       CONTINUE
C         write the message
          CALL ZWRTMN (ZB3F,ZB3N,0,ZMSTXT,
     M                 ZMSLEN)
C         new current group
          ZGP3= LGRP
        END IF
      END IF
C
C     reset colors
      CALL COLSET (FRS,BKS)
C
      CALL SCCUMV(ZCRLIN,ZCRCOL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZWRHLP
     I                    (MESSFL,GHLP,SHLP,WNDID,QHLP)
C
C     + + + PURPOSE + + +
C     display help information
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,GHLP,SHLP,WNDID,QHLP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     GHLP   - pointer to start of general help on WDM file
C     SHLP   - pointer to start of specific option help
C     WNDID  - id for window (help or tutor)
C     QHLP   - flag indicating quick help
C              (0 - show all help, 1 - show 1st four lines)
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I0,I78,I,J,K,CONT,HCNT,IPTR,NREC,
     2           GLEN,MLEN,DREC,DPOS,ITOP,IGKY,
     3           CLASS,ORDER,ID,OLEN,TLEN,LWNDID,LGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZGTKEY, SCCUMV, ZWRTB3, ZWRTB2, ZWRVDR, ZB2ON
      EXTERNAL   WMSBCX, WMSGTE, ZDRWDW, WMSBTR, ZCNTER, ZCMDIS
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I78 = 78
C
C     check the existence of help message
      IPTR= 0
      IF (SHLP.GT.0) THEN
C       specific help exists
        IPTR= SHLP
      ELSE IF (GHLP.GT.0) THEN
C       general help exists, use it
        IPTR= GHLP
      END IF
C
      IF (IPTR.GT.0) THEN
        IF (ZQUFG.EQ.1) THEN
C         turn off quiet
          CALL ZB2ON
        END IF
        IF (ZWN2ID.NE.WNDID) THEN
C         set window id
          ZWN2ID= WNDID
C         draw the help box
          CALL ZDRWDW(ZITYPE,ZWNNAM(ZWN2ID),ZB2F-1,1,ZB2N+2,80,' ')
        END IF
C
C       display help message, a page at a time
        CONT = 0
 200    CONTINUE
          IF (CONT.EQ.0) THEN
C           split block control word for next help
            CALL WMSBCX (MESSFL,IPTR,
     O                   DREC,DPOS,CLASS,ID,ORDER,TLEN)
            GLEN= 0
            MLEN= 0
            ITOP= 1
            CONT= 1
          END IF
          HCNT = 0
          DO 300 I = 1, ZB2N
            IF (CONT.EQ.1) THEN
C             get a line of text from message file
              CALL WMSGTE (MESSFL,TLEN,I78,
     M                     DREC,DPOS,GLEN,MLEN,
     O                     OLEN,ZHLTX1(1,I),CONT)
              HCNT= HCNT+ 1
              IF (OLEN.GT.ZHLPLN(I)) THEN
C               new line longer than current line
                ZHLPLN(I)= OLEN
              END IF
            ELSE
C             no more help
              ZHLTXT(I)= ' '
            END IF
 300      CONTINUE
          IF (QHLP.EQ.1) THEN
C           just show 1st four lines
            CONT= 0
          END IF
C         signal user to page
          LWNDID= 12
          IF (CONT.EQ.1) THEN
C           more help available below
            IF (ITOP.EQ.0) THEN
C             not at top, may review also
              LGRP= 73
            ELSE
C             only below
              LGRP= 71
            END IF
          ELSE IF (ITOP.EQ.0) THEN
C           none below, may review above
            LGRP= 72
          ELSE
C           no more pages of help
            LGRP= 0
          END IF
          IF (LGRP.EQ.0) THEN
C           center help
            I= I0
            CALL ZCNTER (ZB2N,I0,
     M                   HCNT,ZHLTXT,ZHLPLN,I,I,
     O                   J,K)
          END IF
C         write a page of help
          CALL ZWRTB2 (ZHLTXT,ZHLPLN)
          IF (LGRP.GT.0) THEN
C           erase valid commands
            ZCMDST= 1
            I= -1
            CALL ZCMDIS(I,
     O                  J)
            IF (ZHLLIN.GT.0 .AND. ZHLCOL.GT.0) THEN
C             rehighlight current field
              I= ZHLCOL+ ZHLLEN- 1
              CALL ZWRVDR (ZMNTXT(ZHLLIN)(ZHLCOL:I),ZHLLIN+1,ZHLCOL+1)
            END IF
C           user needs to tell what to do
            IGKY= 1
C           assume no error
            ZERR  = 0
            ZESCST= 0
 600        CONTINUE
              IF (IGKY.EQ.0) THEN
C               error box instead of instruction
                LWNDID= 13
              END IF
              CALL ZWRTB3 (LWNDID,LGRP)
C             wait for keyboard interrupt
              CALL SCCUMV(ZCRLIN,ZCRCOL)
              CALL ZGTKEY(I,J)
C             assume invalid key
              IGKY= 0
              IF (I .EQ. 4 .AND. J .EQ. 3) THEN
C               f3, all done
                LGRP= 0
                IGKY= 1
                ZERR= 0
                ZESCST= 0
              ELSE IF (I.EQ.2 .AND. J.EQ.27) THEN
C               escape, all done
                LGRP= 0
                IGKY= 1
                ZERR= 0
                ZESCST= 0
              ELSE IF (I.EQ.3) THEN
C               arrow or page
                IF (J.EQ.1 .AND. ITOP.EQ.0) THEN
C                 up a line
                  NREC= 5
                  IGKY= 1
                  CALL WMSBTR (MESSFL,NREC,
     M                         DREC,DPOS,MLEN,GLEN,
     O                         ITOP)
                ELSE IF (J.EQ.2 .AND. CONT.EQ.1) THEN
C                 down a line
                  NREC= 3
                  IGKY= 1
                  CALL WMSBTR (MESSFL,NREC,
     M                         DREC,DPOS,MLEN,GLEN,
     O                         ITOP)
                ELSE IF (J.EQ.7 .AND. ITOP.EQ.0) THEN
C                 up a page
                  NREC= 8
                  IGKY= 1
                  CALL WMSBTR (MESSFL,NREC,
     M                         DREC,DPOS,MLEN,GLEN,
     O                         ITOP)
                ELSE IF (J.EQ.8 .AND. CONT.EQ.1) THEN
C                 down a page
                  IGKY= 1
                  ITOP= 0
                END IF
              END IF
            IF (IGKY.EQ.0) GO TO 600
          END IF
C         always try for more
          CONT= 1
        IF (LGRP.GT.0) GO TO 200
      ELSE
C       no help for this field
        LWNDID= 13
        LGRP  = 77
        CALL ZWRTB3 (LWNDID,LGRP)
        IF (QHLP.EQ.1) THEN
C         write a blank page of help
          DO 700 I= 1,4
            ZHLTXT(I)= ' '
            ZHLPLN(I)= 1
 700      CONTINUE
          CALL ZWRTB2 (ZHLTXT,ZHLPLN)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZPRTMN
     I                   (EXPFL)
C
C     + + + PURPOSE + + +
C     write menu text to file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   EXPFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     EXPFL  - Fortran unit number of file to write to
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (1X,A78)
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,ZMNNLI
        WRITE (EXPFL,2000) ZMNTXT(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZBLDWR
     I                    (OLEN,STRING,INIT,IWRT,
     O                     DONFG)
C
C     + + + PURPOSE + + +
C     build a screen in the menu box, options are write and wait
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     OLEN,INIT,IWRT,DONFG
      CHARACTER*1 STRING(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OLEN   - length of string to write
C     STRING - string to write
C     INIT   - initialize screen, 0 - save old stuff,
C                                 1 - initialize,
C                                -1 - overwrite last record
C     IWRT   - write flag, -1 - dont write, 0 - write and wait,
C                           1 - write and dont wait
C     DONFG  - user screen exit command
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I1,J,IGET,LGRP,LWRT,OMNCSL,PRMIND,PRMVAL,LMXSCB
C
C     + + + SAVES + + +
      SAVE         LWRT
C
C     + + + FUNCTIONS + + +
      INTEGER    LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   LENSTR, ZDRWSC, ZGTKYD, ZSCINI, ZUNCNT, ZCNTER
      EXTERNAL   SCCUMV, ZWRTMN, ZSTCMA, ANPRGT
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (78A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
C     set local version of max screen buffer to remove
C     any chance of parameter MXSCBF being modified
      LMXSCB= MXSCBF
C
      IF (INIT .EQ. 1) THEN
C       initialize menu text
        CALL ZSCINI
C       not available commands
CPRH    CALL ZSTCMA(1,0)
        CALL ZSTCMA(6,0)
        CALL ZSTCMA(8,0)
        CALL ZSTCMA(9,0)
        CALL ZSTCMA(10,0)
        CALL ZSTCMA(11,0)
        CALL ZSTCMA(14,0)
        CALL ZSTCMA(15,0)
        ZDTYP= 1
C       draw the screen
        CALL ZDRWSC
      ELSE IF (LWRT.EQ.1) THEN
C       uncenter
        CALL ZUNCNT (ZCLEN,ZCWID,ZHLLEN,LMXSCB,
     M               ZMNNLI,ZMNTXT,ZMNLEN,ZHLLIN,ZHLCOL)
      END IF
C
      LWRT  = IWRT
C
      IF (OLEN.GT.0) THEN
C       have something to write
        IF (INIT.NE.-1 .OR. ZMNNLI.EQ.0) THEN
C         write on next record
          ZMNNLI= ZMNNLI+ 1
        END IF
C
        WRITE (ZMNTXT(ZMNNLI),2000) (STRING(I),I=1,OLEN)
C
        IF (ZMNLEN(ZMNNLI).LT.OLEN) THEN
C         new line is longer
          ZMNLEN(ZMNNLI)= LENSTR(OLEN,ZMNTX1(1,ZMNNLI))
        END IF
      END IF
C
      IF (IWRT.GE.0) THEN
C       time to write things out, center text
        CALL ZCNTER (ZB1N,ZHLLEN,
     M               ZMNNLI,ZMNTXT,ZMNLEN,ZHLLIN,ZHLCOL,
     O               ZCLEN,ZCWID)
C
        IF (ZMNNLI.GT.ZB1N) THEN
          IF (IWRT.EQ.0) THEN
C           start at top, page down available
            CALL ZSTCMA(14,1)
            CALL ZSTCMA(15,0)
            IF (ZMNCSL.GT.1) THEN
C             adjust lengths of lines
              DO 550 I= 1,ZB1N
                IF (ZMNLEN(I+ZMNCSL-1).GT.ZMNLEN(I)) THEN
C                 reset length to erase current
                  ZMNLEN(I)= ZMNLEN(I+ZMNCSL-1)
                END IF
 550          CONTINUE
            END IF
            ZMNCSL= 1
          ELSE
C           show most recent line written at bottom
            OMNCSL= ZMNCSL
            ZMNCSL= ZMNNLI- ZB1N+ 1
            IF (ZMNCSL.NE.OMNCSL) THEN
C             start line has changed
              J= OMNCSL+ ZB1N- 1
              IF (ZMNLEN(J).GT.ZMNLEN(ZMNNLI)) THEN
C               adjust length of newest line
                ZMNLEN(ZMNNLI)= ZMNLEN(OMNCSL+ZB1N-1)
              END IF
            END IF
C           page up available
            CALL ZSTCMA(15,1)
            CALL ZSTCMA(14,0)
          END IF
        END IF
C
C       write a page of text
        CALL ZWRTMN (ZB1F,ZB1N,1,ZMNTXT(ZMNCSL),
     M               ZMNLEN(ZMNCSL))
C
 600    CONTINUE
C         signal user to page
          IF (IWRT .EQ. 0) THEN
C           wait for a key
            IGET= 1
            LGRP= 65
          ELSE IF (IWRT.EQ.1) THEN
C           wait for computer
            IGET= 0
            LGRP= 66
          END IF
C
          DONFG = 0
          IF (IGET .EQ. 1) THEN
C           wait for keyboard interrupt
            ZCRLIN= 23
            ZCRCOL= 80
            CALL SCCUMV(ZCRLIN,ZCRCOL)
            CALL ZGTKYD (LGRP,I1,
     O                   I,J)
            IF (I.EQ.7) THEN
C             mouse button
              ZRET= 1
            ELSE IF (I.EQ.2 .AND. J.EQ.13) THEN
C             carriage return, see if we need to act on it
              PRMIND= 9
              CALL ANPRGT (PRMIND,
     O                     PRMVAL)
              IF (PRMVAL.EQ.3) THEN
C               user wants carriage return to act as Next
                ZRET= 1
              END IF
            END IF
            IF (ZRET.NE.0) THEN
C             all done
              DONFG= ZRET
            ELSE
C             staying on this screen, assume no error
              ZERR= 0
            END IF
          ELSE
C           waiting for computer, dont get key
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 600
      ELSE
C       havent finished building screen
        DONFG= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZLIMIT
C
C     + + + PURPOSE + + +
C     display limits for current field
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
C     contains control parameters
      INCLUDE 'zcntrl.inc'
C     contains screen parameters
      INCLUDE 'cscren.inc'
C     numeric constants
      INCLUDE 'const.inc'
C
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I1,I13,INONE,ILEN,LGRP,HLIN,HPOS,LPOS,IND,TLEN,
     $            JUST,ICNT,TRESP,CRESP,OPOS,LLIN,IPOS(50),QFLG,ILIN,
     $            LWNDID,IGKY,TLIN,IRECL,WDMFLG,RETCOD,NAMLEN
      REAL        RNONE,RCHK
      CHARACTER*1 STRIN1(MXRSLN),STRMAT(MXRSLN),ISTR(78)
      CHARACTER*4 CNONE
      CHARACTER*7 CSTAT
      CHARACTER*10 CACCES
      CHARACTER*11 CFORM
      CHARACTER*78 TMPSTR
      CHARACTER*80 NAMES
      CHARACTER*120 FSPTXT
C
C     + + + FUNCTIONS + + +
      INTEGER    ZLNTXT
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   DECCHR, DPRCHR, ZLJUST, GGTGLV, CHKANQ, CHRCHR, CKFSPC
      EXTERNAL   ZLNTXT, ZDRWDW, GETTXT, ZB2ON, ZWRTB2, WMSPIS, ZSTRPR
      EXTERNAL   ZWRTB3, ZCMDIS, SCCUMV, ZGTKEY, ZVALPS, FSPARS, CARVAR
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (I10)
Chnb 2010 FORMAT (G13.4)
C
C     + + + END SPECIFICATIONS + + +
C
      I1  = 1
      I13 = 13
      INONE = -999
      RNONE = -999.
      CNONE = 'none'
Chnb      RCHK  = 1.0E-5
      RCHK  = R0MIN
      QFLG  = 1
C
      IF (ZQUFG.EQ.1) THEN
C       turn off quiet
        CALL ZB2ON
      END IF
      IF (ZWN2ID.NE.8) THEN
C       set window id
        IF (ZWN2ID.EQ.-1) THEN
C         indicates 'Limits' command was invoked, allow scrolling, if needed
          QFLG= 0
        END IF
        ZWN2ID= 8
C       draw the limits window
        CALL ZDRWDW(ZITYPE,ZWNNAM(ZWN2ID),ZB2F-1,1,ZB2N+2,80,' ')
      END IF
C
      HLIN= 2
      ILIN= 1
      IPOS(1)= 1
C
 5    CONTINUE
C       back here if multiple screen of limits
        DO 10 I= 1,4
C         init help text
          ZHLTXT(I)= ' '
 10     CONTINUE
C
        LGRP= 78
        ILEN= 78
        IF (CFLD.EQ.0) CFLD= 1
        IND = APOS(CFLD)
        IF (IND.EQ.0) IND= 1
        IF (FTYP(CFLD).EQ.FTI) THEN
C         integer field
          IF (IMIN(IND).NE.INONE .OR. IMAX(IND).NE.INONE .OR.
     1        IDEF(IND).NE.INONE) THEN
C           some limits exist
            CALL GETTXT (ZMESFL,ZSCLU,LGRP,ILEN,ZHLTX1(1,HLIN))
            IF (IDEF(IND).NE.INONE) THEN
C             valid default exists
              WRITE (ZHLTXT(HLIN)(11:20),2000) IDEF(IND)
            ELSE
C             put in 'none'
              ZHLTXT(HLIN)(11:14)= CNONE
            END IF
            IF (IMIN(IND).NE.INONE) THEN
C             valid min exists
              WRITE (ZHLTXT(HLIN)(36:45),2000) IMIN(IND)
            ELSE
C             put in 'none'
              ZHLTXT(HLIN)(36:39)= CNONE
            END IF
            IF (IMAX(IND).NE.INONE) THEN
C             valid max exists
              WRITE (ZHLTXT(HLIN)(61:70),2000) IMAX(IND)
            ELSE
C             put in 'none'
              ZHLTXT(HLIN)(61:64)= CNONE
            END IF
            CALL ZLJUST(ZHLTXT(HLIN)(11:20))
            CALL ZLJUST(ZHLTXT(HLIN)(36:45))
            CALL ZLJUST(ZHLTXT(HLIN)(61:70))
            HLIN= HLIN+ 1
          END IF
        ELSE IF (FTYP(CFLD).EQ.FTR) THEN
C         real field
          IF (ABS(RMIN(IND)-RNONE).GT.RCHK .OR.
     1        ABS(RMAX(IND)-RNONE).GT.RCHK .OR.
     2        ABS(RDEF(IND)-RNONE).GT.RCHK) THEN
C           some limits exist
            CALL GETTXT (ZMESFL,ZSCLU,LGRP,ILEN,ZHLTX1(1,HLIN))
            IF (ABS(RDEF(IND)-RNONE).GT.RCHK) THEN
C             valid default exists
Chnb              WRITE (ZHLTXT(HLIN)(11:23),2010) RDEF(IND)
              JUST = 1
              CALL DECCHR(RDEF(IND),I13,JUST,TLEN,ZHLTXT(HLIN)(11:23))
            ELSE
C             put in 'none'
              ZHLTXT(HLIN)(11:14)= CNONE
            END IF
            IF (ABS(RMIN(IND)-RNONE).GT.RCHK) THEN
C             valid min exists
Chnb              WRITE (ZHLTXT(HLIN)(36:48),2010) RMIN(IND)
              JUST = 1
              CALL DECCHR(RMIN(IND),I13,JUST,TLEN,ZHLTXT(HLIN)(36:48))
            ELSE
C             put in 'none'
              ZHLTXT(HLIN)(36:39)= CNONE
            END IF
            IF (ABS(RMAX(IND)-RNONE).GT.RCHK) THEN
C             valid max exists
Chnb              WRITE (ZHLTXT(HLIN)(61:73),2010) RMAX(IND)
              JUST = 1
              CALL DECCHR(RMAX(IND),I13,JUST,TLEN,ZHLTXT(HLIN)(61:73))
            ELSE
C             put in 'none'
              ZHLTXT(HLIN)(61:64)= CNONE
            END IF
            CALL ZLJUST(ZHLTXT(HLIN)(11:23))
            CALL ZLJUST(ZHLTXT(HLIN)(36:48))
            CALL ZLJUST(ZHLTXT(HLIN)(61:73))
            HLIN= HLIN+ 1
          END IF
        ELSE IF (FTYP(CFLD).EQ.FTD) THEN
C         double precision field
          IF (ABS(DMIN(IND)-RNONE).GT.RCHK .OR.
     1        ABS(DMAX(IND)-RNONE).GT.RCHK .OR.
     2        ABS(DDEF(IND)-RNONE).GT.RCHK) THEN
C           some limits exist
            CALL GETTXT (ZMESFL,ZSCLU,LGRP,ILEN,ZHLTX1(1,HLIN))
            IF (ABS(DDEF(IND)-RNONE).GT.RCHK) THEN
C             valid default exists
Chnb              WRITE (ZHLTXT(HLIN)(11:23),2010) DDEF(IND)
              JUST = 1
              CALL DPRCHR(DDEF(IND),I13,JUST,TLEN,ZHLTXT(HLIN)(11:23))
            ELSE
C             put in 'none'
              ZHLTXT(HLIN)(11:14)= CNONE
            END IF
            IF (ABS(DMIN(IND)-RNONE).GT.RCHK) THEN
C             valid min exists
Chnb              WRITE (ZHLTXT(HLIN)(36:48),2010) DMIN(IND)
              JUST = 1
              CALL DPRCHR(DMIN(IND),I13,JUST,TLEN,ZHLTXT(HLIN)(36:48))
            ELSE
C             put in 'none'
              ZHLTXT(HLIN)(36:39)= CNONE
            END IF
            IF (ABS(DMAX(IND)-RNONE).GT.RCHK) THEN
C             valid max exists
Chnb              WRITE (ZHLTXT(HLIN)(61:73),2010) DMAX(IND)
              JUST = 1
              CALL DPRCHR(DMAX(IND),I13,JUST,TLEN,ZHLTXT(HLIN)(61:73))
            ELSE
C             put in 'none'
              ZHLTXT(HLIN)(61:64)= CNONE
            END IF
            CALL ZLJUST(ZHLTXT(HLIN)(11:23))
            CALL ZLJUST(ZHLTXT(HLIN)(36:48))
            CALL ZLJUST(ZHLTXT(HLIN)(61:73))
            HLIN= HLIN+ 1
          END IF
        ELSE IF (FTYP(CFLD).EQ.FTF) THEN
C         file field
          CSTAT= 'OLD'
          IF (FDVAL(CFLD).GT.0) THEN
C           some file specs exist
            CALL WMSPIS (FDVAL(CFLD),
     O                   LPOS,ILEN)
C           put specs from buffer into file specs string
            I= 120
            CALL CARVAR (ILEN,RSPSTR(LPOS),I,FSPTXT)
            CALL FSPARS (FSPTXT,
     M                   CSTAT,CACCES,CFORM,IRECL,
     O                   NAMES,WDMFLG,RETCOD)
            NAMLEN= ZLNTXT(NAMES)
            IF (NAMLEN.GT.0) THEN
C             specific name required, show criteria
              IF (NAMLEN.GT.64) THEN
C               only so much room
                I= 64
              ELSE
C               use full length
                I= NAMLEN
              END IF
              TMPSTR= ' Valid Names: '//NAMES(1:I)
            ELSE
C             no name requirements
              TMPSTR= ' Any File Name is Acceptable'
            END IF
            I= ZLNTXT(TMPSTR)
            IF (WDMFLG.EQ.1) THEN
C             must be WDM file, display indicator if possible
              IF (I.LT.68) THEN
                J= (78-I-11)/2
                IF (J.EQ.0) J= 1
                ZHLTXT(HLIN)(J:78)= TMPSTR(1:I)//' (WDM file)'
              ELSE IF (I.LT.73) THEN
                ZHLTXT(HLIN)= TMPSTR(1:I)//' (WDM)'
              END IF
            ELSE
C             store string in help text
              J= (78-I)/2
              IF (J.EQ.0) J= 1
              ZHLTXT(HLIN)(J:78)= TMPSTR(1:I)
            END IF
          ELSE
C           no specifications
            NAMLEN= 0
            WDMFLG= 0
          END IF
        END IF
C
C       see about valid/invalid responses
        IF (FDVAL(CFLD).GT.0) THEN
C         find valid values
          CALL WMSPIS (FDVAL(CFLD),
     O                 LPOS,ILEN)
          IF (FTYP(CFLD).NE.FTF) THEN
C           fill in valid values
            I = 1
            DO 12 J = LPOS, LPOS+ILEN-1
              STRIN1(I) = RSPSTR(J)
              I = I + 1
 12         CONTINUE
C
            IF (STRIN1(1) .EQ. '?') THEN
C             global values, get them
              I= 0
              CALL GGTGLV(FLEN(CFLD),I,
     M                    STRIN1,
     O                    ILEN)
              ICNT= ILEN/FLEN(CFLD)
            ELSE
              ICNT= CCNT(CFLD)
            END IF
          END IF
C
C         show matching valid responses if user has entered something
          IF (ZDTYP .EQ. 4) THEN
C           parm2 field, offset includes header and data row number
            LLIN= NMHDRW+ CROW
          ELSE
C           other type field, offset is highlight line
            LLIN= ZHLLIN
          END IF
          TLEN= 0
          IF (ZCRCOL-1.NE.SCOL(CFLD)) THEN
C           user not at start of field, anything entered?
            TLEN= ZLNTXT(ZMNTXT(LLIN)(SCOL(CFLD):ZCRCOL-2))
            IF (TLEN.GT.0 .AND. FTYP(CFLD).NE.FTF) THEN
C             something entered
              CALL CHRCHR (TLEN,ZMNTX1(SCOL(CFLD),LLIN),ISTR)
C             check for matches to valid responses
              I= 0
              OPOS= 1
 13           CONTINUE
C               look through valid responses for matches to whats been entered
                I= I+ 1
                LPOS= FLEN(CFLD)*(I-1)+ 1
                CALL CHKANQ (I1,FLEN(CFLD),TLEN,FLEN(CFLD),STRIN1(LPOS),
     M                       ISTR,
     O                       TRESP,CRESP)
                IF (TRESP.GT.0) THEN
C                 a match, put in valid string
                  CALL CHRCHR (FLEN(CFLD),STRIN1(LPOS),STRMAT(OPOS))
                  OPOS= OPOS+ FLEN(CFLD)
                END IF
              IF (I.LT.ICNT) GO TO 13
              ILEN= OPOS- 1
              ICNT= ILEN/FLEN(CFLD)
            END IF
          END IF
          IF (FTYP(CFLD).EQ.FTF) THEN
C           file type field
            IF (NAMLEN.GT.0 .OR. TLEN.GT.0) THEN
C             compare user entry w/program specs, show available file names
              TMPSTR= ZMNTXT(LLIN)(SCOL(CFLD):SCOL(CFLD)+FLEN(CFLD)-1)
              CALL CKFSPC (TLEN,TMPSTR,NAMLEN,NAMES,MXRSLN,FLEN(CFLD),
     O                     ILEN,ICNT,STRMAT)
            ELSE
C             nothing entered/no specs to match (or status is new)
              ICNT= 0
            END IF
          ELSE IF (TLEN.EQ.0) THEN
C           nothing entered in field yet, display all valid responses
            CALL CHRCHR (ILEN,STRIN1,STRMAT)
          END IF
          IF (ICNT.GT.0) THEN
C           matching responses, display them
            IF (ILIN.EQ.1) THEN
C             at top of limits, how long are valid responses
              I= ICNT* (FLEN(CFLD)+ 2)
              IF (I.GT.200) THEN
C               lots of valid values, determine total lines needed
                CALL ZVALPS (ILEN,STRMAT,FLEN(CFLD),ICNT,
     O                       IPOS,TLIN)
                IF (HLIN.EQ.2) THEN
C                 use all available lines
                  IF (FTYP(CFLD).NE.FTF) THEN
C                   start at top
                    HLIN= 1
                  ELSE
C                   move existing file text up one line
                    ZHLTXT(HLIN-1)= ZHLTXT(HLIN)
                    ZHLTXT(HLIN)  = ' '
                  END IF
                END IF
              ELSE IF (FTYP(CFLD).EQ.FTF) THEN
C               down one line
                HLIN= HLIN+ 1
              END IF
C             get "Valid" strings header from message file
              TLEN= 9
              LGRP= 79
              CALL GETTXT (ZMESFL,ZSCLU,LGRP,TLEN,ZHLTX1(1,HLIN))
              HPOS= 11
            ELSE
C             at some point in midst of valid responses
              ILEN= ILEN- IPOS(ILIN)+ 1
              ICNT= ILEN/FLEN(CFLD)
              HLIN= 1
              ZHLTXT(HLIN)(1:4)= '...,'
              HPOS= 6
            END IF
            IF (ILEN.GT.0) THEN
C             valid strings to parse
              CALL ZSTRPR (ILEN,STRMAT(IPOS(ILIN)),FLEN(CFLD),ICNT,
     M                     HPOS,HLIN,ZHLTX1)
            ELSE
C             no responses match current input string
              ZHLTXT(HLIN)(HPOS:HPOS+31)=
     $                    'No responses match input string.'
            END IF
          END IF
          IF (HLIN.LT.4) THEN
C           room for more
            HLIN= HLIN+ 1
          END IF
        END IF
        IF (FDINV(CFLD).GT.0 .AND. HLIN.LE.4) THEN
C         invalid resps exist
          HPOS= 11
          ILEN= 9
          LGRP= 80
          CALL GETTXT (ZMESFL,ZSCLU,LGRP,ILEN,ZHLTX1(1,HLIN))
          CALL WMSPIS (FDINV(CFLD),
     O                 LPOS,ILEN)
          I = 1
          DO 15 J = LPOS, LPOS+ILEN-1
            STRIN1(I) = RSPSTR(J)
            I = I + 1
 15       CONTINUE
C
          IF (STRIN1(1) .EQ. '?') THEN
C           global values, get them
            I= 0
            CALL GGTGLV(FLEN(CFLD),I,
     M                  STRIN1,
     O                  ILEN)
            ICNT= ILEN/FLEN(CFLD)
          ELSE
            ICNT= CCNT(CFLD)
          END IF
          CALL ZSTRPR (ILEN,STRIN1,FLEN(CFLD),ICNT,
     M                 HPOS,HLIN,ZHLTX1)
        END IF
C
        IF (HLIN.EQ.2 .AND. FTYP(CFLD).NE.FTF) THEN
C         anything allowed
          IF (FTYP(CFLD).EQ.FTI) THEN
            LGRP= 81
          ELSE IF (FTYP(CFLD).EQ.FTR .OR. FTYP(CFLD).EQ.FTD) THEN
            LGRP= 82
          ELSE
            LGRP= 83
          END IF
          ILEN= 78
          CALL GETTXT (ZMESFL,ZSCLU,LGRP,ILEN,ZHLTX1(1,HLIN))
        END IF
C
        DO 20 I= 1,4
C         set help lengths
          ZHLPLN(I)= ZLNTXT(ZHLTXT(I))
 20     CONTINUE
C
        IF (QFLG.EQ.0) THEN
C         more than a page?
          IF (ILIN.GT.1) THEN
C           multiple screen of limits available
            IF (ILIN+ZB2N.GT.TLIN) THEN
C             at the bottom, none below
              LGRP= 72
            ELSE
C             can back up or go down
              LGRP= 73
            END IF
          ELSE IF (HLIN.GT.4) THEN
C           multiple screens, but only only below
            LGRP= 71
          ELSE
C           only single page of status
            LGRP= 0
          END IF
        ELSE
C         just display one page
          LGRP= 0
        END IF
C
C       write a page of limits
        CALL ZWRTB2 (ZHLTXT,ZHLPLN)
C
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
 50       CONTINUE
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
              IF (J.EQ.1 .AND. ILIN.GT.1) THEN
C               up a line
                IGKY= 1
                ILIN= ILIN- 1
              ELSE IF (J.EQ.2 .AND. ILIN+ZB2N.LE.TLIN) THEN
C               down a line
                IGKY= 1
                ILIN= ILIN+ 1
              ELSE IF (J.EQ.7 .AND. ILIN.GT.1) THEN
C               up a page
                IGKY= 1
                ILIN= ILIN- ZB2N
                IF (ILIN.LT.1) ILIN= 1
              ELSE IF (J.EQ.8 .AND. ILIN+ZB2N.LE.TLIN) THEN
C               down a page
                IGKY= 1
                ILIN= ILIN+ ZB2N
                IF (ILIN+ZB2N.GT.TLIN) THEN
C                 dont go past bottom
                  ILIN= ILIN- ZB2N+ 1
                END IF
              END IF
            END IF
          IF (IGKY.EQ.0) GO TO 50
          HLIN= ILIN
        END IF
      IF (LGRP.NE.0) GO TO 5
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZSTRPR
     I                    (ILEN,ISTR,FLEN,CCNT,
     M                     HPOS,HLIN,OSTR)
C
C     + + + PURPOSE + + +
C     parse string of responses into string with commas
C     seperating the responses
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ILEN,FLEN,CCNT,HPOS,HLIN
      CHARACTER*1 ISTR(ILEN),OSTR(78,4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ILEN   - length of input string
C     ISTR   - input string of responses to parse
C     FLEN   - max length for responses
C     CCNT   - count of valid responses
C     HPOS   - starting position in output string
C     HLIN   - current output line in buffer from calling routine
C     OSTR   - output string of parsed responses
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,LPOS,SPOS,BPOS,DONFG,LCNT,LLEN
      CHARACTER*1 CHCONT(3)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, CHRCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CHCONT/'.','.','.'/
C
C     + + + END SPECFICATIONS + + +
C
      LPOS = 0
      SPOS = 0
      BPOS = 0
      LCNT = 0
      DONFG= 0
C
 10   CONTINUE
C       put commas in valid responses
        LPOS= LPOS+ 1
        SPOS= SPOS+ 1
        IF (ISTR(LPOS).NE.' ') THEN
C         put into output string
          OSTR(HPOS,HLIN)= ISTR(LPOS)
          HPOS= HPOS+ 1
          IF (SPOS.EQ.FLEN) THEN
C           done this string
            DONFG= 1
          END IF
        ELSE
C         blank, done this string
          DONFG= 1
        END IF
        IF (DONFG.EQ.1) THEN
C         add comma, blank
          LCNT= LCNT+ 1
          IF (LCNT.LT.CCNT) THEN
C           not on last response
            OSTR(HPOS,HLIN)= ','
            HPOS= HPOS+ 1
            IF (HPOS.LE.76) THEN
C             room to insert blank character
              OSTR(HPOS,HLIN)= ' '
              HPOS= HPOS+ 1
            END IF
            SPOS = 0
            BPOS = BPOS+ FLEN
            LPOS = BPOS
            DONFG= 0
            LLEN = LENSTR(FLEN,ISTR(LPOS+1))
            IF (HPOS+LLEN.GT.76) THEN
C             start filling in next line
              HLIN= HLIN+ 1
              IF (HLIN.EQ.5) THEN
C               out of room
                LPOS= ILEN+ 1
C               more responses left, indicate so with continuation marks
                IF (HPOS.LE.76) THEN
C                 enough room for 3 dots
                  I = 3
                  CALL CHRCHR (I,CHCONT,OSTR(HPOS,HLIN-1))
                ELSE
C                 may not be enough room for 3 dots
                  IF (OSTR(76,HLIN-1).EQ.' ') THEN
C                   use blank for 1st of 3 dots
                    I= 3
                    HPOS= 76
                  ELSE
C                   not enough room for 3 dots
                    I= 78- HPOS+ 1
                  END IF
                  CALL CHRCHR (I,CHCONT,OSTR(HPOS,HLIN-1))
                END IF
              ELSE
C               reset position on next line
                HPOS= 6
              END IF
            END IF
          ELSE
C           all done
            LPOS= ILEN+ 1
          END IF
        END IF
      IF (LPOS.LE.ILEN) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZCURMV
     I                    (GROUP,CODE,ZHLCOL,ZHLLEN,ZCRLIN,
     M                     ZCRCOL,ISWI,ICHA,ZMNTXT,ZMNLEN)
C
C     + + + PURPOSE + + +
C     move cursor position based on keyboard input
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      GROUP,CODE,ZHLCOL,ZHLLEN,ZCRLIN,
     1             ZCRCOL,ISWI,ICHA,ZMNLEN
      CHARACTER*78 ZMNTXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GROUP  - specifies type of keyboard input
C     CODE   - code specifying which key used for group type
C     ZHLCOL - beginning highlight column for this field
C     ZHLLEN - highlight length for this field
C     ZCRLIN - current cursor line
C     ZCRCOL - current cursor column
C     ISWI   - flag indicating to switch to another field
C     ICHA   - flag indicating field value has been changed
C     ZMNTXT - text containing current field
C     ZMNLEN - length of text in ZMNTXT
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + FUNCTIONS + + +
      INTEGER    ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZLNTXT, ZBEEP, ZWRVDR, SCCUMV
C
C     + + + END SPECIFICATIONS + + +
C
      IF (GROUP .EQ. 2) THEN
C       special character
        IF (CODE .EQ. 13) THEN
C         carriage return
          ISWI= 1
        ELSE IF (CODE .EQ. 9) THEN
C         tab
          ISWI= 1
        ELSE IF (CODE .EQ. 8) THEN
C         backspace
          ICHA= 1
          I   = ZHLCOL + 1
          IF (ZCRCOL .LE. I) THEN
C           at beginning of field
            CALL ZBEEP
          ELSE
C           move back a character
            ZCRCOL = ZCRCOL - 1
            CALL ZWRVDR (' ',ZCRLIN,ZCRCOL)
            ZMNTXT(ZCRCOL-1:ZCRCOL-1) = ' '
            ZMNLEN = ZLNTXT(ZMNTXT)
          END IF
        END IF
      ELSE
C       cursor movement keys
        IF (CODE .GE. 1 .AND. CODE .LE. 4) THEN
C         arrows
          IF (CODE .EQ. 3) THEN
C           right arrow
            IF (ZCRCOL .LT. ZHLCOL+ZHLLEN) THEN
C             move within field
              ZCRCOL = ZCRCOL + 1
              CALL SCCUMV(ZCRLIN,ZCRCOL)
            ELSE
C             switch fields
              ISWI= 1
            END IF
          ELSE IF (CODE .EQ. 4) THEN
C           left arrow
            IF (ZCRCOL .GT. ZHLCOL+1) THEN
C             move within field
              ZCRCOL = ZCRCOL - 1
              CALL SCCUMV(ZCRLIN,ZCRCOL)
            ELSE
C             switch fields
              ISWI= 1
            END IF
          ELSE
C           always switch up or down
            ISWI= 1
          END IF
        ELSE IF (CODE.EQ.5) THEN
C         home
          ZCRCOL= ZHLCOL+ 1
          CALL SCCUMV(ZCRLIN,ZCRCOL)
        ELSE IF (CODE.EQ.6) THEN
C         end
          I= ZLNTXT(ZMNTXT(ZHLCOL:ZHLCOL+ZHLLEN-1))+ 1
          IF (I.GT.ZHLLEN) THEN
C           at end of field
            I= ZHLLEN
          END IF
          ZCRCOL= ZHLCOL+ I
        ELSE IF (CODE.EQ.7) THEN
C         page up
          ISWI= 1
        ELSE IF (CODE.EQ.8) THEN
C         page down
          ISWI= 1
        ELSE IF (CODE.EQ.9) THEN
C         delete
          ICHA= 1
          I= ZHLCOL+ ZHLLEN- 1
          IF (ZCRCOL.LT.I) THEN
C           move later characters
            ZMNTXT(ZCRCOL-1:I-1)=ZMNTXT(ZCRCOL:I)
          END IF
          ZMNTXT(I:I)= ' '
          CALL ZWRVDR (ZMNTXT(ZHLCOL:I),ZCRLIN,ZHLCOL+1)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZTXINI
C
C     + + + PURPOSE + + +
C     initialize parameters for start of a text screen
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZSTCMA, ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
C     set help to not available (both general and specific)
      GPTR   = 0
      I= 64
      J= 0
      CALL ZIPI (I,J,HPTR)
      CALL ZSTCMA(1,0)
      IF (ZWNFLG.EQ.0) THEN
C       initialize screen name
        ZSCNAM = ' '
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZQUIET
C
C     + + + PURPOSE + + +
C     turn on quiet mode
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZDRWSC, ZWRTMN, ZWRMN1, ZSTCMA
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ZQUFG.NE.1) THEN
C       quiet flag on
        ZQUFG = 1
C       no middle window id
        ZWN2ID= 0
C       quiet option not now available
        CALL ZSTCMA(20,0)
C       resize boxes
        ZB1N= ZB1N+ ZB2N+ 2
        ZB2N= 0
C       redraw boxes
        CALL ZDRWSC
        IF (ZDTYP.NE.4) THEN
C         write a page of text
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
      SUBROUTINE   ZWINDO
C
C     + + + PURPOSE + + +
C     turn on window
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    LWNDID,LGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZWRTB3, ZSTCMA
C
C     + + + END SPECIFICATIONS + + +
C
C     window flag on
      WINFLG= 1
C     set window row/field to current row/field
      WROW= CROW
      WFLD= CFLD
C     write window instructions
      LWNDID= 12
      LGRP  = 86
      CALL ZWRTB3 (LWNDID,LGRP)
C     make window command unavailable
      CALL ZSTCMA (9,0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZCHRCH
     I                   (LEN,SPTR,CVAL,RSPLEN,RSPSTR,
     M                    OSTR)
C
C     + + + PURPOSE + + +
C     copy a string, using a global value if needed
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,SPTR,CVAL,RSPLEN
      CHARACTER*1 RSPSTR(RSPLEN),OSTR(LEN)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IPOS,ILEN
C
C     + + + EXTERNALS + + +
      EXTERNAL    WMSPIS, CHRCHR, GGTGLV
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WMSPIS (SPTR,
     O             IPOS,ILEN)
C
      IF (RSPSTR(IPOS) .EQ. '?') THEN
C       global value, make temp copy
        CALL CHRCHR(LEN,RSPSTR(IPOS),
     O              OSTR)
C       get the global value
        CALL GGTGLV(LEN,CVAL,
     M              OSTR,
     O              ILEN)
      ELSE
C       put current local value in field
        IPOS= IPOS+ (CVAL-1)* LEN
        CALL CHRCHR (LEN,RSPSTR(IPOS),
     O               OSTR)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZVALPS
     I                   (ILEN,ISTR,FLEN,ICNT,
     O                    ISTPOS,TLIN)
C
C     + + + PURPOSE + + +
C     Given a set of valid responses, determine the starting
C     positions for each line as it will be displayed in the
C     Limits window and determine the total number of lines needed.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ILEN,FLEN,ICNT,ISTPOS(50),TLIN
      CHARACTER*1 ISTR(ILEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ILEN   - total length of valid responses
C     ISTR   - character array of matching valid responses
C     FLEN   - length of field for which limits are being displayed
C     ICNT   - count of valid responses
C     ISTPOS - position in input string of start of each output line
C     TLIN   - total number of lines needed to display responses
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,LPOS,SPOS,BPOS,DONFG,LCNT,OPOS,LLEN
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL  LENSTR, ZIPI
C
C     + + + END SPECFICATIONS + + +
C
      LPOS = 0
      SPOS = 0
      BPOS = 0
      OPOS = 11
      LCNT = 0
      DONFG= 0
      I= 5
      J= 0
      CALL ZIPI (I,J,ISTPOS)
      TLIN= 1
      ISTPOS(TLIN)= 1
C
 10   CONTINUE
C       put commas in valid responses
        LPOS= LPOS+ 1
        SPOS= SPOS+ 1
        IF (ISTR(LPOS).NE.' ') THEN
C         process this character
          OPOS= OPOS+ 1
          IF (SPOS.EQ.FLEN) THEN
C           done this string
            DONFG= 1
          END IF
        ELSE
C         blank, done this string
          DONFG= 1
        END IF
        IF (DONFG.EQ.1) THEN
C         add comma, blank
          LCNT= LCNT+ 1
          IF (LCNT.LT.ICNT) THEN
C           not on last response
            OPOS= OPOS+ 1
            IF (OPOS.LE.76) THEN
C             room to insert blank character
              OPOS= OPOS+ 1
            END IF
            SPOS = 0
            BPOS = BPOS+ FLEN
            LPOS = BPOS
            DONFG= 0
            LLEN= LENSTR(FLEN,ISTR(LPOS+1))
            IF (OPOS+LLEN.GT.76) THEN
C             start filling in next line
              OPOS= 6
              TLIN= TLIN+ 1
              ISTPOS(TLIN)= BPOS+ 1
            END IF
          ELSE
C           all done
            LPOS= ILEN+ 1
          END IF
        END IF
      IF (LPOS.LE.ILEN) GO TO 10
C
      RETURN
      END
