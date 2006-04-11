C     z1util.f 2.1 9/4/91
C
C
C
      SUBROUTINE   ZEDIT1
     I                   (NROW,
     O                    IRET)
C
C     + + + PURPOSE + + +
C     To perform menu selection for given template file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NROW,IRET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NROW   - number of rows of data
C     IRET   - return control code
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
      INCLUDE 'cscren.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,JI,K,L,M,MI,N,NI,GROUP,CODE,ISWI,ICHA,LGRP,WNDID,
     $           TROW,TFLD,FINC,RINC,MNROW,EROW,ITMP,ILEN,IWRT,
     $           PCOL,ROWNDX,IOPEN
      INTEGER    FPROTI,FDVALI,FDINVI,FLENI
      REAL       RTMP,RTMP1,RTMIN,RTMAX
      CHARACTER  KEY*1,STRING*78,OLDSTR*78,TMPSTR*78,CNONE*4
      CHARACTER  OS1*78,OS2*78,OS3*78
      CHARACTER  FTYPI
      LOGICAL    AT1,CLEARD,ERROR
C
C     + + + FUNCTIONS + + +
      INTEGER    ZLNTXT
      REAL       CHRDEC
C
C     + + + INTRINSICS + + +
      INTRINSIC  CHAR, INDEX
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZGTKYD, ZLOCAR, ZCURMV, ZWRSCR, ZWRVDR, ZBEEP
      EXTERNAL   ZLNTXT, ZVERIF, SCCUMV, ZDRWSC, ZIPI, ZLIMIT, ZFILVF
      EXTERNAL   ZWRTMN, ZWRTB3, ZLJUST, ZARCMP, ZSTCMA, CHRDEC
C
C     + + + END SPECIFICATIONS + + +
C
      CNONE = 'none'
      DTRWF = CROW
      WINFLG= 0
C     set total number of lines in screen text
      ZMNNLI= NMHDRW+ NROW
      DO 10 I= 1,NROW
C       set screen text length for all data rows
        ZMNLEN(NMHDRW+I)= ZLNTXT(ZMNTXT(NMHDRW+I))
 10   CONTINUE
C
C     draw screen box
      CALL ZDRWSC
C
C     display menu
      CALL ZWRTMN (ZB1F,ZB1N,1,ZMNTXT,
     M             ZMNLEN)
C
      ZWN3ID= 0
      ZGP3  = 0
      IRET  = 0
      ZRET  = 0
      ICHA  = 0
C     AT1 tells whether the cursor has moved from its initial
C     position in the field.  CLEARD tells if the old string
C     has been cleared out to make way for the new, something
C     which should only happen if AT1 is true.
      AT1   = .TRUE.
      CLEARD= .FALSE.
C
 100  CONTINUE
        ISWI = 0
        MNROW= NMHDRW+ CROW
        CALL SCCUMV(ZCRLIN,ZCRCOL)
C       wait for keyboad interrupt
        IF (WINFLG.EQ.1) THEN
C         window command message
          LGRP= 86
        ELSE IF (QFLAG.EQ.0) THEN
C         not all fields protected, 'enter data' message
          LGRP = 85
        ELSE
C         all fields protected, 'view data' message
          LGRP= 87
        END IF
        CALL ZGTKYD (LGRP,CFLD,GROUP,CODE)
        IF (GROUP .EQ. 1) THEN
          I = ZHLCOL + ZHLLEN
          IF (ZCRCOL .GT. I) THEN
C           too far in field
            CALL ZBEEP
          ELSE
C           save new character
            KEY= CHAR(CODE)
            IF (ICHA.EQ.0) THEN
C             save current string
              I= ZHLCOL+ ZHLLEN- 1
              OLDSTR= ZMNTXT(MNROW)(ZHLCOL:I)
Chnb              CALL ZLJUST (OLDSTR)
C             first edit, clear whats in field if cursor is
C             in initial position within field.
              IF (AT1) THEN
                ZMNTXT(MNROW)(ZHLCOL:I)= ' '
                CLEARD = .TRUE.
              END IF
              ICHA= 1
              ZERR= 0
              AT1 = .FALSE.
            END IF
            ZMNTXT(MNROW)(ZCRCOL-1:ZCRCOL-1) = KEY
C
            IF (FTYP(CFLD) .EQ. FTI .OR.
     +          FTYP(CFLD) .EQ. FTR .OR.
     +          FTYP(CFLD) .EQ. FTD) THEN
               IF ((KEY .GE. '0' .AND. KEY .LE. '9') .AND.
     +             .NOT. CLEARD) THEN
                  DO 130, I = ZCRCOL,(ZHLCOL+ZHLLEN-1)
                     IF (ZMNTXT(MNROW)(I:I) .EQ. ' ') THEN
                        ZMNTXT(MNROW)(I:I) = '0'
                     ELSE
                        GO TO 135
                     END IF
 130              CONTINUE
 135              CONTINUE
               END IF
            END IF
C
            I = SCOL(CFLD)+ FLEN(CFLD)- 1
            CALL ZWRVDR (ZMNTXT(MNROW)(ZCRCOL-1:I),ZCRLIN,ZCRCOL)
            ZMNLEN(MNROW) = ZLNTXT(ZMNTXT(MNROW))
            ZCRCOL = ZCRCOL + 1
          END IF
        ELSE IF (ZRET.EQ.1) THEN
C         trying to exit, make sure current field value is checked
          ISWI= 1
        ELSE IF (WINFLG.EQ.1 .AND. GROUP.EQ.2 .AND.
     $           (CODE.EQ.27 .OR. (CODE.EQ.13 .AND. AT1))) THEN
C         ESCape or <CR> with nothing entered typed in Window mode,
C         exit Window, remove highlights
          WINFLG= 0
C         Window command again available
          CALL ZSTCMA (9,1)
          IF (WFLD.GT.CFLD) THEN
            FINC= -1
          ELSE
            FINC= 1
          END IF
          IF (WROW.GT.CROW) THEN
            RINC= -1
          ELSE
            RINC= 1
          END IF
          DO 160 TFLD= WFLD,CFLD,FINC
            M = SCOL(TFLD)
            N = SCOL(TFLD) + FLEN(TFLD) - 1
            DO 150 TROW= WROW,CROW,RINC
              L= NMHDRW+ TROW- DTRWF+ 1
              IF (L.GT.NMHDRW .AND. L.LE.ZB1N) THEN
C               overwrite highlight on screen
                CALL ZWRSCR (ZMNTXT(L)(M:N),L+1,M+1)
              END IF
 150        CONTINUE
 160      CONTINUE
C         restore original highlight
          CFLD= WFLD
          CROW= WROW
          CALL ZLOCAR (-CFLD,NROW)
        ELSE IF (GROUP.EQ.2 .OR. GROUP.EQ.3) THEN
C         special character or cursor movement keys
          CALL ZCURMV (GROUP,CODE,ZHLCOL,ZHLLEN,ZCRLIN,
     M                 ZCRCOL,ISWI,ICHA,ZMNTXT(MNROW),ZMNLEN(MNROW))
          IF ((ZCRCOL-1) .NE. ZHLCOL) AT1 = .FALSE.
        END IF
C
        JI = NMHDRW + CROW
        MI = SCOL(CFLD)
        NI = SCOL(CFLD) + FLEN(CFLD) - 1
C
        IF (ISWI.EQ.1 .AND. ZERR.NE.2) THEN
C         switch fields
          ZERR= 0
          AT1    = .TRUE.
          CLEARD = .FALSE.
          IF (ICHA.EQ.1) THEN
C           changed current value, check new one(s)
            IWRT= 1
            IF (WINFLG.EQ.0) THEN
C             set window row/field to current row/field
              WFLD= CFLD
              WROW= CROW
            END IF
            IF (WFLD.GT.CFLD) THEN
C             negative increment
              FINC= -1
            ELSE
C             positive increment
              FINC= 1
            END IF
            DO 300, TFLD = WFLD, CFLD, FINC
              IF (FTYP(TFLD).EQ.FTI) THEN
C               put integer values in reals
                RTMIN= IMIN(APOS(TFLD))
                RTMAX= IMAX(APOS(TFLD))
              ELSE IF (FTYP(TFLD).EQ.FTR) THEN
C               put integer values in reals
                RTMIN= RMIN(APOS(TFLD))
                RTMAX= RMAX(APOS(TFLD))
              ELSE IF (FTYP(TFLD).EQ.FTD) THEN
C               put double precision values in reals
                RTMIN= DMIN(APOS(TFLD))
                RTMAX= DMAX(APOS(TFLD))
              END IF
              IF (WROW.GT.CROW) THEN
C               negative increment
                RINC= -1
              ELSE
C               positive increment
                RINC= 1
              END IF
              M = SCOL(TFLD)
              N = SCOL(TFLD) + FLEN(TFLD) - 1
              DO 200, TROW = WROW, CROW, RINC
                L = NMHDRW + TROW - DTRWF+ 1
                IF (FPROT(TFLD).EQ.2) THEN
C                 protected field in window, overwrite highlight
                  CALL ZWRSCR (ZMNTXT(L)(M:N),L+1,M+1)
                ELSE
C                 ok to modify this field
                  STRING = ZMNTXT(JI)(MI:NI)
                  K = NMHDRW + TROW
                  IF (FTYP(TFLD).EQ.FTI .OR. FTYP(TFLD).EQ.FTR .OR.
     $                FTYP(TFLD).EQ.FTD) THEN
C                   may have arithmetic expression
                    IF (CROW.EQ.TROW .AND. CFLD.EQ.TFLD) THEN
C                     in the current row and field
                      TMPSTR= OLDSTR
                    ELSE
C                     in another part of window
                      TMPSTR= ZMNTXT(K)(M:N)
Chnb                      CALL ZLJUST (TMPSTR)
                    END IF
                    CALL ZLJUST (TMPSTR)
                    CALL ZLJUST (STRING)
                    CALL ZARCMP (FTYP(TFLD),FLEN(TFLD),TMPSTR,
     M                           STRING)
                  END IF
                  IF (FTYP(TFLD).EQ.FTF) THEN
C                   file type field, check file validity
                    IOPEN= 0
                    CALL ZFILVF (STRING,TFLD,IOPEN,
     M                           ZERR)
                  ELSE
C                   numeric or character field, verify validity
                    CALL ZVERIF(FTYP(TFLD),RTMIN,RTMAX,
     I                          FDVAL(TFLD),FDINV(TFLD),FLEN(TFLD),IWRT,
     M                          STRING,ZERR)
                  END IF
C                 ok to rewrite menu text with new value
                  IF (FTYP(TFLD).EQ.'C' .OR. FTYP(TFLD).EQ.'F') THEN
C                   character field, left justify
                    ZMNTXT(K)(M:N)= STRING
                  ELSE
C                   numeric field, right justify, clear field first
                    ZMNTXT(K)(M:N)= ' '
                    IF (ZLNTXT(STRING).GT.0) THEN
C                     something entered in field (otherwise leave field blank)
                      J = N - ZLNTXT(STRING)+ 1
                      ZMNTXT(K)(J:N) = STRING
                    END IF
                  END IF
                  IF (ZERR.NE.0 .AND. FPROT(TFLD).EQ.1) THEN
C                   field must be correct and it isn't
                    ZERR= 2
                  END IF
                  IF (ASDSFG(TFLD).EQ.'A' .AND. CROW.GT.1) THEN
C                   check to make sure values in ascending order
                    RTMP = CHRDEC(FLEN(TFLD),ZMNTX1(M,K))
                    RTMP1= CHRDEC(FLEN(TFLD),ZMNTX1(M,K-1))
                    IF (RTMP1.GE.RTMP) THEN
C                     previous value greater than current, order not ascending
                      LGRP = 74
                      WNDID= 13
                      CALL ZWRTB3 (WNDID,LGRP)
                    END IF
                  ELSE IF (ASDSFG(TFLD).EQ.'D' .AND. CROW.GT.1) THEN
C                   check to make sure values in descending order
                    RTMP = CHRDEC(FLEN(TFLD),ZMNTX1(M,K))
                    RTMP1= CHRDEC(FLEN(TFLD),ZMNTX1(M,K-1))
                    IF (RTMP1.LE.RTMP) THEN
C                     previous value less than current, order not descending
                      LGRP = 74
                      WNDID= 13
                      CALL ZWRTB3 (WNDID,LGRP)
                    END IF
                  END IF
                  IF (L.GT.NMHDRW .AND. L.LE.ZB1N) THEN
C                   rewrite field on screen
                    CALL ZWRSCR (ZMNTXT(K)(M:N),
     I                           L+1,M+1)
                  END IF
                END IF
 200          CONTINUE
 300        CONTINUE
            IF (ZERR.NE.0) THEN
C             move back to start of this field
              CODE= -CFLD
C             dont exit
              ZRET= 0
            END IF
C           turn change flag off
            ICHA  = 0
            IF (WINFLG.NE.0) THEN
C             turn off windowing, make window command available
              WINFLG= 0
              CALL ZSTCMA (9,1)
            END IF
          END IF
          IF (ZRET.EQ.0) THEN
C           ok current field, locate next field
            CALL ZLOCAR (CODE,NROW)
          END IF
        END IF
        IF (ZWN2ID.EQ.8 .AND. ZRET.EQ.0) THEN
C         limits currently displayed, make call to check for matches
          CALL ZLIMIT
        END IF
        IF (ZRET.NE.0) THEN
C         user wants out
C
CPRH          IF (ZRET .EQ. 1 .OR. ZRET .EQ. 2) THEN
CPRH          shouldn't we only check if user selects next (accept)
          IF (ZRET .EQ. 1) THEN
C           next screen, everything ok here?
            IWRT= 0
C
            ERROR = .FALSE.
            DO 400, I = 1, NFLDS
              K= SCOL(I)+ FLEN(I)- 1
              IF (FTYP(I).EQ.FTI) THEN
C               put integer values in reals
                RTMIN= IMIN(APOS(I))
                RTMAX= IMAX(APOS(I))
              ELSE IF (FTYP(I).EQ.FTR) THEN
C               put integer values in reals
                RTMIN= RMIN(APOS(I))
                RTMAX= RMAX(APOS(I))
              ELSE IF (FTYP(I).EQ.FTD) THEN
C               put double precision values in reals
                RTMIN= DMIN(APOS(I))
                RTMAX= DMAX(APOS(I))
              END IF
              FPROTI = FPROT(I)
              FTYPI  = FTYP(I)
              FDVALI = FDVAL(I)
              FDINVI = FDINV(I)
              FLENI  = FLEN(I)
              PCOL = SCOL(I)
              OS1 = ' '
              OS2 = ' '
              OS3 = ' '
              DO 380, CROW = 1, NROW
                  ROWNDX = CROW+NMHDRW
                  ITMP = INDEX (ZMNTXT(ROWNDX)(PCOL:K),CNONE)
                  ILEN = ZLNTXT (ZMNTXT(ROWNDX)(PCOL:K))
                  IF (ITMP .GT. 0 .OR. ILEN .EQ. 0) THEN
                    IF (FPROTI .EQ. 1) THEN
C                      null value in this field and we wont allow it
                       WNDID= 13
                       LGRP = 76
                       CALL ZWRTB3 (WNDID,LGRP)
                       CALL ZLOCAR (-I,CROW)
                       ERROR = .TRUE.
                       EROW= CROW
                     END IF
                  END IF
                  IF (.NOT. ERROR) THEN
C                   not a null value, verify its a good one
                    STRING= ZMNTXT(ROWNDX)(PCOL:K)
                    IF (OS1 .NE. ' ') THEN
                      IF (STRING .EQ. OS1) THEN
                        GOTO 390
                      ELSE IF (OS2 .NE. ' ') THEN
                        IF (STRING .EQ. OS2) THEN
                          GOTO 390
                        ELSE IF (OS3 .NE. ' ') THEN
                          IF (STRING .EQ. OS3) THEN
                            GOTO 390
                          ELSE
                            OS3 = OS2
                            OS2 = OS1
                            OS1 = STRING
                          END IF
                        ELSE
                          OS3 = OS2
                          OS2 = OS1
                          OS1 = STRING
                        END IF
                      ELSE
                        OS2 = OS1
                        OS1 = STRING
                      END IF
                    ELSE
                      OS1 = STRING
                    END IF
                    IF (FTYPI.EQ.FTF) THEN
C                     file type field, check file validity
                      IOPEN= 0
                      CALL ZFILVF (STRING,I,IOPEN,
     M                             ZERR)
                    ELSE
C                     numeric or character field, verify validity
                      CALL ZVERIF(FTYPI,RTMIN,RTMAX,
     I                            FDVALI,FDINVI,FLENI,IWRT,
     M                            STRING,ZERR)
                    END IF
 390                CONTINUE
                    IF (ZERR.NE.0 .AND.
     $                  (FPROTI.EQ.1 .OR. FTYP(I).EQ.'F')) THEN
C                     field must be correct and it isn't
                      IF (FTYP(I).NE.'F') THEN
C                       display message for numeric/character field
                        WNDID= 13
                        LGRP = 75
C                       clear out highlight
                        J = ZHLCOL + ZHLLEN - 1
                        CALL ZWRSCR (ZMNTXT(ZHLLIN+DTRWF-1)(ZHLCOL:J),
     1                               ZHLLIN+1,ZHLCOL+1)
C                       now no active highlight
                        ZHLLIN= 0
C                       write error message
                        CALL ZWRTB3 (WNDID,LGRP)
                      END IF
C                     move to problem field
                      CALL ZLOCAR (-I,CROW)
                      ERROR = .TRUE.
                      EROW= CROW
                    END IF
                  END IF
                  IF (ERROR) GO TO 410
 380          CONTINUE
 400        CONTINUE
 410        CONTINUE
C
            IF (.NOT. ERROR) THEN
C             all data present
              IRET = 1
C             may need to open files
              I= 0
 450          CONTINUE
C               check for any file type fields
                I= I+ 1
                IF (FTYP(I).EQ.'F') THEN
C                 open file by name in this field
                  DO 480, CROW = 1, NROW
C                   open file for each data row
                    ROWNDX= CROW+ NMHDRW
                    K= SCOL(I)+ FLEN(I)- 1
                    STRING= ZMNTXT(ROWNDX)(SCOL(I):K)
                    IOPEN = 1
                    ZERR  = 0
                    CALL ZFILVF (STRING,I,IOPEN,
     O                           ZERR)
                    IF (ZERR.NE.0) THEN
C                     couldn't open file
                      ZRET= 0
                      I= NFLDS
                    END IF
 480              CONTINUE
                END IF
              IF (I.LT.NFLDS) GO TO 450
            ELSE
C             missing data
              ZRET= 0
              CROW= EROW
            END IF
          ELSE
C           other exits
            IRET= ZRET
          END IF
        END IF
      IF (IRET .EQ. 0) GO TO 100
C
C     set dont save menu
      ZMNSAV= 0
C     reset protection
      I= 0
      CALL ZIPI (NFLDS,I,FPROT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZLOCAR
     I                   (CODE,NROW)
C
C     + + + PURPOSE + + +
C     locate neighbor field according to direction of cursor movement
C     with arrow keys
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CODE,NROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CODE   - direction code: =1  up
C                              =2  down
C                              =3  right
C                              =4  left
C                              =7  page up
C                              =8  page down
C                              =9  tab
C                              =13 carriage return
C     NROW   - number of rows of data
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C     screen control parameters
      INCLUDE 'cscren.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,I2,K,NEWLIN,MOVE,WNDID,QHLP,BROW,
     1           OROW,OFLD,TROW,TCOL,ADDHLT,DELHLT,
     2           KFROW,KLROW,KINC,JFFLD,JLFLD,JINC,RWFLG
C
C     + + + SAVES + + +
      SAVE       OROW
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZWRSCR, ZWRVDR, SCCUMV, ZLIMIT, ZWRHLP, ZSTCMA
C
C     + + + END SPECIFICATIONS + + +
C
      I2  = 2
      MOVE= 0
      BROW= DTRWF
      OFLD= CFLD
      ADDHLT= 0
      DELHLT= 0
Chnb  IF (CODE.GT.0) OROW= CROW
      OROW = CROW
C
C     search the closest neighbor
      IF (CODE.EQ.1) THEN
C       up one line
        IF (CROW.GT.1) THEN
C         ok to move up
          NEWLIN= ZHLLIN- 1
          IF (NEWLIN.LE.NMHDRW) THEN
C           at top of current window, need to move up menu text
            DTRWF = DTRWF- 1
            NEWLIN= NMHDRW+ 1
          END IF
          CROW= CROW- 1
        ELSE
C         already at the top
          NEWLIN= ZHLLIN
        END IF
      ELSE IF (CODE.EQ.2) THEN
C       down one line
        IF (CROW.LT.NROW) THEN
C         ok to move down
          NEWLIN= ZHLLIN + 1
          IF (NEWLIN.GT.ZB1N) THEN
C           need to move down menu text
            DTRWF = DTRWF+ 1
            NEWLIN= ZHLLIN
          END IF
          CROW= CROW+ 1
        ELSE
C         already at bottom
          NEWLIN= ZHLLIN
        END IF
      ELSE IF (CODE.EQ.3) THEN
C       move right
        IF (NFLDS.GT.1) THEN
C         other fields to move to
          IF (CFLD.LT.NFLDS .OR. WINFLG.EQ.0) THEN
C           ok to move right
C 10         CONTINUE
C             look for unprotected field
              IF (CFLD.LT.NFLDS) THEN
C               move right one field
                CFLD= CFLD+ 1
              ELSE IF (WINFLG.EQ.0) THEN
C               move to leftmost field
                CFLD= 1
              ELSE
C               cant move right, its protected
                CFLD= OFLD
              END IF
C            IF (FPROT(CFLD).EQ.2) GO TO 10
            IF (CFLD.NE.OFLD) THEN
C             changed fields
              MOVE= 1
            END IF
          END IF
        END IF
        NEWLIN= ZHLLIN
      ELSE IF (CODE.EQ.4) THEN
C       move left
        IF (NFLDS.GT.1) THEN
C         other fields to move to
          IF (CFLD.GT.1 .OR. WINFLG.EQ.0) THEN
C           ok to move left
C 20         CONTINUE
              IF (CFLD.GT.1) THEN
C               move left one field
                CFLD= CFLD- 1
              ELSE IF (WINFLG.EQ.0) THEN
C               move to rightmost field
                CFLD= NFLDS
              ELSE
C               cant move left, its protected
                CFLD= OFLD
              END IF
C            IF (FPROT(CFLD).EQ.2) GO TO 20
            IF (CFLD.NE.OFLD) THEN
C             changed fields
              MOVE= 1
            END IF
          END IF
        END IF
        NEWLIN= ZHLLIN
      ELSE IF (CODE.EQ.7) THEN
C       page up
        IF (DTRWF.GT.1) THEN
C         room to move up
          TROW= ZB1N- NMHDRW
          IF (TROW.GE.DTRWF) THEN
C           cant move up that much
            TROW= DTRWF- 1
          END IF
          DTRWF = DTRWF- TROW
          CROW  = CROW- TROW
          NEWLIN= ZHLLIN
        ELSE
C         move to top of current screen
          NEWLIN= NMHDRW+ 1
          CROW  = 1
        END IF
      ELSE IF (CODE.EQ.8) THEN
C       page down
        TROW= DTRWF+ ZB1N- NMHDRW
        IF (TROW.LE.NROW) THEN
C         room to move down
          IF (TROW+ZB1N-NMHDRW.GT.NROW) THEN
C           cant move that far
            TROW= NROW- ZB1N+ NMHDRW+ 1
          END IF
          DTRWF = TROW
          CROW  = CROW+ DTRWF- BROW
          NEWLIN= ZHLLIN
        ELSE
C         bottom row is on current screen
          NEWLIN= NMHDRW+ NROW- DTRWF+ 1
          CROW  = NROW
        END IF
      ELSE IF ((CODE.EQ.13 .OR. CODE.EQ.9) .AND. WINFLG.EQ.0) THEN
C       carriage return or tab (not allowed when windowing)
 30     CONTINUE
C         look for unprotected field
          CFLD= CFLD+ 1
          IF (CFLD.GT.NFLDS) THEN
C           move to first field, next line if possible
            CFLD = 1
            IF (CROW.LT.NROW) THEN
C             can still move down a row
              CROW= CROW+ 1
              IF (ZHLLIN.GE.ZB1N) THEN
C               at bottom of page
                DTRWF = DTRWF+ 1
              END IF
            END IF
          END IF
        IF (FPROT(CFLD).EQ.2 .AND. QFLAG.EQ.0) GO TO 30
        IF (CFLD.NE.OFLD) THEN
C         changed fields
          MOVE= 1
        END IF
        IF (CROW.NE.OROW .AND. DTRWF.EQ.BROW) THEN
C         changed highlight rows
          NEWLIN= ZHLLIN+ 1
        ELSE
C         same row
          NEWLIN= ZHLLIN
        END IF
      ELSE IF (CODE.LT.0) THEN
C       move to this absolute field
        CFLD= -CODE
Chnb begin
        DTRWL = DTRWF + ZB1N - NMHDRW -1
Chnb end
        IF (CROW.LT.DTRWF) THEN
C         moving to a row above top of window
          DTRWF= CROW
        ELSE IF (CROW.GT.DTRWL) THEN
C         moving to a row below bottom of window
Chnb begin
          DTRWF = CROW - ZB1N + NMHDRW + 1
          IF (DTRWF .LT. 1) THEN
            DTRWF = DTRWF + (1 - DTRWF)
          END IF
Chnb end
        END IF
        NEWLIN= NMHDRW+ CROW- DTRWF+ 1
      END IF
C     calc last data row
      DTRWL= DTRWF+ ZB1N- NMHDRW- 1
C
      IF (DTRWF.NE.BROW .OR. CODE.EQ.7 .OR. CODE.EQ.8) THEN
        IF (DTRWF.NE.BROW .OR. WINFLG.EQ.1) THEN
C         rewrite screen and remove all highlights
          J= NMHDRW+ 1
          DO 100 I= DTRWF,DTRWL
C           write this line
            J= J+ 1
            CALL ZWRSCR (ZMNTXT(NMHDRW+I),J,I2)
100       CONTINUE
        END IF
        RWFLG= 1
      ELSE
C       no screen rewrite needed
        RWFLG= 0
      END IF
C
      IF (WINFLG.EQ.1) THEN
C       windowing on, see if window size changes
        IF (ABS(CROW-WROW).GT.ABS(OROW-WROW)) THEN
C         moving away from start, add highlight
          ADDHLT= 1
        ELSE IF (ABS(CROW-WROW).LT.ABS(OROW-WROW)) THEN
C         moving toward start, remove highlight
          DELHLT= 1
        ELSE IF (ABS(CFLD-WFLD).GT.ABS(OFLD-WFLD)) THEN
C         moving away from start, add highlight
          ADDHLT= 2
        ELSE IF (ABS(CFLD-WFLD).LT.ABS(OFLD-WFLD)) THEN
C         moving toward from start, remove highlight
          DELHLT= 2
        END IF
      ELSE
C       move field
        DELHLT= 3
        ADDHLT= 3
      END IF
C
      IF (DELHLT.NE.0.AND.DTRWF.EQ.BROW) THEN
C       remove highlighted fields
        IF (DELHLT.EQ.1) THEN
C         remove a row
          JFFLD= WFLD
          JLFLD= OFLD
          IF (WFLD.GT.OFLD) THEN
C           move left
            JINC= -1
          ELSE
C           move right
            JINC= 1
          END IF
          KFROW= OROW
          KLROW= OROW
          KINC = 1
        ELSE IF (DELHLT.EQ.2) THEN
C         remove a field
          JFFLD= OFLD
          JLFLD= OFLD
          JINC = 1
          KINC = 1
          IF (WROW.GT.OROW) THEN
C           move up
            KFROW= OROW
            KLROW= WROW
            IF (KLROW.GT.DTRWL) THEN
C             only rows on screen
              KLROW= DTRWL
            END IF
          ELSE
C           move down
            KFROW= WROW
            IF (WROW.LT.DTRWF) THEN
C             only rows on screen
              KFROW= DTRWF
            END IF
            KLROW= OROW
          END IF
        ELSE
C         no window
          JFFLD= OFLD
          JLFLD= OFLD
          JINC = 1
          KFROW= OROW
          KLROW= OROW
          KINC = 1
        END IF
        DO 150 J= JFFLD,JLFLD,JINC
C         fields
          TCOL= SCOL(J)
          DO 140 K= KFROW,KLROW,KINC
C           rows
            I= TCOL+ FLEN(J)- 1
            TROW= K- DTRWF+ NMHDRW
            CALL ZWRSCR(ZMNTXT(K+NMHDRW)(TCOL:I),TROW+2,TCOL+1)
 140      CONTINUE
 150    CONTINUE
      END IF
      IF (RWFLG.EQ.1 .AND. DELHLT.EQ.1) THEN
C       highlight whats left
        ADDHLT= 1
      END IF
      IF (ADDHLT.NE.0) THEN
C       highlight fields
        IF (ADDHLT.EQ.1) THEN
C         add a row
          JFFLD= WFLD
          JLFLD= CFLD
          IF (WFLD.GT.CFLD) THEN
C           move left
            JINC= -1
          ELSE
C           move right
            JINC= 1
          END IF
          IF (RWFLG.EQ.1) THEN
C           rewrite on, lots to highlight
            IF (CODE.EQ.1 .OR. CODE.EQ.7) THEN
C             up
              IF (WROW.GT.DTRWL) THEN
C               just to bottom of screen
                KLROW= DTRWL
              ELSE IF (WROW.LT.DTRWF) THEN
C               window starts above screen, highlight top row
                KLROW= DTRWF
              ELSE
C               to bottom of window
                KLROW= WROW
              END IF
              KFROW= CROW
            ELSE IF (CODE.EQ.2 .OR. CODE.EQ.8) THEN
C             down
              IF (WROW.LT.DTRWF) THEN
C               from start of screen
                KFROW= DTRWF
              ELSE
C               from start of window
                KFROW= WROW
              END IF
              KLROW= CROW
            END IF
          ELSE
C           just the current row
            KFROW= CROW
            KLROW= CROW
          END IF
          IF (KFROW.GT.KLROW) THEN
C           starting below final row
            KINC= -1
          ELSE
            KINC = 1
          END IF
        ELSE IF (ADDHLT.EQ.2) THEN
C         add a field
          JFFLD= CFLD
          JLFLD= CFLD
          JINC = 1
          KINC = 1
          IF (WROW.GT.CROW) THEN
C           move up
            KFROW= CROW
            KLROW= WROW
            IF (KLROW.GT.DTRWL) THEN
C             only rows on screen
              KLROW= DTRWL
            END IF
          ELSE
C           move down
            KFROW= WROW
            IF (WROW.LT.DTRWF) THEN
C             only rows on screen
              KFROW= DTRWF
            END IF
            KLROW= CROW
          END IF
        ELSE
C         no window
          JFFLD= CFLD
          JLFLD= CFLD
          JINC = 1
          KFROW= CROW
          KLROW= CROW
          KINC = 1
        END IF
        DO 170 J= JFFLD,JLFLD,JINC
C         fields
          TCOL= SCOL(J)
          DO 160 K= KFROW,KLROW,KINC
C           rows
            I= TCOL+ FLEN(J)- 1
            TROW= K- DTRWF+ NMHDRW
            CALL ZWRVDR(ZMNTXT(K+NMHDRW)(TCOL:I),TROW+2,TCOL+1)
 160      CONTINUE
 170    CONTINUE
      END IF
C     reset highlighted cursor
      ZHLLIN= NEWLIN
      ZHLCOL= SCOL(CFLD)
      ZHLLEN= FLEN(CFLD)
C     move cursor
      ZCRLIN = ZHLLIN + 1
      ZCRCOL = ZHLCOL + 1
      CALL SCCUMV(ZCRLIN,ZCRCOL)
C
      IF (MOVE.EQ.1) THEN
C       changed fields, display new limits or help?
        IF (ZWN2ID.EQ.8) THEN
C         yes, show new field's limits
          CALL ZLIMIT
        ELSE IF (ZWN2ID.EQ.7) THEN
C         show new help
          QHLP = 1
          WNDID= 7
          CALL ZWRHLP (ZMESFL,GPTR,HPTR(CFLD),WNDID,QHLP)
        END IF
      END IF
C
C     up page/down page available?
      IF (CROW.GT.1 .AND. ZCMDAV(15).EQ.0) THEN
C       make up page available
        CALL ZSTCMA(15,1)
      ELSE IF (CROW.EQ.1 .AND. ZCMDAV(15).EQ.1) THEN
C       make up page unavailable
        CALL ZSTCMA(15,0)
      END IF
      IF (CROW.LT.NROW .AND. ZCMDAV(14).EQ.0) THEN
C       make down page available
        CALL ZSTCMA(14,1)
      ELSE IF (CROW.EQ.NROW .AND. ZCMDAV(14).EQ.1) THEN
C       make down page unavailable
        CALL ZSTCMA(14,0)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZWRMN1
C
C     + + + PURPOSE + + +
C     rewrite 2-d data screen
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
      INCLUDE 'cscren.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I2,J,NROW,LROW
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZWRSCR
C     EXTERNAL   ZWRVDR
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
C
C     always write header
      DO 100 I= 1,NMHDRW
        CALL ZWRSCR (ZMNTXT(I),I+1,I2)
 100  CONTINUE
C
      IF (ZHLLIN.GT.ZB1N) THEN
C       highlighted field outside of new window
        DTRWF = DTRWF+ ZHLLIN- ZB1N
        ZHLLIN= ZB1N
        ZCRLIN= ZHLLIN+ 1
      END IF
C     new last row
      DTRWL= DTRWF+ ZB1N- NMHDRW- 1
      NROW = ZMNNLI- NMHDRW
      IF (DTRWL.GT.NROW) THEN
C       move up
        DTRWL= NROW
        J    = DTRWF
        DTRWF= DTRWL- ZB1N+ NMHDRW+ 1
        IF (DTRWF.LT.1) THEN
C         display starts at first data row
          DTRWF = 1
        END IF
        ZHLLIN= ZHLLIN+ J- DTRWF
        ZCRLIN= ZHLLIN+ 1
      END IF
C     rewrite screen
      J= NMHDRW+ 1
      IF (NMHDRW+DTRWL.LT.ZB1N) THEN
C       need some blank menu text to fill screen
        LROW = ZB1N - NMHDRW
      ELSE
C       data fills up screen
        LROW = DTRWL
      END IF
      DO 200 I= DTRWF,LROW
C       write this line
        J= J+ 1
        CALL ZWRSCR (ZMNTXT(NMHDRW+I),J,I2)
 200  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2INIT
     I                   (MESSFL,SCLU,SGRP)
C
C     + + + PURPOSE + + +
C     Set values in common for a 2-dimensional data screen
C     from information off the message file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,SGRP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     SGRP   - group number on message file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,RETCOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   WMSGTP
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WMSGTP (MESSFL,SCLU,SGRP,
     O             I,RETCOD)
C
      IF (RETCOD.NE.0) THEN
C       problem reading parms, echo to ERROR.FIL
        WRITE (99,*) 'Problem reading information from message file.'
        WRITE (99,*) 'MESSFL,SCLU,SGRP,RETCOD',MESSFL,SCLU,SGRP,RETCOD
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q2EDIT
     I                   (NROW,
     O                    IRET)
C
C     + + + PURPOSE + + +
C     Edit screen of 2-dimensional data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NROW,IRET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NROW   - number of rows of data values
C     IRET   - value of user exit command
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZEDIT1
C
C     + + + END SPECIFICATIONS + + +
C
C     perform data screen editing
      CALL ZEDIT1 (NROW,
     O             IRET)
C
      RETURN
      END
