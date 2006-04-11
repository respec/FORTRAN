C     z4util.f 2.1 9/4/91
C
C
C
      SUBROUTINE   ZSLMNN
     I                   (NUMB,WIDTH,CLEN,
     M                    ANS,
     O                    ID,IRET)
C
C     + + + PURPOSE + + +
C     To perform menu selection for given template file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NUMB,WIDTH,CLEN,ANS,IRET,ZTEST
      CHARACTER ID*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NUMB   - number option flag (0- no numbers, 1- number options)
C     WIDTH  - column width
C     CLEN   - column length
C     ANS    - users answer
C     ID     - return ID
C     IRET   - return control code
C              1 - next screen
C              2 - prev screen
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cqrsp.inc'
C     control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GROUP,CODE,MIN,MAX,ULEN,I1,I,J,K,ZRESP,IWRT,INUM,
     1          ZCONFL(64),ZCONCT,CLIN,CCOL,MXCOL,WNDID,IGN,QHLP,
     2          PRMIND,PRMVAL,MINH,MINV,L,M,ILEN,TMPANS,LROW,LCOL,OANS
C
C     + + + FUNCTIONS + + +
      INTEGER    LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   LENSTR, ZGTKEY, ZWRHLP, ZDRWSC, ZSTCMA
      EXTERNAL   ZWRTMN, ZWRTB3, ZWRSCR, ZWRVDR, SCCUMV
      EXTERNAL   ZIPI, Z4POS, ZGTKYD, QUPCAS, ANPRGT, ZMVMSE
C
C     + + + INTRINSICS + + +
      INTRINSIC  CHAR, IABS, MOD
C
C     + + + END SPECIFICATIONS + + +
C
      ZTEST = 0
      I1   = 1
      MXCOL= 1+ (NANS-1)/CLEN
C
C     write screen
      ZDTYP = 2
      TMPANS= DANS
      IF (ANS.GT.0) THEN
C       temporarily set DANS (in common) to current option
C       so that proper help info is displayed in ZDRWSC
        DANS= ANS
      END IF
      CALL ZDRWSC
C     reset DANS
      DANS= TMPANS
C     allowable numeric responses are ascii codes for numbers
      MIN = 49
      MAX = MIN + NANS - 1
      IF (MAX .GT. 57) THEN
C       more than 9 responses allowed
        MAX= 57
      END IF
C
C     display menu
      IF (ANS .EQ. 0) THEN
C       need a value for ANS
        IF (DANS.GT.0) THEN
C         use default
          ANS= DANS
        ELSE
C         default to first
          ANS= 1
        END IF
      END IF
C     set highlight for default
      CALL Z4POS (ZCLEN,ZCWID,ZOPLIN(ANS),ZOPCOL(ANS),ZOPLEN(ANS),
     O            ZHLLIN,ZHLCOL,ZHLLEN)
      CALL ZWRTMN (ZB1F,ZB1N,I1,ZMNTXT,
     M             ZMNLEN)
C
C     position cursor in lower right corner
      ZCRLIN= 23
      ZCRCOL= 79
      ZWN3ID= 0
      ZGP3  = 0
      ZRET  = 0
      IRET  = 0
 100  CONTINUE
        IWRT= 0
C       wait for keyboad interrupt
        CALL SCCUMV(ZCRLIN,ZCRCOL)
        IGN = 41
        CALL ZGTKYD(IGN,ANS,
     O              GROUP,CODE)
        IF (GROUP .EQ. 7) THEN
          LROW = CODE/1000
          LCOL = MOD(CODE,1000)
          CALL ZMVMSE (LROW,LCOL,
     O                 OANS)
          WRITE(99,*) 'ZSLMNN:',ANS,OANS,LROW,LCOL,ZRET
          IF (OANS .GT. 0) THEN
C           use clicked on a menu option
            IF (ANS .EQ. OANS) THEN
C             click on this field - choose it
              ZRET= 1
              IRET= ZRET
            ELSE
              ANS = OANS
              IWRT= 1
            END IF
          END IF
        END IF
        IF (ZRET.EQ.0 .OR. ANS.EQ.0) THEN
C         no selection yet
          ZERR= 0
          IF (GROUP .EQ. 3) THEN
C           not a number
            INUM= 0
            IF (CODE .EQ. 1) THEN
C             up arrow
              IWRT = 1
              MINV= 1000
              J   = ANS
              DO 110 I = 1, NANS
                IF (I .NE. ANS) THEN
C                 check each field except current one
                  IF (ZOPLIN(I) .LT. ZOPLIN(ANS)) THEN
C                   go up
                    L = ZOPLIN(ANS) - ZOPLIN(I)
                  ELSE
C                   go to bottom
                    L = ZOPLIN(ANS) - ZOPLIN(I) + 14
                  END IF
                  IF (L .LT. MINV) THEN
C                   new closest
                    J   = I
                    MINV= L
                    MINH= IABS(ZOPCOL(ANS) - ZOPCOL(I))
                  ELSE IF (L .EQ. MINV) THEN
C                   might be close, check column
                    M = IABS(ZOPCOL(ANS) - ZOPCOL(I))
                    IF (M .LT. MINH) THEN
C                     new closest
                      J   = I
                      MINV= L
                      MINH= IABS(ZOPCOL(ANS) - ZOPCOL(I))
                    END IF
                  END IF
                END IF
 110          CONTINUE
              IF (J .NE. ANS) THEN
C               change to new menu option
                ANS= J
              END IF
            ELSE IF (CODE .EQ. 2) THEN
C             down arrow
              IWRT = 1
              MINV= 1000
              J   = ANS
              DO 120 I = 1, NANS
                IF (I .NE. ANS) THEN
C                 check each field except current one
                  IF (ZOPLIN(I) .GT. ZOPLIN(ANS)) THEN
C                   go down
                    L = ZOPLIN(I) - ZOPLIN(ANS)
                  ELSE
C                   go to top
                    L = ZOPLIN(I) - ZOPLIN(ANS) + 14
                  END IF
                  IF (L .LT. MINV) THEN
C                   new closest
                    J   = I
                    MINV= L
                    MINH= IABS(ZOPCOL(ANS) - ZOPCOL(I))
                  ELSE IF (L .EQ. MINV) THEN
C                   might be close, check columns
                    M = IABS(ZOPCOL(ANS) - ZOPCOL(I))
                    IF (M .LT. MINH) THEN
C                     new closest
                      J   = I
                      MINV= L
                      MINH= IABS(ZOPCOL(ANS) - ZOPCOL(I))
                    END IF
                  END IF
                END IF
 120          CONTINUE
              IF (J .NE. ANS) THEN
C               change to new menu option
                ANS= J
              END IF
            ELSE IF (CODE .EQ. 3) THEN
C             right arrow
              IWRT= 1
              MINH= 1000
              J   = ANS
              DO 130 I = 1, NANS
C               check each field
                IF (I .NE. ANS) THEN
C                 dont check current field
                  IF (ZOPLIN(I) .EQ. ZOPLIN(ANS)) THEN
C                   same menu record
                    IF (ZOPCOL(I) .GE. ZOPCOL(ANS)) THEN
C                     new is to right
                      L = ZOPCOL(I) - ZOPCOL(ANS)
                    ELSE
C                     new is to left
                      L = ZOPCOL(I) - ZOPCOL(ANS) + 78
                    END IF
                    IF (L .LT. MINH) THEN
C                     new closest
                      MINH= L
                      J   = I
                    END IF
                  END IF
                END IF
 130          CONTINUE
              IF (J .NE. ANS) THEN
C               change to new menu option
                ANS= J
              END IF
            ELSE IF (CODE .EQ. 4) THEN
C             left arrow
              IWRT= 1
              MINH= 1000
              J   = ANS
              DO 140 I = 1, NANS
C               check each option
                IF (I .NE. ANS) THEN
C                 dont check current field
                  IF (ZOPLIN(I) .EQ. ZOPLIN(ANS)) THEN
C                   same menu record
                    IF (ZOPCOL(I) .LE. ZOPCOL(ANS)) THEN
C                     new is to left
                      L = ZOPCOL(ANS) - ZOPCOL(I)
                    ELSE
C                     new is to right
                      L = ZOPCOL(ANS) - ZOPCOL(I) + 78
                    END IF
                    IF (L .LT. MINH) THEN
C                     new closest
                      MINH= L
                      J   = I
                    END IF
                  END IF
                END IF
 140          CONTINUE
              IF (J .NE. ANS) THEN
C               change to new menu option
                ANS= J
              END IF
            END IF
          ELSE IF (GROUP .EQ. 1) THEN
C           normal ASCII
            IF (((CODE .GE. MIN .AND. CODE .LE. MAX) .OR.
     1         (CODE .EQ. 48  .AND. INUM .NE. 0)) .AND. NUMB.EQ.1) THEN
C             a number, and were accepting numbers
              IF (INUM .NE. 0 .AND. NANS .GT. 9) THEN
C               might be second digit
                J = ANS*10+ CODE- MIN+ 1
                IF (J .LE. NANS) THEN
C                 good value
                  ANS = J
                  IWRT= 1
                END IF
                INUM  = 0
              END IF
              IF (IWRT .EQ. 0 .AND. ANS .NE. CODE-MIN+1) THEN
C               move directly to option specified
                ANS = CODE - MIN + 1
                IWRT= 1
                INUM= ANS
              END IF
            ELSE
C             not a number
              INUM= 0
C             maybe user specified character string
              ULEN= 1
              UANS(ULEN)= CHAR(CODE)
C             be sure upper case
              CALL QUPCAS(I1,UANS(ULEN))
              IF (ZHLLEN .GT. 0) THEN
C               get rid of old option
                CALL ZWRSCR(ZMNTXT(ZHLLIN)(ZHLCOL:ZHLCOL+ZHLLEN-1),
     1                       ZHLLIN+1,ZHLCOL+1)
              END IF
C             no conflicts yet
              ZCONCT= 0
              CALL ZIPI (NANS,ZCONCT,ZCONFL)
 200          CONTINUE
C               check answer
                ZCONCT= 0
                J     = ULEN
                DO 300 I= 1, NANS
                  IF (ZCONFL(I) .EQ. ULEN-1 ) THEN
C                   a match last time
                    IF (UANS(ULEN) .EQ. TANS(J)) THEN
C                     a match this time
                      ZCONCT   = ZCONCT+ 1
                      ZCONFL(I)= ULEN
                      ZRESP    = I
                      CALL Z4POS (ZCLEN,ZCWID,
     I                            ZOPLIN(I),ZOPCOL(I),ZOPLEN(I),
     O                            CLIN,CCOL,K)
                      CALL ZWRVDR
     I                  (ZMNTXT(CLIN)(CCOL+NUMB:CCOL+NUMB+ULEN-1),
     1                   CLIN+1,CCOL+NUMB+1)
                    ELSE IF (ULEN.GT.1) THEN
C                     get rid of old inverse video
                      CALL Z4POS (ZCLEN,ZCWID,
     I                            ZOPLIN(I),ZOPCOL(I),ZOPLEN(I),
     O                            CLIN,CCOL,K)
                      CALL ZWRSCR
     I                  (ZMNTXT(CLIN)(CCOL+NUMB:CCOL+NUMB+ULEN-1),
     1                   CLIN+1,CCOL+NUMB+1)
                    END IF
                  END IF
                  J= J+ LANS
 300            CONTINUE
                IF (ZCONCT .GT. 1 .AND. ULEN .LT. LANS) THEN
C                 conflicts, get another character
                  CALL SCCUMV(ZCRLIN,ZCRCOL)
                  IGN  = 42
                  WNDID= 12
                  CALL ZWRTB3(WNDID,IGN)
                  CALL ZGTKEY(GROUP,CODE)
                  ULEN= ULEN+ 1
                  UANS(ULEN)= CHAR(CODE)
C                 convert to upper case
                  CALL QUPCAS(I1,UANS(ULEN))
                ELSE IF (ZCONCT .EQ. 1) THEN
C                 good answer
                  ANS = ZRESP
                  IWRT= 1
                  ULEN= LANS+ 1
C                 jlk 9/2 quick exit without confirmation
                  ZRET= 1
                  IRET= ZRET
                ELSE
C                 bad answer
                  ULEN = LANS+ 1
                  IGN  = 43
                  WNDID= 13
                  CALL ZWRTB3(WNDID,IGN)
                  IF (ZHLLEN .GT. 0) THEN
C                   write old option in inverse video
                    CALL ZWRVDR (ZMNTXT(ZHLLIN)(ZHLCOL:ZHLCOL+ZHLLEN-1),
     I                           ZHLLIN+1,ZHLCOL+1)
                  END IF
                END IF
              IF (ULEN .LE. LANS) GOTO 200
            END IF
          ELSE IF (GROUP.EQ.2 .AND. CODE.EQ.9) THEN
C           tab
            IF (ANS.EQ.NANS) THEN
C             at bottom, go to top
              ANS= 1
            ELSE
              ANS= ANS+ 1
            END IF
            IWRT= 1
          ELSE IF (GROUP.EQ.2 .AND. CODE.EQ.13) THEN
C           carriage return, may need to do something
            PRMIND= 9
            CALL ANPRGT (PRMIND,PRMVAL)
            IF (PRMVAL.EQ.2) THEN
C             treat CR down arrow
              IF (ANS.EQ.NANS) THEN
C               at bottom, go to top
                ANS= 1
              ELSE
                ANS= ANS+ 1
              END IF
              IWRT= 1
            ELSE IF (PRMVAL.EQ.3) THEN
C             treat CR as 'next' command
              ZRET= 1
              IRET= ZRET
            END IF
          ELSE
C           not a number
            INUM= 0
          END IF
        ELSE
C         an option has been selected
          IRET= ZRET
        END IF
C
        IF (IRET .EQ. 0) THEN
          IF (IWRT .EQ. 1) THEN
C           rewrite menu with different option in reverse video
            IF (ZHLLEN .GT. 0) THEN
C             turn off inverse video on old option
              CALL ZWRSCR (ZMNTXT(ZHLLIN)(ZHLCOL:ZHLCOL+ZHLLEN-1),
     1                     ZHLLIN+1,ZHLCOL+1)
            END IF
C           set new option
            CALL Z4POS (ZCLEN,ZCWID,ZOPLIN(ANS),ZOPCOL(ANS),ZOPLEN(ANS),
     O                  ZHLLIN,ZHLCOL,ZHLLEN)
C           write new option in inverse video
            CALL ZWRVDR (ZMNTXT(ZHLLIN)(ZHLCOL:ZHLCOL+ZHLLEN-1),
     1                   ZHLLIN+1,ZHLCOL+1)
            IF (ZWN2ID.EQ.7) THEN
C             show new help
              QHLP = 1
              WNDID= 7
              CALL ZWRHLP (ZMESFL,GPTR,HPTR(ANS),WNDID,QHLP)
            END IF
          END IF
        END IF
      IF (IRET .EQ. 0) GO TO 100
C
      IF (IRET .EQ. 1) THEN
C       make sure new position is set on new option
        CALL Z4POS (ZCLEN,ZCWID,ZOPLIN(ANS),ZOPCOL(ANS),ZOPLEN(ANS),
     O              ZHLLIN,ZHLCOL,ZHLLEN)
C       pass users choice back to calling program
        ILEN = LENSTR(LANS,TANS((ANS-1)*LANS+1))
C       use only the length of the current menu option
        ZTEST = ZHLCOL+ILEN-1
        IF (ZTEST .LE. 78) THEN
          ID = ZMNTXT(ZHLLIN)(ZHLCOL:ZHLCOL+ILEN-1)
        ELSE
          ID = ZMNTXT(ZHLLIN)(ZHLCOL:78)
        END IF
      END IF
C     turn off inverse video
C     CALL ZWRSCR (ZMNTXT(ZHLLIN)(ZHLCOL:ZHLCOL+ZHLLEN-1),
C    1             ZHLLIN+1,ZHLCOL+1)
C     move cursor to bottom of screen
      CALL SCCUMV(24,80)
C     no active highlight
      ZHLLIN= 0
C     set dont save menu
      ZMNSAV= 0
C     turn off some commands
      DO 400 I= 17,18
        CALL ZSTCMA (I,0)
 400  CONTINUE
C     reset question, response, and help init flags
      QUINIT= 0
      RSINIT= 0
      HPINIT= 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   Z4POS (ZCLEN,ZCWID,OPLIN,OPCOL,OPLEN,
     O                    ZHLLIN,ZHLCOL,ZHLLEN)
C
C     Calculate line, column, and position to highlight
C     for current menu option.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ZCLEN,ZCWID,OPLIN,OPCOL,OPLEN,ZHLLIN,ZHLCOL,ZHLLEN
C
C     + + + ARGUMENT DEFINTIONS + + +
C     ZCLEN  - number of lines offset to center
C     ZCWID  - number of characters offset to center
C     OPLIN  - line number in screen buffer containing option
C     OPCOL  - column number in screen buffer containing option
C     OPLEN  - length of option and description in text buffer
C     ZHLLIN - line number on screen containing option
C     ZHLCOL - column number on screen where option starts
C     ZHLLEN - length of option and description on screen to highlight
C
C     + + + END SPECIFICATIONS + + +
C
      ZHLLIN= OPLIN+ ZCLEN
      ZHLCOL= OPCOL+ ZCWID
      ZHLLEN= OPLEN
C
      RETURN
      END
