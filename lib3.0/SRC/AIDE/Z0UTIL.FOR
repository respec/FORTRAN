C     z0util.f 2.1 9/4/91
C
C
C
      SUBROUTINE   ZEDT0M
     I                   (SGLCHR,
     O                    IRET)
C
C     + + + PURPOSE + + +
C     perform menu selections for multiple responses
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SGLCHR,IRET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SGLCHR - single character exit flag,
C              0 - stay in this routine until user exits (IRET>0)
C              1 - return to application after each keystroke
C     IRET   - return control code
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C     screen control parameters
      INCLUDE 'cscren.inc'
C     option field parameters
      INCLUDE 'czoptn.inc'
C     hidden field parameters
      INCLUDE 'czhide.inc'
C     callback IDs
      INCLUDE 'cclbak.inc'
C
C     + + + SAVES + + +
      INTEGER   DISPFG,BLNKFG,ISWI,ICHA
      LOGICAL   AT1,CLEARD
      CHARACTER CNONE*4
      SAVE      DISPFG,AT1,CLEARD,CNONE,BLNKFG,ISWI,ICHA
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,K,L,GROUP,CODE,WNDID,LGRP,IOPEN,
     $          IND,ILEN,ITMP,IWRT,LFLD,CFLHID,MROW,MCOL,NFLD
      REAL      LMIN,LMAX
      CHARACTER KEY*1,STRING*78,OLDSTR*78
C
C     + + + FUNCTIONS + + +
      INTEGER    ZLNTXT, FLDHID
C
C     + + + INTRINSICS + + +
      INTRINSIC  CHAR, INDEX, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZLNTXT, FLDHID, ZGTKYD, ZLOCFD, ZWRTMN, ZWRTB3, ZOPFLD
      EXTERNAL   ZVERIF, ZWRVDR, SCCUMV, ZDRWSC,  ZWRSCR, ZCURMV
      EXTERNAL   ZLJUST, ZARCMP, ZBEEP, ZIPI, ZLIMIT, ZFILVF, ZMVMSE
      EXTERNAL   ZCBCHR, ZCBFLD, ZCBEXT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA DISPFG,BLNKFG /0,0/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DISPFG.EQ.0) THEN
        CNONE = 'none'
        AT1   = .TRUE.
        CLEARD= .FALSE.
        ICHA  = 0
C       draw screen box
        CALL ZDRWSC
C       display menu
        CALL ZWRTMN (ZB1F,ZB1N,1,ZMNTXT,
     M               ZMNLEN)
C       set to not display next time through
        DISPFG= 1
      END IF
C
      IF (SGLCHR.EQ.1) THEN
C       display current field
        I= SCOL(CFLD)
        CALL ZWRVDR (ZMNTXT(ZHLLIN)(I:I+FLEN(CFLD)-1),ZHLLIN+1,ZHLCOL+1)
        IF (FTYP(CFLD).NE.FTO .AND. ISWI.EQ.0) THEN
C         position cursor at end of current field (unless option field)
          ZCRCOL= I+ ZLNTXT(ZMNTXT(ZHLLIN)(I:I+FLEN(CFLD)-1))+1+BLNKFG
        END IF
        BLNKFG= 0
      ELSE
C       always re-init changed value flag for regular edit mode
        ICHA = 0
      END IF
C
      ZWN3ID= 0
      ZGP3  = 0
      IRET  = 0
      ZRET  = 0
 100  CONTINUE
        ISWI= 0
        IF (FTYP(CFLD).EQ.FTO) THEN
C         different instruction message for option type field
          LGRP= 95
        ELSE IF (QFLAG.EQ.0) THEN
C         not all fields protected, 'enter data' message
          LGRP = 85
        ELSE
C         all fields protected, 'view data' message
          LGRP= 87
        END IF
        CALL SCCUMV(ZCRLIN,ZCRCOL)
        CALL ZGTKYD(LGRP,CFLD,GROUP,CODE)
        IF (FTYP(CFLD).EQ.FTO) THEN
C         option type field, handle differently than numeric/character fields
C         clear any error messages
          ZERR= 0
          CALL ZOPFLD (GROUP,CODE,IRET)
        ELSE
C         numeric or character field
          IF (GROUP .EQ. 1) THEN
C           normal ascii
            I = ZHLCOL + ZHLLEN
            IF (ZCRCOL .GT. I) THEN
C             too far in field
              CALL ZBEEP
            ELSE
C             save new character
              KEY = CHAR(CODE)
              IF (ICHA.EQ.0) THEN
C               save current string
                I= ZHLCOL+ ZHLLEN- 1
                OLDSTR= ZMNTXT(ZHLLIN)(ZHLCOL:I)
Chnb                CALL ZLJUST (OLDSTR)
C               first edit, clear whats in field
                IF (AT1) THEN
                  ZMNTXT(ZHLLIN)(ZHLCOL:I)= ' '
                  CLEARD = .TRUE.
                END IF
                ICHA= 1
                ZERR= 0
                AT1 = .FALSE.
              END IF
              ZMNTXT(ZHLLIN)(ZCRCOL-1:ZCRCOL-1) = KEY
              IF (KEY.EQ.' ' .AND. SGLCHR.EQ.1) THEN
C               single character mode, indicate blank entered to save it
                BLNKFG= 1
              END IF
C
              IF (FTYP(CFLD) .EQ. FTI .OR.
     +            FTYP(CFLD) .EQ. FTR .OR.
     +            FTYP(CFLD) .EQ. FTD) THEN
                 IF ((KEY .GE. '0' .AND. KEY .LE. '9') .AND.
     +               .NOT. CLEARD) THEN
                    DO 130, I = ZCRCOL,(ZHLCOL+ZHLLEN-1)
                       IF (ZMNTXT(ZHLLIN)(I:I) .EQ. ' ') THEN
                          ZMNTXT(ZHLLIN)(I:I) = '0'
                       ELSE
                          GO TO 135
                       END IF
 130                CONTINUE
 135                CONTINUE
                 END IF
              END IF
C
              I= SCOL(CFLD)+ FLEN(CFLD)- 1
              CALL ZWRVDR (ZMNTXT(ZHLLIN)(ZCRCOL-1:I),ZCRLIN,ZCRCOL)
              ZMNLEN(ZHLLIN) = ZLNTXT(ZMNTXT(ZHLLIN))
              ZCRCOL = ZCRCOL + 1
            END IF
          ELSE IF (ZRET.EQ.1) THEN
C           trying to exit, make sure current field value is checked
            ISWI= 1
          ELSE IF (GROUP.EQ.2 .OR. GROUP.EQ.3) THEN
C           special character or cursor movement keys
            CALL ZCURMV (GROUP,CODE,ZHLCOL,ZHLLEN,ZCRLIN,
     M                   ZCRCOL,ISWI,ICHA,ZMNTXT(ZHLLIN),ZMNLEN(ZHLLIN))
            IF ((ZCRCOL-1) .NE. ZHLCOL) AT1 = .FALSE.
          ELSE IF (GROUP.EQ.7) THEN
C           mouse movement - what is new field
            MROW= CODE/1000
            MCOL= MOD(CODE,1000)
            CALL ZMVMSE(MROW,MCOL,
     O                  NFLD)
            IF (NFLD .GT. 0) THEN
              ISWI= 1
              CODE= -NFLD
            END IF
          END IF
C         make character response event callback
          CALL ZCBCHR (CBCHID,CFLD,
     O                 ZERR)
          IF (ISWI.EQ.1 .AND. ZERR.NE.2) THEN
C           switch fields
            ZERR= 0
            AT1    = .TRUE.
            CLEARD = .FALSE.
            IF (ICHA.EQ.1) THEN
C             changed current value, check new one
              IWRT= 1
              I = SCOL(CFLD) + FLEN(CFLD) - 1
              STRING= ZMNTXT(ZHLLIN)(ZHLCOL:I)
Chnb              CALL ZLJUST(STRING)
              IND= APOS(CFLD)
              IF (FTYP(CFLD).EQ.'I') THEN
C               put integer values in reals
                LMIN= IMIN(IND)
                LMAX= IMAX(IND)
              ELSE IF (FTYP(CFLD).EQ.'R') THEN
C               put real field ranges in local min/max/def
                LMIN= RMIN(IND)
                LMAX= RMAX(IND)
              ELSE IF (FTYP(CFLD).EQ.'D') THEN
C               put double precision values in reals
                LMIN= DMIN(IND)
                LMAX= DMAX(IND)
              END IF
              IF (FTYP(CFLD).EQ.FTI .OR. FTYP(CFLD).EQ.FTR .OR.
     $            FTYP(CFLD).EQ.FTD) THEN
C               may have arithmetic expression
Chnb
                CALL ZLJUST(STRING)
                CALL ZLJUST (OLDSTR)
Chnb
                CALL ZARCMP (FTYP(CFLD),FLEN(CFLD),OLDSTR,
     M                       STRING)
              END IF
              IF (FTYP(CFLD).EQ.FTF) THEN
C               file type field, check file validity
                IOPEN= 0
                CALL ZFILVF (CFLD,IOPEN,
     M                       STRING,ZERR)
              ELSE
C               numeric or character field, verify validity
                CALL ZVERIF (FTYP(CFLD),LMIN,LMAX,
     I                       FDVAL(CFLD),FDINV(CFLD),FLEN(CFLD),IWRT,
     M                       STRING,ZERR)
              END IF
C             ok to rewrite menu text with new value
              I= SCOL(CFLD)+ FLEN(CFLD)- 1
              IF (FTYP(CFLD).EQ.'C' .OR. FTYP(CFLD).EQ.'F') THEN
C               character field, left justify
                ZMNTXT(ZHLLIN)(SCOL(CFLD):I)= STRING
              ELSE
C               numeric field, right justify, clear field first
                ZMNTXT(ZHLLIN)(SCOL(CFLD):I)= ' '
                IF (ZLNTXT(STRING).GT.0) THEN
C                 something entered in field (otherwise leave field blank)
                  J= I- ZLNTXT(STRING)+ 1
                  ZMNTXT(ZHLLIN)(J:I)= STRING
                END IF
              END IF
              IF (ZERR.NE.0 .AND. FPROT(CFLD).EQ.1) THEN
C               field must be correct and it isn't
                ZERR= 2
              END IF
C             make field exit event callback
              CALL ZCBFLD (CBFLID,CFLD,
     O                     ZERR)
              CALL ZWRVDR (ZMNTXT(ZHLLIN)(ZHLCOL:I),ZHLLIN+1,ZHLCOL+1)
              IF (ZERR.NE.0) THEN
C               move back to start of this field
                CODE= 0
C               dont exit
                ZRET= 0
              END IF
C             turn change flag off
              ICHA= 0
            END IF
            IF (ZRET.EQ.0) THEN
C             locate next field(or start of this one)
              CALL ZLOCFD(CODE)
            END IF
          END IF
          IF (ZWN2ID.EQ.8 .AND. ZRET.EQ.0) THEN
C           limits currently displayed, make call to check for matches
            CALL ZLIMIT
          END IF
        END IF
        IF (ZRET.NE.0) THEN
C         user wants out
          IF (ZRET.EQ.1) THEN
C           next screen, everything filled in here?
            IWRT= 0
            I= 0
 380        CONTINUE
              I= I+ 1
C             check to see if current field is hidden
              CFLHID= FLDHID(NUMHID,HIDFLD,HIDFLG,I)
              IF (FTYP(I).EQ.FTO .AND. CFLHID.EQ.0) THEN
C               option field, check number of options selected for this set
                IF (OPSTNO(I).EQ.1 .AND. OPSET(I).GT.0) THEN
C                 first field of set, check this set for min selected
                  IF (CURSEL(OPSET(I)).LT.OPMNSL(OPSET(I))) THEN
C                   too few options selected for this set
                    WNDID= 13
                    LGRP = 97
                    CALL ZWRTB3 (WNDID,LGRP)
C                   move to this field, save current field
                    LFLD= CFLD
                    CALL ZLOCFD (-I)
                    IF (OPBOX(LFLD).GT.0) THEN
C                     moved out of this field,
C                     but box next to it needs highlighting
                      CALL ZWRVDR (ZMNTX1(SCOL(LFLD),FLIN(LFLD)),
     I                             FLIN(LFLD)+1,SCOL(LFLD)+1)
                    END IF
C                   dont exit yet
                    I= NFLDS+ 1
                  END IF
                END IF
              ELSE IF (CFLHID.EQ.0) THEN
C               character or numeric, check value in field
                J= FLIN(I)
                K= SCOL(I)+ FLEN(I)- 1
                IF (FLEN(I).GT.4) THEN
C                 use all characters of 'none' string
                  L= 4
                ELSE
C                 use only FLEN characters of 'none' string
                  L= FLEN(I)
                END IF
                ITMP= INDEX (ZMNTXT(J)(SCOL(I):K),CNONE(1:L))
                ILEN= ZLNTXT(ZMNTXT(J)(SCOL(I):K))
                IF ((ITMP.GT.0 .OR. ILEN.EQ.0) .AND.
     1                           FPROT(I).EQ.1) THEN
C                 no value (or 'none') in this field and we wont allow it
                  WNDID= 13
                  LGRP = 76
                  CALL ZWRTB3 (WNDID,LGRP)
                  CALL ZLOCFD (-I)
                  I= NFLDS+ 1
                END IF
                IF (I.LE.NFLDS) THEN
C                 not a null value, verify its a good one
                  IND= APOS(I)
                  IF (FTYP(I).EQ.'I') THEN
C                   put integer values in reals
                    LMIN= IMIN(IND)
                    LMAX= IMAX(IND)
                  ELSE IF (FTYP(I).EQ.'R') THEN
C                   put real field ranges in local min/max/def
                    LMIN= RMIN(IND)
                    LMAX= RMAX(IND)
                  ELSE IF (FTYP(I).EQ.'D') THEN
C                   put double precision values in reals
                    LMIN= DMIN(IND)
                    LMAX= DMAX(IND)
                  END IF
                  STRING= ZMNTXT(J)(SCOL(I):K)
                  IF (FTYP(I).EQ.'F') THEN
C                   file type field, check file validity
                    IOPEN= 0
                    CALL ZFILVF (I,IOPEN,
     M                           STRING,ZERR)
                  ELSE
C                   numeric or character field, verify validity
                    CALL ZVERIF (FTYP(I),LMIN,LMAX,
     I                           FDVAL(I),FDINV(I),FLEN(I),IWRT,
     M                           STRING,ZERR)
                  END IF
                  IF (ZERR.NE.0 .AND.
     $                (FPROT(I).EQ.1 .OR. FTYP(I).EQ.'F')) THEN
C                   field must be correct and it isn't
                    IF (FTYP(I).NE.'F') THEN
C                     display message for numeric/character field
                      WNDID= 13
                      LGRP = 75
                      CALL ZWRTB3 (WNDID,LGRP)
                    END IF
C                   move to problem field
                    CALL ZLOCFD (-I)
                    I= NFLDS+ 1
                  END IF
                END IF
              END IF
            IF (I.LT.NFLDS) GO TO 380
            IF (I.EQ.NFLDS) THEN
C             all data present, may need to open files
              I= 0
 400          CONTINUE
C               check for any file type fields
                I= I+ 1
                IF (FTYP(I).EQ.'F' .AND.
     $              FLDHID(NUMHID,HIDFLD,HIDFLG,I).EQ.0) THEN
C                 open file by name in this field
                  K= SCOL(I)+ FLEN(I)- 1
                  STRING= ZMNTXT(FLIN(I))(SCOL(I):K)
                  IOPEN= 1
                  ZERR = 0
                  CALL ZFILVF (I,IOPEN,
     O                         STRING,ZERR)
                  IF (ZERR.NE.0) THEN
C                   couldn't open file
                    ZRET= 0
                    I= NFLDS
                  END IF
                END IF
              IF (I.LT.NFLDS) GO TO 400
              IF (ZRET.NE.0) THEN
C               make screen exit event callback
                CALL ZCBEXT (CBEXID,
     O                       ZERR)
              END IF
              IF (ZERR.EQ.0) THEN
                IRET = ZRET
              END IF
            ELSE
C             missing or bad data
              ZRET= 0
            END IF
          ELSE
C           other exits
            IRET= ZRET
          END IF
        END IF
      IF (IRET .EQ. 0 .AND. (SGLCHR.EQ.0 .OR. GROUP.GE.3)) GO TO 100
C
      IF (IRET.NE.0) THEN
C       user leaving screen for good, clear out highlignt
        I = ZHLCOL + ZHLLEN - 1
        CALL ZWRSCR(ZMNTXT(ZHLLIN)(ZHLCOL:I),ZHLLIN+1,ZHLCOL+1)
C       now no active highlight
        ZHLLIN= 0
C       set dont save menu
        ZMNSAV= 0
C       reset protection, box flags (option type fields), and hidden fields
        I= 0
        CALL ZIPI (NFLDS,I,FPROT)
        CALL ZIPI (NFLDS,I,OPBOX)
        NUMHID= 0
C       set flag to display screen next time through
        DISPFG= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZARCMP
     I                   (FTYP,FLEN,OLDSTR,
     M                    STRING)
C
C     + + + PURPOSE + + +
C     arithmetic compiler
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     FLEN
      CHARACTER   FTYP,OLDSTR*(*),STRING*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FTYP   - field type
C     FLEN   - field length
C     OLDSTR - current string in field
C     STRING - new string in field
C
C     + + + COMMON BLOCKS + + +
C     numeric constants
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K,L,JUST,ITMP,IARCMP
      DOUBLE PRECISION DVAL,DTMP,DTMP2
      REAL         RVAL
Chnb      CHARACTER*1  CM,CS,CP,CD,CT,CE,CEXP,CI,STRIN1(20),LT
      CHARACTER*1  CM,CS,CP,CD,CT,CE,CEXP,CI,LT,STRIN1(78)
      CHARACTER*156 TSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TSTR,TARR)
      CHARACTER     TARR(156)
C
C     + + + SAVES + + +
      SAVE   IARCMP
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
      DOUBLE PRECISION CHRDPR
C
C     + + + INTRINSICS + + +
      INTRINSIC    INT, ABS, CHAR, ICHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZLNTXT, CHRDPR, DECCHR, DPRCHR, INTCHR
      EXTERNAL     ANPRGT, CARVAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA IARCMP/0/
      DATA CM,CS,CP,CD/'*','-','+','/'/
C
C     + + + INPUT FORMATS + + +
Chnb 1000  FORMAT (D10.0)
C
C     + + + OUTPUT FORMATS + + +
Chnb 2000  FORMAT (20A1)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IARCMP .EQ. 0) THEN
C       first time thru, need compiler delimeter
        I= 10
        CALL ANPRGT(I,IARCMP)
      END IF
C
Chnb      JUST= 0
      DVAL= 0.0
      CE  = CHAR(94)
      CI  = CHAR(92)
C
      CT  = STRING(1:1)
      IF (ICHAR(CT) .NE. IARCMP) THEN
C       NO need to do arithmatic compile, no action required
      ELSE
C       get rid of the arith compiler spec string
        J = ZLNTXT(STRING)
        TSTR  = STRING(2:J)
        STRING= TSTR
        CT= STRING(1:1)
C       do we need the current string
        IF (CT.EQ.CM .OR. CT.EQ.CS .OR. CT.EQ.CP .OR. CT.EQ.CD .OR.
     1      CT.EQ.CE) THEN
C         we need it
          J= ZLNTXT(STRING)
          L= ZLNTXT(OLDSTR)
          TSTR= OLDSTR(1:L)//STRING(1:J)
          STRING= TSTR
        END IF
C
        CEXP= ' '
        L= ZLNTXT(STRING)+ 1
        J= 1
        I= 0
 10     CONTINUE
          I = I+ 1
          CT= STRING(I:I)
          IF (CT.EQ.CM .OR. CT.EQ.CS .OR. CT.EQ.CP .OR. CT.EQ.CD .OR.
     1        CT.EQ.CE .OR. CT.EQ.CI .OR. I.EQ.L) THEN
            IF (LT.NE.'E' .AND. LT.NE.'e') THEN
C             not a number in exponential format, ok to continue
              IF (I.GT.J) THEN
C               get value of string just passed
Chnb                READ (STRING(J:I-1),1000,ERR=80) DTMP
                K = I - J
                TSTR = STRING(J:I-1)
                DTMP = CHRDPR(K,TARR)
                IF (DTMP .LE. -D0MAX) GO TO 80
                IF (DTMP.LE.D0MIN) THEN
C                 value is 0, don't check for NaN
                  DTMP2= 0.0
                ELSE
C                 check value for NaN
                  DTMP2= ABS((DTMP/DTMP)-1.0)
                END IF
                IF (DTMP2.LE.D0MIN) THEN
C                 DTMP value ok, need to do arithmetic expression
                  IF (CEXP.EQ.' ') THEN
C                   no current expression
                    DVAL= DTMP
                  ELSE IF (CEXP.EQ.CP) THEN
C                   add
Chnb
                    IF (DTMP .GT. 0.0D0) THEN
                      IF ((D0MAX-DTMP) .LT. DVAL) GOTO 80
                    ELSE
                      IF ((-D0MAX-DTMP) .GT. DVAL) GOTO 80
                    END IF
Chnb
                    DVAL= DVAL+ DTMP
                  ELSE IF (CEXP.EQ.CS) THEN
C                   subtract
Chnb
                    IF (DTMP .GT. 0.0D0) THEN
                      IF ((-D0MAX+DTMP) .GT. DVAL) GOTO 80
                    ELSE
                      IF ((D0MAX+DTMP) .LT. DVAL) GOTO 80
                    END IF
Chnb
                    DVAL= DVAL- DTMP
                  ELSE IF (CEXP.EQ.CM) THEN
C                   multiply
                    DVAL= DVAL* DTMP
                  ELSE IF (CEXP.EQ.CD) THEN
C                   divide
                    IF (ABS(DTMP).GT.D0MIN) THEN
                      DVAL= DVAL/DTMP
                    ELSE
C                     divide by 0
                      DVAL= 0.0
                    END IF
                  ELSE IF (CEXP.EQ.CE) THEN
C                   exponential
                    DVAL= DVAL** DTMP
                  END IF
                END IF
              END IF
C             save current expression
              CEXP= CT
              J   = I+ 1
              IF (STRING(J:J).EQ.CM) THEN
C               next expression is exponiential (**)
                CEXP= CE
                J   = J+ 1
                I   = I+ 1
              END IF
            END IF
          END IF
C         save current character
          LT= CT
        IF (I.LT.L) GO TO 10
C
        IF (J.GT.1) THEN
C         new string
Chnb
          JUST = 1
Chnb
          IF (FTYP.EQ.'I') THEN
C           integer
            ITMP= INT(DVAL*DP1MIN)
            CALL INTCHR (ITMP,FLEN,JUST,
     O                   J,STRIN1)
          ELSE IF (FTYP.EQ.'R') THEN
C           real
            RVAL= DVAL
            CALL DECCHR (RVAL,FLEN,JUST,
     J                   J,STRIN1)
          ELSE IF (FTYP.EQ.'D') THEN
C           double precision
            CALL DPRCHR (DVAL,FLEN,JUST,
     O                   J,STRIN1)
          END IF
          CALL CARVAR (FLEN,STRIN1,FLEN,STRING)
          STRING(FLEN+1:) = ' '
Chnb          WRITE (STRING,2000,ERR=80) (STRIN1(I),I=1,FLEN)
Chnb          CALL ZLJUST(STRING)
        END IF
C
        GO TO 90
 80     CONTINUE
C         error on converting to number, use old string
          STRING= OLDSTR
 90     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZLOCFD
     I                   (CODE)
C
C     + + + PURPOSE + + +
C     locate the neighbor field according to the direction of
C     the cursor movement (arrow key interrupt)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CODE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CODE   - direction code: =1  up
C                              =2  down
C                              =3  right
C                              =4  left
C                              =13 carriage return
C                              <0  absolute field number
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C     screen control parameters
      INCLUDE 'cscren.inc'
C     option field parameters
      INCLUDE 'czoptn.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,L,M,MINH,MINV,MOVE,QHLP,WNDID,PRMIND,PRMVAL,
     1           ILEN,COL
C
C     + + + FUNCTIONS + + +
      INTEGER    ZHIDCK
C
C     + + + INTRINSICS + + +
      INTRINSIC  IABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZHIDCK, ZWRSCR, ZWRVDR, SCCUMV, ZLIMIT, ZWRHLP, ANPRGT
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CODE.GT.0) THEN
C       search for the closest neighbor
        IF (CODE .EQ. 1) THEN
C         up
          MINV= 1000
          J   = CFLD
          DO 100 I = 1, NFLDS
            IF (I .NE. CFLD .AND. ZHIDCK(I).EQ.0) THEN
C             check each field except current one
              IF (FLIN(I) .LT. FLIN(CFLD)) THEN
C               go up
                L = FLIN(CFLD) - FLIN(I)
              ELSE
C               go to bottom
                L = FLIN(CFLD) - FLIN(I) + 14
              END IF
              IF (L .LT. MINV) THEN
C               new closest
                J   = I
                MINV= L
                MINH= IABS(SCOL(CFLD) - SCOL(I))
              ELSE IF (L .EQ. MINV) THEN
C               might be close, check column
                M = IABS(SCOL(CFLD) - SCOL(I))
                IF (M .LT. MINH) THEN
C                 new closest
                  J   = I
                  MINV= L
                  MINH= IABS(SCOL(CFLD) - SCOL(I))
                END IF
              END IF
            END IF
 100      CONTINUE
          IF (J .EQ. CFLD) THEN
C           dont change fields
            MOVE= 0
          ELSE
C           set up to change
            MOVE= 2
            CFLD= J
          END IF
        ELSE IF (CODE .EQ. 2) THEN
C         down
          MINV= 1000
          J   = CFLD
          DO 200 I = 1, NFLDS
            IF (I .NE. CFLD .AND. ZHIDCK(I).EQ.0) THEN
C           check each field except current one
              IF (FLIN(I) .GT. FLIN(CFLD)) THEN
C               go down
                L = FLIN(I) - FLIN(CFLD)
              ELSE
C               go to top
                L = FLIN(I) - FLIN(CFLD) + 14
              END IF
              IF (L .LT. MINV) THEN
C               new closest
                J   = I
                MINV= L
                MINH= IABS(SCOL(CFLD) - SCOL(I))
              ELSE IF (L .EQ. MINV) THEN
C               might be close, check columns
                M = IABS(SCOL(CFLD) - SCOL(I))
                IF (M .LT. MINH) THEN
C                 new closest
                  J   = I
                  MINV= L
                  MINH= IABS(SCOL(CFLD) - SCOL(I))
                END IF
              END IF
            END IF
 200      CONTINUE
          IF (J .EQ. CFLD) THEN
C           dont change fields
            MOVE= 0
          ELSE
C           set up to change
            CFLD= J
            MOVE= 2
          END IF
        ELSE IF (CODE .EQ. 3) THEN
C         right
          J   = CFLD
          MINH= 1000
          DO 300 I = 1, NFLDS
C           check each field
            IF (I .NE. CFLD .AND. ZHIDCK(I).EQ.0) THEN
C             dont check current field
              IF (FLIN(I) .EQ. FLIN(CFLD)) THEN
C               same menu record
                IF (SCOL(I) .GE. SCOL(CFLD)) THEN
C                 new is to right
                  L = SCOL(I) - SCOL(CFLD)
                ELSE
C                 new is to left
                  L = SCOL(I) - SCOL(CFLD) + 78
                END IF
                IF (L .LT. MINH) THEN
C                 new closest
                  MINH= L
                  J   = I
                END IF
              END IF
            END IF
 300      CONTINUE
          IF (J .EQ. CFLD) THEN
C           stay in current field, just move cursor
            MOVE= 1
          ELSE
C           move to new field
            MOVE= 2
            CFLD= J
          END IF
        ELSE IF (CODE .EQ. 4) THEN
C         left
          J   = CFLD
          MINH= 1000
          DO 400 I = 1, NFLDS
C           check each field
            IF (I .NE. CFLD .AND. ZHIDCK(I).EQ.0) THEN
C             dont check current field
              IF (FLIN(I) .EQ. FLIN(CFLD)) THEN
C               same menu record
                IF (SCOL(I) .LE. SCOL(CFLD)) THEN
C                 new is to left
                  L = SCOL(CFLD) - SCOL(I)
                ELSE
C                 new is to right
                  L = SCOL(CFLD) - SCOL(I) + 78
                END IF
                IF (L .LT. MINH) THEN
C                 new closest
                  MINH= L
                  J   = I
                END IF
              END IF
            END IF
 400      CONTINUE
          IF (J .EQ. CFLD) THEN
C           stay in current field, just move cursor
            MOVE= 1
          ELSE
C           move to new field
            MOVE= 2
            CFLD= J
          END IF
        ELSE
C         carriage return, always change fields
 500      CONTINUE
C           continue looking for next unprotected field
            MOVE= 2
            IF (CFLD .GE. NFLDS) THEN
C             go back to first field
              CFLD= 1
CPRH              IF (NFLDS.EQ.1) THEN
C               see if user has carriage return acting as Next command
                PRMIND= 9
                CALL ANPRGT (PRMIND,PRMVAL)
                IF (PRMVAL.EQ.3) THEN
C                 yes, they do
                  ZRET= 1
                  MOVE= 0
                END IF
CPRH              END IF
            ELSE
C             go to next field
              CFLD= CFLD + 1
            END IF
          IF ((FPROT(CFLD).EQ.2 .AND. QFLAG.EQ.0)
     1                        .OR. ZHIDCK(CFLD).EQ.1) GO TO 500
        END IF
      ELSE IF (CODE.LT.0) THEN
C       move to absolute field
        MOVE= 2
        CFLD= -CODE
      ELSE IF (CODE.EQ.0) THEN
C       move to start of this field
        MOVE= 1
      END IF
C
      IF (MOVE .EQ. 2) THEN
C       move highlight and cursor, first turn off old
        I = ZHLCOL + ZHLLEN - 1
        CALL ZWRSCR(ZMNTXT(ZHLLIN)(ZHLCOL:I),ZHLLIN+1,ZHLCOL+1)
        ZHLCOL = SCOL(CFLD)
        ZHLLEN = FLEN(CFLD)
        ZHLLIN = FLIN(CFLD)
        IF (OPBOX(CFLD).GT.0) THEN
C         1st position already inverse videoed, dont duplicate
          COL = ZHLCOL+ 1
          ILEN= ZHLLEN- 1
        ELSE
C         highlight whole field
          COL = ZHLCOL
          ILEN= ZHLLEN
        END IF
        IF (ILEN.GT.0) THEN
C         some more of field to highlight
          I = COL + ILEN - 1
          CALL ZWRVDR (ZMNTXT(ZHLLIN)(COL:I),ZHLLIN+1,COL+1)
        END IF
C       adjust cursor position here as it is used in ZLIMIT
        ZCRLIN = ZHLLIN + 1
        ZCRCOL = ZHLCOL + 1
C       display new limits or help?
        IF (ZWN2ID.EQ.8) THEN
C         yes, show new field's limits
          CALL ZLIMIT
        ELSE IF (ZWN2ID.EQ.7) THEN
C         show new help
          QHLP = 1
          WNDID= 7
          CALL ZWRHLP (ZMESFL,GPTR,HPTR(CFLD),WNDID,QHLP)
        END IF
      ELSE IF (MOVE.EQ.1) THEN
C       just make cursor position adjustment
        ZCRLIN = ZHLLIN + 1
        ZCRCOL = ZHLCOL + 1
      END IF
C
      IF (MOVE .GE. 1) THEN
C       move cursor to correct position
        CALL SCCUMV(ZCRLIN,ZCRCOL)
      END IF
C
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZOPFLD (GROUP,CODE,
     O                     IRET)
C
C     + + + PURPOSE + + +
C     Handle all actions related to an option type data field.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GROUP,CODE,IRET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GROUP  - specifies type of keyboard input
C     CODE   - code specifying which key used for group type
C     IRET   - return control code
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'czoptn.inc'
      INCLUDE 'czhide.inc'
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
      INCLUDE 'cclbak.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,LOFTFL(4),LOFTVL(4),LFLD,LSET,LPOS,LGRP,ITOG,
     $            ONPTR,ONCNT,OFFPTR,OFFCNT,IERR,LMXSEL,BOXFG,WNDID,
     $            LMXFLD,LMXSCB,MROW,MCOL,ISWI,NFLD
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZLOCFD, WOPWDS, ZOPON, ZOPOFF, ZHIDFD,ZMVMSE
      EXTERNAL    ZWRSCR, ZWRVDR, ZWRTB3, ZCBCHR, ZCBFLD
C
C     + + + END SPECIFICATIONS + + +
C
      IRET= 0
      IERR= 0
C     set local version of max fields to remove
C     any chance of parameter MXFLD being modified
      LMXFLD= MXFLD
C     set local version of max screen buffer to remove
C     any chance of parameter MXSCBF being modified
      LMXSCB= MXSCBF
C
      IF (GROUP.EQ.1) THEN
C       ASCII character, treat as a toggle
        IF (ZMNTX1(SCOL(CFLD),FLIN(CFLD)).EQ.' ') THEN
C         currently option is off, try to switch to on
C         any conditionals for this field being on
          CALL WOPWDS (OPONOF(CFLD),
     O                 ONPTR,ONCNT,OFFPTR,OFFCNT)
          IF (ONCNT.GT.0) THEN
C           conditions exist for this field being on
            J= 0
            DO 20 I= 1,ONCNT
C             try to adjust for each condition
              ITOG= -1
              IF (MOD(I,4).EQ.1) THEN
C               get next four conditions out of array words
                CALL WOPWDS (ONFTFL(ONPTR+J),
     O                       LOFTFL(1),LOFTFL(2),
     O                       LOFTFL(3),LOFTFL(4))
                CALL WOPWDS (ONFTVL(ONPTR+J),
     O                       LOFTVL(1),LOFTVL(2),
     O                       LOFTVL(3),LOFTVL(4))
C               increment pointer position within array
                J= J+ 1
              END IF
              LPOS= MOD(I,4)
              IF (LPOS.EQ.0) LPOS = 4
              LFLD= LOFTFL(LPOS)
              LSET= OPSET(LFLD)
              IF (OPBOX(LOFTFL(LPOS)).GT.0) THEN
C               this field needs a box highlighted next to it
                BOXFG= 1
              ELSE
                BOXFG= 0
              END IF
              IF (LOFTVL(LPOS).EQ.0 .AND.
     1            ZMNTX1(SCOL(LFLD),FLIN(LFLD)).EQ.'X') THEN
C               field is on and needs to be toggled off
                IF (LSET.GT.0) THEN
C                 valid set, toggle off and make adjustments for set
                  CALL ZOPOFF (LSET,OPSTNO(LFLD),OPMXSL,LMXFLD,
     I                         FLIN(LFLD)+1,SCOL(LFLD)+1,BOXFG,
     M                         CURSEL(LSET),OPSVAL,
     M                         ZMNTX1(SCOL(LFLD),FLIN(LFLD)))
                ELSE
C                 All field, just toggle it off
                  ZMNTX1(SCOL(LFLD),FLIN(LFLD))= ' '
                  IF (BOXFG.EQ.1) THEN
                    CALL ZWRVDR (ZMNTX1(SCOL(LFLD),FLIN(LFLD)),
     I                           FLIN(LFLD)+1,SCOL(LFLD)+1)
                  ELSE
                    CALL ZWRSCR (ZMNTX1(SCOL(LFLD),FLIN(LFLD)),
     I                           FLIN(LFLD)+1,SCOL(LFLD)+1)
                  END IF
                END IF
                ITOG= 0
              ELSE IF (LOFTVL(LPOS).EQ.1 .AND.
     1                 ZMNTX1(SCOL(LFLD),FLIN(LFLD)).EQ.' ') THEN
C               field is off and needs to be toggled on
                IF (CURSEL(LSET).LT.OPMXSL(LSET)) THEN
C                 ok to toggle on
                  IF (LSET.GT.0) THEN
C                   valid set, toggle on and make adjustments for set
                    CALL ZOPON (LSET,OPSTNO(LFLD),OPMXSL,LMXFLD,
     I                          FLIN(LFLD)+1,SCOL(LFLD)+1,BOXFG,
     M                          CURSEL(LSET),OPSVAL,
     M                          ZMNTX1(SCOL(LFLD),FLIN(LFLD)))
                  ELSE
C                   All field, just toggle it on
                    ZMNTX1(SCOL(LFLD),FLIN(LFLD))= 'X'
                    IF (BOXFG.EQ.1) THEN
                      CALL ZWRVDR (ZMNTX1(SCOL(LFLD),FLIN(LFLD)),
     I                             FLIN(LFLD)+1,SCOL(LFLD)+1)
                    ELSE
                      CALL ZWRSCR (ZMNTX1(SCOL(LFLD),FLIN(LFLD)),
     I                             FLIN(LFLD)+1,SCOL(LFLD)+1)
                    END IF
                  END IF
                  ITOG= 1
                ELSE
C                 no more room to toggle on, show in instruction window
                  WNDID= 13
                  LGRP = 96
                  CALL ZWRTB3 (WNDID,LGRP)
                END IF
              END IF
              IF (NUMHID.GT.0 .AND. ITOG.GE.0) THEN
C               field toggled, check hidden field information
                CALL ZHIDFD (NUMHID,HIDTFL,HIDVAL,HIDFLD,HIDBOX,
     I                       LMXSCB,LMXFLD,ZMNTXT,OPBOX,ITOG,LFLD,
     M                       HIDFLG)
              END IF
 20         CONTINUE
          END IF
          IF (OPSET(CFLD).GT.0) THEN
C           see how many options may be selected for this set
            LMXSEL= OPMXSL(OPSET(CFLD))
          ELSE
C           set number is 0, must be an ALL field
            LMXSEL= 0
          END IF
C         finally try to toggle on current field
          IF (LMXSEL.EQ.1) THEN
C           only one option allowed for this set, turn others off
            DO 50 I= 1,NFLDS
              IF (OPSET(I).EQ.OPSET(CFLD)) THEN
C               turn it off
                IF (OPBOX(I).GT.0) THEN
C                 this field needs a box highlighted next to it
                  BOXFG= 1
                ELSE
                  BOXFG= 0
                END IF
                CALL ZOPOFF (OPSET(I),OPSTNO(I),OPMXSL,LMXFLD,
     I                       FLIN(I)+1,SCOL(I)+1,BOXFG,
     M                       CURSEL(OPSET(I)),OPSVAL,
     M                       ZMNTX1(SCOL(I),FLIN(I)))
                IF (NUMHID.GT.0) THEN
C                 check hidden field information
                  ITOG= 0
                  CALL ZHIDFD (NUMHID,HIDTFL,HIDVAL,HIDFLD,HIDBOX,
     I                         LMXSCB,LMXFLD,ZMNTXT,OPBOX,ITOG,I,
     M                         HIDFLG)
                END IF
              END IF
 50         CONTINUE
C           now turn on the field the user selected
            LSET= OPSET(CFLD)
            CALL ZOPON (LSET,OPSTNO(CFLD),OPMXSL,LMXFLD,
     I                  FLIN(CFLD)+1,SCOL(CFLD)+1,BOXFG,
     M                  CURSEL(LSET),OPSVAL,
     M                  ZMNTX1(SCOL(CFLD),FLIN(CFLD)))
C           current number selected will always be 1
            CURSEL(LSET)= 1
            IF (NUMHID.GT.0) THEN
C             check hidden field information
              ITOG= 1
              CALL ZHIDFD (NUMHID,HIDTFL,HIDVAL,HIDFLD,HIDBOX,
     I                     LMXSCB,LMXFLD,ZMNTXT,OPBOX,ITOG,CFLD,
     M                     HIDFLG)
            END IF
C           need to highlight current field
            CALL ZWRVDR (ZMNTX1(SCOL(CFLD),FLIN(CFLD)),
     I                   FLIN(CFLD)+1,SCOL(CFLD)+1)
          ELSE
C           multiple selections allowed, need to check conditionals
            IF (IERR.EQ.0) THEN
C             ok so far
              IF (OPSET(CFLD).GT.0) THEN
C               see if theres room in this set for another option to be on
                IF (CURSEL(OPSET(CFLD)).LT.OPMXSL(OPSET(CFLD))) THEN
C                 ok to toggle on
                  CALL ZOPON (OPSET(CFLD),OPSTNO(CFLD),OPMXSL,LMXFLD,
     I                        FLIN(CFLD)+1,SCOL(CFLD)+1,BOXFG,
     M                        CURSEL(OPSET(CFLD)),OPSVAL,
     M                        ZMNTX1(SCOL(CFLD),FLIN(CFLD)))
                  IF (NUMHID.GT.0) THEN
C                   field toggled, check hidden field information
                    ITOG= 1
                    CALL ZHIDFD (NUMHID,HIDTFL,HIDVAL,HIDFLD,HIDBOX,
     I                           LMXSCB,LMXFLD,ZMNTXT,OPBOX,ITOG,CFLD,
     M                           HIDFLG)
                  END IF
                ELSE
C                 no more room to toggle on, show in instruction window
                  WNDID= 13
                  LGRP = 96
                  CALL ZWRTB3 (WNDID,LGRP)
                END IF
              ELSE
C               All field, just indicate in screen text
                ZMNTX1(SCOL(CFLD),FLIN(CFLD))= 'X'
              END IF
C             need to highlight current field
              CALL ZWRVDR (ZMNTX1(SCOL(CFLD),FLIN(CFLD)),
     I                     FLIN(CFLD)+1,SCOL(CFLD)+1)
            END IF
          END IF
        ELSE
C         currently option is on, try to switch to off
          IF (OPSET(CFLD).GT.0) THEN
C           start by toggling off current field
            CALL ZOPOFF (OPSET(CFLD),OPSTNO(CFLD),OPMXSL,LMXFLD,
     I                   FLIN(CFLD)+1,SCOL(CFLD)+1,BOXFG,
     M                   CURSEL(OPSET(CFLD)),OPSVAL,
     M                   ZMNTX1(SCOL(CFLD),FLIN(CFLD)))
          ELSE
C           All field, just turn off on screen
            ZMNTX1(SCOL(CFLD),FLIN(CFLD))= ' '
          END IF
C         need to highlight current field
          CALL ZWRVDR (ZMNTX1(SCOL(CFLD),FLIN(CFLD)),
     I                 FLIN(CFLD)+1,SCOL(CFLD)+1)
          IF (NUMHID.GT.0) THEN
C           field toggled, check hidden field information
            ITOG= 0
            CALL ZHIDFD (NUMHID,HIDTFL,HIDVAL,HIDFLD,HIDBOX,
     I                   LMXSCB,LMXFLD,ZMNTXT,OPBOX,ITOG,CFLD,
     M                   HIDFLG)
          END IF
C         any conditionals for this field being off
          CALL WOPWDS (OPONOF(CFLD),
     O                 ONPTR,ONCNT,OFFPTR,OFFCNT)
          IF (OFFCNT.GT.0) THEN
C           conditions exist for this field being on
            J= 0
            DO 60 I= 1,OFFCNT
C             try to adjust for each condition
              ITOG= -1
              IF (MOD(I,4).EQ.1) THEN
C               get next four conditions out of array words
                CALL WOPWDS (ONFTFL(OFFPTR+J),
     O                       LOFTFL(1),LOFTFL(2),
     O                       LOFTFL(3),LOFTFL(4))
                CALL WOPWDS (ONFTVL(OFFPTR+J),
     O                       LOFTVL(1),LOFTVL(2),
     O                       LOFTVL(3),LOFTVL(4))
C               increment pointer position within array
                J= J+ 1
              END IF
              LPOS= MOD(I,4)
              IF (LPOS.EQ.0) LPOS = 4
              LFLD= LOFTFL(LPOS)
              LSET= OPSET(LFLD)
              IF (OPBOX(LOFTFL(LPOS)).GT.0) THEN
C               this field needs a box highlighted next to it
                BOXFG= 1
              ELSE
                BOXFG= 0
              END IF
              IF (LOFTVL(LPOS).EQ.0 .AND.
     1            ZMNTX1(SCOL(LFLD),FLIN(LFLD)).EQ.'X') THEN
C               field is on and needs to be toggled off
                IF (LSET.GT.0) THEN
C                 valid set, toggle off and make adjustments for set
                  CALL ZOPOFF (LSET,OPSTNO(LFLD),OPMXSL,LMXFLD,
     I                         FLIN(LFLD)+1,SCOL(LFLD)+1,BOXFG,
     M                         CURSEL(LSET),OPSVAL,
     M                         ZMNTX1(SCOL(LFLD),FLIN(LFLD)))
                ELSE
C                 All field, just toggle it off
                  ZMNTX1(SCOL(LFLD),FLIN(LFLD))= ' '
                  IF (BOXFG.EQ.1) THEN
                    CALL ZWRVDR (ZMNTX1(SCOL(LFLD),FLIN(LFLD)),
     I                           FLIN(LFLD)+1,SCOL(LFLD)+1)
                  ELSE
                    CALL ZWRSCR (ZMNTX1(SCOL(LFLD),FLIN(LFLD)),
     I                           FLIN(LFLD)+1,SCOL(LFLD)+1)
                  END IF
                END IF
                ITOG= 0
              ELSE IF (LOFTVL(LPOS).EQ.1 .AND.
     1                 ZMNTX1(SCOL(LFLD),FLIN(LFLD)).EQ.' ') THEN
C               field is off and needs to be toggled on
                IF (CURSEL(LSET).LT.OPMXSL(LSET)) THEN
C                 ok to toggle on
                  IF (LSET.GT.0) THEN
C                   valid set, toggle on and make adjustments for set
                    CALL ZOPON (LSET,OPSTNO(LFLD),OPMXSL,LMXFLD,
     I                          FLIN(LFLD)+1,SCOL(LFLD)+1,BOXFG,
     M                          CURSEL(LSET),OPSVAL,
     M                          ZMNTX1(SCOL(LFLD),FLIN(LFLD)))
                  ELSE
C                   All field, just toggle it on
                    ZMNTX1(SCOL(LFLD),FLIN(LFLD))= 'X'
                    IF (BOXFG.EQ.1) THEN
                      CALL ZWRVDR (ZMNTX1(SCOL(LFLD),FLIN(LFLD)),
     I                             FLIN(LFLD)+1,SCOL(LFLD)+1)
                    ELSE
                      CALL ZWRSCR (ZMNTX1(SCOL(LFLD),FLIN(LFLD)),
     I                             FLIN(LFLD)+1,SCOL(LFLD)+1)
                    END IF
                  END IF
                  ITOG= 1
                ELSE
C                 no more room to toggle on, show in instruction window
                  WNDID= 13
                  LGRP = 96
                  CALL ZWRTB3 (WNDID,LGRP)
                END IF
              END IF
              IF (NUMHID.GT.0 .AND. ITOG.GE.0) THEN
C               field toggled, check hidden field information
                CALL ZHIDFD (NUMHID,HIDTFL,HIDVAL,HIDFLD,HIDBOX,
     I                       LMXSCB,LMXFLD,ZMNTXT,OPBOX,ITOG,LFLD,
     M                       HIDFLG)
              END IF
 60         CONTINUE
          END IF
        END IF
C       make character event callback
        CALL ZCBCHR (CBCHID,CFLD,
     O               ZERR)
      ELSE IF ((GROUP.EQ.2 .AND. (CODE.EQ.9 .OR. CODE.EQ.13)) .OR.
     1         (GROUP.EQ.3 .AND. (CODE.GE.1 .AND. CODE.LE.4)) .OR.
     2         (GROUP.EQ.7)) THEN
C       carriage return or arrow keys or mouse, change fields
        IF (GROUP.EQ.7) THEN
C         mouse movement - what is new field
          ISWI= 0
          MROW= CODE/1000
          MCOL= MOD(CODE,1000)
          CALL ZMVMSE(MROW,MCOL,
     O                NFLD)
          IF (NFLD .GT. 0) THEN
            ISWI= 1
            CODE= -NFLD
          END IF
        ELSE
C         others mean switch
          ISWI= 1
        END IF
        IF (ISWI .EQ. 1) THEN
C         save original field number in case box needs highlighting for it
          LFLD= CFLD
          CALL ZLOCFD (CODE)
          IF (OPBOX(LFLD).GT.0) THEN
C           moved out of this field, but box next to it needs highlighting
            CALL ZWRVDR (ZMNTX1(SCOL(LFLD),FLIN(LFLD)),
     I                   FLIN(LFLD)+1,SCOL(LFLD)+1)
          END IF
C         make field exit event callback
          CALL ZCBFLD (CBFLID,LFLD,
     O                 ZERR)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZOPOFF
     I                   (LSET,LFLD,MAXSEL,LMXFLD,LINE,COL,BOXFG,
     M                    CURSEL,OPSVAL,SCRTXT)
C
C     + + + PURPOSE + + +
C     Toggle off option type field LFLD and
C     adjust selected options array appropriately.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LSET,LFLD,MAXSEL(LSET),LMXFLD,LINE,COL,BOXFG,
     1            CURSEL,OPSVAL(LMXFLD)
      CHARACTER*1 SCRTXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LSET   - current set number of option fields
C     LFLD   - field number within set to be toggled off
C     MAXSEL - maximum number of options to select for this set
C     LMXFLD - maximum number of data fields
C     LINE   - line number on screen of option being changed
C     COL    - column number on screen of option being changed
C     BOXFG  - flag indicating box to be highlighted next to field
C              0 - no highlighted box, 1 - highlight box
C     CURSEL - current number of options selected for this set
C     OPSVAL - array of option field numbers currently selected
C     SCRTXT - character on screen being changed to blank
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,OFFSET
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZWRSCR, ZWRVDR
C
C     + + + END SPECIFICATIONS + + +
C
C     determine offset within option selected array for this field
      OFFSET= 0
      IF (LSET.GT.1) THEN
C       determine offset within options selected array
        DO 5 I= 1,LSET-1
          OFFSET= OFFSET+ MAXSEL(I)
 5      CONTINUE
      END IF
C
C     adjust number of options selected
      CURSEL= CURSEL- 1
C
C     adjust options selected array
      I= 0
 10   CONTINUE
C       check each value of the option selected array for this set
        I= I+ 1
        IF (OPSVAL(OFFSET+I).EQ.LFLD) THEN
C         option found to toggle off
          OPSVAL(OFFSET+I)= 0
          IF (I.LT.MAXSEL(LSET)) THEN
C           adjust rest of option selected array
            DO 20 J= I,MAXSEL(LSET)-1
              OPSVAL(OFFSET+J)= OPSVAL(OFFSET+J+1)
 20         CONTINUE
            OPSVAL(OFFSET+MAXSEL(LSET))= 0
          END IF
          I= MAXSEL(LSET)
        END IF
      IF (I.LT.MAXSEL(LSET)) GO TO 10
C
C     update screen text
      SCRTXT = ' '
      IF (BOXFG.EQ.1) THEN
C       highlight this character
        CALL ZWRVDR (SCRTXT,LINE,COL)
      ELSE
C       just write text to screen
        CALL ZWRSCR (SCRTXT,LINE,COL)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZOPON
     I                  (LSET,LFLD,MAXSEL,LMXFLD,LINE,COL,BOXFG,
     M                   CURSEL,OPSVAL,SCRTXT)
C
C     + + + PURPOSE + + +
C     Toggle on option type field LFLD and
C     adjust selected options array appropriately.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LSET,LFLD,MAXSEL(LSET),LMXFLD,LINE,COL,BOXFG,
     1            CURSEL,OPSVAL(LMXFLD)
      CHARACTER*1 SCRTXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LSET   - current set number of option fields
C     LFLD   - field number within set to be toggled on
C     MAXSEL - maximum number of options to select for this set
C     LMXFLD - maximum number of data fields
C     LINE   - line number on screen of option being changed
C     COL    - column number on screen of option being changed
C     BOXFG  - flag indicating box to be highlighted next to field
C              0 - no highlighted box, 1 - highlight box
C     CURSEL - current number of options selected for this set
C     OPSVAL - array of option field numbers currently selected
C     SCRTXT - character on screen being changed to blank
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,OFFSET
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZWRSCR, ZWRVDR
C
C     + + + END SPECIFICATIONS + + +
C
C     determine offset within option selected array for this field
      OFFSET= 0
      IF (LSET.GT.1) THEN
C       determine offset within options selected array
        DO 5 I= 1,LSET-1
          OFFSET= OFFSET+ MAXSEL(I)
 5      CONTINUE
      END IF
C
C     adjust number of options selected
      CURSEL= CURSEL+ 1
C
C     adjust options selected array
      I= 0
 10   CONTINUE
C       find appropriate position in array for option toggled on
        I= I+ 1
        IF (OPSVAL(OFFSET+I).GT.0) THEN
C         see if current option should get inserted here
          IF (LFLD.LT.OPSVAL(OFFSET+I)) THEN
C           found position in array to put this field
            DO 20 J= MAXSEL(LSET),I+1,-1
              OPSVAL(OFFSET+J)= OPSVAL(OFFSET+J-1)
 20         CONTINUE
            OPSVAL(OFFSET+I)= LFLD
            I= MAXSEL(LSET)
          END IF
        ELSE
C         no value in this spot, put current option in this spot
          OPSVAL(OFFSET+I)= LFLD
          I= MAXSEL(LSET)
        END IF
      IF (I.LT.MAXSEL(LSET)) GO TO 10
C
C     update screen text
      SCRTXT = 'X'
      IF (BOXFG.EQ.1) THEN
C       highlight this character
        CALL ZWRVDR (SCRTXT,LINE,COL)
      ELSE
C       just write text to screen
        CALL ZWRSCR (SCRTXT,LINE,COL)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZHIDFD
     I                   (NUMHID,HIDTFL,HIDVAL,HIDFLD,HIDBOX,
     I                    LMXSCB,LMXFLD,ZMNTXT,OPBOX,ITOG,LFLD,
     M                    HIDFLG)
C
C     + + + PURPOSE + + +
C     Check to see if any field should be hidden or shown (unhidden)
C     based on the value of the option field just toggled.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      NUMHID,HIDTFL(NUMHID),HIDVAL(NUMHID),HIDFLD(NUMHID),
     $             HIDBOX(NUMHID),LMXSCB,LMXFLD,OPBOX(LMXFLD),ITOG,LFLD,
     $             HIDFLG(NUMHID)
      CHARACTER*78 ZMNTXT(LMXSCB)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NUMHID - number of hidden fields
C     HIDTFL - array of trigger field numbers to hide specified field
C     HIDVAL - array of values of trigger field to hide specified field
C     HIDFLD - array of field numbers being hidden
C     HIDBOX - box defining screen text to be hidden
C     LMXSCB - max number of lines in screen text buffer
C     LMXFLD - max number of data fields
C     ZMNTXT - screen text
C     OPBOX  - array indicating whether or not highlight box is in use
C     ITOG   - current value of field just toggled
C     LFLD   - field number of field just toggled
C     HIDFLG - flag indicating whether or not field is currently hidden
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,HIDRWL,HIDRWH,HIDCLL,HIDCLH,LINE,COL
      CHARACTER*78 BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL     WOPWDS, ZWRSCR, WDPTSP, ZWRVDR
C
C     + + + END SPECIFICATIONS + + +
C
      BLNK= ' '
C
      DO 100 I= 1,NUMHID
C       check for each hidden field
        IF (HIDTFL(I).EQ.LFLD) THEN
C         found trigger field, get row and column numbers to hide
          CALL WOPWDS (HIDBOX(I),
     O                 HIDRWL,HIDCLL,HIDRWH,HIDCLH)
          IF (HIDVAL(I).EQ.ITOG .AND. HIDFLG(I).EQ.0) THEN
C           trigger value matches current value and field is not hidden,
C           time to hide it
            DO 10 J= HIDRWL,HIDRWH
C             blank out area on screen
              CALL ZWRSCR (BLNK(HIDCLL:HIDCLH),J+1,HIDCLL+1)
 10         CONTINUE
            HIDFLG(I)= 1
          ELSE IF (HIDVAL(I).NE.ITOG .AND. HIDFLG(I).EQ.1) THEN
C           trigger value doesn't match current value and field is hidden,
C           time to show (unhide) it
            DO 20 J= HIDRWL,HIDRWH
C             fill in area on screen with screen text
              CALL ZWRSCR (ZMNTXT(J)(HIDCLL:HIDCLH),J+1,HIDCLL+1)
 20         CONTINUE
            IF (OPBOX(HIDFLD(I)).GT.0) THEN
C             highlight this character
              CALL WDPTSP (OPBOX(HIDFLD(I)),
     O                     LINE,COL)
              CALL ZWRVDR (ZMNTXT(LINE)(COL:COL),LINE+1,COL+1)
            END IF
            HIDFLG(I)= 0
          END IF
        END IF
 100  CONTINUE
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   ZHIDCK
     I                         (CFLD)
C
C     + + + PURPOSE + + +
C     Determine whether or not a field is hidden, thus
C     indicating whether or not user may move into the field.
C     ZHIDCK = 0 if field is not hidden or = 1 if field is hidden.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CFLD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CFLD   - current field being checked
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
C     hidden field parameters
      INCLUDE 'czhide.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      I= 0
      IF (NUMHID.GT.0) THEN
C       hidden fields exist
        DO 100 J= 1,NUMHID
          IF (HIDFLD(J).EQ.CFLD .AND. HIDFLG(J).EQ.1) THEN
C           this field is currently hidden
            I= 1
          END IF
 100    CONTINUE
      END IF
C
      ZHIDCK= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q1INIT
     I                   (MESSFL,SCLU,SGRP)
C
C     + + + PURPOSE + + +
C     Set values in common for a 1-dimensional data screen
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
      SUBROUTINE   Q1EDIT
     O                   (IRET)
C
C     + + + PURPOSE + + +
C     Edit screen of 1-dimensional data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IRET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IRET   - value of user exit command
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SGLCHR
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZEDT0M
C
C     + + + END SPECIFICATIONS + + +
C
C     perform data screen editing without single character exit
      SGLCHR= 0
      CALL ZEDT0M (SGLCHR,
     O             IRET)
C
      RETURN
      END
C
C
C
      SUBROUTINE   Q1EDSC
     O                   (IRET)
C
C     + + + PURPOSE + + +
C     Edit screen of 1-dimensional data with control being
C     returned to the application after each keystroke.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IRET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IRET   - value of user exit command
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SGLCHR
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZEDT0M
C
C     + + + END SPECIFICATIONS + + +
C
C     perform data screen editing with single character exit
      SGLCHR= 1
      CALL ZEDT0M (SGLCHR,
     O             IRET)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZFILVF
     I                   (FFLD,IOPEN,
     M                    STRING,IERR)
C
C     + + + PURPOSE + + +
C     Check validity of file name entered in a file type data field.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      FFLD,IOPEN,IERR
      CHARACTER*78 STRING
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FFLD   - field number for this file
C     IOPEN  - open file flag,
C              0 - don't open, just check validity of file
C              1 - open file, store unit number in common
C     STRING - character variable containing file name entered
C     IERR   - error code, non-zero indicates problems
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,IPOS,ILEN,IRECL,RETCOD,NAMLEN,IMATCH,LOPEN,
     $              IFL,IOS,IOERR(3),NFLD,WNDID,SGRP,WDMFLG,OLEN,NFILES
      CHARACTER*1   STRMAT(MXRSLN)
      CHARACTER*7   CSTAT
      CHARACTER*8   TMPSTR,CHOVWR,CHAPND
      CHARACTER*10  CACCES
      CHARACTER*11  CFORM
      CHARACTER*64  FNAME,WRKDIR
      CHARACTER*80  NAMES
      CHARACTER*120 FSPTXT
      LOGICAL       LEXIST
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (TMPST1,TMPSTR)
      CHARACTER*1   TMPST1(8)
C
C     + + + FUNCTIONS + + +
      INTEGER       ZLNTXT, LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC     INDEX
C
C     + + + EXTERNALS + + +
      EXTERNAL      ZLNTXT, LENSTR, WMSPIS, FSPARS, CARVAR, QFCLOS
      EXTERNAL      GETFUN, IOESET, ZWRTB3, WDBOPN, WDFLCL, QUPCAS
      EXTERNAL      CKFSPC, QFDPRS
C
C     + + + END SPECIFICATIONS + + +
C
      CHOVWR= 'OVERWRIT'
      CHAPND= 'APPEND  '
      LOPEN = 0
C
C     set file specifications defaults
      CSTAT = 'OLD'
      CACCES= 'SEQUENTIAL'
      CFORM = 'FORMATTED'
      IRECL = 0
      WDMFLG= 0
      NAMLEN= 0
      WNDID = 13
C
      IF (FDVAL(FFLD).GT.0) THEN
C       determine offset position and length of file specs in response buffer
        CALL WMSPIS (FDVAL(FFLD),
     O               IPOS,ILEN)
C       put specs from buffer into file specs string
        I= 120
        CALL CARVAR (ILEN,RSPSTR(IPOS),I,FSPTXT)
        CALL FSPARS (FSPTXT,
     M               CSTAT,CACCES,CFORM,IRECL,
     O               NAMES,WDMFLG,RETCOD)
        NAMLEN= ZLNTXT(NAMES)
      END IF
C
C     assume no match
      IMATCH= 0
      FNAME = STRING(1:64)
      ILEN  = ZLNTXT(STRING)
      IF (ILEN.GT.0 .OR. NAMLEN.GT.0) THEN
C       name entered and/or specs defined, check for match
        IFL= 0
        IF (CSTAT.NE.'OLD') THEN
C         file entered may not exist
          INQUIRE (FILE=FNAME,EXIST=LEXIST)
          IF (.NOT. LEXIST) THEN
C           file doesn't exist, create temporary version for checking
            I= 1
            CALL GETFUN (I,IFL)
            OPEN (UNIT=IFL,FILE=FNAME,STATUS='NEW',ERR=10)
            GO TO 20
 10         CONTINUE
C             problem opening file
              IFL = 0
 20         CONTINUE
          END IF
        END IF
C       check for match to file specifications
C       looking for unique match, add to length to indicate so
        ILEN = ILEN + 1
        CALL CKFSPC (ILEN,STRING,NAMLEN,NAMES,MXRSLN,FLEN(FFLD),
     O               OLEN,NFILES,STRMAT)
        IF (IFL.NE.0) THEN
C         temp file opened, close and delete it
          I= 1
          CALL QFCLOS (IFL,I)
        END IF
        IF (NFILES.EQ.1) THEN
C         unique match found
          IMATCH= 1
C         fill in full file name, separate into directory and file name
          CALL QFDPRS (STRING,
     O                 WRKDIR,FNAME)
          I= ZLNTXT(WRKDIR)
          IF (I.GT.0) THEN
C           include working directory
            STRING= WRKDIR(1:I)
          END IF
C         put full name of matching file into string
          J= 78
          ILEN = LENSTR(J,STRMAT)
          CALL CARVAR (ILEN,STRMAT,78-I,STRING(I+1:78))
          FNAME= STRING(1:64)
        END IF
      END IF
C
      IF (IMATCH.EQ.1) THEN
C       name is ok, check validity of file
        IF (CSTAT.EQ.'NEW' .AND. IOPEN.EQ.0) THEN
C         for new file, only check existence
          INQUIRE (FILE=FNAME,EXIST=LEXIST)
          IF (LEXIST) THEN
C           file exists, problem if overwrite not allowed
            IOS= 1
            CALL IOESET (IOS,
     O                   IOERR)
            IOS= IOERR(1)
          ELSE
C           doesn't exist, should open ok
            IOS= 0
          END IF
        ELSE
C         for old file (or actual open), see if file opens using full specs
          I= 1
          CALL GETFUN (I,IFL)
          IF (CACCES.EQ.'DIRECT    ') THEN
C           open for direct access files
            IF (CSTAT .EQ. 'SCRATCH') THEN
              OPEN (UNIT=IFL,ACCESS=CACCES,STATUS=CSTAT,
     $              RECL=IRECL,FORM=CFORM,IOSTAT=IOS)
            ELSE IF (WDMFLG.EQ.1) THEN
C             WDM file, use special routine to open
              IF (CSTAT.EQ.'NEW') THEN
                I= 2
              ELSE
                I= 0
              END IF
              CALL WDBOPN (IFL,FNAME,I,
     O                     RETCOD)
              IF (RETCOD.NE.0) THEN
C               problem opening WDM file, IOS is -RETCOD
                IOS= -RETCOD
                WDMFLG= 0
              ELSE
C               WDM file opened ok
                IOS= 0
              END IF
            ELSE
              OPEN (UNIT=IFL,FILE=FNAME,ACCESS=CACCES,
     $              STATUS=CSTAT,RECL=IRECL,FORM=CFORM,IOSTAT=IOS)
            END IF
          ELSE
C           open for sequential files
            IF (CSTAT.EQ.'SCRATCH') THEN
              OPEN (UNIT=IFL,ACCESS=CACCES,STATUS=CSTAT,
     $              FORM=CFORM,IOSTAT=IOS)
            ELSE
              OPEN (UNIT=IFL,FILE=FNAME,ACCESS=CACCES,
     $              STATUS=CSTAT,FORM=CFORM,IOSTAT=IOS)
            END IF
          END IF
          IF (IOS.EQ.0 .AND. IOPEN.EQ.0) THEN
C           file was opened, but only temporarily
            LOPEN= 1
          END IF
        END IF
        IF (IOS.NE.0) THEN
C         couldn't open file, problem
          IERR= 1
          CALL IOESET
     M               (IOS,
     O                IOERR)
          IF (IOS.EQ.IOERR(1) .AND. FFLD.LT.NFLDS) THEN
C           file exists and STATUS='NEW', see if overwrite is ok
            NFLD= FFLD+ 1
C           is next field an overwrite? field
            TMPSTR= FDFMT(NFLD)
            I= 8
            CALL QUPCAS (I,TMPST1)
            IF (INDEX(TMPSTR,'OVER').GT.0) THEN
C             it is an overwrite field
              IF (FTYP(NFLD).EQ.'O') THEN
C               option type field, check toggle value
                IF (ZMNTX1(SCOL(NFLD),FLIN(NFLD)).EQ.'X') THEN
C                 overwrite? option field is on, file ok to open
                  IERR = 0
                  CSTAT= 'OLD'
                ELSE
C                 overwrite allowed, but not currently toggled ON
                  SGRP= 110
                  CALL ZWRTB3 (WNDID,SGRP)
                END IF
              ELSE IF (FTYP(NFLD).EQ.'C') THEN
C               character type field, check response value
                IF (INDEX(ZMNTXT(FLIN(NFLD))(SCOL(NFLD):
     $                    SCOL(NFLD)+FLEN(NFLD)-1),CHOVWR).GT.0) THEN
C                 user wants to overwrite file, ok to open
                  IERR = 0
                  CSTAT= 'OLD'
                ELSE IF (INDEX(ZMNTXT(FLIN(NFLD))(SCOL(NFLD):
     $                    SCOL(NFLD)+FLEN(NFLD)-1),CHAPND).GT.0) THEN
C                 user wants to append to file, ok to open
                  IERR= 0
                  CSTAT = 'OLD'
                  CACCES= 'APPEND'
                ELSE
C                 overwrite or append allowed, but not currently set to do so
                  SGRP= 111
                  CALL ZWRTB3 (WNDID,SGRP)
                END IF
              END IF
              IF (IERR.EQ.0) THEN
C               trying to overwrite or append to file
                IF (IOPEN.EQ.1) THEN
C                 try opening file with overwrite or append specs
                  IF (CACCES.EQ.'DIRECT    ') THEN
                    OPEN (UNIT=IFL,FILE=FNAME,ACCESS=CACCES,
     $                    STATUS=CSTAT,RECL=IRECL,FORM=CFORM,IOSTAT=IOS)
                  ELSE
                    OPEN (UNIT=IFL,FILE=FNAME,ACCESS=CACCES,
     $                    STATUS=CSTAT,FORM=CFORM,IOSTAT=IOS)
                  END IF
                ELSE
C                 ok for now
                  IOS= 0
                END IF
                IF (IOS.NE.0) THEN
C                 some other problem
                  SGRP= 115
                  CALL ZWRTB3 (WNDID,SGRP)
                END IF
              END IF
            ELSE
C             overwrite or append not allowed, problem
              SGRP= 112
              CALL ZWRTB3 (WNDID,SGRP)
            END IF
          ELSE IF (IOS.EQ.IOERR(2)) THEN
C           file does not exist and STATUS='OLD'
            SGRP= 113
            CALL ZWRTB3 (WNDID,SGRP)
          ELSE IF (IOS.EQ.IOERR(3)) THEN
C           file currently in use
            SGRP= 114
            CALL ZWRTB3 (WNDID,SGRP)
          ELSE
C           some other problem
            SGRP= 115
            CALL ZWRTB3 (WNDID,SGRP)
          END IF
        END IF
      ELSE
C       name doesn't match any of those listed in specs
        IERR= 1
        SGRP= 116
        CALL ZWRTB3 (WNDID,SGRP)
      END IF
C
      IF (IOS.EQ.0) THEN
C       file opens ok
        IF (LOPEN.EQ.1) THEN
C         checked validity of file name by opening, close it for now
          IF (WDMFLG.EQ.0) THEN
C           normal close
            I= 0
            CALL QFCLOS (IFL,I)
          ELSE
C           close WDM file with special routine
            CALL WDFLCL (IFL,
     O                   RETCOD)
C           return unit number to available file unit numbers
            I = 2
            CALL GETFUN (I,IFL)
          END IF
        ELSE IF (IOPEN.EQ.1) THEN
C         assign unit number to common variable
          I= APOS(FFLD)
          ZFILUN(I)= IFL
        END IF
      END IF
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   FLDHID
     I                         (NUMHID,HIDFLD,HIDFLG,IFLD)
C
C     + + + PURPOSE + + +
C     Function returns whether or not a field is currently hidden.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NUMHID,HIDFLD(*),HIDFLG(*),IFLD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NUMHID - number of hidden fields
C     HIDFLD - array of field numbers which are hidden
C     HIDFLG - array of status of hidden fields
C              0 - field is currently not hidden
C              1 - field is currently hidden
C     IFLD   - field number being check to see if it is hidden
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,CFLHID
C
C     + + + END SPECIFICATIONS + + +
C
      CFLHID= 0
      IF (NUMHID.GT.0) THEN
C       hidden fields exist, see if current field is hidden
        DO 10 I= 1,NUMHID
C         check through potentially hidden fields for match
          IF (HIDFLD(I).EQ.IFLD .AND. HIDFLG(I).EQ.1) THEN
C           this field is currently hidden
            CFLHID= 1
          END IF
 10     CONTINUE
      END IF
C
      FLDHID = CFLHID
C
      RETURN
      END
