C
C
C
      SUBROUTINE   SSPECI
     I                   (MESSFL,PTHNAM)
C
C     + + + PURPOSE + + +
C     specify scenario/reach pairs for analysis
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     PTHNAM - path of options chosen to get here
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxcls.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SCLU,SGRP,I0,I1,I3,I8,I,IRET,MAXLIN,DROPFG,
     $              OPS,OPLEN,MNSEL(3),TMPCNT,RESP2,INUM(1),ID,
     $              OPVAL(3),TLEN,CNUM,CLEN(3),ACT,NCLASS,
     $              IVAL(3,MXCLAS),CVAL(6,3,MXCLAS),CLID,RETFLG,RESP
      REAL          RVAL(1,MXCLAS)
      CHARACTER*1   BLNK,CTXT(24),TBUFF(80,MXCLAS)
      CHARACTER*8   SCEN,LOC,CONS
C
C     + + + EXTERNALS + + +
      EXTERNAL      SGSCBF,ZSTCMA,ZGTRET,ZWNSOP
      EXTERNAL      ZIPI,TSESPC,CLALLS,PRNTXT
      EXTERNAL      Q1INIT,QSETOP,ZIPC,QSETCT,Q1EDIT
      EXTERNAL      QGETOP,QGETCT,CARVAR,CVARAR,CLCNT,PMXTXI
      EXTERNAL      CLGET,QRESCX,CLPUT,CLADD,CLDEL,CLFDSN,CLSDSN
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 63
      I1   = 1
      I3   = 3
      I0   = 0
      I8   = 8
      BLNK = ' '
C
C     make previous available
      I= 4
      CALL ZSTCMA (I,I1)
C     set window names
      CALL ZWNSOP (I1,PTHNAM)
C
  5   CONTINUE
C       assume wont need to return to this screen
        RETFLG = 0
C       get number of classes
        CALL CLCNT (NCLASS)
C       clear screen buffer arrays
        CALL ZIPI (18*NCLASS,I0,CVAL)
        CALL ZIPC (80*NCLASS,BLNK,TBUFF)
        DO 10 I = 1,NCLASS
C         fill in screen for each class
          CALL CLGET (I,
     O                SCEN,LOC,CONS,ACT)
          CVAL(1,1,I)= 1
          CVAL(2,1,I)= 1
          CVAL(3,1,I)= ACT+1
          RVAL(1,I)  = 0
          IVAL(1,I)  = 0
          IVAL(2,I)  = 0
          IVAL(3,I)  = 0
C         for blank scen, loc, or cons put 'all' in screen
          IF (SCEN.EQ.'        ') THEN
            SCEN = '<all>   '
          END IF
          IF (LOC.EQ.'        ') THEN
            LOC = '<all>   '
          END IF
          IF (CONS.EQ.'        ') THEN
            CONS = '<all>   '
          END IF
          CALL CVARAR (I8,SCEN,I8,TBUFF(16,I))
          CALL CVARAR (I8,LOC,I8,TBUFF(24,I))
          CALL CVARAR (I8,CONS,I8,TBUFF(32,I))
C         recalc specs for class
          CALL CLFDSN(I)
C         dataset and location counts
          CALL CLSDSN (I,
     O                 IVAL(1,I),IVAL(2,I),IVAL(3,I))
 10     CONTINUE
        CNUM = 6
        SGRP = 2
        CALL QRESCX (MESSFL,SCLU,SGRP,I3,I1,CNUM,NCLASS,I1,
     M               IVAL,RVAL,CVAL,TBUFF)
C       how did user exit
        CALL ZGTRET(IRET)
        IF (IRET .EQ. 1) THEN
C         with accept
          CLID = NCLASS
          I = 1
 20       CONTINUE
            DROPFG = 0
C           check each class for action desired, active/inactive
            CALL CLGET (I,
     O                  SCEN,LOC,CONS,ACT)
            IF (CVAL(1,1,I).EQ.2) THEN
C             browse through this class
              RETFLG = 1
C             set specs
              CALL TSESPC (SCEN,LOC,CONS)
              CALL SGSCBF (MESSFL,SCLU,I,PTHNAM)
              I = I + 1
            ELSE IF (CVAL(1,1,I).EQ.3) THEN
C             modify this class
              RETFLG = 1
              SGRP = 23
              CALL Q1INIT (MESSFL,SCLU,SGRP)
              OPS  = 3
              OPLEN= 3
              MNSEL(1)= 1
              MNSEL(2)= 1
              MNSEL(3)= 1
              TLEN = 24
              BLNK = ' '
              CLEN(1) = 8
              CLEN(2) = 8
              CLEN(3) = 8
              CALL ZIPC (TLEN,BLNK,CTXT)
C             set initial values on screen
              IF (SCEN.EQ.'        ') THEN
C               scenario set to all
                OPVAL(1)= 1
              ELSE
C               scenario specified, put in screen
                OPVAL(1)= 2
                CALL CVARAR (CLEN(1),SCEN,CLEN(1),CTXT(1))
              END IF
              IF (LOC.EQ.'        ') THEN
C               location set to all
                OPVAL(2)= 1
              ELSE
C               location specified, put in screen
                OPVAL(2)= 2
                CALL CVARAR (CLEN(1),LOC,CLEN(1),CTXT(9))
              END IF
              IF (CONS.EQ.'        ') THEN
C               constituent set to all
                OPVAL(3)= 1
              ELSE
C               constituent specified, put in screen
                OPVAL(3)= 2
                CALL CVARAR (CLEN(1),CONS,CLEN(1),CTXT(17))
              END IF
C             set option fields for specifying loc, scen, or const
              CALL QSETOP (OPS,OPLEN,MNSEL,MNSEL,OPVAL)
C             set values of scen, loc, and constit for editing
              CNUM = 3
              CALL QSETCT (CNUM,CLEN,TLEN,CTXT)
C             edit scen, loc, or constit value
              CALL Q1EDIT (
     O                     RESP)
              IF (RESP.EQ.1) THEN
C               user wants to continue
C               read options selected
                CALL QGETOP (OPLEN,
     O                       OPVAL)
C               read scen, loc, constit char string
                CALL QGETCT (CNUM,CLEN,TLEN,
     O                       CTXT)
                IF (OPVAL(1).EQ.1) THEN
C                 user wants all scenarios
                  SCEN = '        '
                ELSE IF (OPVAL(1).EQ.2) THEN
C                 user is specifying scenario
                  CALL CARVAR (CLEN(1),CTXT(1),CLEN(1),SCEN)
                END IF
                IF (OPVAL(2).EQ.1) THEN
C                 user wants all locations
                  LOC = '        '
                ELSE IF (OPVAL(2).EQ.2) THEN
C                 user is specifying location
                  CALL CARVAR (CLEN(1),CTXT(9),CLEN(1),LOC)
                END IF
                IF (OPVAL(3).EQ.1) THEN
C                 user wants all constituents
                  CONS = '        '
                ELSE IF (OPVAL(3).EQ.2) THEN
C                 user is specifying constituent
                  CALL CARVAR (CLEN(1),CTXT(17),CLEN(1),CONS)
                END IF
              END IF
              I = I + 1
            ELSE IF (CVAL(1,1,I).EQ.4) THEN
C             drop this class
              DROPFG = 1
C             get number of classes
              CALL CLCNT (TMPCNT)
              IF (TMPCNT.EQ.1) THEN
C               no way, you cant delete the only class
                SGRP = 40
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                I = I + 1
              ELSE
C               give warning
                SGRP   = 41
                MAXLIN = 4
                INUM(1)= I
                CALL PMXTXI (MESSFL,SCLU,SGRP,MAXLIN,I1,I0,I1,INUM)
C               get user exit command value
                CALL ZGTRET (RESP2)
                IF (RESP2.EQ.1) THEN
C                 go ahead
                  CALL CLDEL (I)
                  IF (NCLASS.GE.I) THEN
C                   there are more lines to follow, move actions up
                    DO 100 ID= I,NCLASS
                      CVAL(1,1,ID) = CVAL(1,1,ID+1)
 100                CONTINUE
                  END IF
                  NCLASS = NCLASS - 1
                ELSE
C                 dont drop
                  I = I + 1
                END IF
                RETFLG = 1
              END IF
            ELSE IF (CVAL(1,1,I).EQ.5) THEN
C             want to copy this class
              RETFLG = 1
              CALL CLADD (I,CLID)
              IF (CLID.EQ.0) THEN
C               no room for a new class, tell user
                SGRP = 42
                INUM(1)= MXCLAS
                CALL PMXTXI (MESSFL,SCLU,SGRP,I8,I1,I0,I1,INUM)
              END IF
              I = I + 1
            ELSE IF (CVAL(1,1,I).EQ.6) THEN
C             want to select all data sets in this class
              RETFLG = 1
              CALL TSESPC (SCEN,LOC,CONS)
              CALL CLALLS (MESSFL,SCLU,I,I1)
              I = I + 1
            ELSE IF (CVAL(1,1,I).EQ.7) THEN
C             want to unselect all data sets in this class
              RETFLG = 1
              CALL TSESPC (SCEN,LOC,CONS)
              CALL CLALLS (MESSFL,SCLU,I,I0)
              I = I + 1
            ELSE IF (CVAL(1,1,I).EQ.8) THEN
C             want this class to be active, set it that way
              ACT = 1
              RETFLG = 1
              I = I + 1
            ELSE IF (CVAL(1,1,I).EQ.9) THEN
C             want this class to be inactive, set it that way
              ACT = 0
              RETFLG = 1
              I = I + 1
            ELSE
C             no action desired
              I = I + 1
            END IF
            IF (DROPFG.EQ.0) THEN
C             unless user wants to delete this class,
C             put class specs back to common
              CALL CLPUT (I-1,SCEN,LOC,CONS,ACT)
            END IF
          IF (I.LE.NCLASS) GO TO 20
          NCLASS = CLID
        END IF
      IF (IRET.EQ.1 .AND. RETFLG.EQ.1) GO TO 5
C
C     make previous unavailable
      I= 4
      CALL ZSTCMA (I,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SGSCBF
     I                  (MESSFL,SCLU,CLID,PTHNAM)
C
C     + + + PURPOSE + + +
C     **** cousin to SCANBF in WAIDE ****
C     This routine lists all possible data sets and lets user
C     select ones for analysis (browse)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU,CLID
      CHARACTER*8  PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - Cluster containing screen specs
C     CLID   - class id
C     PTHNAM - path chosen to get here
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxts.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      QFLG,SGRP,LINE,L1,IYS(1),INO(1),ICNT,
     $             J,K,L72,POS,IRET,LDSN(MXTS),LCNT,TDSN,
     $             YORN(1),WCNT,JUST,OLEN,I8,I3,ITMP,I0
      CHARACTER*1  OBUFF(80),WNAM(8)
      CHARACTER*8  CWNAM(3)
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     Q1INIT,QSTCOB,QSTCTF,Q1EDIT,QGTCOB
      EXTERNAL     SGNTXT,ZSTCMA,INTCHR,CARVAR,ZWNSOP
      EXTERNAL     PRNTXT,CLGDSN,TSDSAC,TSDSAS
C
C     + + + END SPECIFICATIONS + + +
C
      IYS(1)= 2
      INO(1)= 1
      L1  = 1
      L72 = 72
      I0  = 0
      I3  = 3
      I8  = 8
C     build buffer of possible datasets
      CALL CLGDSN(CLID,MXTS,I0,
     O            LCNT,LDSN)
C
      IF (LCNT .EQ. 0) THEN
C       nothing meets spec
        SGRP= 13
        CALL PRNTXT(MESSFL,SCLU,SGRP)
      ELSE
C       begin loop to display stations selected
        QFLG = 0
        ICNT = 0
C       turn on intrpt
        CALL ZSTCMA (16,1)
        JUST = 1
 520    CONTINUE
C         this procedure uses negative DSN numbers as flag to drop
C         stations from the list
C         set window name
          CWNAM(1) = PTHNAM(1)
          WCNT = (ICNT)/8 + 1
          CALL INTCHR (WCNT,I8,JUST,OLEN,WNAM)
          CALL CARVAR (I8,WNAM,I8,CWNAM(2))
          WCNT = (LCNT-1)/8 + 1
          CALL INTCHR (WCNT,I8,JUST,OLEN,WNAM)
          CALL CARVAR (I8,WNAM,I8,CWNAM(3))
          CALL ZWNSOP (I3,CWNAM)
          SGRP = 12
          CALL Q1INIT (MESSFL, SCLU, SGRP)
          LINE = 0
C         fill in site names
 530      CONTINUE
            ICNT = ICNT + 1
            LINE = LINE + 1
            CALL TSDSAC(LDSN(ICNT),ITMP)
            IF (ITMP .GE. 1) THEN
              CALL QSTCOB (L1,LINE, IYS)
            ELSE
              CALL QSTCOB (L1,LINE, INO)
            END IF
C           get and set station name
            TDSN = ABS(LDSN(ICNT))
            CALL SGNTXT (TDSN,OBUFF)
            POS = LINE + 8
            CALL QSTCTF (POS,L72,OBUFF)
          IF (LINE .LT. 8 .AND. ICNT .LT. LCNT) GO TO 530
C         let user edit screen
          CALL Q1EDIT (IRET)
C
          IF (IRET .EQ. -1) THEN
C           oops, reset line pointer
            ICNT = ICNT - LINE
          ELSE
C           not oops
C           set y/n for keep
            J = ICNT - LINE
            K = 0
 540        CONTINUE
              J = J + 1
              K = K + 1
              CALL QGTCOB (L1,K,YORN)
              IF (YORN(1) .EQ. INO(1)) THEN
C               not active
                CALL TSDSAS(LDSN(J),I0)
              ELSE
C               active
                CALL TSDSAS(LDSN(J),L1)
              END IF
            IF (K .LT. LINE) GO TO 540
C
            IF (IRET .EQ. 2) THEN
C             previous
              ICNT = ICNT - 8 - LINE
              IF (ICNT .LT. 0) THEN
                QFLG = 1
              ELSE
                QFLG = 0
              END IF
            END IF
C
            IF (IRET .EQ. 7) THEN
C             interupt
              QFLG = 1
            END IF
C
            IF (IRET .EQ. 1 .AND. ICNT .GE. LCNT) THEN
C             no more sites, end of list
              QFLG = 1
            END IF
          END IF
        IF (QFLG .EQ. 0) GO TO 520
      END IF
C
C     disable intrpt
      CALL ZSTCMA (16,0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLALLS
     I                   (MESSFL,SCLU,CLID,SELUNS)
C
C     + + + PURPOSE + + +
C     select/unselect all data sets in a class
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU,CLID,SELUNS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - Cluster containing screen specs
C     CLID   - class id
C     SELUNS - select(1) or unselect(0)
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxts.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SGRP,J,LDSN(MXTS),LCNT,I0
C
C     + + + EXTERNALS + + +
      EXTERNAL     PRNTXT,CLGDSN,TSDSAS
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
C     build buffer of possible datasets
      CALL CLGDSN(CLID,MXTS,I0,
     O            LCNT,LDSN)
C
      IF (LCNT .EQ. 0) THEN
C       nothing meets spec
        IF (SELUNS.EQ.1) THEN
C         message for select
          SGRP= 14
          CALL PRNTXT(MESSFL,SCLU,SGRP)
        ELSE IF (SELUNS.EQ.0) THEN
C         message for unselect
          SGRP= 15
          CALL PRNTXT(MESSFL,SCLU,SGRP)
        END IF
      ELSE
C       add data sets to buffer or remove them
        DO 10 J = 1,LCNT
          CALL TSDSAS(LDSN(J),SELUNS)
 10     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SGNTXT
     I                   (DSNID,
     O                    OBUFF)
C
C     + + + PURPOSE + + +
C     **** Cousin to SCNTXT ****
C     This routine retrieves attribute information from the specified
C     data set. The retrieved information is placed in the
C     character string OBUFF.  If the attribute can't be retrieved
C     the characters "        " are used for not available.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DSNID
      CHARACTER*1 OBUFF(72)
C
C     + + + ARGUMENT DEFINITION + + +
C     DSNID  - data det number (or dsn) to use to get attribute values
C     OBUFF  - character string of the data set information
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SDATE(6),EDATE(6),TS,TU,L5,JUST,OLEN,L72,I,GRPSIZ,
     1             DSN,I0,I4
      CHARACTER*1  BLNK
      CHARACTER*4  WDID
      CHARACTER*8  LSCENM,LRCHNM,LCONNM
      CHARACTER*12 CTMP
C
C     + + + EXTERNALS + + +
      EXTERNAL    TSDSPC, INTCHR, ZIPC, WID2UA, CVARAR
C
C     + + + DATA INITIALIZATIONS + + + 
      DATA  BLNK/' '/, JUST,L5,L72/0,5,72/
C
C     + + + INPUT FORMATS + + +
1000  FORMAT (12A1)
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (I4,2I3)
C
C     + + + END SPECIFICATIONS + + +
C
      CALL ZIPC (L72, BLNK, OBUFF)
C
      I0 = 0
C     convert id to data set number and wdid
      CALL WID2UA (I0,DSNID,
     O             I,DSN,WDID)
C
C     fill with wdm file id
      I4 = 4
      CALL CVARAR (I4,WDID,L72,OBUFF(4))
C
C     fill with data set number
      CALL INTCHR (DSN,L5,JUST,OLEN,OBUFF(9))
C
C     other info
      CALL TSDSPC(DSNID,
     O            LSCENM,LRCHNM,LCONNM,
     O            TU,TS,SDATE,EDATE,GRPSIZ)
C
      READ(LSCENM,1000) (OBUFF(I),I=16,23)
      READ(LRCHNM,1000) (OBUFF(I),I=26,33)
      READ(LCONNM,1000) (OBUFF(I),I=36,43)
      WRITE(CTMP,2000)  (SDATE(I),I=1,3)
      READ(CTMP,1000)   (OBUFF(I),I=46,57)
      WRITE(CTMP,2000)  (EDATE(I),I=1,3)
      READ(CTMP,1000)   (OBUFF(I),I=58,69)
C
      RETURN
      END
