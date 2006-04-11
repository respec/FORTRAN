C
C
C
      SUBROUTINE   STSPEC
     I                   (MESSFL,LSENNM)
C
C     + + + PURPOSE + + +
C     scenario translator for special actions
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL
      CHARACTER*8   LSENNM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     LSENNM - scenario name
C
C     + + + PARAMETERS + + +
      INTEGER    MXUVQ,     MXGRP,   MXROW
      PARAMETER (MXUVQ=3000,MXGRP=20,MXROW=295)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I1,UVQFIL,SCLU,SGRP,I0,SREC,EREC,ID,I,I4,
     1             AKEY(MXUVQ),NROW,I2,I6,IRET,INUM,
     2             DATFLG(MXUVQ),CONFLG(MXUVQ),I3,ITEMP,OFLOW,INTFG,
     3             RETFLG,VLINFG(2),UCCNT(2),CLEN(1),
     4             MSEL(1),J,ILEN,I120,USTRLN(2),URSPLN,IFOUND,
     5             IGROUP,GNUM(MXGRP),OPMOD(1),CVAL(2),IPOS,ICNT,
     6             GDAT(MXROW),GCON(MXROW),GAKEY(MXROW),GRECNO(MXROW),
     7             AINT(MXUVQ),GAINT(MXROW),RESP,IRET1,RETCOD
      REAL         AVAL(MXUVQ),TMIN,TMAX,TDEF,AMIN(MXUVQ),
     1             AMAX(MXUVQ),ADEF(MXUVQ),GAVAL(MXROW),GAMIN(MXROW),
     2             GAMAX(MXROW),GADEF(MXROW)
      CHARACTER*1  CAC(3,MXUVQ),CSTR1(120),URSPST(5000),GCAC(3,MXROW),
     1             CTXT(6),BLNK,GROPS(63,MXGRP)
      CHARACTER*6  UVQUAN,UVNAME,AUVQ(MXUVQ),GNAME(MXGRP),
     1             GCONT(MXROW,MXGRP),GAUVQ(MXROW),CTXT6
      CHARACTER*10 CWLOC
      CHARACTER*36 CMESS
      CHARACTER*64 CDESC(MXUVQ),DESC1,GDESC(MXGRP),GCDESC(MXROW),
     1             ACOND(MXUVQ),GACOND(MXROW)
      CHARACTER*78 CHEAD
      CHARACTER*120  CSTRIN
C
C     + + + INTRINSICS + + +
      INTRINSIC   FLOAT
C
C     + + + FUNCTIONS + + +
      INTEGER     ZCMDON,LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETFUN,PRNTXT,GETSE,PMXCNW,QGETCO,QUPCAS
      EXTERNAL   UVQ2WS,WS2UVN,CVARAR,ZSTCMA,Q1EDIT,QGETOP,QSETCO,QGETCT
      EXTERNAL   QSETOP,Q1INIT,ZWRSCR,ZCMDON,LENSTR,STSPAR,QSCSTV,STSPMO
      EXTERNAL   CARVAR,QRSPIN,QRESP,ZGTRET,ZIPC,QSETCT,STSPFA,STSPGR
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A6,4X,A64,6X,3G10.5)
 1005 FORMAT (A120)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I2 = 2
      I3 = 3
      I4 = 4
      I6 = 6
      I120 = 120
      SCLU = 58
C
C     check to see if any special actions records exist
      ID = 9
      CALL GETSE (ID,I1,
     O            SREC,EREC)
      IF (SREC.EQ.0) THEN
C       no special actions records exist in this uci file
        SGRP = 12
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      ELSE
C       have some special actions records
C
C       open file for uvquan info
        CALL GETFUN (I1,UVQFIL)
        OPEN(UNIT=UVQFIL,FILE='uvquan.inp',STATUS='OLD',ERR=90)
C
C       let user know that processing is going on
        SGRP = 60
        CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,I)
C
        CTXT6 = '      '
        IGROUP= 0
        NROW  = 0
 10     CONTINUE
C         read each line of uvquan file
          READ(UVQFIL,1005,ERR=91,END=98) CSTRIN
          CALL CVARAR (I120,CSTRIN,I120,CSTR1)
          ILEN = LENSTR(I120,CSTR1)
          IF (ILEN.GT.0) THEN
C           something on this line
            READ(CSTRIN,1000,ERR=91,END=98) UVQUAN,DESC1,TMIN,TMAX,TDEF
            CWLOC = ' '
            UVNAME= ' '
C           find global workspace location which corresponds to this uvquan
            CALL UVQ2WS (UVQUAN,SREC,EREC,
     O                   INTFG,CWLOC)
            IF (CWLOC.NE.'          ') THEN
C             found global workspace location, find corresponding uvname
              CALL WS2UVN (CWLOC,SREC,EREC,
     O                     UVNAME)
              IF (UVNAME.NE.'      ') THEN
C               look for this name in spec-act records
C               let user know that processing is going on
                I = 20
                IF (ZCMDON(I).EQ.0) THEN
C                 quiet not on, no middle window
                  I= 11
                  J= 24
                ELSE
C                 quiet on, middle window on
                  I= 8
                  J= 24
                END IF
                CMESS = 'Finding occurrances of UVQUAN '//UVQUAN
                CALL ZWRSCR (CMESS,I,J)
C
C               call routine to find all occurrances of this UVQUAN
                CALL STSPFA (SREC,EREC,INTFG,UVQUAN,UVNAME,DESC1,
     I                       TMIN,TMAX,TDEF,MXUVQ,
     M                       NROW,DATFLG,CONFLG,AKEY,AVAL,
     M                       AMIN,AMAX,ADEF,AINT,
     M                       CAC,AUVQ,CDESC,ACOND)
              END IF
            END IF
          END IF
        IF (ILEN.GT.0) GO TO 10
C
        RETCOD = 0
        IF (ILEN.EQ.0) THEN
C         found blank line
C         let user know that groups being processed
          I = 20
          IF (ZCMDON(I).EQ.0) THEN
C           quiet not on, no middle window
            I= 11
            J= 24
          ELSE
C           quiet on, middle window on
            I= 8
            J= 24
          END IF
          CMESS = '     Processing UVQUAN groups       '
          CALL ZWRSCR (CMESS,I,J)
C         now read group information
          CALL STSPGR (UVQFIL,MXGRP,MXROW,
     M                 IGROUP,GNUM,GNAME,GCONT,GDESC,
     O                 RETCOD)
        END IF
        IF (RETCOD.EQ.1) GO TO 91
C
 98     CONTINUE
        CLOSE(UNIT=UVQFIL)
        CALL GETFUN (I2,UVQFIL)
C
        IF (NROW.EQ.0) THEN
C         did not find any uvquans to show
          SGRP = 13
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        ELSE
C         do screen to choose option
C         turn on 'Prev' command
          CALL ZSTCMA (I4,I1)
          IF (IGROUP.GT.0) THEN
            OPMOD(1) = 3
          ELSE
            OPMOD(1) = 2
          END IF
          CVAL(1)  = 0
  5       CONTINUE
C           assume wont need to return to this screen
            RETFLG = 0
            IF (IGROUP.GT.0) THEN
              SGRP = 30
            ELSE
              SGRP = 31
            END IF
            CALL Q1INIT (MESSFL,SCLU,SGRP)
            MSEL(1) = 1
            CALL QSETOP (I1,I1,MSEL,MSEL,OPMOD)
C           set valid values for names and groups
            VLINFG(1) = 1
            UCCNT(1)  = 1
            USTRLN(1) = 6
            CALL CVARAR (I6,AUVQ(1),I6,URSPST(1))
            OFLOW = 0
            IF (NROW.GT.1) THEN
C             fill in the rest of the uvquan names
              DO 61 I = 2,NROW
                IF (AUVQ(I).NE.AUVQ(I-1)) THEN
C                 dont have this uvquan name in valid list yet
                  IPOS = (UCCNT(1)*6) + 1
                  IF (IPOS.LT.4900 .AND. UCCNT(1).LT.400) THEN
C                   room in buffer
                    CALL CVARAR (I6,AUVQ(I),I6,URSPST(IPOS))
                    UCCNT(1) = UCCNT(1) + 1
                  ELSE
C                   overflow in valid responses
                    OFLOW = 1
                  END IF
                END IF
 61           CONTINUE
            END IF
            IF (OFLOW.EQ.1) THEN
C             overflow in valid responses, dont use valid names
              VLINFG(1) = 0
            END IF
            URSPLN = UCCNT(1)*6
C           needs to be all upper case for aide
            CALL QUPCAS (URSPLN,URSPST)
            IF (IGROUP.GT.0) THEN
              CALL QSCSTV (I1,I4,VLINFG,UCCNT,USTRLN,URSPLN,URSPST)
              CALL QSETCO (I1,CVAL)
            ELSE
              CALL QSCSTV (I1,I3,VLINFG,UCCNT,USTRLN,URSPLN,URSPST)
              CALL QSETCO (I1,CVAL)
            END IF
            IF (OFLOW.EQ.1) THEN
C             need to set actual character string
              CLEN(1) = 6
              CALL CVARAR (CLEN(1),CTXT6,CLEN(1),CTXT)
              CALL QSETCT (I1,CLEN,CLEN(1),CTXT)
            END IF
            CALL Q1EDIT (IRET)
            IF (IRET.EQ.1) THEN
C             user wants to continue
              RETFLG = 1
              CALL QGETOP (I1,OPMOD)
              CALL QGETCO (I1,CVAL)
              IF (OFLOW.EQ.1) THEN
C               need to get actual character string
                CLEN(1) = 6
                CALL QGETCT (I1,CLEN,CLEN(1),CTXT)
                CALL CARVAR (CLEN(1),CTXT,CLEN(1),CTXT6)
              END IF
              IF (OPMOD(1).EQ.1) THEN
C               user wants to modify one uvquan
                IF (OFLOW.EQ.0) THEN
C                 didnt overflow valid buffer
                  INUM = 1
                  IF (CVAL(1).NE.1) THEN
C                   find the 'cval(1)'th uvquan name
                    INUM = 1
                    I    = 1
 71                 CONTINUE
                      INUM = INUM + 1
                      IF (AUVQ(INUM).NE.AUVQ(INUM-1)) THEN
C                       count this one
                        I = I + 1
                      END IF
                    IF (I.NE.CVAL(1)) GO TO 71
                  END IF
                ELSE
C                 overflowed valid buffer
                  IFOUND = 0
                  INUM = 1
 73               CONTINUE
                    IF (AUVQ(INUM).EQ.CTXT6) THEN
C                     found it in the list
                      IFOUND = 1
                    ELSE
                      INUM = INUM + 1
                    END IF
                  IF (IFOUND.EQ.0 .AND. INUM.LT.NROW) GO TO 73
                END IF
                IF (OFLOW.EQ.0 .OR. (OFLOW.EQ.1.AND.IFOUND.EQ.1)) THEN
C                 find out how many of these uvquans exist
                  ICNT = 1
                  IF (INUM.LT.NROW) THEN
C                   have some more records to check
                    I = INUM
 72                 CONTINUE
                      I = I + 1
                      IF (AUVQ(INUM).EQ.AUVQ(I)) THEN
C                       count this one
                        ICNT = ICNT + 1
                      END IF
                    IF (I.LT.NROW .AND. AUVQ(INUM).EQ.AUVQ(I)) GO TO 72
                  END IF
                  IF (ICNT.EQ.1) THEN
C                   only one line with this uvquan
                    CALL STSPMO (MESSFL,SCLU,
     M                           DATFLG(INUM),CONFLG(INUM),AKEY(INUM),
     M                           AVAL(INUM),AMIN(INUM),AMAX(INUM),
     M                           ADEF(INUM),AINT(INUM),CAC(1,INUM),
     M                           AUVQ(INUM),CDESC(INUM),ACOND(INUM))
                  ELSE IF (ICNT.GT.1) THEN
C                   several lines include this uvquan name
                    IF (ICNT.GT.295) THEN
C                     can only do as many as fit on one screen
                      ICNT = 295
                    END IF
                    DO 81 J = 1,ICNT
                      GDAT(J)  = DATFLG(INUM+J-1)
                      GCON(J)  = CONFLG(INUM+J-1)
                      GAKEY(J) = AKEY(INUM+J-1)
                      GAVAL(J) = AVAL(INUM+J-1)
                      GAMIN(J) = AMIN(INUM+J-1)
                      GAMAX(J) = AMAX(INUM+J-1)
                      GADEF(J) = ADEF(INUM+J-1)
                      GAINT(J) = AINT(INUM+J-1)
                      GCAC(1,J)= CAC(1,INUM+J-1)
                      GCAC(2,J)= CAC(2,INUM+J-1)
                      GCAC(3,J)= CAC(3,INUM+J-1)
                      GAUVQ(J) = AUVQ(INUM+J-1)
                      GCDESC(J)= CDESC(INUM+J-1)
                      GACOND(J)= ACOND(INUM+J-1)
                      GRECNO(J)= INUM+J-1
 81                 CONTINUE
C                   do screen to modify
                    CHEAD = 'View table and select row to modify:'
                    CALL STSPAR (MESSFL,SCLU,ICNT,CHEAD,
     M                           GDAT,GCON,GAKEY,GAVAL,GAMIN,GAMAX,
     M                           GADEF,GAINT,GCAC,GAUVQ,GCDESC,GACOND)
C                   now put records back
                    DO 82 I = 1,ICNT
                      DATFLG(GRECNO(I))= GDAT(I)
                      AVAL(GRECNO(I))  = GAVAL(I)
                      CAC(1,GRECNO(I)) = GCAC(1,I)
                      CAC(2,GRECNO(I)) = GCAC(2,I)
                      CAC(3,GRECNO(I)) = GCAC(3,I)
 82                 CONTINUE
                  END IF
                ELSE
C                 this name does not exist
                  SGRP = 32
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                END IF
              ELSE IF (OPMOD(1).EQ.2 .AND. IGROUP.GT.0) THEN
C               user wants to modify a group of uvquans
C               do menu screen to determine which group
                I   = MXGRP*63
                BLNK= ' '
                CALL ZIPC (I,BLNK,GROPS)
C               build group option character strings
                DO 74 I = 1,IGROUP
                  CALL CVARAR (I6,GNAME(I),I6,
     1                         GROPS(1,I))
                  GROPS(8,I) = '-'
                  J = 54
                  CALL CVARAR (J,GDESC(I),J,
     1                         GROPS(10,I))
 74             CONTINUE
                I = 63
                CALL QRSPIN (IGROUP,I,GROPS)
                SGRP = 40
                CALL QRESP (MESSFL,SCLU,SGRP,RESP)
                CALL ZGTRET (IRET1)
                IF (IRET1.EQ.1) THEN
C                 user wants to continue
                  ICNT = 0
                  DO 75 I = 1,GNUM(RESP)
                    DO 76 J = 1,NROW
                      IF (GCONT(I,RESP).EQ.AUVQ(J) .AND.
     1                    ICNT.LT.MXROW) THEN
C                       found a record to save
                        ICNT = ICNT + 1
                        GDAT(ICNT)  = DATFLG(J)
                        GCON(ICNT)  = CONFLG(J)
                        GAKEY(ICNT) = AKEY(J)
                        GAVAL(ICNT) = AVAL(J)
                        GAMIN(ICNT) = AMIN(J)
                        GAMAX(ICNT) = AMAX(J)
                        GADEF(ICNT) = ADEF(J)
                        GAINT(ICNT) = AINT(J)
                        GCAC(1,ICNT)= CAC(1,J)
                        GCAC(2,ICNT)= CAC(2,J)
                        GCAC(3,ICNT)= CAC(3,J)
                        GAUVQ(ICNT) = AUVQ(J)
                        GCDESC(ICNT)= CDESC(J)
                        GACOND(ICNT)= ACOND(J)
                        GRECNO(ICNT)= J
                      END IF
 76                 CONTINUE
 75               CONTINUE
C                 do screen to modify
                  CHEAD = 'Group '//GNAME(RESP)//' - '//GDESC(RESP)
                  CALL STSPAR (MESSFL,SCLU,ICNT,CHEAD,
     M                         GDAT,GCON,GAKEY,GAVAL,GAMIN,GAMAX,GADEF,
     M                         GAINT,GCAC,GAUVQ,GCDESC,GACOND)
C                 now put records back
                  DO 78 I = 1,ICNT
                    DATFLG(GRECNO(I))= GDAT(I)
                    AVAL(GRECNO(I))  = GAVAL(I)
                    CAC(1,GRECNO(I)) = GCAC(1,I)
                    CAC(2,GRECNO(I)) = GCAC(2,I)
                    CAC(3,GRECNO(I)) = GCAC(3,I)
 78               CONTINUE
                END IF
              ELSE IF (OPMOD(1).EQ.3 .OR.
     1                 (OPMOD(1).EQ.2.AND.IGROUP.EQ.0)) THEN
C               user wants to modify all uvquans
                CHEAD = 'View summary table and select row to modify:'
                IF (NROW.LE.MXROW) THEN
                  CALL STSPAR (MESSFL,SCLU,NROW,CHEAD,
     M                         DATFLG,CONFLG,AKEY,AVAL,AMIN,AMAX,ADEF,
     M                         AINT,CAC,AUVQ,CDESC,ACOND)
                ELSE
                  ITEMP = MXROW
                  CALL STSPAR (MESSFL,SCLU,ITEMP,CHEAD,
     M                         DATFLG,CONFLG,AKEY,AVAL,AMIN,AMAX,ADEF,
     M                         AINT,CAC,AUVQ,CDESC,ACOND)
                END IF
              END IF
            END IF
          IF (IRET.EQ.1 .AND. RETFLG.EQ.1) GO TO 5
C         turn off 'Prev' command
          CALL ZSTCMA (I4,I0)
        END IF
C
        GO TO 99
 90     CONTINUE
C         do screen for error in opening uvquan file
          SGRP = 10
          CALL PRNTXT (MESSFL,SCLU,SGRP)
          GO TO 99
 91     CONTINUE
C         do screen for error in reading uvquan file
          SGRP = 11
          CALL PRNTXT (MESSFL,SCLU,SGRP)
 99     CONTINUE
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   STSPAR
     I                   (MESSFL,SCLU,NROW,CHEAD,
     M                    DATFLG,CONFLG,AKEY,AVAL,AMIN,AMAX,ADEF,
     M                    AINT,CAC,AUVQ,CDESC,ACOND)
C
C     + + + PURPOSE + + +
C     scenario translator for special actions action row screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,SCLU,NROW,
     1              DATFLG(NROW),CONFLG(NROW),AKEY(NROW),AINT(NROW)
      REAL          AVAL(NROW),AMIN(NROW),AMAX(NROW),ADEF(NROW)
      CHARACTER*1   CAC(3,NROW)
      CHARACTER*6   AUVQ(NROW)
      CHARACTER*64  CDESC(NROW),ACOND(NROW)
      CHARACTER*78  CHEAD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     NROW   - number of rows for action-row screen
C     DATFLG - dated or undated special action flag
C     CONFLG - conditional special action flag
C     AKEY   - array of uci buffer keys for each record
C     AVAL   - array of values for each special action
C     AMIN   - array of min values for each special action
C     AMAX   - array of max values for each special action
C     ADEF   - array of default values for each special action
C     AINT   - array of integer value flags
C     CAC    - array of action codes for each special action
C     AUVQ   - array of uvquan names
C     CDESC  - array of uvquan descriptions
C     ACOND  - array of conditional strings
C     CHEAD  - header string
C
C     + + + PARAMETERS + + +
      INTEGER    MXUVQ
      PARAMETER (MXUVQ=3000)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I1,SGRP,I,CLEN(3),TLEN,I2,I6,IRET,
     1             CVAL2(2,MXUVQ),RETFLG,I78,IM1,
     2             I3,CVAL(2,MXUVQ)
      CHARACTER*1  CTXT(42,MXUVQ),CHEAD1(78)
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2INIT,Q2EDIT,Q2STCO,ZBLDWR,ZMNSST
      EXTERNAL   Q2SETR,CVARAR,STSPMO
      EXTERNAL   Q2SCTB,Q2SCOB,Q2GTCO
C
C     + + + END SPECIFICATIONS + + +
C
      IM1= -1
      I1 = 1
      I2 = 2
      I3 = 3
      I6 = 6
      I78= 78
      CALL CVARAR (I78,CHEAD,I78,CHEAD1)
C
  5   CONTINUE
C       assume wont need to return to this screen
        RETFLG = 0
        CALL ZBLDWR (I78,CHEAD1,I1,IM1,I)
        CALL ZMNSST
        SGRP = 1
        CALL Q2INIT (MESSFL,SCLU,SGRP)
        CLEN(1) = 6
        CLEN(2) = 33
        CLEN(3) = 3
        TLEN    = CLEN(1) + CLEN(2) + CLEN(3)
        DO 81 I = 1,NROW
          CALL CVARAR (I6,AUVQ(I),I6,CTXT(1,I))
          CALL CVARAR (CLEN(2),CDESC(I),CLEN(2),CTXT(7,I))
          CTXT(40,I)= CAC(1,I)
          CTXT(41,I)= CAC(2,I)
          CTXT(42,I)= CAC(3,I)
          CVAL(1,I) = 1
          CVAL(2,I) = 1
          CVAL2(1,I) = DATFLG(I)
          CVAL2(2,I) = CONFLG(I)
 81     CONTINUE
        CALL Q2STCO (I2,NROW,CVAL)
        CALL Q2SCTB (I3,CLEN,TLEN,I3,NROW,CTXT)
        CALL Q2SCOB (I2,I6,NROW,CVAL2)
        CALL Q2SETR (I1,NROW,AVAL)
        CALL Q2EDIT (NROW,
     O               IRET)
        IF (IRET.EQ.1) THEN
C         user wants to continue
          CALL Q2GTCO (I2,NROW,
     O                 CVAL)
          I = 1
 30       CONTINUE
C           check each row for action desired
            IF (CVAL(1,I).EQ.2) THEN
C             modify this row
              CALL STSPMO (MESSFL,SCLU,
     M                     DATFLG(I),CONFLG(I),AKEY(I),AVAL(I),
     M                     AMIN(I),AMAX(I),ADEF(I),AINT(I),
     M                     CAC(1,I),AUVQ(I),CDESC(I),ACOND(I))
              RETFLG = 1
              I = I + 1
            ELSE
C             look at the next row
              I = I + 1
            END IF
          IF (I.LE.NROW) GO TO 30
        END IF
      IF (IRET.EQ.1 .AND. RETFLG.EQ.1) GO TO 5
C
      RETURN
      END
C
C
C
      SUBROUTINE   STSPMO
     I                   (MESSFL,SCLU,
     M                    DATFLG,CONFLG,AKEY,AVAL,AMIN,AMAX,ADEF,AINT,
     M                    CAC,AUVQ,CDESC,ACOND)
C
C     + + + PURPOSE + + +
C     scenario translator for special actions modify screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,SCLU,DATFLG,CONFLG,AKEY,AINT
      REAL          AVAL(1),AMIN(1),AMAX(1),ADEF(1)
      CHARACTER*1   CAC(3)
      CHARACTER*6   AUVQ
      CHARACTER*64  CDESC,ACOND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     DATFLG - dated or undated special action flag
C     CONFLG - conditional special action flag
C     AKEY   - uci buffer keys for this record
C     AVAL   - value for this special action
C     AMIN   - min value for this special action
C     AMAX   - max value for this special action
C     ADEF   - default value for this special action
C     AINT   - integer value flag
C     CAC    - action code for this special action
C     AUVQ   - uvquan name
C     CDESC  - uvquan description
C     ACOND  - conditional string if applicable
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I1,SGRP,I0,CLEN(4),TLEN,I6,IMIN(1),IMAX(1),I4,
     1             I5,IVAL(6),ITMP(5),MSEL(1),OPVAL(1),RESP,I3,IDEF(1)
      CHARACTER*1  CTXT1(137)
      CHARACTER*80 UCIBF
C
C     + + + INTRINSICS + + +
      INTRINSIC    INT,FLOAT
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETUCI,REPUCI,CVARAR,PREUCI,QSCSTR,QSCSTI
      EXTERNAL   QSETCT,QSETI,QSETR,QSETOP,Q1INIT,Q1EDIT,QGETR,QGETOP
      EXTERNAL   HDATGT,QGETCT,QGETI
C
C     + + + INPUT FORMATS + + +
 1030 FORMAT (20X,I4,4(1X,I2))
C
C     + + + OUTPUT FORMATS + + +
 2020 FORMAT (G10.5)
 2022 FORMAT (I10)
 2030 FORMAT (I4,4(1X,I2))
 2040 FORMAT ('                ')
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I3 = 3
      I4 = 4
      I5 = 5
      I6 = 6
C
C     okay to modify this record
      IF (CONFLG.EQ.1) THEN
C       not in a conditional
        SGRP = 20 + AINT
      ELSE
C       in a conditional
        SGRP = 22 + AINT
      END IF
      CALL Q1INIT (MESSFL,SCLU,SGRP)
      IF (CONFLG.EQ.1) THEN
C       not in a conditional
        CLEN(1) = 6
        CLEN(2) = 64
        CLEN(3) = 3
        TLEN    = CLEN(1) + CLEN(2) + CLEN(3)
        CALL CVARAR (I6,AUVQ,I6,CTXT1)
        CALL CVARAR (CLEN(2),CDESC,CLEN(2),CTXT1(7))
        CTXT1(71)= CAC(1)
        CTXT1(72)= CAC(2)
        CTXT1(73)= CAC(3)
        CALL QSETCT (I3,CLEN,TLEN,CTXT1)
      ELSE
C       in a conditional
        CLEN(1) = 6
        CLEN(2) = 64
        CLEN(3) = 64
        CLEN(4) = 3
        TLEN    = CLEN(1) + CLEN(2) + CLEN(3) + CLEN(4)
        CALL CVARAR (I6,AUVQ,I6,CTXT1)
        CALL CVARAR (CLEN(2),CDESC,CLEN(2),CTXT1(7))
        CALL CVARAR (CLEN(3),ACOND,CLEN(3),CTXT1(71))
        CTXT1(135)= CAC(1)
        CTXT1(136)= CAC(2)
        CTXT1(137)= CAC(3)
        CALL QSETCT (I4,CLEN,TLEN,CTXT1)
      END IF
      CALL PREUCI (AKEY)
      CALL GETUCI (I0,
     M             AKEY,
     O             UCIBF)
      READ (UCIBF,1030,ERR=96) IVAL(1),IVAL(2),IVAL(3),IVAL(4),IVAL(5)
      GO TO 97
 96   CONTINUE
        DATFLG = 1
        IVAL(1)= 0
 97   CONTINUE
      IF (IVAL(1).EQ.0) THEN
C       no dates entered, set defaults
        CALL HDATGT (IVAL,ITMP)
      END IF
      IF (AINT.EQ.0) THEN
        CALL QSETR (I1,AVAL)
        CALL QSETI (I5,IVAL)
C       set real min, max, default
        CALL QSCSTR (I1,I1,AMIN,AMAX,ADEF)
      ELSE IF (AINT.EQ.1) THEN
        IVAL(6) = IVAL(5)
        IVAL(5) = IVAL(4)
        IVAL(4) = IVAL(3)
        IVAL(3) = IVAL(2)
        IVAL(2) = IVAL(1)
        IVAL(1) = INT(AVAL(1))
        CALL QSETI (I6,IVAL)
C       set integer min, max, default
        IMIN(1) = INT(AMIN(1))
        IMAX(1) = INT(AMAX(1))
        IDEF(1) = INT(ADEF(1))
        CALL QSCSTI (I1,I1,IMIN,IMAX,IDEF)
      END IF
      MSEL(1) = 1
      OPVAL(1)= DATFLG
      CALL QSETOP (I1,I1,MSEL,MSEL,OPVAL)
C     edit values
      CALL Q1EDIT (
     O             RESP)
      IF (RESP.EQ.1) THEN
C       user wants to continue
        IF (CONFLG.EQ.1) THEN
C         not in a conditional
          CALL QGETCT (I3,CLEN,TLEN,CTXT1)
          CAC(1) = CTXT1(71)
          CAC(2) = CTXT1(72)
          CAC(3) = CTXT1(73)
          IF (CTXT1(72).EQ.' '.AND.CTXT1(73).EQ.' ') THEN
C           need to move up action code two places
            UCIBF(58:58) = ' '
            UCIBF(59:59) = ' '
            UCIBF(60:60) = CTXT1(71)
          ELSE IF (CTXT1(73).EQ.' ') THEN
C           need to move up action code one place
            UCIBF(58:58) = ' '
            UCIBF(59:59) = CTXT1(71)
            UCIBF(60:60) = CTXT1(72)
          ELSE
C           just store action codes
            UCIBF(58:58) = CTXT1(71)
            UCIBF(59:59) = CTXT1(72)
            UCIBF(60:60) = CTXT1(73)
          END IF
        ELSE
C         in a conditional
          CALL QGETCT (I4,CLEN,TLEN,CTXT1)
          CAC(1) = CTXT1(135)
          CAC(2) = CTXT1(136)
          CAC(3) = CTXT1(137)
          IF (CTXT1(136).EQ.' '.AND.CTXT1(137).EQ.' ') THEN
C           need to move up action code two places
            UCIBF(58:58) = ' '
            UCIBF(59:59) = ' '
            UCIBF(60:60) = CTXT1(135)
          ELSE IF (CTXT1(137).EQ.' ') THEN
C           need to move up action code one place
            UCIBF(58:58) = ' '
            UCIBF(59:59) = CTXT1(135)
            UCIBF(60:60) = CTXT1(136)
          ELSE
C           just store action codes
            UCIBF(58:58) = CTXT1(135)
            UCIBF(59:59) = CTXT1(136)
            UCIBF(60:60) = CTXT1(137)
          END IF
        END IF
        CALL QGETOP (I1,OPVAL)
        DATFLG = OPVAL(1)
        IF (AINT.EQ.0) THEN
C         real number
          CALL QGETR (I1,AVAL)
          CALL QGETI (I5,IVAL)
          WRITE (UCIBF(61:70),2020) AVAL(1)
        ELSE
C         integer
          CALL QGETI (I6,IVAL)
          WRITE (UCIBF(61:70),2022) IVAL(1)
          AVAL(1) = FLOAT(IVAL(1))
          IVAL(1) = IVAL(2)
          IVAL(2) = IVAL(3)
          IVAL(3) = IVAL(4)
          IVAL(4) = IVAL(5)
          IVAL(5) = IVAL(6)
        END IF
        IF (DATFLG.EQ.1) THEN
C         no, undated special action
          WRITE (UCIBF(21:36),2040)
        ELSE
C         dated special action
          WRITE (UCIBF(21:36),2030) IVAL(1),IVAL(2),IVAL(3),
     1                              IVAL(4),IVAL(5)
        END IF
        CALL REPUCI (AKEY,UCIBF)
      END IF
C
      RETURN
      END
