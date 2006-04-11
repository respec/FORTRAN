C
C
C
      SUBROUTINE   DSNSEL
     I                   (MESSFL,MXDSN,
     M                    SPSEN,SPLOC,SPCON,
     O                    DSNCNT,DSNBUF)
C
C     + + + PURPOSE + + +
C     Select data set(s) by specifying scenario, location, and constituent.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,MXDSN,DSNCNT,DSNBUF(MXDSN)
      CHARACTER*8 SPSEN,SPLOC,SPCON
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     MXDSN  - max number of data sets to select
C     SPSEN  - scenario specification
C     SPLOC  - location specification
C     SPCON  - constituent specification
C     DSNCNT - count of data sets selected
C     DSNBUF - array of data-set numbers selected
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I1,I4,I8,SCLU,SGRP,ILEN,OLEN,IPOS,
     $            CNUM,CLEN(4),TLEN,IVAL(1),IRET
      CHARACTER*1 CTXT(94),CNONE(4),BLNK(1)
C
C     + + + FUNCTIONS + + +
      INTEGER     STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRFND, Q1INIT, Q1EDSC, QSETCT, QGETCT, QSETI, QSTCTF
      EXTERNAL    ZRFRSH, CVARAR, CARVAR, TSPARF, TSESPC, DSMTCH
      EXTERNAL    INTCHR, ZIPC, ZIPI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CNONE/'n','o','n','e'/
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I4 = 4
      I8 = 8
      BLNK(1) = ' '
C
      SCLU = 201
C
      CNUM = 3
      CALL ZIPI (CNUM,I8,CLEN)
      CLEN(4) = 70
      TLEN = I8 * CNUM + CLEN(4)
      SGRP = 1
      CALL Q1INIT (MESSFL,SCLU,SGRP)
C     set current text field values
      CALL CVARAR (I8,SPSEN,I8,CTXT(1))
      CALL CVARAR (I8,SPLOC,I8,CTXT(9))
      CALL CVARAR (I8,SPCON,I8,CTXT(17))
      CALL QSETCT (CNUM,CLEN,TLEN,CTXT)
C     allow partial name matching
      CALL TSPARF (I1)
 10   CONTINUE
C       set current match specs
        CALL TSESPC (SPSEN,SPLOC,SPCON)
C       search for matches
        CALL DSMTCH (MXDSN,
     O               DSNCNT,DSNBUF)
        IVAL(1) = DSNCNT
        CALL QSETI (I1,IVAL)
        CALL ZIPC (CLEN(4),BLNK,CTXT(25))
        IF (DSNCNT.GT.0) THEN
C         display data-set numbers
          ILEN = 5
          IPOS = 25
          I = 0
 50       CONTINUE
C           put data-set numbers into text buffer
            I = I + 1
            CALL INTCHR (DSNBUF(I),ILEN,I1,
     O                   OLEN,CTXT(IPOS))
            IPOS = IPOS + OLEN
            IF (I.LT.DSNCNT) THEN
C             more data sets to come
              CTXT(IPOS) = ','
              IPOS = IPOS + 2
              IF (IPOS.GT.84) THEN
C               no more room, use '...'
                CTXT(IPOS) = '.'
                IPOS = IPOS + 1
                CTXT(IPOS) = '.'
                IPOS = IPOS + 1
                CTXT(IPOS) = '.'
                I = DSNCNT
              END IF
            END IF
          IF (I.LT.DSNCNT) GO TO 50
        END IF
        CALL QSTCTF (I4,CLEN(4),CTXT(25))
        CALL ZRFRSH (I1)
        CALL Q1EDSC (IRET)
C       get edited text fields
        CALL QGETCT (CNUM,CLEN,TLEN,
     O               CTXT)
        IF (STRFND(I8,CTXT(1),I4,CNONE).EQ.0) THEN
C         not 'none', use whats in field for spec
          CALL CARVAR (I8,CTXT(1),I8,SPSEN)
        END IF
        IF (STRFND(I8,CTXT(9),I4,CNONE).EQ.0) THEN
C         not 'none', use whats in field for spec
          CALL CARVAR (I8,CTXT(9),I8,SPLOC)
        END IF
        IF (STRFND(I8,CTXT(17),I4,CNONE).EQ.0) THEN
C         not 'none', use whats in field for spec
          CALL CARVAR (I8,CTXT(17),I8,SPCON)
        END IF
      IF (IRET.NE.1 .AND. IRET.NE.2) GO TO 10
C
C     turn off partial name matching flag
      CALL TSPARF (I0)
C
      IF (DSNCNT.GT.MXDSN) THEN
C       limit count to max allowable
        DSNCNT = MXDSN
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DSMTCH
     I                   (MXDSN,
     O                    DSNCNT,DSNBUF)
C
C     + + + PURPOSE + + +
C     Search for data sets matching specified
C     scenario, location, constituent
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MXDSN,DSNCNT,DSNBUF(MXDSN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MXDSN  - max number of data sets to select
C     DSNCNT - number of data sets matching criteria
C     DSNBUF - array of selected data set numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,IDSN
C
C     + + + EXTERNALS + + +
      EXTERNAL   TSDSM, ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
C     init output args
      DSNCNT = 0
      I = 0
      CALL ZIPI (MXDSN,I,DSNBUF)
C
      IDSN = 0
 10   CONTINUE
C       check for next matching data set
        CALL TSDSM (IDSN)
        IF (IDSN.GT.0) THEN
C         found another match
          DSNCNT = DSNCNT + 1
          IF (DSNCNT.LE.MXDSN) THEN
C           room for more in buffer
            DSNBUF(DSNCNT) = IDSN
          END IF
C         resume search starting at next data-set number
          IDSN = IDSN + 1
        END IF
      IF (IDSN.NE.0) GO TO 10
C
      RETURN
      END
