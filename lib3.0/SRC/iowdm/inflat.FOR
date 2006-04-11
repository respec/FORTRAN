C
C
C
      SUBROUTINE   INFLAT
     I                   (MESSFL,WDMFL)
C
C     + + + PURPOSE + + +
C     Input attributes from a flat file to WDM.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     WDMFL  - Fortran unit number for WDM file
C
C     + + + PARAMETERS + + +
      INTEGER      MAXAT
      PARAMETER   (MAXAT=14)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I0,I1,I4,I64,SCLU,SGRP,IRET,IVAL(3),RETCOD,
     $             SFILE,IHEAD,FDSN,CLEN(MAXAT),TLEN,CNUM,INTFG,
     $             SORC,RPLCFG,MNSEL(2),MXSEL(2),OPVAL(2)
      CHARACTER*1  TBUFF(64),SANAMS(6,MAXAT),BLNK
      CHARACTER*64 AFMT
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZSTCMA, Q1INIT, Q1EDIT, QGETF, QSETOP, QGETOP, QGETI
      EXTERNAL     QSETCO, QGETCO, QSETI, INFREE, QSETCT, QGETCT, ATFLAT
      EXTERNAL     QSTCTF, QGTCTF, CARVAR, CVARAR, DISTOP, ZIPI, ZIPC
      EXTERNAL     ATNUDS, PRNTXT, QGTCOB, WDDSCK, QFCLOS, QUPCAS
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I4  = 4
      I64 = 64
      BLNK= ' '
C
      SCLU = 126
C
      TLEN = 6*MAXAT
      CALL ZIPC (TLEN,BLNK,SANAMS)
      AFMT = ' '
      INTFG= 1
C
 10   CONTINUE
C       back here on previous from ensuing screens
C       specify source flat file
        SGRP = 10
        CALL Q1INIT (MESSFL,SCLU,SGRP)
C       set attribute entry and data-set procesing options
        I = 2
        CALL ZIPI (I,I1,MXSEL)
        CALL ZIPI (I,I1,MNSEL)
        OPVAL(1) = 1
        OPVAL(2) = INTFG + 1
        CALL QSETOP (I,I,MXSEL,MNSEL,OPVAL)
        CALL ZSTCMA (I4,I1)
        CALL Q1EDIT (IRET)
        CALL ZSTCMA (I4,I0)
        IF (IRET.EQ.1) THEN
C         continue, get file name
          CALL QGETF (I1,
     O                IVAL)
          SFILE = IVAL(1)
C         get header lines
          CALL QGETI (I1,
     O                IVAL)
          IHEAD = IVAL(1)
C         get attribute entry option
          I = 2
          CALL QGETOP (I,
     O                 OPVAL)
          SORC  = OPVAL(1)
          INTFG = OPVAL(2) - 1
          IF (SORC.EQ.1) THEN
C           single entry of attributes
 50         CONTINUE
C             back here on previous from new data set screen
              SGRP = 20
              CALL Q1INIT (MESSFL,SCLU,SGRP)
C             get next available dsn
              CALL INFREE (WDMFL,I1,I1,I1,
     O                     IVAL(1),RETCOD)
C             set data-set number
              CALL QSETI (I1,IVAL)
C             set data set status and attribute overwrite flag
              IVAL(1) = 1
              IVAL(2) = 1
              CNUM = 2
              CALL QSETCO (CNUM,IVAL)
C             edit screen
              CALL ZSTCMA (I4,I1)
              CALL Q1EDIT (IRET)
              CALL ZSTCMA (I4,I0)
              IF (IRET.EQ.1) THEN
C               continue, get data-set number
                CALL QGETI (I1,
     O                      IVAL)
                FDSN = IVAL(1)
C               get data set status and attribute overwrite flag
                CALL QGETCO (CNUM,
     O                       IVAL)
                RPLCFG = IVAL(2) - 1
                AFMT   = '(A6,V80)'
                IF (IVAL(1).EQ.1) THEN
C                 new data set for attributes
                  CALL ATNUDS (MESSFL,SCLU,WDMFL,FDSN,INTFG,
     O                         IRET)
                  IF (IRET.EQ.0 .OR. IRET.EQ.2) THEN
C                   data-set already exists or user wants previous screen
                    IF (IRET.EQ.0) THEN
C                     tell user DSN already exists
                      SGRP = 41
                      CALL PRNTXT (MESSFL,SCLU,SGRP)
                    END IF
C                   back to Single attribute entry screen
                    IRET = -2
                  ELSE IF (INTFG.EQ.1) THEN
C                   data set created, no interaction for ensuing new data sets
                    INTFG = 0
                  END IF
                ELSE
C                 old data set, make sure it exists
                  CALL WDDSCK (WDMFL,FDSN,
     O                         I,RETCOD)
                  IF (RETCOD.NE.0) THEN
C                   data set does not exist
                    SGRP = 43
                    CALL PRNTXT (MESSFL,SCLU,SGRP)
                    IRET = -2
                  END IF
                END IF
              END IF
            IF (IRET.EQ.-2) GO TO 50
          ELSE
C           column entry of attributes
 100        CONTINUE
C             back here on problem with column specs
              SGRP = 30
              CALL Q1INIT (MESSFL,SCLU,SGRP)
C             set attribute name fields
              CNUM = MAXAT
              I = 6
              CALL ZIPI (CNUM,I,CLEN)
              TLEN = 6*MAXAT
              CALL QSETCT (CNUM,CLEN,TLEN,SANAMS)
              CALL CVARAR (I64,AFMT,I64,TBUFF)
C             set format field
              I = MAXAT + 1
              CALL QSTCTF (I,I64,TBUFF)
C             set attribute overwrite flag
              IVAL(1) = 1
              CALL QSETCO (I1,IVAL)
C             display top of source file
              I = 9
              CALL DISTOP (SFILE,IHEAD,I)
              CALL ZSTCMA (I4,I1)
              CALL Q1EDIT (IRET)
              CALL ZSTCMA (I4,I0)
              IF (IRET.EQ.1) THEN
C               continue, get attribute names
                CALL QGETCT (CNUM,CLEN,TLEN,
     O                       SANAMS)
C               get format
                I = MAXAT + 1
                CALL QGTCTF (I,I64,
     O                       TBUFF)
C               convert format to upper case
                CALL QUPCAS (I64,TBUFF)
                CALL CARVAR (I64,TBUFF,I64,AFMT)
C               get attribute overwrite flag
                I = MAXAT + 2
                CALL QGTCOB (I1,I,
     O                       IVAL)
                RPLCFG = IVAL(1) - 1
                IF (AFMT.EQ.' ') THEN
C                 can't process w/out format
                  SGRP = 31
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                  IRET = -2
                END IF
              END IF
            IF (IRET.EQ.-2) GO TO 100
          END IF
          IF (IRET.EQ.1) THEN
C           time to process attributes
            CALL ATFLAT (MESSFL,SCLU,WDMFL,SFILE,IHEAD,INTFG,
     I                   SORC,FDSN,RPLCFG,MAXAT,SANAMS,AFMT,IRET)
          END IF
C         close flat file
          CALL QFCLOS (SFILE,I0)
        ELSE
C         back to Input screen
          IRET = 1
        END IF
      IF (IRET.EQ.2) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   ATFLAT
     I                   (MESSFL,SCLU,WDMSFL,SFILE,IHEAD,INTFG,
     I                    SORC,FDSN,RPLCFG,MAXAT,SANAMS,AFMT,
     O                    IRET)
C
C     + + + PURPOSE + + +
C     Process attributes from flat file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU,WDMSFL,SFILE,IHEAD,INTFG,
     $             SORC,FDSN,RPLCFG,MAXAT,IRET
      CHARACTER*1  SANAMS(6,MAXAT)
      CHARACTER*64 AFMT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number for WDM file
C     SFILE  - Fortran unit number for source flat file
C     IHEAD  - number of header lines in source file
C     INTFG  - interaction flag,
C              0 - no user interaction, just create new data set
C             >0 - allow user to specify data-set type and space allocations
C     SORC   - single (1) or column (2) attribute entry
C     FDSN   - first data-set number to add attributes to (single entry only)
C     RPLCFG - replace existing attributes flat
C              0 - no, 1 -yes
C     MAXAT  - max number of attributes allowed in column entry
C     SANAMS - character array of attributes being added (column entry only)
C     AFMT   - format of attribute values on source file
C     IRET   - screen exit command code
C
C     + + + PARAMETERS + + +
      INTEGER    MXSA
      PARAMETER (MXSA=14)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,I0,I1,I6,I78,ILEN,IVAL(10),RETCOD,INIT,DONFG,
     $              SACNT,IPOS,ICNT,RPOS,DSNPOS,CDSN,NRVAL,LINTFG,
     $              MULCNT,SAIND(MXSA),SALEN(MXSA),SATYP(MXSA),LHEAD
      REAL          RVAL(MXSA),TRVAL(10)
      CHARACTER*1   CBLNK,SABUF(80)
      CHARACTER*6   LSANAM
      CHARACTER*78  EBUFF
      CHARACTER*255 LBUFF,BUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (LSANM1,LSANAM),(EBUF1,EBUFF)
      CHARACTER*1   LSANM1(6),      EBUF1(78)
C
C     + + + FUNCTIONS + + +
      INTEGER       LENSTR, CHRINT
C
C     + + + INTRINSICS + + +
      INTRINSIC     INDEX, INT, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL      LENSTR, CHRINT, ZBLDWR, WDBSAI, WDBSAR, WDBSAC
      EXTERNAL      WDBSGX, CVARAR, CHRCHR, ZLJUST, GFMVAL, QUPCAS
      EXTERNAL      ZSTCMA, ATNUDS, WDBSGI, WDBSGR, WDBSGC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA SAIND/MXSA*0/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A255)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('  Problem adding attribute ',A6,'   RETCOD ',I4)
 2010 FORMAT ('  Attribute name ',A6,' is unknown.')
 2020 FORMAT ('  Correct attribute name(s) and try again.')
 2030 FORMAT ('  Problem, DSN not found in attribue NAMES')
 2040 FORMAT ('  Processed',I3,' name(s) of attributes, DSN in pos:',I6)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I6 = 6
      I78= 78
      CBLNK = ' '
      INIT  = 1
      IRET  = 1
C
      IF (SORC.EQ.1) THEN
C       single attribute entry
        CDSN = FDSN
      ELSE
C       column attribute entry
        DSNPOS = 0
        ICNT   = 0
        I = 0
 50     CONTINUE
          I = I + 1
C         look for next attribute name
          IF (LENSTR(I6,SANAMS(1,I)).GT.0) THEN
C           something in name buffer to process
            ICNT = ICNT + 1
C           copy input name to local array and convert to upper case
            CALL CHRCHR (I6,SANAMS(1,I),LSANM1)
            CALL QUPCAS (I6,LSANM1)
            IF (LSANAM.EQ.'DSN') THEN
C             data-set numbers are in this column
              DSNPOS = ICNT
              SATYP(ICNT) = 0
              WRITE(99,*) '  DSN in positon',ICNT
            ELSE
C             get info about this attribute
              CALL WDBSGX (MESSFL,
     M                     LSANM1,
     O                     SAIND(ICNT),SATYP(ICNT),SALEN(ICNT))
              IF (SAIND(ICNT).EQ.0) THEN
C               bad attribute
                ICNT   = ICNT - 1
                DSNPOS = -1
C               let user know
                WRITE(EBUFF,2010) LSANAM
                CALL ZBLDWR (I78,EBUF1,INIT,I1,J)
                INIT = 0
C               kick out of loop
                I = MAXAT
              END IF
            END IF
          END IF
        IF (I.LT.MAXAT) GO TO 50
C       final checks
        IF (DSNPOS .EQ. -1) THEN
C         bad attribute name, kick out of processing
          WRITE(EBUFF,2020)
          CALL ZBLDWR (I78,EBUF1,INIT,I0,J)
          IRET = 2
        ELSE IF (DSNPOS .EQ. 0) THEN
C         must specify dsn in column mode
          WRITE(EBUFF,2030)
          CALL ZBLDWR (I78,EBUF1,INIT,I0,J)
          IRET = 2
        ELSE
C         ok so far, display number of attributes and position of DSN
          MULCNT = ICNT
          WRITE(EBUFF,2040) MULCNT-1,DSNPOS
C         allow previous in case they want to bail out
          I = 4
          CALL ZSTCMA (I,I1)
          CALL ZBLDWR (I78,EBUF1,INIT,I0,IRET)
          CALL ZSTCMA (I,I0)
        END IF
        INIT = 1
      END IF
C
      IF (IRET.EQ.1) THEN
C       add attribute values
        EBUFF = 'Processing attributes on flat file.'
        CALL ZBLDWR (I78,EBUF1,INIT,I1,IRET)
        LINTFG = INTFG
        DONFG  = 0
        RETCOD = 0
        LHEAD  = IHEAD
 100    CONTINUE
C         get next source file record
 150      CONTINUE
C           skip any header lines
            READ (SFILE,1000,END=170) BUFF
            LHEAD = LHEAD - 1
            GO TO 180
 170        CONTINUE
C             get here on end of file
              DONFG = 1
              SACNT = 0
 180        CONTINUE
          IF (LHEAD.GE.0) GO TO 150
          IF (DONFG.EQ.0) THEN
C           continue with attribute processing
            LBUFF = BUFF
C           process attribute(s)
            IF (SORC.EQ.1) THEN
C             processing one attribute per record
              CALL ZLJUST (LBUFF)
              IF (LBUFF(1:3).EQ.'DSN') THEN
C               change data set
                LBUFF = LBUFF(4:255)
                CALL ZLJUST (LBUFF)
                ILEN = INDEX(LBUFF,CBLNK)
                CDSN = CHRINT (ILEN-1,LBUFF)
C               see if data set needs creating
                CALL ATNUDS (MESSFL,SCLU,WDMSFL,CDSN,LINTFG,
     O                       IRET)
                IF (IRET.EQ.1 .AND. LINTFG.EQ.1) THEN
C                 new data set created, no interaction on ensuing new data sets
                  LINTFG = 0
                END IF
                SAIND(1) = -1
              ELSE
C               another attribute name/value pair
                ILEN  = INDEX(LBUFF,CBLNK)
                LSANAM= LBUFF(1:ILEN-1)
                CALL WDBSGX (MESSFL,
     M                       LSANM1,
     O                       SAIND(1),SATYP(1),SALEN(1))
              END IF
              IF (SAIND(1).GT.0) THEN
                SACNT = 1
              ELSE
                SACNT = 0
              END IF
            ELSE
              SACNT = MULCNT
            END IF
          END IF
          IF (SACNT.GT.0) THEN
C           valid attributes to process, read record containing value(s)
            CALL GFMVAL (AFMT,LBUFF,SATYP,
     O                   IVAL,NRVAL,RVAL)
            IF (NRVAL.NE.SACNT) THEN
C             number of values read not the same as number expected
              EBUFF = 'Number of attribute values read differs '//
     $                'from number defined by FORMAT.'
              CALL ZBLDWR (I78,EBUF1,INIT,I1,J)
              INIT = 0
            ELSE
              IF (SORC.EQ.2) THEN
C               get data-set number for this record of attribute values
                CDSN  = RVAL(DSNPOS)
C               see if data set needs creating
                CALL ATNUDS (MESSFL,SCLU,WDMSFL,CDSN,LINTFG,
     O                       IRET)
                IF (IRET.EQ.1 .AND. LINTFG.EQ.1) THEN
C                 new data set created, no interaction on ensuing new data sets
                  LINTFG = 0
                END IF
              END IF
              ICNT = 1
              RPOS = 1
 200          CONTINUE
C               process next value from buffer
                IF (SATYP(ICNT).EQ.0) THEN
C                 data-set number, skip to next value
                  RPOS = RPOS + 1
                ELSE IF (SATYP(ICNT).EQ.1) THEN
C                 integer attribute, see if it already is on this data set
                  CALL WDBSGI (WDMSFL,CDSN,SAIND(ICNT),SALEN(ICNT),
     O                         IVAL,RETCOD)
                  IF (RETCOD.EQ.-107 .OR. RPLCFG.EQ.1) THEN
C                   ok to store attribute on data set
                    DO 250 I = 1,SALEN(ICNT)
C                     process attribute values
                      IVAL(I)= RVAL(RPOS+I-1)
 250                CONTINUE
                    CALL WDBSAI (WDMSFL,CDSN,MESSFL,
     I                           SAIND(ICNT),SALEN(ICNT),IVAL,
     O                           RETCOD)
                  END IF
                  RPOS = RPOS + SALEN(ICNT)
                ELSE IF (SATYP(ICNT).EQ.2) THEN
C                 real attribute, see if it already is on this data set
                  CALL WDBSGR (WDMSFL,CDSN,SAIND(ICNT),SALEN(ICNT),
     O                         TRVAL,RETCOD)
                  IF (RETCOD.EQ.-107 .OR. RPLCFG.EQ.1) THEN
C                   ok to store attribute on data set
                    CALL WDBSAR (WDMSFL,CDSN,MESSFL,
     I                           SAIND(ICNT),SALEN(ICNT),RVAL(RPOS),
     O                           RETCOD)
                  END IF
                  RPOS = RPOS + SALEN(ICNT)
                ELSE IF (SATYP(ICNT).EQ.3) THEN
C                 character attribute, see if it already is on this data set
                  CALL WDBSGC (WDMSFL,CDSN,SAIND(ICNT),SALEN(ICNT),
     O                         SABUF,RETCOD)
                  IF (RETCOD.EQ.-107 .OR. RPLCFG.EQ.1) THEN
C                   ok to store attribute on data set
                    ILEN = MOD(INT(RVAL(RPOS)),100)
                    IPOS = INT(RVAL(RPOS)/100)
                    CALL CVARAR (ILEN,LBUFF(IPOS:IPOS+ILEN-1),
     I                           SALEN(ICNT),
     O                           SABUF)
                    CALL WDBSAC (WDMSFL,CDSN,MESSFL,
     I                           SAIND(ICNT),SALEN(ICNT),SABUF,
     O                           RETCOD)
                  END IF
                  RPOS = RPOS + 1
                END IF
                ICNT = ICNT + 1
              IF (ICNT.LE.SACNT) GO TO 200
            END IF
          ELSE IF (DONFG.EQ.0 .AND. SAIND(1).EQ.0) THEN
C           unknown attribute
            RETCOD= 1
          END IF
C
          IF (RETCOD.NE.0) THEN
C           problem writing out attributes
            WRITE(EBUFF,2000) LSANAM,RETCOD
            CALL ZBLDWR (I78,EBUF1,INIT,I1,J)
            INIT = 0
            RETCOD= 0
          END IF
        IF (DONFG.EQ.0) GO TO 100
        EBUFF = 'Completed processing of attributes from source file.'
        CALL ZBLDWR (I78,EBUF1,INIT,I0,J)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ATNUDS
     I                   (MESSFL,SCLU,WDMFL,DSN,INTFG,
     O                    IRET)
C
C     + + + PURPOSE + + +
C     Create new data set DSN if it does not exist already.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMFL,DSN,INTFG,IRET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WDMFL  - Fortran unit number for WDM file
C     DSN    - number of data set being created
C     INTFG  - interaction flag,
C              0 - no user interaction, just create new data set
C             >0 - allow user to specify data-set type and space allocations
C     IRET   - user exit command value from screen OR
C              indicator that DSN already exists (IRET = 0)
C
C     + + + SAVES + + +
      INTEGER    DSTYPE,NSA,NSASP,NDP
      SAVE       DSTYPE,NSA,NSASP,NDP
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I0,I1,SGRP,IDUM,RETCOD,IVAL(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDDSCK, WDLBAX, ZSTCMA, PMXTXI, ZMNSST
      EXTERNAL   Q1INIT, Q1EDIT, QSETCO, QSETI, QGETCO, QGETI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA DSTYPE,NSA,NSASP,NDP /1, 100, 120, 10/
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
      CALL WDDSCK (WDMFL,DSN,
     O             IDUM,RETCOD)
      IF (RETCOD.NE.0 .AND. INTFG.GT.0) THEN
C       data set does not exist, get parameters for creating it
        SGRP = 42
        IVAL(1) = DSN
        CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,-I1,I1,IVAL)
        CALL ZMNSST
        SGRP = 40
        CALL Q1INIT (MESSFL,SCLU,SGRP)
C       set data-set type
        IVAL(1) = DSTYPE
        I = 1
        CALL QSETCO (I,IVAL)
C       set pointer values
        IVAL(1) = NSA
        IVAL(2) = NSASP
        IVAL(3) = NDP
        I = 3
        CALL QSETI (I,IVAL)
C       allow previous
        I = 4
        CALL ZSTCMA (I,I1)
        CALL Q1EDIT (IRET)
C       turn off previous
        CALL ZSTCMA (I,I0)
        IF (IRET.EQ.1) THEN
C         get data-set type
          I = 1
          CALL QGETCO (I,
     O                 IVAL)
          DSTYPE = IVAL(1)
          I = 3
          CALL QGETI (I,
     O                IVAL)
          NSA = IVAL(1)
          NSASP = IVAL(2)
          NDP = IVAL(3)
        ELSE
C         set return code to not create new data set
          RETCOD = 0
        END IF
      ELSE IF (RETCOD.EQ.0) THEN
C       data set already exists can't create it
        RETCOD = 0
        IRET = 0
      END IF
C
      IF (RETCOD.NE.0) THEN
C       create new data set
        I = 1
        CALL WDLBAX (WDMFL,DSN,DSTYPE,I,I,
     I               NSA,NSASP,NDP,
     O               IDUM)
      END IF
C
      RETURN
      END
