C
C
C
      SUBROUTINE   PRWARC
     I                   (MESSFL,WDMSFL)
C
C     + + + PURPOSE + + +
C     Import to, export from, or dump WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     WDMSFL - Fortran unit number for WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SCLU,SGRP,RESP
C
C     + + + EXTERNALS + + +
      EXTERNAL   QRESP, PRWMIM, PRWMEX, WDMDMP
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 30
C
 10   CONTINUE
C       do archive menu
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
        GO TO (100,200,300,400), RESP
C
 100    CONTINUE
C         import datasets
          CALL PRWMIM (MESSFL,WDMSFL,SCLU)
          GO TO 900
C
 200    CONTINUE
C         export datasets
          CALL PRWMEX (MESSFL,WDMSFL,SCLU)
          GO TO 900
C
 300    CONTINUE
C         dump WDM records
          CALL WDMDMP (MESSFL,WDMSFL,SCLU)
          GO TO 900
C
 400    CONTINUE
C         back to opening screen
          GO TO 900
C
 900    CONTINUE
      IF (RESP.NE.4) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMIM
     I                    (MESSFL,WDMSFL,SCLU)
C
C     inport WDM dataset from sequential file to WDMSFL
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL,SCLU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SCLU   - cluster number on message file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,I0,I1,I3,I4,I6,I10,I40,I80,SGRP,
     1            DONFG,SKIPFG,DSN,DSTYPE,PSA,NDN,NUP,NSA,NSP,NDP,
     2            SAIND,SATYP,SALEN,IVAL(10),SUCIFL,RETCOD,DREC,IRET
      REAL        RVAL(10)
      CHARACTER*1 BUFF(160),CDSN(3),CEND(3),CCOM(3),CLAB(3),
     1            CPOI(3),CDAT(3),CTYPE(40),CCLU(3)
C
C     + + + FUNCTIONS + + +
      INTEGER     STRFND, CHRINT, LENSTR
      REAL        CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRFND, CHRINT, LENSTR, CHRDEC, WDDSCK, WDDSDL
      EXTERNAL    QFCLOS, QFOPEN, QRESP,  QRESPI, CHRCHR, WDBSAC
      EXTERNAL    WDBSAI, WDBSAR, WDBSGX, WDLBAX, PMXCNW, PRNTXT
      EXTERNAL    PRWMTI, PRWMXI, PRWMDI, PRWMSI, PRWMAI, PMXTXI
      EXTERNAL    ZSTCMA, ZGTRET, PMXTXT, ZBLDWR, ZMNSST, GETTXT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I3,I4,I6,I10,I40,I80/0,1,3,4,6,10,40,80/
      DATA CCLU/'C','L','U'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (80A1)
C
C     + + + END SPECIFICATIONS + + +
C
C     open import file
      SGRP= 11
      CALL QFOPEN (MESSFL,SCLU,SGRP,SUCIFL,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       make interrupt available
        I= 16
        CALL ZSTCMA(I,I1)
C       get needed block headings from messfl
        I   = I80
        SGRP= 12
        CALL GETTXT (MESSFL,SCLU,SGRP,
     M               I,
     O               BUFF)
        CALL CHRCHR (I3,BUFF(1),CDSN)
        CALL CHRCHR (I3,BUFF(5),CEND)
        CALL CHRCHR (I3,BUFF(9),CCOM)
        CALL CHRCHR (I3,BUFF(13),CLAB)
        CALL CHRCHR (I3,BUFF(17),CPOI)
        CALL CHRCHR (I3,BUFF(21),CDAT)
        CALL CHRCHR (I3,BUFF(13),CLAB)
C       get text dataset types
        I   = I40
        SGRP= 13
        CALL GETTXT (MESSFL,SCLU,SGRP,I,
     O               CTYPE)
C
        SGRP= 14
        CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,-I1,J)
C
C       loop to write out general comments
        DONFG= 0
 10     CONTINUE
          READ (SUCIFL,1000) (BUFF(I),I=1,I80)
          IF (STRFND(I4,BUFF,I3,CDSN).EQ.0 .AND.
     1        STRFND(I4,BUFF,I3,CCLU).EQ.0) THEN
C           general comment line, write it to terminal
            I= LENSTR(I80-2,BUFF)
            CALL ZBLDWR (I,BUFF,I0,-I1,J)
          ELSE
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 10
C       blank line to hold screen
        BUFF(1)= ' '
        CALL ZBLDWR (I1,BUFF,I0,I0,J)
        IF (J .EQ. 7) THEN
C         interrupt user wants out
          RETCOD= 1
        END IF
      END IF
C
      IF (RETCOD .EQ. 0) THEN
 15     CONTINUE
C         process a dsn
          DSN= CHRINT(I6,BUFF(11))
          IF (DSN.GT.0) THEN
C           record contains dataset number
            J     = STRFND(I40,CTYPE,I4,BUFF(27))
            DSTYPE= 1+ ((J-1)/ 4)
            NDN   = CHRINT(I3,BUFF(38))
            IF (NDN.EQ.0) THEN
C             default
              NDN= 10
            END IF
            NUP= CHRINT(I3,BUFF(48))
            IF (NUP.EQ.0) THEN
C             default
              NUP= 20
            END IF
            NSA= CHRINT(I3,BUFF(58))
            IF (NSA.EQ.0) THEN
C             default
              NSA= 20
            END IF
            NSP= CHRINT(I3,BUFF(68))
            IF (NSP.EQ.0) THEN
C             default
              NSP= 50
            END IF
            NDP= CHRINT(I3,BUFF(78))
            IF (NDP.EQ.0) THEN
C             default
              NDP= 100
            END IF
C
            SGRP= 15
            CALL PMXTXI (MESSFL,SCLU,SGRP,I3,I1,I1,I1,DSN)
C
C           loop to find valid dsn to write or skip
 20         CONTINUE
              SKIPFG= 0
              CALL WDDSCK (WDMSFL,DSN,DREC,IRET)
              IF (IRET .EQ. 0) THEN
C               dsn exists, change, skip, or add data?
                SGRP= 15
                CALL PMXTXI (MESSFL,SCLU,SGRP,I3,I1,-I1,I1,DSN)
                CALL ZMNSST
                SGRP= 16
                CALL QRESP (MESSFL,SCLU,SGRP,SKIPFG)
C               check exit code
                CALL ZGTRET(IRET)
                IF (IRET .EQ. 7) THEN
C                 user wants out
                  SKIPFG= 5
                END IF
                IF (SKIPFG .EQ. 1) THEN
C                 make prev available
                  I= 4
                  CALL ZSTCMA(I,I1)
C                 change dsn to write
                  I   = DSN
                  DSN = -999
                  SGRP= 17
                  CALL QRESPI (MESSFL,SCLU,SGRP,DSN)
C                 check exit code
                  CALL ZGTRET(IRET)
                  IF (IRET .EQ. 2) THEN
C                   user selected prev
                    DSN   = I
                  ELSE IF (IRET .EQ. 7) THEN
C                   user wants out
                    SKIPFG= 5
                  END IF
C                 turn off prev
                  I= 4
                  CALL ZSTCMA(I,I0)
                ELSE IF (SKIPFG .EQ. 4) THEN
C                 overwrite, delete current dataset
                  CALL WDDSDL (WDMSFL,DSN,
     O                         RETCOD)
                  SKIPFG= 0
                END IF
              END IF
            IF (SKIPFG .EQ. 1) GO TO 20
C
            IF (SKIPFG .EQ. 0) THEN
C             copy label from import file to new dsn, first add label
              CALL WDLBAX (WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSP,NDP,
     O                     PSA)
C             next attributes
 30           CONTINUE
                READ (SUCIFL,1000) (BUFF(I),I=1,I80)
                DONFG= STRFND(I3,BUFF(3),I3,CDAT)
              IF (STRFND(I3,BUFF(3),I3,CLAB).EQ.0.AND.DONFG.EQ.0)
     *          GOTO 30
C
              IF (DONFG.EQ.0) THEN
C               now in attributes
 40             CONTINUE
                  READ (SUCIFL,1000) (BUFF(I),I=1,I80)
C                 are we at end?
                  IF (STRFND(I3,BUFF(3),I3,CEND).GT.0) THEN
C                   yes, get out of this loop
                    DONFG= 2
                    SAIND= 0
                  ELSE
C                   which attribute
                    CALL WDBSGX (MESSFL,BUFF(5),
     O                           SAIND,SATYP,SALEN)
                  END IF
                  IF (SAIND.GT.0) THEN
C                   valid attribute type
                    GOTO (41,43,45), SATYP
C
 41                 CONTINUE
C                     integer attribute
                      J= 1
                      DO 42 I= 1,SALEN
                        J= J+ 10
                        IF (J.GT.I80) THEN
                          J= J+ 10
                          READ(SUCIFL,1000) (BUFF(K),K=81,160)
                        END IF
                        IVAL(I)= CHRINT(I10,BUFF(J))
 42                   CONTINUE
                      CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,SALEN,IVAL,
     O                             RETCOD)
                      GO TO 50
C
 43                 CONTINUE
C                     real attribute
                      J= 1
                      DO 44 I= 1,SALEN
                        J= J+ 10
                        IF (J.GT.I80) THEN
                          J= J+ 10
                          READ(SUCIFL,1000) (BUFF(K),K=81,160)
                        END IF
                        RVAL(I)= CHRDEC(I10,BUFF(J))
 44                   CONTINUE
                      CALL WDBSAR (WDMSFL,DSN,MESSFL,SAIND,SALEN,RVAL,
     O                             RETCOD)
                      GO TO 50
C
 45                 CONTINUE
C                     character attribute
                      IF ((SALEN+12).GT.I80) THEN
                        READ (SUCIFL,1000) (BUFF(K),K=81,148)
                      END IF
                      CALL WDBSAC (WDMSFL,DSN,MESSFL,
     I                             SAIND,SALEN,BUFF(13),
     O                             RETCOD)
                      GO TO 50
C
 50                 CONTINUE
                  ELSE IF (DONFG .EQ. 0) THEN
C                   unknown attribute
                    RETCOD= 1
                  END IF
C
                  IF (RETCOD .NE. 0) THEN
C                   problem writing out attributes
                    SGRP= 18
                    CALL PMXTXI (MESSFL,SCLU,SGRP,I3,-I1,I0,I1,RETCOD)
C                   check exit code
                    CALL ZGTRET(IRET)
                    IF (IRET .EQ. 7) THEN
C                     user wants out
                      SKIPFG= 5
                    END IF
C                   reset return code
                    RETCOD= 0
                  END IF
C                 look for more attributes
                IF (DONFG .EQ. 0 .AND. SKIPFG .NE. 5) GO TO 40
              ELSE
C               no attributes found to write on a new dataset, skip
                SGRP= 19
                CALL PMXTXT (MESSFL,SCLU,SGRP,I3,-I1)
C               check exit code
                CALL ZGTRET(IRET)
                IF (IRET .EQ. 7) THEN
C                 user wants out
                  SKIPFG= 5
                ELSE
C                 skip
                  SKIPFG= 2
                END IF
              END IF
C
C             **** ADD POINTERS HERE SOMEDAY ****
C
            END IF
C
            IF (SKIPFG .NE. 2 .AND. SKIPFG .NE. 5) THEN
C             skip to data
              DONFG= 0
 60           CONTINUE
                READ (SUCIFL,1000) (BUFF(I),I=1,I80)
                IF (STRFND(I3,BUFF,I3,CEND).GT.0) DONFG= 1
              IF (STRFND(I3,BUFF(3),I3,CDAT).EQ.0
     1            .AND. DONFG.EQ.0) GOTO 60
C
              IF (DONFG.EQ.0) THEN
C               data exists, now input it
                GO TO (61,62,70,70,65,70,70,68,69),DSTYPE
C
 61             CONTINUE
C                 timeseries data
                  CALL PRWMTI (WDMSFL,SUCIFL,DSN,
     O                         RETCOD)
                  GO TO 80
C
 62             CONTINUE
C                 table data
                  CALL PRWMXI (MESSFL,WDMSFL,SUCIFL,DSN,
     O                         RETCOD)
                  GO TO 80
C
 65             CONTINUE
C                 DLG data
                  CALL PRWMDI (WDMSFL,SUCIFL,DSN,
     O                         RETCOD)
                  GO TO 80
C
 68             CONTINUE
C                 attribute datasets
                  CALL PRWMAI (WDMSFL,SUCIFL,DSN,
     O                         RETCOD)
                  GO TO 80
C
 69             CONTINUE
C                 message file data
                  CALL PRWMSI (WDMSFL,SUCIFL,DSN,
     O                         RETCOD)
                  GO TO 80
C
 70             CONTINUE
                  SGRP= 20
                  CALL PMXTXI (MESSFL,SCLU,SGRP,I3,-I1,I0,I1,DSTYPE)
C                 check exit code
                  CALL ZGTRET(IRET)
                  IF (IRET .EQ. 7) THEN
C                   user wants out
                    SKIPFG= 5
                  END IF
                  GO TO 80
C
 80             CONTINUE
              END IF
            END IF
          END IF
C
          IF (SKIPFG .NE. 5) THEN
C           look for more dsns
 800        CONTINUE
              READ (SUCIFL,1000,END=900) (BUFF(I),I=1,I80)
            IF (STRFND(I4,BUFF,I3,CDSN).EQ.0 .AND.
     1          STRFND(I4,BUFF,I3,CCLU).EQ.0) GO TO 800
          END IF
        IF (SKIPFG .NE. 5) GO TO 15
 900    CONTINUE
C       turn off interrupt
        I= 16
        CALL ZSTCMA(I,I0)
C       close import file
        I= 0
        CALL QFCLOS(SUCIFL,I)
        IF (SKIPFG .NE. 5) THEN
C         end of file, all datasets copied
          SGRP= 21
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        ELSE
C         import interrupted
          SGRP= 22
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        END IF
      ELSE
C       turn off interrupt
        I= 16
        CALL ZSTCMA(I,I0)
C       couldnt open import file or user didnt like comments
        SGRP= 23
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMEX
     I                    (MESSFL,WDMSFL,SCLU)
C
C     export WDM dataset from WDMSFL to sequential file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL,SCLU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SCLU   - cluster number on message file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdsn.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SGRP,RETCOD,I78,I80,I10,I1,I0,IPAU,IRET,DLEN,
     1            I,J,JW,K,L,L1,L2,LEN,DSN,DSTYPE,DSFREC,JUST,
     2            ATANS,DTANS,DIND,SAMAX,PSA,PSIND,DSAIND,RESP,
     3            SALEN,SATYP,PSAVAL,WRTFG,DONFG,LSDAT(6),LEDAT(6),
     4            EXSDAT(6),EXEDAT(6),EXDTFG,SUCIFL,
     5            DSMXTP,DSNXT,DREC,PFDSN,DSCOUN,
     6            CNUM,IVAL,CVAL(5,3),RETC,GPFLG,INIT,EXPFG
      REAL        RVAL
      DOUBLE PRECISION DVAL
      CHARACTER*1 CDSN(80),CSTR(52),BUFF(80),BLNK,CBUFF(78,5),CTYPE(40)
      CHARACTER*4 CDUM
      CHARACTER*8 PTHNAM
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR, WDRCGO, TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, WDRCGO, TIMCHK, CHRCHR, DECCHR, GETTXT
      EXTERNAL    QFCLOS, QFOPEN, QRESP, ZIPC, QRESPM, WDSAGY
      EXTERNAL    WDDSCK, PRWMSE, PMXTXI, WTFNDT, ZIPI
      EXTERNAL    PRWMTE, PRWMXE, PRWMDE, PRWMME, PRWMAE, QRESCZ
      EXTERNAL    ZSTCMA, ZGTRET, INTCHR, PRNTXT, WDATCP, DATESQ
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I10,I78,I80,JUST/0,1,10,78,80,0/
      DATA BLNK/' '/
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (4A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (80A1)
 2010 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      DLEN  = 6
      EXDTFG= 0
C     export not performed yet
      EXPFG = 0
C     init to export data and attributes (with pause)
      ATANS = 1
      DTANS = 1
      IPAU  = 1
C     init comment buffer
      I= 5* I78
      CALL ZIPC (I,BLNK,CBUFF)
C     default to archive all datasets
      DSNIND= -1
      DSMXTP= 9
      DSNXT = 0
      DSTYPE= 0
C     init output file to 'not opened'
      SUCIFL= 0
C
 10   CONTINUE
C       do main Eport menu
        SGRP= 31
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
        GO TO (100,200,300,400,500,600), RESP
C
 100    CONTINUE
C         define file for export output
          IF (SUCIFL.GT.0) THEN
C           file opened already, close it
            CALL QFCLOS (SUCIFL,I0)
            SUCIFL= 0
          END IF
C         open export file
          SGRP  = 32
          CALL QFOPEN (MESSFL,SCLU,SGRP,
     O                 SUCIFL,RETCOD)
          IF (RETCOD.NE.0) THEN
C           still no output file opened
            SUCIFL= 0
          END IF
          GO TO 900
C
 200    CONTINUE
C         select datasets to export
          PTHNAM= 'AE'
          CALL PRWMSE (MESSFL,WDMSFL,DSNBMX,PTHNAM,
     M                 DSNBUF,DSNCNT)
          IF (DSNCNT.GT.0) THEN
C           datasets selected, init index
            DSNIND= 0
          ELSE
C           no data sets selected, set to export all
            DSNIND= -1
          END IF
          GO TO 900
C
 300    CONTINUE
C         define whether or not to output attributes and data?
          CNUM = 3
          CVAL(1,1) = ATANS
          CVAL(2,1) = DTANS
          CVAL(3,1) = IPAU
          SGRP = 38
          CALL QRESPM (MESSFL,SCLU,SGRP,I1,I1,CNUM,
     M                 IVAL,RVAL,CVAL,BUFF)
          ATANS = CVAL(1,1)
          DTANS = CVAL(2,1)
          IPAU  = CVAL(3,1)
          GO TO 900
C
 400    CONTINUE
C         specify comments for export file
          CNUM= 5
          SGRP= 33
          CALL QRESCZ (MESSFL,SCLU,SGRP,I1,I1,I1,CNUM,I1,I1,5*I78,
     M                 IVAL,RVAL,DVAL,CVAL,CBUFF)
          GO TO 900
C
 500    CONTINUE
C         perform export
          IF (SUCIFL.GT.0) THEN
C           ok to export, get headings from messfl
            I   = 52
            SGRP= 35
            CALL GETTXT (MESSFL,SCLU,SGRP,I,
     O                   CSTR)
C           write out general headings
C           **** need to add date and names ****
C           date
            WRITE (SUCIFL,2000) (CSTR(I),I=9,16)
C           wdmsfl
            WRITE (SUCIFL,2000) (CSTR(I),I=1,8)
C           system
            WRITE (SUCIFL,2000) (CSTR(I),I=17,24)
C           comment
            WRITE (SUCIFL,2000) (CSTR(I),I=25,32)
            K= 0
 510        CONTINUE
C             export any comment lines containing text
              K= K+ 1
              I= LENSTR(I78,CBUFF(1,K))
              IF (I.GT.0) THEN
C               more comments
                WRITE (SUCIFL,2000) BLNK,BLNK,(CBUFF(J,K),J=1,I)
              END IF
            IF (K.LT.5) GO TO 510
C           end comments
            WRITE (SUCIFL,2000) (CSTR(J),J=49,52),(CSTR(I),I=25,32)
C
C           get dsn types
            I   = 40
            SGRP= 34
            CALL GETTXT (MESSFL,SCLU,SGRP,I,
     O                   CTYPE)
C
            DSCOUN= 0
C
C           make interrupt available
            I= 16
            CALL ZSTCMA(I,I1)
C
 520        CONTINUE
C             write dsn loop
              DSCOUN= DSCOUN+ 1
C             next message should clear the screen
              INIT= 1
C             keep going
              IF (DSNIND .EQ. -1) THEN
C               get next dataset to archive from directory
 525            CONTINUE
                  IF (DSNXT.EQ.0) THEN
C                   no datasets of current type, try next one
                    DSTYPE= DSTYPE+ 1
                    IF (DSTYPE.LE.DSMXTP) THEN
C                     get directory record
                      DREC = 1
                      DIND = WDRCGO(WDMSFL,DREC)
                      PFDSN= PTSNUM+ (DSTYPE- 1)* 2+ 1
                      DSNXT= WIBUFF(PFDSN,DIND)
                    END IF
                  END IF
                IF (DSNXT.EQ.0 .AND. DSTYPE.LT.DSMXTP) GO TO 525
C
                DSN= DSNXT
C
                IF (DSN.GT.0) THEN
C                 get next dsn
                  CALL WDDSCK (WDMSFL,DSN,DREC,RETC)
                  DIND = WDRCGO(WDMSFL,DREC)
                  DSNXT= WIBUFF(2,DIND)
                END IF
              ELSE
C               dsn from buffer
                DSNIND= DSNIND+ 1
                IF (DSNIND.LE.DSNCNT) THEN
                  DSN= DSNBUF(DSNIND)
                ELSE
C                 all dsn written
                  DSN= 0
                END IF
              END IF
C
              IF (DSN.GT.0) THEN
C               another dataset to export
                CALL WDDSCK (WDMSFL,DSN,DSFREC,RETC)
                IF (RETC.NE.0) THEN
C                 dataset doesnt exist
                  SGRP= 36
                  CALL PMXTXI (MESSFL,SCLU,SGRP,I10,INIT,I0,I1,DSN)
                ELSE
                  DIND= WDRCGO(WDMSFL,DSFREC)
C                 beginning message
                  SGRP= 39
                  CALL PMXTXI (MESSFL,SCLU,SGRP,I10,INIT,I1,I1,DSN)
C                 next text message should not clear screen
                  INIT= -1
C                 get template dsn record
                  I   = 80
                  SGRP= 37
                  CALL GETTXT (MESSFL,SCLU,SGRP,I,
     O                         CDSN)
C                 fill in dsn
                  LEN= 6
                  CALL INTCHR (DSN,LEN,JUST,
     O                         J,CDSN(11))
C                 fill in type
                  DSTYPE= WIBUFF(6,DIND)
                  IF (DSTYPE.EQ.9) THEN
C                   message data set, use CLU instead of DSN for header
                    CDSN(1)= 'C'
                    CDSN(2)= 'L'
                    CDSN(3)= 'U'
                  END IF
                  I= 4
                  CALL CHRCHR (I,CTYPE((DSTYPE*4)-3),CDSN(27))
C                 fill in ndn
                  K= WIBUFF(9,DIND)- WIBUFF(8,DIND)- 1
                  LEN= 3
                  CALL INTCHR (K,LEN,JUST,
     O                         J,CDSN(38))
C                 fill in nup
                  K= WIBUFF(10,DIND)- WIBUFF(9,DIND)- 1
                  CALL INTCHR (K,LEN,JUST,
     O                         J,CDSN(48))
C                 fill in nsa
                  K= (WIBUFF((WIBUFF(10,DIND)+1),DIND)-
     >                WIBUFF(10,DIND)-2)/2
                  CALL INTCHR (K,LEN,JUST,
     O                         J,CDSN(58))
C                 fill in nsasp (nsp)
                  K= WIBUFF(11,DIND)-WIBUFF(WIBUFF(10,DIND)+1,DIND)
                  CALL INTCHR (K,LEN,JUST,
     O                         J,CDSN(68))
C                 fill in ndp
                  K= WIBUFF(12,DIND)- WIBUFF(11,DIND)- 2
                  CALL INTCHR (K,LEN,JUST,
     O                         J,CDSN(78))
C                 write out label parms
                  WRITE (SUCIFL,2000) (CDSN(J),J=1,80)
C
C                 *** add label comments ***
C
                  IF (ATANS.EQ.1) THEN
                    WRITE (SUCIFL,2000) BLNK,BLNK,(CSTR(I),I=33,40)
                    PSA  = WIBUFF(10,DIND)
                    SAMAX= WIBUFF(PSA,DIND)
                    IF (SAMAX.GT.0) THEN
                      I= 0
 530                  CONTINUE
                        I     = I+ 1
                        PSIND = PSA+ (I*2)
                        DSAIND= WIBUFF(PSIND,DIND)
                        PSAVAL= WIBUFF(PSIND+1,DIND)
                        CALL ZIPC (I80,BLNK,BUFF)
                        CALL WDSAGY (MESSFL,DSAIND,
     O                               BUFF(5),L,SATYP,SALEN,L,L)
C                       be sure label still in memory
                        DIND = WDRCGO(WDMSFL,DSFREC)
                        IF (SATYP.EQ.3) SALEN= SALEN/4
                        DONFG= 0
                        WRTFG= 0
                        LEN  = 10
                        J    = 0
                        JW   = 1

 531                    CONTINUE
                          GOTO (533,535,537), SATYP
 533                      CONTINUE
C                           integer type
                            K= WIBUFF(PSAVAL+J,DIND)
                            CALL INTCHR (K,LEN,JUST,
     O                                   L,BUFF(JW*10+1))
                            IF (J.EQ.7) WRTFG= 1
                            GO TO 539
C
 535                      CONTINUE
C                           real type
                            RVAL= WRBUFF(PSAVAL+J,DIND)
                            CALL DECCHR (RVAL,LEN,JUST,
     O                                   L,BUFF(JW*10+1))
                            IF (J.EQ.7) WRTFG= 1
                            GO TO 539
C
 537                      CONTINUE
C                           character type
                            WRITE (CDUM,2010) WIBUFF(PSAVAL+J,DIND)
                            L1= JW*4+ 9
                            L2= L1+ 3
                            READ (CDUM,1010) (BUFF(L),L=L1,L2)
                            IF (L2.EQ.80) WRTFG= 1
                            GO TO 539
C
 539                      CONTINUE
                          J = J+ 1
                          JW= JW+ 1
                          IF (J.GE.SALEN) THEN
                            WRTFG= 1
                            DONFG= 1
                          END IF
                          IF (WRTFG.EQ.1) THEN
                            WRITE (SUCIFL,2000) BUFF
                            JW   = 0
                            WRTFG= 0
                            CALL ZIPC(I80,BLNK,BUFF)
                          END IF
                        IF (DONFG.EQ.0) GO TO 531
                      IF (I.LT.SAMAX) GO TO 530
C
                    END IF
C
C                   write end label
                    WRITE (SUCIFL,2000) BLNK,BLNK,(CSTR(J),J=49,52),
     1                                            (CSTR(I),I=33,40)
                  END IF
C
C                 *** add pointer write here ****
C
                  IF (DTANS.EQ.1) THEN
                    GO TO (551,552,560,560,555,560,560,558,559),DSTYPE
C
 551                CONTINUE
C                     timeseries data, determine data avialable for this dsn?
                      GPFLG = 1
                      CALL WTFNDT (WDMSFL,DSN,GPFLG,
     O                             DSFREC,LSDAT,LEDAT,RETCOD)
C
                      IF (RETCOD.NE.0) THEN
C                       no data to export
                        SGRP= 51
                        CALL PMXTXI (MESSFL,SCLU,SGRP,I1,INIT,I1,I1,DSN)
C                       check exit code
                        CALL ZGTRET(IRET)
                        IF (IRET .EQ. 7) THEN
C                         user wants to get out of here
                          DSN= -1
                        END IF
                      ELSE
                        IF (EXDTFG .EQ. 0 .AND. DSCOUN .EQ. 1) THEN
C                         ask if global dates to be used
                          SGRP  = 52
                          EXDTFG= 1
                          CALL QRESP (MESSFL,SCLU,SGRP,EXDTFG)
C                         check exit code
                          CALL ZGTRET(IRET)
                          IF (IRET .EQ. 7) THEN
C                           user wants to get out of here
                            DSN= -1
                          ELSE
C                           user continuing, next message should init screen
                            INIT= 1
                          END IF
                          IF (EXDTFG .EQ. 1 .AND. DSN .GT. 0) THEN
C                           read global dates
                            CALL WDATCP (LSDAT,EXSDAT)
                            CALL WDATCP (LEDAT,EXEDAT)
                            CALL DATESQ (DLEN,EXSDAT,EXEDAT)
C                           check exit code
                            CALL ZGTRET(IRET)
                            IF (IRET .EQ. 7) THEN
C                             user wants to get out of here
                              DSN= -1
                            END IF
                          END IF
                        END IF
C
                        IF (DSN .GT. 0) THEN
C                         export this data set
                          IF (EXDTFG.EQ.1) THEN
C                           adjust to global dates based on available data
                            IF (TIMCHK(EXSDAT,LSDAT).LE.0) THEN
C                             global export date equal to/later than
C                             start of data sets data, use global date
                              CALL WDATCP (EXSDAT,LSDAT)
                            END IF
                            IF (TIMCHK(EXEDAT,LEDAT).GT.0) THEN
C                             global export end data equal to/before
C                             end of data sets data, use global date
                              CALL WDATCP (EXEDAT,LEDAT)
                            END IF
                          ELSE IF (EXDTFG .EQ. 2) THEN
C                           prompt for dates for each data set
                            CALL DATESQ (DLEN,LSDAT,LEDAT)
C                           check exit code
                            CALL ZGTRET(IRET)
                            IF (IRET .EQ. 7) THEN
C                             user wants to get out of here
                              DSN= -1
                            ELSE
C                             next text message should init screen
                              INIT= 1
                            END IF
                          END IF
C
                          IF (DSN.GT.0) THEN
C                           still ok to export
                            CALL PRWMTE (WDMSFL,SUCIFL,DSN,LSDAT,LEDAT)
                          END IF
                        END IF
                      END IF
C
                      GO TO 570
C
 552                CONTINUE
C                     table data, export all tables within data set
                      CALL PRWMXE (MESSFL,WDMSFL,SUCIFL,DSN)
C
                      GO TO 570
C
 555                CONTINUE
C                     DLG data
                      CALL PRWMDE (WDMSFL,SUCIFL,DSN)
C
                      GOTO 570
C
 558                CONTINUE
C                     attribute data
                      CALL PRWMAE (WDMSFL,SUCIFL,DSN)
C
                      GO TO 570
C
 559                CONTINUE
C                     export message dataset
                      CALL PRWMME (WDMSFL,SUCIFL,DSN)
C
                      GO TO 570
C
 560                CONTINUE
C                     this type not available yet
                      SGRP= 40
                      CALL PMXTXI (MESSFL,SCLU,SGRP,I10,
     I                             INIT,I0,I1,DSTYPE)
C                     check exit code
                      CALL ZGTRET(IRET)
                      IF (IRET .EQ. 7) THEN
C                       user wants out
                        DSN= -1
                      END IF
C
 570                CONTINUE
C
                  END IF
C
C                 write end dsn
                  WRITE (SUCIFL,2000) (CSTR(J),J=49,52),(CDSN(I),I=1,4)
C
                  IF (DSN .GT. 0) THEN
                    IF (IPAU.EQ.1) THEN
C                     pause between dsn's
                      I= I0
                    ELSE
C                     fast version
                      I= I1
                    END IF
C                   indicate dsn exported
                    SGRP= 41
                    CALL PMXTXI (MESSFL,SCLU,SGRP,I10,INIT,I,I1,DSN)
                    IF (IPAU .EQ. 1) THEN
C                     check exit code
                      CALL ZGTRET(IRET)
                      IF (IRET .EQ. 7) THEN
C                       user wants out
                        DSN= -1
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            IF (DSN .GT. 0) GO TO 520
C
C           turn off interrupt
            I= 16
            CALL ZSTCMA(I,I0)
C           finish message
            IF (DSN .EQ. 0) THEN
C             successful export
              SGRP= 42
            ELSE IF (DSN .LT. 0) THEN
C             export interrupted
              SGRP= 43
            END IF
            CALL PRNTXT (MESSFL,SCLU,SGRP)
C           export completed
            EXPFG= 1
C           close export file
            CALL QFCLOS (SUCIFL,I0)
            SUCIFL= 0
C           reset data set index
            IF (DSNCNT.GT.0) THEN
C             datasets selected, init index
              DSNIND= 0
            ELSE
C             no data sets selected, set to export all
              DSNIND= -1
            END IF
          ELSE
C           no output file to export to
            SGRP= 44
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 900
C
 600    CONTINUE
C         return to archive screen
          IF (SUCIFL.GT.0 .AND. EXPFG.EQ.0) THEN
C           export set up, but not yet performed, sure you want to return
            SGRP= 45
            RESP= 2
            CALL QRESP (MESSFL,SCLU,SGRP,RESP)
            IF (RESP.EQ.2) THEN
C             user doesnt want out yet, set default to Export option
              RESP= 5
            ELSE
C             user wants out
              RESP= 6
            END IF
          END IF
          GO TO 900
C
 900    CONTINUE
C
      IF (RESP.NE.6) GO TO 10
C
C     clear data-set buffer
      DSNCNT= 0
      CALL ZIPI (DSNBMX,I0,DSNBUF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDMDMP
     I                   (MESSFL,WDMSFL,SCLU)
C
C     + + + PURPOSE + + +
C     Dump WDM file records into flat file with real, integer
C     and character*4 values.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMSFL, SCLU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for ANNIE message file
C     WDMSFL - Fortran unit number for users WDM file
C     SCLU   - Cluster containing messages for this routine
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I, IBUFF(512,4), SUNIT, K, J, ICNT, INUM, I1, I0,
     &             ITE, L, OLEN, IPOS, BFLG, L132, L6,
     &             SGRP, RETCOD, IVAL(4), CVAL(3), IMXPOS, IMXREC
      REAL         RBUFF(512,4), RVAL
      CHARACTER*1  CTMP(4), CSTR(132), BLNK, RCD(6)
      CHARACTER*4  CBUFF(512,4), BAD
      CHARACTER*30 STR, UND
      CHARACTER*132 OBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (IBUFF(1,1), RBUFF(1,1)),
     1            (IVAL(1),IS),(IVAL(2),IE),(IVAL(3),PS),(IVAL(4),PE)
      INTEGER     PS,PE,IS,IE
C
C     + + + FUNCTIONS + + +
      INTEGER     WDGIVL
C
C     + + + INTRINSICS + + +
      INTRINSIC   ICHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL   CHRCHR, INTCHR, ZIPC, QRESPM, QFOPEN, PRNTXT
      EXTERNAL   ZSTCMA, ZGTRET, QFCLOS, WDGIVL
C
C     + + + DATA INITIALIZATIONS + + +
      DATA STR /'   INTEGER        REAL   CHAR'/
      DATA UND /'-----------------------------'/
      DATA L6, L132, IMXPOS /6, 132, 29/
      DATA RCD /'R','E','C','O','R','D'/
      DATA BAD /'----'/,   BLNK /' '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (4A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (7X,A29,2X,A29,2X,A29,2X,A29)
 2001 FORMAT (A132)
 2002 FORMAT (A4)
 2003 FORMAT (132A1)
 2010 FORMAT (I5)
 2011 FORMAT (I10)
 2012 FORMAT (G14.5)
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      I0= 0
C
C     open output file
      SGRP = 63
      CALL QFOPEN (MESSFL,SCLU,SGRP,
     O             SUNIT,RETCOD)
C     make interrupt available
      I= 16
      CALL ZSTCMA (I,I1)
C
      IF (RETCOD .EQ. 0) THEN
C       warning message about the dumped data
        SGRP = 61
        CALL PRNTXT (MESSFL, SCLU, SGRP)
C       check exit
        CALL ZGTRET(I)
        IF (I .EQ. 7) THEN
C         interrupt requested
          RETCOD= 1
        END IF
      END IF
C
      IF (RETCOD .EQ. 0) THEN
C       user still wants to dump, set defaults to dump
C       (equiv to IVAL)
        IS  = 1
        IE  = 1
        PS  = 1
        PE  = 512
 20     CONTINUE
C         What to dump? (0 or interrupt if finished)
          INUM= 4
          SGRP= 62
          CALL QRESPM (MESSFL,SCLU,SGRP,INUM,I1,I1,
     M                 IVAL,RVAL,CVAL,CSTR)
C         check exit
          CALL ZGTRET (I)
          IF (I .EQ. 7) THEN
C           interrupt requested
            IS= 0
          END IF
          IF (IS .GT. 0) THEN
C           wants to dump records, 1st determine max record
            IMXREC= WDGIVL (WDMSFL,I1,IMXPOS)
            IF (IE.GT.IMXREC) THEN
C             only process up to last record
              IE= IMXREC
            END IF
 25         CONTINUE
C             loop to process up to four records
              ICNT= IS
              K   = 1
 30           CONTINUE
C               loop to read records
                IF (ICNT.LE.IMXREC) THEN
C                 valid record to read
                  READ(WDMSFL,REC=ICNT) (IBUFF(I,K),I=1,512)
                  DO 35 I = PS,PE
C                   loop to process data values
                    WRITE(CBUFF(I,K),2002) IBUFF(I,K)
                    READ (CBUFF(I,K),1000) CTMP
                    BFLG = 0
                    DO 32 J = 1,4
C                     loop to process characters within data values
                      L = ICHAR(CTMP(J))
                      IF (L .GE. 128) THEN
C                       turn off high bit
                        L = L - 128
                      END IF
                      IF (L .LT. 32 .OR. L .GT. 122) THEN
C                       can't print this character
                        BFLG = 1
                      END IF
 32                 CONTINUE
                    IF (BFLG .EQ. 1) THEN
                      CBUFF(I,K) = BAD
                    END IF
 35               CONTINUE
                END IF
                ICNT = ICNT + 1
                K    = K + 1
              IF (K .LE. 4 .AND. ICNT .LE. IE) GO TO 30
              K = K - 1
C
              ITE = IS + K - 1
C             dump results
              CALL ZIPC (L132, BLNK, CSTR)
              CSTR(1) = '1'
              DO 38 I = IS,ITE
                IPOS = 17 + (I-IS)*29
                CALL CHRCHR (L6,RCD,CSTR(IPOS))
                IPOS = IPOS + 9
                CALL INTCHR (I,L6,I0,OLEN,CSTR(IPOS))
 38           CONTINUE
              WRITE (SUNIT,2003) CSTR
              IS = ITE + 1
              WRITE(SUNIT,2000) (STR,I=1,K)
              WRITE(SUNIT,2000) (UND,I=1,K)
              DO 60 I = PS,PE
C               position on record loop
                OBUFF= ' '
                WRITE (OBUFF(1:5),2010) I
                DO 50 J= 1,K
C                 set of 4 records loop
                  IPOS= 29*(J-1)+ 2*J+ 6
                  WRITE (OBUFF(IPOS:IPOS+9),2011,ERR=41) IBUFF(I,J)
 41               CONTINUE
                  IPOS= IPOS+ 10
                  IF (IBUFF(I,J) .GE. 0) THEN
                    WRITE (OBUFF(IPOS:IPOS+13),2012,ERR=42) RBUFF(I,J)
                  ELSE
                    OBUFF(IPOS:IPOS+13)= '     ----     '
                  END IF
 42               CONTINUE
                  IPOS= IPOS+ 15
                  OBUFF(IPOS:IPOS+3)= CBUFF(I,J)
 50             CONTINUE
                WRITE(SUNIT,2001) OBUFF
 60           CONTINUE
            IF (IE .GE. IS) GO TO 25
          END IF
        IF (IS .GT. 0) GO TO 20
      END IF
C
C     turn off interrupt
      I= 16
      CALL ZSTCMA (I,I0)
C
      IF (SUNIT .GT. 0) THEN
C       close output file
        CALL QFCLOS (SUNIT,I0)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   BLDWRT
     I                   (ILEN,TBUFF,INIT,IWRT,
     O                    DONFG)
C
C     + + + PURPOSE + + +
C     write import messages to aide screen for program WDMS
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ILEN,INIT,IWRT,DONFG
      CHARACTER*1 TBUFF(ILEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ILEN   - length of input buffer
C     TBUFF  - buffer to write to screen
C     INIT   - initialization flag (-1 - overwrite last line,
C                                    0 - save previous lines,
C                                    1 - init whole screen)
C     IWRT   - write flag (-1 - dont write, 0 - write and wait,
C                           1 - write and dont wait)
C     DONFG  - user exit value
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZBLDWR, ZGTRET
C
C     + + + END SPECIFICATIONS + + +
C
      CALL ZBLDWR (ILEN,TBUFF,INIT,IWRT,DONFG)
C     check exit code
      CALL ZGTRET(DONFG)
C
      RETURN
      END
