C
C
C
      SUBROUTINE   PRWMLA
     I                    (MESSFL,WDMSFL)
C
C     + + + PURPOSE + + +
C     Add a dataset label and attributes to the WDMSFL,
C     may be a copy of an existing dataset label and attributes
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I0,I1,COPYFG,RETCOD,ERRFLG,SCLU,SGRP,INUM,CNUM,
     1             NDSN,ODSN,IRET,IVAL(2),CVAL(3),DSNFRC,
     >             NWDMFL,NTYPE
      REAL         RVAL(1)
      CHARACTER*1  TBUFF(80)
C     CHARACTER*10 WNDNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL    QRESPM, PRWLAD, WDDSCK, WDDSCL, PMXTXI
      EXTERNAL    ZSTCMA, ZGTRET, ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      SCLU= 27
C
C     allow previous command
      I= 4
      CALL ZSTCMA (I,I1)
C
  50  CONTINUE
        ERRFLG= 0
        INUM  = 2
        CALL ZIPI (INUM,I0,IVAL)
        CNUM  = 1
        CALL ZIPI (3*CNUM,I1,CVAL)
        SGRP  = 1
C       ask if the new dataset is like any others
        CALL QRESPM (MESSFL,SCLU,SGRP,INUM,I1,CNUM,
     M               IVAL,RVAL,CVAL,TBUFF)
C       get user exit command value
        CALL ZGTRET (IRET)
C
        IF (IRET.EQ.1) THEN
C         user wants to continue
          COPYFG= CVAL(1)
          NDSN  = IVAL(1)
          ODSN  = IVAL(2)
C         check existance
          CALL WDDSCK (WDMSFL,NDSN,DSNFRC,RETCOD)
          IF (RETCOD.EQ.0) THEN
C           error, dataset already exists
            SGRP= 2
            CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,I0,I1,NDSN)
            ERRFLG= 1
          END IF
          IF (COPYFG.EQ.1 .AND. ERRFLG.EQ.0) THEN
C           copying from existing data set
C           try to copy the attributes
C           assume same wdm file and data set type
            NWDMFL = WDMSFL
            NTYPE  = 0
            CALL WDDSCL (WDMSFL,ODSN,NWDMFL,NDSN,NTYPE,RETCOD)
            IF (RETCOD.EQ.-61) THEN
C             old dataset doesnt exist
              SGRP= 3
              CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,I0,I1,ODSN)
              ERRFLG= 1
            END IF
          ELSE IF (ERRFLG.EQ.0) THEN
C           dataset attributes to be entered by user
            CALL PRWLAD (MESSFL,WDMSFL,SCLU,NDSN,
     O                   ERRFLG)
          END IF
        ELSE
C         user wants outta here
          ERRFLG= -1
        END IF
      IF (ERRFLG.GT.0) GO TO 50
C
C     turn off previous command
      I= 4
      CALL ZSTCMA (I,I0)
C
CPRH      IF (ERRFLG.EQ.0) THEN
CPRHC       ask if modifications are required
CPRH        SGRP= 4
CPRH        CALL QRESP (MESSFL,SCLU,SGRP,MODFLG)
CPRH        MODFLG= MODFLG- 1
CPRHC       user want to modify
CPRH        I= 1
CPRH        WNDNAM= 'Build (DB)'
CPRH        CALL PRWMLM (MESSFL,WDMSFL,I,NDSN,WNDNAM)
CPRH      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWLAD
     I                    (MESSFL,WDMSFL,SCLU,DSN,
     O                     ERRFLG)
C
C     + + + PURPOSE + + +
C     ask a user for information relating to adding a new
C     dataset attributes, add the attributes, but no data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL,SCLU,DSN,ERRFLG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SCLU   - Cluster containing information for this subroutine
C     DSN    - dataset number to be added
C     ERRFLG - Error flag, -1 user wants outta here, 0 ok
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SGRP,DSTYPE,PSA,I,I1,USRLEV,PRMIND,IRET,
     1            INUM,IVAL(5),CVAL(3),TSSTEP,TCODE
      REAL        RVAL(1)
      CHARACTER*1 TBUFF(80)
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (IVAL(1),NDN),(IVAL(2),NUP),(IVAL(3),NSA),
     1            (IVAL(4),NSASP),(IVAL(5),NDP)
      INTEGER    NDN,NUP,NSA,NSASP,NDP
C
C     + + + EXTERNALS + + +
      EXTERNAL   QRESP, QRESPM, PRNTXI, WDLBAX, ANPRGT
      EXTERNAL   ZGTRET, ATRADQ
C
C     + + + END SPECIFICATIONS + + +
C
      I1    = 1
C
C     get users experience level (0-lots, 1-some, 2-none)
      PRMIND = 45
      CALL ANPRGT (PRMIND, USRLEV)
C
 10   CONTINUE
        ERRFLG= 0
C       prompt user for dsn type
        SGRP = 11
        CALL QRESP (MESSFL,SCLU,SGRP,DSTYPE)
C       get user exit command value
        CALL ZGTRET (IRET)
        IF (IRET .EQ. 1) THEN
C         user wants to keep going, default attributes space allocation
          NDN  = 10
          NUP  = 20
          NSA  = 20
          NSASP= 50
          IF (DSTYPE .EQ. 1) THEN
C           timeseries
            NDP= 100
          ELSE
C           other dataset types need lots of data pointer space
            NDP= 200
          END IF
C
          IF (USRLEV .EQ. 0) THEN
 20         CONTINUE
C             lots of experience, user may specify label space allocation
              INUM= 5
              SGRP= 12
              CALL QRESPM(MESSFL,SCLU,SGRP,INUM,I1,I1,
     M                    IVAL,RVAL,CVAL,TBUFF)
C             return code
              CALL ZGTRET(IRET)
              IF (IRET .EQ. 1) THEN
C               is their enough space?
                I= NDN+ NUP+ (NSA* 2)+ NSASP+ NDP+ 20
                IF (I.GT.508) THEN
                  ERRFLG= 1
                  SGRP  = 13
                  CALL PRNTXI (MESSFL,SCLU,SGRP,I)
                END IF
              ELSE IF (IRET .EQ. 2) THEN
C               user wants prev
                ERRFLG= -1
              END IF
            IF (ERRFLG.EQ.1) GO TO 20
          END IF
        ELSE
C         user wants out
          ERRFLG= -2
        END IF
      IF (ERRFLG .EQ. -1) GO TO 10
C
      IF (ERRFLG .EQ. 0) THEN
C       add the general part of the attributes
        CALL WDLBAX (WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSASP,NDP,
     O               PSA)
C
        IF (DSTYPE .EQ. 1) THEN
C         add timeseries attributes
          TSSTEP= 1
          TCODE = 5
          CALL ATRADQ (MESSFL,WDMSFL,SCLU,DSN,
     M                 TSSTEP,TCODE,
     O                 IRET)
        END IF
C
C       add attributes
CPRH        CALL PRWLAS (MESSFL,WDMSFL,SCLU,DSN,DSTYPE)
      END IF
C
      RETURN
      END
C
C
C
C
      SUBROUTINE   ATRADQ
     I                    (MESSFL, WDMSFL, SCLU, DSN,
     M                     TSSTEP, TCODE,
     O                     RETC)
C
C     + + + PURPOSE + + +
C     This routine adds some of the required attributes to a new
C     dataset.  General specification attributes added are TSTYPE,
C     STCODE, STAID, and STANAM.  Time specification attributes are
C     TSSTEP, TCODE, TGROUP, TSBYR, TSFORM, and VBTIME.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMSFL, SCLU, DSN, TSSTEP, TCODE, RETC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     WDMSFL - Fortran unit number of message file
C     SCLU   - Cluster containing information for this subroutine
C     DSN    - number of new dataset
C     TSSTEP - time step in TCODE units
C     TCODE  - time units
C              1 - second   4 - day
C              2 - minute   5 - month
C              3 - hour     6 - year
C     RETC   - return code, the sum of the absolute values of
C              the returns for each of the attributes added, a
C              0 indicates no errors
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   INUM, CNUM, IVAL(2), CVAL(7,3), OTFLG, LEN5,
     #          SGRP, IPT, IPTN(3), ATTR(10), LENA(3),
     #          LEN1, LENRET, I, I0, I1, RET(12), LEN, STA, BADI,
     #          STORE(5), LEN80
      REAL      RVAL(1)
      CHARACTER*1  TBUFF(80), BLNK
      CHARACTER*18 WNDNAM
C
C     + + + FUNCTIONS + + +
      INTEGER    LENSTR, CRINTE
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPI, QRESPM, WDBSAC, CKTSTU, PRNTXT, WDBSAI
      EXTERNAL   LENSTR, CRINTE, COPYI, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  OTFLG, LEN1, LEN5, BADI
     #    /     1,    1,    5, -999 /
      DATA  BLNK, LEN80
     #    /  ' ',    80 /
C
C                 TS-  STA STA T-   T-    TS-  VB-  CO-  TS-  TS-
C                 TYPE  ID NAM CODE GROUP FORM TIME MPFG STEP BYR
      DATA  ATTR /  1,  2, 45,  17,   34,  84,  85,  83,  33,  27 /
      DATA  IPTN /  1,  5, 21 /
      DATA  LENA /  4, 16, 48 /
      DATA  STORE             /  4,    4,   1,   1,    1 /
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
C
      LENRET= 12
      CALL ZIPI (LENRET,I0,RET)
C
C     get general description attributes
      CALL ZIPC (LEN80,BLNK,TBUFF)
      CNUM = 3
      SGRP = 21
      CALL QRESPM (MESSFL, SCLU, SGRP, I1, I1, CNUM,
     O             IVAL, RVAL, CVAL, TBUFF )
C
C     add attributes to dataset
      DO 100 I = 1, 3
        IPT = IPTN(I)
        LEN = LENSTR ( LENA(I), TBUFF(IPT) )
        IF (I .EQ. 2 .AND. LEN .GT. 0) THEN
C         special case for station id
          STA = CRINTE ( BADI, LEN, TBUFF(IPT) )
          IF (STA .NE. BADI  .AND.  LEN .LT. 10) THEN
C           integer station id
            CALL WDBSAI ( WDMSFL, DSN, MESSFL, 51, 1, STA,
     O                    RET(I+1) )
            LEN = 0
          END IF
        END IF
        IF (LEN .GT. 0) THEN
C         character attributes
          CALL WDBSAC ( WDMSFL,DSN,MESSFL,ATTR(I),LENA(I),TBUFF(IPT),
     O                  RET(I+1) )
        END IF
 100  CONTINUE
C
      IF (TSSTEP .GT. 0  .AND.  TSSTEP .LT. 1441) THEN
C       already defined timesetp
        IVAL(1) = TSSTEP
      ELSE
C       unknown timestep
        IVAL(1) = -999
      END IF
      IVAL(2) = -999
      CALL COPYI ( LEN5, STORE, CVAL(1,1) )
      IF (TCODE .GT. 0  .AND.  TCODE .LT. 7) THEN
        CVAL(1,1) = TCODE
        IF (TCODE .EQ. 6) THEN
          CVAL(2,1) = 7 - 2
        END IF
      END IF

 200  CONTINUE
C       get time specification attributes
        RETC = 0
        INUM = 2
        CNUM = 5
        SGRP = 22
        CALL QRESPM ( MESSFL, SCLU, SGRP, INUM, I1, CNUM,
     O                IVAL, RVAL, CVAL, TBUFF )
C       check that time step and time units are compatible
        WNDNAM= 'Build (DB) Problem'
        CALL CKTSTU ( IVAL(1), CVAL(1,1), OTFLG,WNDNAM,
     O                RETC )
        IF (RETC .EQ. 0  .AND.  CVAL(1,1) .GE. CVAL(2,1)+2) THEN
C         group pointer not compatible with time units
          RETC = 1
          SGRP = 23
          CALL PRNTXT ( MESSFL, SCLU, SGRP )
        END IF
      IF (RETC .NE. 0) GO TO 200
C
C     add attributes to dataset
      CVAL(2,1) = CVAL(2,1) + 2
      CVAL(6,1) = IVAL(1)
      CVAL(7,1) = IVAL(2)
      DO 250 I = 4, 10
        CALL WDBSAI ( WDMSFL, DSN, MESSFL, ATTR(I), LEN1, CVAL(I-3,1),
     O                RET(I+1) )
 250  CONTINUE
C
      TSSTEP= IVAL(1)
      TCODE = CVAL(1,1)
C
C     check return codes for attributes added
      RETC = 0
      DO 300 I = 2, LENRET
        RETC = RETC + ABS ( RET(I) )
 300  CONTINUE
      IF (RETC .NE. 0) THEN
C       error in adding attribute(s)
        RET(1) = RETC
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWLAS
     I                    (MESSFL,WDMSFL,SCLU,DSN,DSTYPE)
C
C     + + + PURPOSE + + +
C     adds search attributes to a new dataset
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL,SCLU,DSN,DSTYPE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SCLU   - Cluster containing information for this subroutine
C     DSN    - dataset number on WDM file
C     DSTYPE - type of dataset, 1- timeseries, etc.
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,SGRP,ERRFLG,REQFG,DONFG,RREC,RIND,
     1             SAIND,SAREQ(12),SARQWD,DPTR,SALEN,SATYP,MATCH,RETC
      CHARACTER*1  SANAM(6)
      CHARACTER*10 WNDNAM
C
C     + + + FUNCTIONS + + +
      INTEGER     WDSASV, WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDSASV, WDDSCK, WDRCGO, WDRCUP, WDSAGN
      EXTERNAL    WDSAGY, WATTUS, WADNSA, WDSAAM, PRNTXT
C
C     + + + END SPECIFICATIONS + + +
C
      ERRFLG= 0
      REQFG = 1
      SAIND = 0
C
C     loop to add attributes
 10   CONTINUE
        IF (REQFG.GT.0) THEN
C         required loop
          MATCH= 0
 15       CONTINUE
            SAIND= SAIND+ 1
C           be sure attributes record stays in the buffer
            CALL WDDSCK (WDMSFL,DSN,RREC,RETC)
            RIND= WDRCGO (WDMSFL,RREC)
            IF (SAIND.EQ.1) THEN
C             first time through, set flag for call to WADNSA
              I= 1
C             may need to update record for previously added attributes
              CALL WDRCUP (WDMSFL,RIND)
C             force get of first available attribute
              SAIND= 0
            ELSE
              I= 0
            END IF
C           get the next available attribute's index from message file
            CALL WADNSA (MESSFL,I,
     M                   SAIND)
            IF (I.EQ.1) THEN
C             be sure attributes record didnt get kicked out of buffer
C             due to shuffling of attribute datasets in 1st call to WADNSA
              CALL WDDSCK (WDMSFL,DSN,RREC,RETC)
              RIND= WDRCGO (WDMSFL,RREC)
            END IF
            IF (SAIND.GT.0) THEN
C             valid index, get info from message file
              CALL WDSAGY (MESSFL,SAIND,
     O                     SANAM,DPTR,SATYP,SALEN,SARQWD,I)
              CALL WATTUS (SARQWD,
     O                     SAREQ)
              I= SAREQ(DSTYPE)
            END IF
          IF (I.NE.2 .AND. SAIND.GT.0) GO TO 15
          IF (SAIND.EQ.0) THEN
            REQFG= 0
C           may need to update attributes record
            CALL WDRCUP (WDMSFL,RIND)
          ELSE
C           is attribute already present?
            MATCH = WDSASV (SAIND,WIBUFF(1,RIND))
            IF (MATCH.EQ.0 .AND. REQFG.EQ.1) THEN
C             will be prompted for required attributes
              SGRP = 30
              CALL PRNTXT (MESSFL,SCLU,SGRP)
              REQFG= 2
            END IF
          END IF
        END IF
        IF (REQFG.EQ.0) THEN
C         optional loop, ask which attribute
          CALL WDSAGN (MESSFL,SAIND,SANAM,DONFG)
C         be sure attributes record didnt get kicked out of buffer
          CALL WDDSCK (WDMSFL,DSN,RREC,RETC)
          RIND= WDRCGO (WDMSFL,RREC)
          IF (DONFG.EQ.0) THEN
C           not done, check to see if already present
            MATCH= WDSASV(SAIND,WIBUFF(1,RIND))
          ELSE
C           done, force exit
            MATCH = 1
            ERRFLG= 1
          END IF
        END IF
C
        IF (MATCH.EQ.0) THEN
C         looking to add this attribute
          IF (REQFG.EQ.0) THEN
C           still need to get attribute info
            CALL WDSAGY (MESSFL,SAIND,
     O                   SANAM,DPTR,SATYP,SALEN,SARQWD,I)
            CALL WATTUS (SARQWD,
     O                   SAREQ)
            I= SAREQ(DSTYPE)
          END IF
          IF (I.EQ.2.OR.(REQFG.EQ.0.AND.I.GT.0)) THEN
C           add this attribute
            WNDNAM= 'Build (DB)'
            CALL WDSAAM (MESSFL,SAIND,SANAM,SATYP,SALEN,DPTR,WNDNAM,
     M                   WIBUFF(1,RIND),WRBUFF(1,RIND),
     O                   ERRFLG)
          ELSE IF (REQFG.EQ.0) THEN
C           attribute not allowed for this type
            SGRP= 31
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
        ELSE IF (ERRFLG.EQ.0 .AND.  REQFG .EQ. 0) THEN
C         attribute present already
          SGRP= 32
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        END IF
      IF (ERRFLG.EQ.0) GO TO 10
C
C     update new attributes
      CALL WDRCUP (WDMSFL,RIND)
C
      RETURN
      END
