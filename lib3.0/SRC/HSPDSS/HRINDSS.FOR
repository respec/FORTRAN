C
C
C
      SUBROUTINE   DSSDS
     I                   (MSGFL,MESSU,VOLNO,GAPST,GAPKW1,SYST,SYSKW1,
     I                    AMDKW1,AMDST,SDATIM,EDATIM,TRFLAG,OUTLEV,
     I                    MAXTTB,
     M                    ECOUNT,
     O                    NUM,DELT,UNT,NTS,GAPCD,AMDCD,TABL,TABLR)
C
C     + + + PURPOSE + + +
C     process a reference to a time series dataset in a dss file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MSGFL,MESSU,VOLNO,SDATIM(5),EDATIM(5),TRFLAG,OUTLEV,
     $            MAXTTB,ECOUNT,NUM,DELT,UNT,NTS,GAPCD,AMDCD,
     $            TABL(10,MAXTTB)
      REAL        TABLR(10,MAXTTB)
      CHARACTER*1 GAPKW1(8),SYSKW1(8),AMDKW1(12)
      CHARACTER*4 GAPST,SYST,AMDST
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - fortran unit number of HSPF message file
C     MESSU  - ftn unit no. to be used for printout of messages
C     VOLNO  - data record ID number in PATHNAMES block.
C     GAPST  - gap value string
C     GAPKW1 - gap value keyword library
C     SYST   - unit system string
C     SYSKW1 - unit system keyword library
C     AMDKW1 - access mode keyword library
C     AMDST  - amend code string
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     TRFLAG - target flag - 1 if target, 0 if source
C     OUTLEV - run interpreter output level
C     MAXTTB - maximum size of time series group table
C     ECOUNT - count(s) of specific errors
C     NUM    - data record ID number (negative)
C     DELT   - data time step in minutes
C     UNT    - unit system
C     NTS    - number of components, always 1 for DSS
C     GAPCD  - gap code for handling missing source data
C     AMDCD  - amend code for writing target data
C     TABL   - time series group table - integer version
C     TABLR  - time series group table - real version
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cpthnm.inc'
      INCLUDE 'cifltb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU,SGRP,I80,I,ID,DSSFIL,I64,I8,BPART(6),EPART(6),
     $             LPART(6),RETCOD,I3,PTYPI,J,I2,I4,BLANKI,
     $             RETS,RETE,KIND
      REAL         RDUM
      CHARACTER*1  BLANK1
      CHARACTER*4  STIME,ETIME,OBUFF,BLANK4
      CHARACTER*8  PTYPE,DTYPE1,DTYPE2,CDUM,CHSTR
      CHARACTER*20 SDATE,EDATE
      CHARACTER*80 PATH
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1),(OBUFF,OBUF1)
      CHARACTER*1  CHSTR1(8),OBUF1(4)
C
C     + + + FUNCTIONS + + +
      INTEGER STRFND,CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSG,OMSTC,OMSTI,ZIPC,COPYC,ZUPATH,ZGINTL,CHKSTR,
     $           TIM2CH,ZRRTS,STRFND,ADDCDT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I2,I3,I4,I8,I64,I80/2,3,4,8,64,80/
      DATA BLANK1/' '/
      DATA BLANK4/'    '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A4)
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT (1X,'BEGIN CHECKING/EXPANDING A DSS FILE REFERENCE')
 2020 FORMAT (1X,'END CHECKING/EXPANDING A DSS FILE REFERENCE')
 2030 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 219
C
      IF (OUTLEV .GT. 6) THEN
        WRITE (MESSU,2010)
      END IF
C
      CALL ZIPC (I80,BLANK1,
     O           PATH)
      READ (BLANK4,1000) BLANKI
C
C     look up pathname
      ID= 0
      I=  0
 10   CONTINUE
        I= I+ 1
        IF (I .GT. NPATH) THEN
C         error - data record not found in PATHNAMES block
          ID= -1
        ELSE
C         check for pathname
          IF (VOLNO .EQ. DSSDSN(I)) THEN
C           found
            ID=     I
            DSSFIL= DSSFL(ID)
            PTYPI=  DTYPI(ID)
            CALL COPYC (I64,CPATH(ID),
     O                  PATH)
            CALL COPYC (I8,CTYPE(ID),
     O                  PTYPE)
          END IF
        END IF
      IF (ID .EQ. 0) GO TO 10
C
      IF (ID .LE. 0) THEN
C       error - data record not found in PATHNAMES block
        CALL OMSTI (VOLNO)
        SGRP= 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      ELSE IF (ID .GT. 0) THEN
C       check to see if file is open
        IF (DSSOPN(DSSFL(ID)) .LE. 0) THEN
C         error - file is not opened
          CALL OMSTI (VOLNO)
          CALL OMSTI (DSSFL(ID))
          SGRP= 12
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ID= -2
        END IF
      END IF
C
      IF (ID .GT. 0) THEN
C       process member
C
C       check delt
        CALL ZUPATH (PATH,
     O               BPART,EPART,LPART,RETCOD)
        I= 1
        CALL ZGINTL (
     O               DELT,
     I               PATH(BPART(5):EPART(5)),
     O               J,
     M               I)
C
        IF (I .NE. 0) THEN
C         not a regular time series - set dummy value for DELT
          DELT= 60
        END IF
C
        IF (TRFLAG .EQ. 0) THEN
C         check user-input for gapst; set gapcd
          IF (GAPST .EQ. BLANK4) THEN
C           default to gap value of undefined
            GAPCD= 1
          ELSE
C           check user-supplied value
            CHSTR(1:4)= GAPST
            GAPCD= CHKSTR (I4,I2,CHSTR1,GAPKW1)
            IF (GAPCD .EQ. 0) THEN
C             error - invalid gap value requested - undefined assumed
              CALL OMSTI (VOLNO)
              OBUFF= GAPST
              CALL OMSTC (I4,OBUF1)
              SGRP= 2
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            ELSE
              GAPCD = GAPCD - 1
            END IF
          END IF
        ELSE
C         target dataset - find access mode
          IF (AMDST .EQ. BLANK4) THEN
C           default to REPL
            AMDCD= 1
          ELSE
C           check user-supplied value
            CHSTR(1:4)= AMDST
            AMDCD= CHKSTR (I4,I3,CHSTR1,AMDKW1)
            IF ( (AMDCD .EQ. 0) .OR. (AMDCD .EQ. 2) ) THEN
C             error - invalid amend code
              CALL OMSTI (VOLNO)
              OBUFF= AMDST
              CALL OMSTC (I4,OBUF1)
              SGRP= 3
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
          END IF
        END IF
C
C       convert run times from hspf internal format to dss
        CALL TIM2CH (SDATIM,
     O               SDATE,STIME,RETCOD)
C       move run start time forward 1 'data' interval
        I= 1
        CALL ADDCDT (DELT,I,
     M               SDATE,STIME)
        CALL TIM2CH (EDATIM,
     O               EDATE,ETIME,RETCOD)
C
C       check data record by retrieving first and last data points
        I= 1
        CALL ZRRTS (IFLTAB(1,DSSFIL),PATH,SDATE,STIME,
     M              I,
     O              RDUM,CDUM,DTYPE1,J,RETS)
        I= 1
        CALL ZRRTS (IFLTAB(1,DSSFIL),PATH,EDATE,ETIME,
     M              I,
     O              RDUM,CDUM,DTYPE2,J,RETE)
C
        IF ( (RETS .EQ. 12) .OR. (RETS .EQ. 24) ) THEN
C         error - pathname does not follow regular time-series
C         conventions, including a correct "E" part
          CALL OMSTI (VOLNO)
          SGRP= 4
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE IF (RETS .EQ. 20) THEN
C         error - data is not regular time-series
          CALL OMSTI (VOLNO)
          SGRP= 5
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE IF ( (RETS .GT. 10) .OR. (RETE .GT. 10) ) THEN
C         error - program bug - fatal problem reading data
          CALL OMSTI (VOLNO)
          CALL OMSTI (RETS)
          CALL OMSTI (RETE)
          SGRP= 6
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE
C         no read error - check for data present
          IF (TRFLAG .EQ. 0) THEN
C           source - all data must be present
            IF ( (RETS .GT. 1) .OR. (RETE .GT. 1) ) THEN
C             error - an endpoint is missing - assume data is absent
C             or incomplete
              CALL OMSTI (VOLNO)
              CALL OMSTI (RETS)
              CALL OMSTI (RETE)
              SGRP= 7
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
          ELSE
C           target
            IF (AMDCD .EQ. 1) THEN
C             adding data - data cannot already be present
              IF ( (RETS .LE. 4) .OR. (RETE .LE. 4) ) THEN
C               error - data already present
                CALL OMSTI (VOLNO)
                SGRP= 8
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                      ECOUNT)
              END IF
            END IF
          END IF
        END IF
C
C       check user-supplied unit system
        IF (SYST .EQ. BLANK4) THEN
C         blank - default to ENGL
          UNT= 1
        ELSE
C         check for valid unit system
          CHSTR(1:4)= SYST
          UNT= CHKSTR (I4,I2,CHSTR1,SYSKW1)
          IF (UNT .EQ. 0) THEN
C           error - invalid system string
            CALL OMSTI (VOLNO)
            OBUFF= SYST
            CALL OMSTC (I4,OBUF1)
            SGRP= 9
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
        END IF
C
C       check type of data record
C
        IF ( (RETS .LE. 1) .AND. (RETE .LE. 1) ) THEN
C         data already exists
C
C         check if type is consistent
          I= STRFND (I8,DTYPE1,I8,DTYPE2)
          IF (I .NE. 1) THEN
C           error - type should not change over time
            CALL OMSTI (VOLNO)
            CALL OMSTC (I8,DTYPE1)
            CALL OMSTC (I8,DTYPE2)
            SGRP= 10
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
C
          IF (PTYPI .NE. 0) THEN
C           user supplied type - check if matches data type
            I= STRFND (I8,DTYPE1,I8,PTYPE)
            IF (I .NE. 1) THEN
C             error - type doesn't match
              CALL OMSTI (VOLNO)
              CALL OMSTC (I8,PTYPE)
              CALL OMSTC (I8,DTYPE1)
              SGRP= 11
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
          ELSE
C           no user supplied type - use data type if valid
            CHSTR(1:8)= DTYPE1
            PTYPI= CHKSTR (I8,I4,CHSTR1,DTPKW1)
          END IF
        END IF
C
C       find kind from type code
        IF (PTYPI .EQ. 3) THEN
C         inst-val is point
          KIND= 1
        ELSE
C         per-aver or per-cum is mean, or invalid/blank defaults
          KIND= 2
        END IF
C
C       output
        NUM = -VOLNO
        NTS = 1
        DO 20 I = 1, 2
          TABL(I,NTS) = BLANKI
 20     CONTINUE
        TABL(3,NTS)  = 0
        TABL(4,NTS)  = 0
        TABL(5,NTS)  = 0
        TABL(6,NTS)  = KIND
        TABLR(8,NTS) = 0.0
        TABLR(9,NTS) = 1.0
      END IF
C
      IF (OUTLEV .GT. 6) THEN
        WRITE(MESSU,2020)
      END IF
C
      RETURN
      END
