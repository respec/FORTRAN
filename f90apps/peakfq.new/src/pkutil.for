C
C
C
      SUBROUTINE   PKLOOK
     I                    ( MESSFL, WDMSFL,
     I                      DSN, MXPK, J407FG,
     O                      NPKS, PK, WYR, QFLG, RETCOD )
C
C     + + + PURPOSE + + +
C     Gets basic peak information
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL, WDMSFL, DSN, MXPK, NPKS, WYR(MXPK), RETCOD
      REAL        PK(MXPK)
      CHARACTER*1 QFLG(12,MXPK)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of the message file
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data set number
C     MXPK   - maximum number of peaks allowed
C     J407FG - 0 = use full period of record
C              1 = limit output based on start and end attributes
C                  J407BY and J407EY
C     NPKS   - actual  number of peaks found
C     PK     - annual maximum peak discharge
C     WYR    - water year of peak
C     QFLG   - data quality flags for peak
C     RETCOD - return code
C                -1 - unknow problem retrieving time series data
C               -22 - template not found
C               -23 - not a valid table
C               -24 - not a valid associated table
C               -30 - want more than whole table
C               -31 - want more than whole extension
C               -33 - problems with row/space secifications
C               -63 - data-set type is invalid for requested operation
C               -81 - data set does not exist
C               -82 - data set exists, but is wrong DSTYP
C               -84 - data set number out of range
C              -321 - table template names didn't match
C
C    + + + LOCAL VARIABLES + + +
      INTEGER      TABID, TBCNT, LREC, TGRPT, GRP, QNU, NROW,
     $             LMXP, J407FG, IDUMMY, DSTYPE
      CHARACTER*1  MFID(2)
      CHARACTER*16 TNAME
C
C     + + + LOCAL VARIABLES + + +
C     LMXP   - maximum number of peaks to retrieve
C     NROW   - number of rows in table
C     TABID  - table identifier number
C     TABNAM - name of table
C     TBCNT  - total number of tables in data set
C     LREC   - label record number
C     TGRPT  - table group pointer
C     MFID   - message file name id
C     GRP    - table message file cluster number
C     QNU    - table message file group number
C     NROW   - number of rows in table
C
C     + + + FUNCTIONS + + +
      INTEGER   WDCKDT
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDCKDT, WDTBFX, GETTB, GETTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   TABID, TNAME,             IDUMMY
     $     /     1, 'AN.PEAKS        ',     1 /
C     + + + END SPECIFICATIONS + + +
C
      LMXP   = MXPK
      DSTYPE = WDCKDT ( WDMSFL, DSN )
      IF (DSTYPE .EQ. 2) THEN
C       table type data set
        CALL WDTBFX ( WDMSFL, DSN, TABID, TNAME,
     O                TBCNT, LREC, TGRPT, MFID, GRP, QNU, NROW, RETCOD )
        IF (RETCOD .EQ. 0) THEN
C         table data set
          CALL GETTB ( MESSFL, TNAME, WDMSFL, DSN, LMXP, J407FG, IDUMMY,
     M                 NROW,
     O                 PK, WYR, QFLG, RETCOD )
          NPKS  = NROW
        END IF
      ELSE IF (DSTYPE .EQ. 1) THEN
C       time series type data set
        CALL GETTS ( WDMSFL, DSN, LMXP,
     O               PK, WYR, QFLG, NPKS )
        IF (NPKS .GT. 0) THEN
C         successfully retrieved data
          RETCOD = 0
        ELSE
C         unknown problem retrieving data
          RETCOD = NPKS
          NPKS = 0
        END IF
      ELSE
C       unexected data set type
        RETCOD = -63
      ENDIF
C
      RETURN
      END
