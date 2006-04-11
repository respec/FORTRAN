C
C
C
      SUBROUTINE   CLINIT
C
C     + + + PURPOSE + + +
C     initialize class data structure
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclas.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      NCLAS= 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLCNT
     O                  (CCNT)
C
C     + + + PURPOSE + + +
C     return number of classes
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CCNT
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CCNT  - number of classes
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclas.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CCNT= NCLAS
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLADD
     I                  (CLBID,
     O                   CLID)
C
C     + + + PURPOSE + + +
C     add a new class
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CLBID,CLID
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CLBID - class base id - default values for new class
C     CLID  - identifier of new class
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclas.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      CLID = NCLAS+ 1
      IF (CLID .LE. MXCLAS) THEN
C       room for a new class
        IF (CLBID.GT.0 .AND. CLBID.LE.NCLAS) THEN
C         use base for class defaults
          CSEN(CLID)= CSEN(CLBID)
          CLOC(CLID)= CLOC(CLBID)
          CCON(CLID)= CCON(CLBID)
          CACT(CLID)= CACT(CLBID)
          CSYM(CLID)= CSYM(CLBID)
          CCUN(CLID)= CCUN(CLBID)
          CCSL(CLID)= CCSL(CLBID)
          CMSIZE(CLID)= CMSIZE(CLBID)
          CDSNCT(CLID)= CDSNCT(CLBID)
          CADSNC(CLID)= CADSNC(CLBID)
          CLOCCT(CLID)= CLOCCT(CLBID)
          DO 5 I= 1,CDSNCT(CLID)
            CDSN(I,CLID)= CDSN(I,CLBID)
  5       CONTINUE
        ELSE
C         general defaults
          CSEN(CLID)= '        '
          CLOC(CLID)= '        '
          CCON(CLID)= '        '
          CACT(CLID)= 1
          CSYM(CLID)= 1
          CCUN(CLID)= 1
          CCSL(CLID)= 2
          CMSIZE(CLID)= 1.0
          CDSNCT(CLID)= 0
          CADSNC(CLID)= 0
          CLOCCT(CLID)= 0
        END IF
C       update number of classes
        NCLAS= CLID
      ELSE
C       no room
        CLID= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLDEL
     I                  (CLDID)
C
C     + + + PURPOSE + + +
C     delete a class
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CLDID
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CLDID - class id to delete
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclas.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ID,I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CLDID.GT.0) THEN
C       the class exists to delete
        IF (CLDID.LT.NCLAS) THEN
C         move later classes
          DO 10 ID= CLDID,NCLAS-1
            CSEN(ID)= CSEN(ID+1)
            CLOC(ID)= CLOC(ID+1)
            CCON(ID)= CCON(ID+1)
            CACT(ID)= CACT(ID+1)
            CSYM(ID)= CSYM(ID+1)
            CCUN(ID)= CCUN(ID+1)
            CCSL(ID)= CCSL(ID+1)
            CMSIZE(ID)= CMSIZE(ID+1)
            CDSNCT(ID)= CDSNCT(ID+1)
            CADSNC(ID)= CADSNC(ID+1)
            CLOCCT(ID)= CLOCCT(ID+1)
            DO 5 I= 1,CDSNCT(ID)
              CDSN(I,ID)= CDSN(I,ID+1)
  5         CONTINUE
 10       CONTINUE
        END IF
C       update number of classes
        NCLAS= NCLAS- 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLPUT
     I                  (CLID,SEN,LOC,CON,ACT)
C
C     + + + PURPOSE + + +
C     update general info in a class
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CLID,ACT
      CHARACTER*8 SEN,LOC,CON
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CLID   - class id to update
C     SEN    - class senerio
C     LOC    - class location
C     CON    - class constituent
C     ACT    - class active flag(1-active)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclas.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CLID.GT.0 .AND. CLID.LE.NCLAS) THEN
C       the class exists to update
        CSEN(CLID)= SEN
        CLOC(CLID)= LOC
        CCON(CLID)= CON
        CACT(CLID)= ACT
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLPUTG
     I                   (CLID,SYM,UNSL,SELC,MSIZE)
C
C     + + + PURPOSE + + +
C     update map info in a class
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CLID,SYM,UNSL,SELC
      REAL        MSIZE
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CLID   - class id to update
C     SYM    - class map symbol
C     UNSL   - class unselect color
C     SELC   - class select color
C     MSIZE  - marker size
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclas.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CLID.GT.0 .AND. CLID.LE.NCLAS) THEN
C       the class exists to update
        CSYM(CLID)= SYM
        CCUN(CLID)= UNSL
        CCSL(CLID)= SELC
        CMSIZE(CLID)= MSIZE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLGET
     I                  (CLID,
     O                   SEN,LOC,CON,ACT)
C
C     + + + PURPOSE + + +
C     get general info about a class
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CLID,ACT
      CHARACTER*8 SEN,LOC,CON
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CLID   - class id to get info about
C     SEN    - class senerio
C     LOC    - class location
C     CON    - class constituent
C     ACT    - class active flag(1-active)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclas.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CLID.GT.0 .AND. CLID.LE.NCLAS) THEN
C       the class exists
        SEN= CSEN(CLID)
        LOC= CLOC(CLID)
        CON= CCON(CLID)
        ACT= CACT(CLID)
      ELSE
        SEN= ' '
        LOC= ' '
        CON= ' '
        ACT= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLGETG
     I                   (CLID,
     O                    SYM,UNSL,SELC,MSIZE)
C
C     + + + PURPOSE + + +
C     get map info about a class
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CLID,SYM,UNSL,SELC
      REAL        MSIZE
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CLID   - class id to update
C     SYM    - class map symbol
C     UNSL   - class unselect color
C     SELC   - class select color
C     MSIZE  - class marker size
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclas.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CLID.GT.0 .AND. CLID.LE.NCLAS) THEN
C       the class exists
        SYM  = CSYM(CLID)
        UNSL = CCUN(CLID)
        SELC = CCSL(CLID)
        MSIZE= CMSIZE(CLID)
      ELSE
        SYM  = 1
        UNSL = 1
        SELC = 1
        MSIZE= 1.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLFDSN
     I                   (CLID)
C
C     + + + PURPOSE + + +
C     identify datasets meeting specs in defined class
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      CLID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CLID    - class id number to find datasets for
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclas.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DSN,DSNCNT,DSNACT,DSNLOC,DLOCID(500),ITMP,IPOS
C
C     + + + EXTERNALS + + +
      EXTERNAL  TSESPC,TSDSM,TSDSAC,TSDSLC
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CLID.GT.0 .AND. CLID.LE.NCLAS) THEN
C       valid class, set specs
        CALL TSESPC(CSEN(CLID),CLOC(CLID),CCON(CLID))
        DSNCNT= 0
        DSNACT= 0
        DSNLOC= 0
        DSN   = 0
C
 10     CONTINUE
          DSN = DSN + 1
          CALL TSDSM (DSN)
          IF (DSN .GT. 0) THEN
C           save dsn
            DSNCNT= DSNCNT+ 1
            CDSN(DSNCNT,CLID)= DSN
C           active?
            CALL TSDSAC(DSN,
     O                  ITMP)
            IF (ITMP.EQ.1) THEN
C             it is active
              DSNACT= DSNACT+ 1
            END IF
C           location
            CALL TSDSLC(DSN,
     O                  ITMP)
            IF (DSNLOC .EQ. 0) THEN
C             first location
              DSNLOC= 1
              DLOCID(DSNLOC)= ITMP
            ELSE
C             already have it?
              IPOS= 0
 20           CONTINUE
                IPOS= IPOS+ 1
                IF (DLOCID(IPOS) .EQ. ITMP) THEN
C                 already have it
                  ITMP= 0
                END IF
              IF (IPOS.LT.DSNLOC .AND. ITMP.GT.0) GO TO 20
C
              IF (ITMP.GT.0) THEN
C               new one
                DSNLOC= DSNLOC+ 1
                DLOCID(DSNLOC)= ITMP
              END IF
            END IF
          ELSE
C           no more meet
            DSN= MXTS
          END IF
        IF (DSN .LT. MXTS) GO TO 10
C       save dsn count
        CDSNCT(CLID)= DSNCNT
        CADSNC(CLID)= DSNACT
        CLOCCT(CLID)= DSNLOC
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLSDSN
     I                   (CLID,
     O                    NLOC,NDSN,NADSN)
C
C     + + + PURPOSE + + +
C     summarize datasets in defined class
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      CLID,NLOC,NDSN,NADSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CLID    - class id number to find datasets for
C     NLOC    - number of locations
C     NDSN    - number of datasets
C     NADSN   - number of active datasets
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclas.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CLID.GT.0 .AND. CLID.LE.NCLAS) THEN
C       valid class
        NLOC = CLOCCT(CLID)
        NDSN = CDSNCT(CLID)
        NADSN= CADSNC(CLID)
      ELSE
C       bad class
        NLOC = 0
        NDSN = 0
        NADSN= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLACTQ
     M                   (CLID)
C
C     + + + PURPOSE + + +
C     identify next active class
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      CLID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CLID    - class id to start looking for active from
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclas.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DONFG
C
C     + + + END SPECIFICATIONS + + +
C
      DONFG= 0
C
      IF (CLID.GT.0 .AND. CLID.LE.NCLAS) THEN
C       may have an active class
 5      CONTINUE
          IF (CACT(CLID) .EQ. 1) THEN
C           active
            DONFG= 1
          ELSE
C           not active, try next
            CLID= CLID+ 1
          END IF
        IF (DONFG.EQ.0 .AND. CLID.LE.NCLAS) GO TO 5
      END IF
C
      IF (DONFG .EQ. 0) THEN
C       no active class
        CLID= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLGDSN
     I                   (CLID,MXDSN,ISTAT,
     O                    NDSN,DSN)
C
C     + + + PURPOSE + + +
C     datasets in specified class
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      CLID,MXDSN,ISTAT,NDSN,DSN(MXDSN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CLID    - class id number to find datasets for
C     MXDSN   - max number of datasets to return
C     ISTAT   - accept datasets with status >= this
C     NDSN    - number of datasets
C     DSN     - array of dataset numbers
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclas.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J,ITMP
C
C     + + + EXTERNALS + + +
      EXTERNAL  TSDSAC
C
C     + + + END SPECIFICATIONS + + +
C
C     assume no valid datasets
      NDSN= 0
C
      IF (CLID.GT.0 .AND. CLID.LE.NCLAS) THEN
C       valid class
        IF (CDSNCT(CLID) .GT. 0) THEN
C         some datasets to check
          J= 0
 5        CONTINUE
            J= J+ 1
C           active?
            CALL TSDSAC(CDSN(J,CLID),
     O                  ITMP)
            IF (ITMP .GE. ISTAT) THEN
C             need this one
              NDSN= NDSN+ 1
              DSN(NDSN)= CDSN(J,CLID)
            END IF
          IF (NDSN.LT.MXDSN .AND. J.LT.CDSNCT(CLID)) GO TO 5
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GTCDSN
     I                   (TSID,CLASID,
     O                    LDSN)
C
C     + + + PURPOSE + + +
C     get data set number from array of data sets in each class
C     (get the dsn of the xth data set in the yth class)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      TSID,CLASID,LDSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TSID   - order number of data set desired
C     CLASID - class number desired
C     LDSN   - data set number
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclas.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      LDSN= CDSN(TSID,CLASID)
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLESET
     I                   (SENNAM,LOCNAM)
C
C     + + + PURPOSE + + +
C     set classes for expert system
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*8  SENNAM,LOCNAM
C
C     + + + ARGUMENT DEFINTIONS + + +
C     SENNAM - active scenario name
C     LOCNAM - active location name
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclas.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,SELFG,I0,DSN,I1
      CHARACTER*8   BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL  CLADD,CLINIT,TSESPC,TSDSM,TSDSAS,CLFDSN
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      BLNK = ' '
C
      IF (NCLAS.GT.0) THEN
C       have some classes, see if any are active
        SELFG = 0
        DO 10 I = 1,NCLAS
          IF (CACT(I).EQ.1) THEN
C           this class is active
            IF (CADSNC(I).GT.0) THEN
C             some selected data sets in this class
              SELFG = 1
            END IF
          END IF
 10     CONTINUE
        IF (SELFG.EQ.0) THEN
C         no selected data sets in active classes, initialize
          CALL CLINIT
        END IF
      END IF
C
      IF (NCLAS.EQ.0) THEN
C       set up classes for hspexp
        CALL CLADD(I0,I)
        CALL CLADD(I0,I)
        CALL CLADD(I0,I)
C       first class all-all-all
C       second class obs-flow-all
        CSEN(2) = 'OBSERVED'
        CCON(2) = 'FLOW'
C       third class sim-flow-all
        CSEN(3) = SENNAM
        CCON(3) = 'FLOW'
C       select all flow at current loc
        CALL TSESPC (BLNK,LOCNAM,CCON(3))
        DSN = 0
 20     CONTINUE
          DSN = DSN + 1
          CALL TSDSM (DSN)
          IF (DSN.GT.0) THEN
C           found a data set, mark it selected
            CALL TSDSAS (DSN,I1)
          END IF
        IF (DSN.GT.0) GO TO 20
C       set selected data sets
        DO 30 I = 1,3
          CALL CLFDSN (I)
 30     CONTINUE
      END IF
C
      RETURN
      END
