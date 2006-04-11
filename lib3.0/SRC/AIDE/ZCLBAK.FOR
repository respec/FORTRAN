C
C
C
      SUBROUTINE   ZCBCHR
     I                   (CCBID,CFLD,
     O                    ZERR)
C
C     + + + PURPOSE + + +
C     Perform AIDE character response callback.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CCBID, CFLD, ZERR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CCBID  - field call back id number
C     CFLD   - current field id number
C     ZERR   - error from call back
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZCBCH1,ZCBCH2,ZCBCH3,ZCBCH4,ZCBCH5
      EXTERNAL   ZCBCH6,ZCBCH7,ZCBCH8,ZCBCH9
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CCBID .EQ. 0) THEN
C       no call back
        ZERR = 0
      ELSE IF (CCBID .EQ. 1) THEN
        CALL ZCBCH1(CFLD,
     O              ZERR)
      ELSE IF (CCBID .EQ. 2) THEN
        CALL ZCBCH2(CFLD,
     O              ZERR)
      ELSE IF (CCBID .EQ. 3) THEN
        CALL ZCBCH3(CFLD,
     O              ZERR)
      ELSE IF (CCBID .EQ. 4) THEN
        CALL ZCBCH4(CFLD,
     O              ZERR)
      ELSE IF (CCBID .EQ. 5) THEN
        CALL ZCBCH5(CFLD,
     O              ZERR)
      ELSE IF (CCBID .EQ. 6) THEN
        CALL ZCBCH6(CFLD,
     O              ZERR)
      ELSE IF (CCBID .EQ. 7) THEN
        CALL ZCBCH7(CFLD,
     O              ZERR)
      ELSE IF (CCBID .EQ. 8) THEN
        CALL ZCBCH8(CFLD,
     O              ZERR)
      ELSE IF (CCBID .EQ. 9) THEN
        CALL ZCBCH9(CFLD,
     O              ZERR)
      ELSE
        WRITE(99,*) 'BAD CHARACTER CALLBACK ID:',CCBID,CFLD
        ZERR= 0
      END IF
C
      IF (ZERR .EQ. -1) THEN
        WRITE(99,*) 'CALLED CHARACTER DUMMY CALLBACK:', CCBID
        ZERR= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZCBFLD
     I                   (FCBID,CFLD,
     O                    ZERR)
C
C     + + + PURPOSE + + +
C     Perform AIDE field change callback.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FCBID, CFLD, ZERR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FCBID  - field call back id number
C     CFLD   - current field id number
C     ZERR   - error from call back
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZCBFL1,ZCBFL2,ZCBFL3,ZCBFL4,ZCBFL5
      EXTERNAL   ZCBFL6,ZCBFL7,ZCBFL8,ZCBFL9
C
C     + + + END SPECIFICATIONS + + +
C
      IF (FCBID .EQ. 0) THEN
C       no call back
        ZERR = 0
      ELSE IF (FCBID .EQ. 1) THEN
        CALL ZCBFL1(CFLD,
     O              ZERR)
      ELSE IF (FCBID .EQ. 2) THEN
        CALL ZCBFL2(CFLD,
     O              ZERR)
      ELSE IF (FCBID .EQ. 3) THEN
        CALL ZCBFL3(CFLD,
     O              ZERR)
      ELSE IF (FCBID .EQ. 4) THEN
        CALL ZCBFL4(CFLD,
     O              ZERR)
      ELSE IF (FCBID .EQ. 5) THEN
        CALL ZCBFL5(CFLD,
     O              ZERR)
      ELSE IF (FCBID .EQ. 6) THEN
        CALL ZCBFL6(CFLD,
     O              ZERR)
      ELSE IF (FCBID .EQ. 7) THEN
        CALL ZCBFL7(CFLD,
     O              ZERR)
      ELSE IF (FCBID .EQ. 8) THEN
        CALL ZCBFL8(CFLD,
     O              ZERR)
      ELSE IF (FCBID .EQ. 9) THEN
        CALL ZCBFL9(CFLD,
     O              ZERR)
      ELSE
        WRITE(99,*) 'BAD FIELD CALLBACK ID:',FCBID,CFLD
        ZERR= 0
      END IF
C
      IF (ZERR .EQ. -1) THEN
        WRITE(99,*) 'CALLED FIELD DUMMY CALLBACK:', FCBID
        ZERR= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZCBEXT
     I                   (XCBID,
     O                    ZERR)
C
C     + + + PURPOSE + + +
C     Perform AIDE screen exit callback.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   XCBID, ZERR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XCBID  - exit call back id number
C     ZERR   - error from call back
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZCBEX1,ZCBEX2,ZCBEX3,ZCBEX4,ZCBEX5
      EXTERNAL   ZCBEX6,ZCBEX7,ZCBEX8,ZCBEX9
C
C     + + + END SPECIFICATIONS + + +
C
      IF (XCBID .EQ. 0) THEN
C       no call back
        ZERR = 0
      ELSE IF (XCBID .EQ. 1) THEN
        CALL ZCBEX1
     O             (ZERR)
      ELSE IF (XCBID .EQ. 2) THEN
        CALL ZCBEX2
     O             (ZERR)
      ELSE IF (XCBID .EQ. 3) THEN
        CALL ZCBEX3
     O             (ZERR)
      ELSE IF (XCBID .EQ. 4) THEN
        CALL ZCBEX4
     O             (ZERR)
      ELSE IF (XCBID .EQ. 5) THEN
        CALL ZCBEX5
     O             (ZERR)
      ELSE IF (XCBID .EQ. 6) THEN
        CALL ZCBEX6
     O             (ZERR)
      ELSE IF (XCBID .EQ. 7) THEN
        CALL ZCBEX7
     O             (ZERR)
      ELSE IF (XCBID .EQ. 8) THEN
        CALL ZCBEX8
     O             (ZERR)
      ELSE IF (XCBID .EQ. 9) THEN
        CALL ZCBEX9
     O             (ZERR)
      ELSE
        WRITE(99,*) 'BAD EXIT CALLBACK ID:',XCBID
        ZERR= 0
      END IF
C
      IF (ZERR .EQ. -1) THEN
        WRITE(99,*) 'CALLED EXIT DUMMY CALLBACK:', XCBID
        ZERR= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZCBSST
     I                   (LCLBST)
C
C     + + + PURPOSE + + +
C     Set common block string containing needed
C     information for callback routines.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*1 LCLBST(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LCLBST - character array containing information
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclbak.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYC
C
C     + + + END SPECIFICATIONS + + +
C
      I = 80
      CALL COPYC (I,LCLBST,CLBSTR)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZCBGST
     O                   (LCLBST)
C
C     + + + PURPOSE + + +
C     Get common block string containing needed
C     information for callback routines.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*1 LCLBST(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LCLBST - character array containing information
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclbak.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYC
C
C     + + + END SPECIFICATIONS + + +
C
      I = 80
      CALL COPYC (I,CLBSTR,LCLBST)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSETCB
     I                   (LCBCID,LCBFID,LCBXID)
C
C     + + + PURPOSE + + +
C     Set common block callback IDs for character response,
C     field exit, and screen exit events.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LCBCID,LCBFID,LCBXID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LCBCID - character response event callback ID
C     LCBFID - field exit event callback ID
C     LCBXID - screen exit event callback ID
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cclbak.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CBCHID = LCBCID
      CBFLID = LCBFID
      CBEXID = LCBXID
C
      RETURN
      END
