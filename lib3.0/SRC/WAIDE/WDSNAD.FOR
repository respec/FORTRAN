C
C
C
      SUBROUTINE   WDSNAD
     I                   ( WDMSFL, DSTYPE, SPACE,
     M                     DSNL, DSN,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     This routine adds a dataset to a WDM file.  If the requested
C     dataset DSN already exists, the first available dataset after
C     DSNL is added.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL, DSTYPE, SPACE, DSNL, DSN, RETC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of the WDM file
C     DSTYPE - data-set type to be added
C              1 - time series
C              2 - table
C     SPACE  - indicator flag for space allocation
C              1 - default space
C              2 - increased space for attributes and reduced
C                  space for group pointers.  Allows for 100
C                  search attributes with space of 130 and
C                  pointers for 7 data groups.
C     DSNL   - input - dataset number to start at if the input
C              DSN already exists
C              output - remains unchanged if DSN is available,
C              otherwise, the dataset that was added
C     DSN    - input - requested dataset number
C              output - dataset added
C     RETC   - return code
C              0 - successful
C              /=0 - unsuccessful
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   EXIST, NDN, NUP, NSA, NSASP, NDP, PSA
C
C     + + + FUNCTIONS + + +
      INTEGER   WDCKDT
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDCKDT, WDBCRL, WDLBAX
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  NDN, NUP, NSA, NSASP, NDP
     #     /  1,   1, 100,   130,   7 /
C
C     NDN    - number of down pointers
C     NUP    - number of up pointers
C     NSA    - nusdmber of search attributes
C     NSASP  - space for search attributes
C     NDP    - number of group pointers
C
C     + + + END SPECIFICATIONS + + +
C
      RETC = 0
      IF (DSN .GT. 0  .AND.  DSN .LE. 32000) THEN
C       check to see if DSN exists
        EXIST = WDCKDT ( WDMSFL, DSN )
        IF (EXIST .NE. 0) THEN
C         DSN already exists, find new one
          DSN = 0
        END IF
      END IF
C
      IF (DSN .LE. 0  .OR.  DSN .GT. 32000) THEN
C       find an unused DSN
        DSNL = DSNL - 1
        IF (DSNL .LT. 0  .OR.  DSNL .GT. 31999) DSNL = 0
 100    CONTINUE
          DSNL = DSNL + 1
          EXIST = WDCKDT ( WDMSFL, DSNL )
        IF (EXIST .NE. 0) GO TO 100
        DSN = DSNL
      END IF
C
      IF (SPACE .EQ. 2) THEN
C       increase space for attributes, reduce space for group pointers
        CALL WDLBAX ( WDMSFL, DSN, DSTYPE, NDN, NUP, NSA, NSASP, NDP,
     O                PSA )
      ELSE
C       use default space allocation
        CALL WDBCRL ( WDMSFL, DSN, DSTYPE, RETC )
      END IF
C
      RETURN
      END
