C
C
C
      SUBROUTINE   DTINIT
C
C     + + + PURPOSE + + +
C     initialize date set data structure
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdate.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      NDATE= 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   DTCNT
     O                  (DCNT)
C
C     + + + PURPOSE + + +
C     return number of date sets
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DCNT
C
C     + + + ARGUMENT DEFINTIONS + + +
C     DCNT  - number of date sets
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdate.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      DCNT= NDATE
C
      RETURN
      END
C
C
C
      SUBROUTINE   DTADD
     I                  (DTBID,
     O                   DTID)
C
C     + + + PURPOSE + + +
C     add a new date set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DTBID,DTID
C
C     + + + ARGUMENT DEFINTIONS + + +
C     DTBID - date set base id - default values for new date set
C     DTID  - identifier of new date set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdate.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      DTID = NDATE+ 1
      IF (DTID .LE. MXDATE) THEN
C       room for a new date set
        IF (DTBID.GT.0 .AND. DTBID.LE.NDATE) THEN
C         use base for date set defaults
          DACT(DTID)  = 0
          DATETU(DTID)= DATETU(DTBID)
          DATETS(DTID)= DATETS(DTBID)
          DATAGG(DTID)= DATAGG(DTBID)
          DO 5 I= 1,6
            SDATE(I,DTID)= SDATE(I,DTBID)
            EDATE(I,DTID)= EDATE(I,DTBID)
  5       CONTINUE
          DO 7 I= 1,2
            SNSDAT(I,DTID)= SNSDAT(I,DTBID)
            SNEDAT(I,DTID)= SNEDAT(I,DTBID)
  7       CONTINUE
          CDATID(DTID)= '<COPY>'
        ELSE
C         general defaults
          DACT(DTID)  = 0
          DATETU(DTID)= 1
          DATETS(DTID)= 1
          DATAGG(DTID)= 1
          DO 6 I= 1,6
            SDATE(I,DTID)= 0
            EDATE(I,DTID)= 0
  6       CONTINUE
          DO 8 I= 1,2
            SNSDAT(I,DTID)= 0
            SNEDAT(I,DTID)= 0
  8       CONTINUE
          CDATID(DTID)= '<COPY>'
        END IF
C       update number of classes
        NDATE= DTID
      ELSE
C       no room
        DTID= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DTDEL
     I                  (DTDID)
C
C     + + + PURPOSE + + +
C     delete a class
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DTDID
C
C     + + + ARGUMENT DEFINTIONS + + +
C     DTDID - date set id to delete
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdate.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ID,I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DTDID.GT.0 .AND. DTDID.LE.NDATE) THEN
C       the date set exists to delete
        IF (DTDID.LT.NDATE) THEN
C         move later date sets
          DO 10 ID= DTDID,NDATE-1
            DACT(ID)  = DACT(ID+1)
            DATETU(ID)= DATETU(ID+1)
            DATETS(ID)= DATETS(ID+1)
            DATAGG(ID)= DATAGG(ID+1)
            DO 5 I= 1,6
              SDATE(I,ID)= SDATE(I,ID+1)
              EDATE(I,ID)= EDATE(I,ID+1)
  5         CONTINUE
            DO 7 I= 1,2
              SNSDAT(I,ID)= SNSDAT(I,ID+1)
              SNEDAT(I,ID)= SNEDAT(I,ID+1)
  7         CONTINUE
            CDATID(ID)= CDATID(ID+1)
 10       CONTINUE
        END IF
C       update number of date sets
        NDATE= NDATE- 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DTPUT
     I                  (DTID,ACT,CDSID,LSDATE,LEDATE,LSSDAT,LSEDAT,
     I                   DTTU,DTTS,DTAG)
C
C     + + + PURPOSE + + +
C     update general info in a date set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DTID,ACT,LSDATE(6),LEDATE(6),LSSDAT(2),LSEDAT(2),
     1            DTTU,DTTS,DTAG
      CHARACTER*8 CDSID
C
C     + + + ARGUMENT DEFINTIONS + + +
C     DTID   - date set to update
C     ACT    - active/inactive flag
C     CDSID  - character date set id
C     LSDATE - starting date and time
C     LEDATE - ending date and time
C     LSSDAT - season starting date
C     LSEDAT - season ending date
C     DTTU   - date set time units
C     DTTS   - date set time step
C     DTAG   - date set aggrigation flag
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdate.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DTID.GT.0 .AND. DTID.LE.NDATE) THEN
C       the date set exists to update
        DACT(DTID)  = ACT
        DATETU(DTID)= DTTU
        DATETS(DTID)= DTTS
        DATAGG(DTID)= DTAG
        DO 5 I= 1,6
          SDATE(I,DTID)= LSDATE(I)
          EDATE(I,DTID)= LEDATE(I)
  5     CONTINUE
        DO 7 I= 1,2
          SNSDAT(I,DTID)= LSSDAT(I)
          SNEDAT(I,DTID)= LSEDAT(I)
  7     CONTINUE
        CDATID(DTID)= CDSID
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DTGET
     I                  (DTID,
     O                   ACT,CDSID,LSDATE,LEDATE,LSSDAT,LSEDAT,
     O                   DTTU,DTTS,DTAG)
C
C     + + + PURPOSE + + +
C     get general info about a date set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DTID,ACT,LSDATE(6),LEDATE(6),LSSDAT(2),LSEDAT(2),
     1            DTTU,DTTS,DTAG
      CHARACTER*8 CDSID
C
C     + + + ARGUMENT DEFINTIONS + + +
C     DTID   - date set to update
C     ACT    - active/inactive flag
C     CDSID  - character date set id
C     LSDATE - starting date and time
C     LEDATE - ending date and time
C     LSSDAT - season starting date
C     LSEDAT - season ending date
C     DTTU   - date set time units
C     DTTS   - date set time step
C     DTAG   - date set aggrigation flag
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdate.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DTID.GT.0 .AND. DTID.LE.NDATE) THEN
C       the date set exists
        ACT  = DACT(DTID)
        DTTU = DATETU(DTID)
        DTTS = DATETS(DTID)
        DTAG = DATAGG(DTID)
        DO 5 I= 1,6
          LSDATE(I) = SDATE(I,DTID)
          LEDATE(I) = EDATE(I,DTID)
  5     CONTINUE
        DO 7 I= 1,2
          LSSDAT(I) = SNSDAT(I,DTID)
          LSEDAT(I) = SNEDAT(I,DTID)
  7     CONTINUE
        CDSID = CDATID(DTID)
      ELSE
        ACT= 0
        DTTU = 1
        DTTS = 1
        DTAG = 1
        DO 6 I= 1,6
          LSDATE(I) = 0
          LEDATE(I) = 0
  6     CONTINUE
        DO 8 I= 1,2
          LSSDAT(I) = 0
          LSEDAT(I) = 0
  8     CONTINUE
        CDSID = ' '
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DTACT
     O                  (ACTDT)
C
C     + + + PURPOSE + + +
C     find out which date set is currently active
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ACTDT
C
C     + + + ARGUMENT DEFINTIONS + + +
C     ACTDT - number of active date set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdate.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      ACTDT = 0
      IF (NDATE.GT.0) THEN
C       at least one date set exists
        I = 0
 10     CONTINUE
C         chech each date set for active
          I = I + 1
          IF (DACT(I).EQ.1) THEN
C           this date set is active
            ACTDT = I
          END IF
        IF (ACTDT.EQ.0 .AND. I.LT.NDATE) GO TO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DTSETA
     I                   (ACTDT)
C
C     + + + PURPOSE + + +
C     set this date set to active
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ACTDT
C
C     + + + ARGUMENT DEFINTIONS + + +
C     ACTDT - number of active date set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdate.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      DACT(ACTDT) = 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   DTSETI
     I                   (ACTDT)
C
C     + + + PURPOSE + + +
C     set this date set to inactive
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ACTDT
C
C     + + + ARGUMENT DEFINTIONS + + +
C     ACTDT - number of inactive date set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdate.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      DACT(ACTDT) = 0
C
      RETURN
      END
