C
C
C
      SUBROUTINE WDGTTM
     I                  ( WDMFL, DSN,
     O                    DATBGN, DATEND, TSSTEP, TCODE, TSFILL, RETC )
C
C     + + + PURPOSE + + +
C     Get period of record, time step, and missing value indicator
C     from a data set.  Note that if either time step or time code
C     is not present, they will be defaulted to 1 day (4).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL, DSN, DATBGN(6), DATEND(6), TSSTEP, TCODE, RETC
      REAL      TSFILL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of the wdm file
C     DSN    - number of the data set
C     DATBGN - starting date of data in DSN
C     DATEND - ending date of data in DSN
C     TSSTEP - time step, in TCODE units of the data in DSN
C              defaults to 1, if not present
C     TCODE  - time units of the data in DSN
C              defaults to 4 (day), if not present
C     TSFILL - missing value indicator
C              defaults to 0.0, if not present
C     RETC   - return code
C                 0 - everything is ok
C                -6 - no data present in data set
C               -81 - data set does not exist
C               -82 - data set exists but is not time series
C               -85 - trying to write to a read-only data set
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   INDEX, DATE(12), GPFLG, RET, LEN1, LEN6, ZERO, TDSFRC
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDBSGI, WDBSGR, WTFNDT
      EXTERNAL  COPYI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  LEN1, LEN6, ZERO
     $     /   1,    6,    0 /
C
C     + + + END SPECIFICATIONS + + +
C
C     get period of record
      GPFLG = 1
      CALL WTFNDT ( WDMFL, DSN, GPFLG,
     O              TDSFRC, DATE(1), DATE(7), RET )
      IF (RET .EQ. 0) THEN
C       data is present
        CALL COPYI ( LEN6, DATE(1), DATBGN )
        CALL COPYI ( LEN6, DATE(7), DATEND )
C       get time step
        INDEX = 33
        CALL WDBSGI ( WDMFL, DSN, INDEX, LEN1, TSSTEP, RET )
        IF (RET .EQ. 0) THEN
C         time step present, get time code
          INDEX = 17
          CALL WDBSGI ( WDMFL, DSN, INDEX, LEN1, TCODE, RET )
        END IF
        IF (RET .EQ. -107) THEN
C         time step or time code is not present, default to 1 day
          TSSTEP = 1
          TCODE  = 4
          RET = 0
        END IF
C       get missing value filler
        INDEX = 32
        CALL WDBSGR ( WDMFL, DSN, INDEX, LEN1, TSFILL, RET )
        IF (RET .EQ. -107) THEN
C         default to 0.0
          TSFILL = 0.0
          RET = 0
        END IF
        RETC = 0
      ELSE
C       no data present
        CALL COPYI ( LEN6, ZERO, DATBGN )
        CALL COPYI ( LEN6, ZERO, DATEND )
        RETC = RET
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DSINFO
     I                    (WDMFL,DSN,LEN,
     M                     TEXT)
C
C     + + + PURPOSE + + +
C     This routine created a character string describing a time
C     series data set.  Its priority on use of attributes is STAID,
C     ISTAID, STANAM, TSTYPE, data set number.  The later are only
C     used when the former aren't available.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMFL, DSN, LEN
      CHARACTER*1  TEXT(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of wdm file
C     DSN    - data set number
C     LEN    - length of character string
C     TEXT   - character string to be filled
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SAIND, SALEN, ISTAID, I8, RETCOD, I80, I16, IPOS,
     $          I6, OLEN, I0
      CHARACTER*1  STAID(16), STANAM(48), TSTYPE(4), CDSN(16),BLNK
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBSGI, WDBSGC, CHRCHR, INTCHR, LENSTR, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  CDSN/'D','a','t','a',' ','s','e','t',' ','n','u','m',
     $           'b','e','r',' '/,  BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I6  = 6
      I8  = 8
      I16 = 16
      I80 = 80
      CALL ZIPC (LEN,BLNK,TEXT)
C
C     try character station id
      SAIND= 2
      SALEN= 16
      CALL WDBSGC (WDMFL,DSN,SAIND,SALEN,
     O             STAID,RETCOD)
      IF (RETCOD.EQ.0 .AND. 16.LT.LEN) THEN
C       station id found
        CALL CHRCHR (SALEN,STAID,TEXT(1))
      ELSE
C       try integer station id
        SAIND= 51
        SALEN= 1
        CALL WDBSGI (WDMFL,DSN,SAIND,SALEN,
     O               ISTAID,RETCOD)
        IF (RETCOD .EQ. 0 .AND. 8.LT.LEN) THEN
C         station id found
          CALL INTCHR (ISTAID,I8,I0,OLEN,TEXT(1))
        END IF
      END IF
C     find current length
      IPOS = LENSTR(LEN,TEXT(1))
      IF (IPOS .GT. 0) THEN
        IPOS = IPOS + 2
      ELSE
        IPOS = 1
      END IF
C     try station description
      SAIND= 45
      SALEN= 48
      CALL WDBSGC (WDMFL,DSN,SAIND,SALEN,
     O             STANAM,RETCOD)
      IF (RETCOD.EQ.0 .AND. IPOS+SALEN.LT.LEN) THEN
C       station name found, put in title
        CALL CHRCHR (SALEN,STANAM,TEXT(IPOS))
      ELSE
C       put time-series type in title
        SAIND= 1
        SALEN= 4
        CALL WDBSGC (WDMFL,DSN,SAIND,SALEN,
     O               TSTYPE,RETCOD)
        IF (RETCOD.EQ.0 .AND. IPOS+SALEN.LT.LEN) THEN
          CALL CHRCHR (SALEN,TSTYPE,TEXT(IPOS))
        ELSE
C         use data set number
          IF (IPOS+22 .LT. LEN) THEN
            CALL CHRCHR (I16,CDSN,TEXT(IPOS))
            CALL INTCHR (DSN,I6,I0,OLEN,TEXT(IPOS+16))
          END IF
        END IF
      END IF
C
      RETURN
      END
