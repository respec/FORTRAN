C
C
C
      SUBROUTINE   WDGTTM
     I                   ( WDMFL, DSN,
     O                     DATBGN, DATEND, TSSTEP, TCODE, TSFILL, RETC )
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
        DATBGN= 0
        DATEND= 0
        RETC  = RET
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DSINFO
     I                    (WDMFL,DSN,LEN,
     O                     TEXT)
C
C     + + + PURPOSE + + +
C     This routine creates a character string describing a data
C     set.  The expected content of TEXT is attributes STAID and
C     STANAM; this assumes LEN is long enough to accomodate both.
C     If attribute STAID is not present or LEN is < 16 attribute
C     ISTAID will be used.  If STANAM is not available or the
C     remaining space is less than 48, attribute TSTYPE will be
C     used if there is room.  If STAID, ISTAID, STANAM, and TSTYPE
C     cannot be retrieved, the data set number is used.
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
      INTEGER   SAIND, SALEN, ISTAID(1), I8, RETCOD, I80, I16, IPOS,
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
          CALL INTCHR (ISTAID(1),I8,I0,OLEN,TEXT(1))
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
C
C
C
      SUBROUTINE   DSINF1
     I                    (WDMFL,DSN,LEN,
     O                     TEXT)
C
C     + + + PURPOSE + + +
C     This routine attempts to retrieve the station number of
C     the data set.  TEXT will contain the attribute STAID if
C     it is present in the data set and LEN is at least 16.
C     If STAID is not present or LEN is < 16, TEXT will contain
C     the attribute ISTAID (assuming it is present and LEN is
C     at least 8).  If neither STAID nor ISTAID is present,
C     and LEN is at least 8, TEXT will be returned with the
C     string 'no info.'
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
      INTEGER      SAIND, SALEN, ISTAID(1), I8, RETCOD, I0, OLEN
      CHARACTER*1  STAID(16), BLNK, STAR
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBSGI, WDBSGC, CHRCHR, INTCHR, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  BLNK, STAR
     $     / ' ', '*' /
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I8  = 8
      CALL ZIPC (LEN,BLNK,TEXT)
      IF (LEN .GE. 16) THEN
C       character station id will fit, try to get it
        SAIND = 2
        SALEN = 16
        CALL WDBSGC (WDMFL,DSN,SAIND,SALEN,   STAID,RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         found station id, use it
          CALL CHRCHR ( SALEN, STAID, TEXT )
        END IF
      ELSE
C       not enough room, try integer station id
        RETCOD = -1
      END IF
      IF (LEN .GE. 8  .AND.  RETCOD .NE. 0) THEN
C       no character station id, integer will fit, try to get it
        SAIND= 51
        SALEN= 1
        CALL WDBSGI (WDMFL,DSN,SAIND,SALEN,ISTAID,RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         found station id
          CALL INTCHR (ISTAID(1),I8,I0,OLEN,TEXT)
        ELSE
C         no information, fill with *
          CALL ZIPC ( LEN, STAR, TEXT )
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DSINF2
     I                   ( WDMFL, DSN, LENT1, LENT2, LENT3,
     I                     DATBGN, DATEND,
     M                     TITLE1, TITLE2, PERIOD )
C
C     + + + PURPOSE + + +
C     Fill a character string with information describing the data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMFL, DSN, LENT1, LENT2, LENT3,
     $             DATBGN(3), DATEND(3)
      CHARACTER*1  TITLE1(LENT1), TITLE2(LENT2), PERIOD(LENT3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of wdm file
C     DSN    - data set number
C     LENT1  - length of first title
C     LENT2  - length of second title
C     LENT3  - length of period of record
C     TITLE1 - first title, contains station name if available,
C              else contains station id if available,
C              else contains dsn and, if available, TSTYPE.
C     TITLE2 - second title, contains station id if available and
C              if station name was available for TITLE1,
C              else left blank
C     PERIOD - period of record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SAIND, SALEN, ISTAID(1), RETCOD,
     $          OLEN, LOC, JUSTR, FOUND, I4, I5, I8, I15
      CHARACTER*1  STAID(16), STANAM(48), TSTYPE(4)
      CHARACTER*4  CTYPE
      CHARACTER*8  C8
      CHARACTER*16 CDSN
      CHARACTER*35 CTEMP
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBSGI, WDBSGC, CHRCHR, INTCHR, CVARAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   I4, I5, I8, I15, CDSN,               CTYPE
     $    /   4,  5,  8,  15, 'Data set number ', 'type' /
C
C     + + + OUTPUT FORMATS + + +
 2005 FORMAT ( I8 )
 2010 FORMAT ( 'For period ', I4,'/',I2,'/',I2,
     $                ' to ', I4,'/',I2,'/',I2  )
C
C     + + + END SPECIFICATIONS + + +
C
      FOUND = 0
      JUSTR  = 0
C
C     look for station name
      SAIND= 45
      SALEN= 48
      CALL WDBSGC (WDMFL,DSN,SAIND,SALEN,
     O             STANAM,RETCOD)
      IF (RETCOD .EQ. 0) THEN
C       station name found, put in title
        CALL CHRCHR (SALEN,STANAM,TITLE1)
        FOUND = 1
      END IF
C
C     look for character station id
      SAIND= 2
      SALEN= 16
      CALL WDBSGC (WDMFL,DSN,SAIND,SALEN,
     O             STAID,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       character station id found
        IF (FOUND .EQ. 0) THEN
C         put station id in first title since station name not found
          CALL CHRCHR (SALEN,STAID,TITLE1)
          FOUND = 1
        ELSE
C         put station id in second title
          CALL CHRCHR (SALEN,STAID,TITLE2)
        END IF
      ELSE
C       character station id not found, try integer
        SAIND= 51
        SALEN= 1
        CALL WDBSGI (WDMFL,DSN,SAIND,SALEN,
     O               ISTAID,RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         integer station id found
          IF (FOUND .EQ. 0) THEN
C           put in first title since still empty
            WRITE(C8,2005) ISTAID(1)
            CALL CVARAR (I8,C8,I8,TITLE1)
            FOUND = 1
          ELSE
C           put station id in second title
            CALL INTCHR (ISTAID(1),I8,JUSTR,OLEN,TITLE2)
          END IF
        END IF
      END IF
C
      IF (FOUND .EQ. 0) THEN
C       both titles still empty, put dsn in first title
        CALL CVARAR ( I15, CDSN, I15, TITLE1 )
        CALL INTCHR (DSN,I5,JUSTR,OLEN,TITLE1(16))
        LOC = 16 + OLEN
C       try to add tstype
        SAIND= 1
        SALEN= 4
        CALL WDBSGC (WDMFL,DSN,SAIND,SALEN,
     O               TSTYPE,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         add type
          CALL CVARAR ( I4, CTYPE, I4, TITLE1(LOC) )
          LOC = LOC + 5
          CALL CHRCHR (SALEN,TSTYPE,TITLE1(LOC))
        END IF
      END IF
C
C     period of record
      WRITE (CTEMP,2010) DATBGN, DATEND
      CALL CVARAR ( LENT3, CTEMP, LENT3, PERIOD )
C
      RETURN
      END
C
C
C
      SUBROUTINE   GTINFO
     I                    ( WDMFL, DSN,
     O                      STAID, STANAM, TSTYPE )
C
C     + + + PURPOSE + + +
C     This rourine returns desscriptive information about the
C     requested data set.  
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMFL, DSN
      CHARACTER*1  STAID(16), STANAM(48), TSTYPE(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of wdm file
C     DSN    - data set number
C     STAID  - station id (first tries for attribute staid,
C              then tries istaid; istaid is left justified)
C     STANAM - station name (first tries for attribute stanam,
C              then tries descrp; descrp is truncated to 48
C              characters)
C     TSTYPE - tstype
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SAIND, SALEN, ISTAID(1),  RETCOD, LEN, JUSTL, OLEN
      CHARACTER*1  DESCRP(80), BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBSGI, WDBSGC
      EXTERNAL   CHRCHR, INTCHR, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  BLNK, JUSTL
     $     / ' ',     1 /
C
C     + + + END SPECIFICATIONS + + +
C
C     try character station id
      SAIND= 2
      SALEN= 16
      CALL WDBSGC ( WDMFL, DSN, SAIND, SALEN, STAID, RETCOD )
      IF (RETCOD .NE. 0) THEN
C       try integer station id
        SAIND= 51
        SALEN= 1
        CALL WDBSGI ( WDMFL, DSN, SAIND, SALEN, ISTAID, RETCOD )
        IF (RETCOD .EQ. 0) THEN
C         station id found
          LEN = 16
          CALL INTCHR ( ISTAID(1), LEN, JUSTL, OLEN, STAID )
        ELSE
C         nothing, make sure it is blank
          LEN = 16
          CALL ZIPC ( LEN, BLNK, STAID )
        END IF
      END IF
C
C     try station description
      SAIND= 45
      SALEN= 48
      CALL WDBSGC ( WDMFL, DSN, SAIND, SALEN, STANAM, RETCOD )
      IF (RETCOD .NE. 0) THEN
C       station name not found, try descrp
        SAIND = 10
        SALEN = 80
        CALL WDBSGC ( WDMFL, DSN, SAIND, SALEN, DESCRP, RETCOD )
        IF (RETCOD .EQ. 0) THEN
C         descrp found, user first 48 characters
          LEN = 48
          CALL CHRCHR ( LEN, DESCRP, STANAM )
        ELSE
C         nothing, make sure it is blank
          LEN = 48
          CALL ZIPC ( LEN, BLNK, STANAM )
        END IF
      END IF
C
C     try tstype
      SAIND= 1
      SALEN= 4
      CALL WDBSGC ( WDMFL, DSN, SAIND, SALEN, TSTYPE, RETCOD )
      IF (RETCOD .NE. 0) THEN
C       nothing, make sure it is blank
        LEN = 4
        CALL ZIPC ( LEN, BLNK, TSTYPE )
      END IF
C
      RETURN
      END
