C
C
C
      SUBROUTINE   TSFLAT
     I                   (WDMSFL,DSN,SFILE,IHEAD,DAFMT,BEFLG,
     I                    TS,TU,QC,OV,TSFILL,BSDATE,BEDATE,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     Process time-series data values from flat file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,DSN,SFILE,IHEAD,BEFLG,TS,TU,QC,OV,
     $             BSDATE(6),BEDATE(6),RETCOD
      REAL         TSFILL
      CHARACTER*64 DAFMT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - number of data set on which data is being stored
C     SFILE  - source flat file containing time-series data
C     IHEAD  - number of header lines in source file
C     DAFMT  - format of data on source file
C     BEFLG  - begin/end time tag flag,
C              0 - date value indicates the start of the time interval
C              1 - date value indicates the end of the time interval
C     TS     - time step of the data
C     TU     - time units of the data
C     QC     - quality code of the data
C     OV     - overwrite flag (1 - overwrite existing data)
C     TSFILL - fill value for missing data values
C     BSDATE - start date of data being stored
C     BEDATE - end date of data being stored
C     RETCOD - exit code
C
C     + + + PARAMETERS + + +
      INTEGER       MAXBUF,      MXVAL
      PARAMETER    (MAXBUF=20000,MXVAL=30)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,I6,CDATE(6),LHEAD,LSDATE(6),LBEFLG,
     $              DATPOS,DTSPOS,DTEPOS,DOFF,LVAL,NVAL,NRVAL,
     $              DONFG,TOTVAL,VTYPE(MXVAL),XDATE(6),TMPDAT(6)
      REAL          DBUFF(MAXBUF),RVAL(MXVAL)
      CHARACTER*1   DATSTR(6)
      CHARACTER*400 BUFF
C
C     + + + INTRINSICS + + +
      INTRINSIC     INDEX
C
C     + + + EXTERNALS + + +
      EXTERNAL      TIMDIF, GFMVAL, WDTPUT
      EXTERNAL      ZIPR, COPYR, COPYI, TIMBAK, TIMADD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA DATSTR /'Y','M','D','H','N','S'/
      DATA VTYPE/MXVAL*2/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A400)
C
C     + + + END SPECIFICATIONS + + +
C
      I6   = 6
C
C     add data values
      DTSPOS = 7
      DTEPOS = 0
      DO 5 I = 1,6
C       find start/end of date specs in format
        DATPOS = INDEX(DAFMT,DATSTR(I))
        IF (DATPOS.GT.0) THEN
C         this date position specified in format
          IF (I.LT.DTSPOS) THEN
C           new date start position
            DTSPOS = I
          END IF
          IF (I.GT.DTEPOS) THEN
C           new date end position
            DTEPOS = I
          END IF
        END IF
 5    CONTINUE
      LBEFLG = BEFLG
C     fill data buffer with missing value
      CALL ZIPR (MAXBUF,TSFILL,DBUFF)
C     make local copy of start date
      CALL COPYI (I6,BSDATE,LSDATE)
      IF (LSDATE(1).GT.0 .AND. BEDATE(1).GT.0) THEN
C       start/end date specified, determine total number of values
        CALL TIMDIF (LSDATE,BEDATE,TU,TS,
     O               TOTVAL)
        IF (TOTVAL.GT.MAXBUF) THEN
C         only process max size of buffer at a time
          TOTVAL = MAXBUF
        END IF
      ELSE
C       allow max number of values
        TOTVAL = MAXBUF
      END IF
C
      NVAL  = 0
      LVAL  = 0
      DONFG = 0
      LHEAD = IHEAD
 10   CONTINUE
C       get next record
 20     CONTINUE
C         skip any header lines
          READ (SFILE,1000,END=40) BUFF
          LHEAD = LHEAD - 1
          GO TO 30
 40       CONTINUE
C           get here on end of file
            DONFG = 1
 30       CONTINUE
        IF (LHEAD.GE.0) GO TO 20
        IF (DONFG.EQ.0) THEN
C         process data value
          CALL GFMVAL (DAFMT,BUFF,VTYPE,
     O                 CDATE,NRVAL,RVAL)
          IF (DTSPOS.GT.1 .AND. DTSPOS.LT.7) THEN
C           fill current date with base date up to start position
            DO 60 I = 1,DTSPOS-1
              CDATE(I) = LSDATE(I)
 60         CONTINUE
          END IF
          IF (DTEPOS.GT.0 .AND. DTEPOS.LT.6) THEN
C           fill current date with base date up to start position
            DO 70 I = DTEPOS+1,6
CPBD             CDATE(I) = LSDATE(I)
              CDATE(I) = 0
 70         CONTINUE
          END IF
          IF (LSDATE(1).EQ.0) THEN
C           base date not yet defined, use initial date value read from file
            CALL COPYI (I6,CDATE,LSDATE)
            IF (BEDATE(1).GT.0) THEN
C             end date defined also, determine total number of values
              CALL TIMDIF (LSDATE,BEDATE,TU,TS,
     O                     TOTVAL)
              IF (TOTVAL.GT.MAXBUF) THEN
C               only process max size of buffer at a time
                TOTVAL = MAXBUF
              END IF
            END IF
          END IF
          IF (DTSPOS.LT.7) THEN
C           date specified on file, use to position in data buffer
            CALL TIMDIF (LSDATE,CDATE,TU,TS,
     O                   DOFF)
            NVAL = DOFF + 1
          ELSE
C           dates not in use, just add to next buffer position
            NVAL = NVAL + 1
          END IF
          IF (NVAL+NRVAL-1.GT.TOTVAL) THEN
C           buffer full, output to WDM now
            IF (LBEFLG.EQ.1) THEN
C             back up a time interval for start date
              CALL COPYI (I6,LSDATE,TMPDAT)
              DO 80 I = 1,TS
                CALL TIMBAK (TU,
     M                       TMPDAT)
 80           CONTINUE
              CALL WDTPUT (WDMSFL,DSN,TS,TMPDAT,LVAL,
     I                     OV,QC,TU,DBUFF,
     O                     RETCOD)
            ELSE
              CALL WDTPUT (WDMSFL,DSN,TS,LSDATE,LVAL,
     I                     OV,QC,TU,DBUFF,
     O                     RETCOD)
            END IF
C           re-init data buffer
            CALL ZIPR (LVAL,TSFILL,DBUFF)
C           update start date
            CALL TIMADD (LSDATE,TU,TS,LVAL,
     O                   XDATE)
            CALL COPYI (I6,XDATE,LSDATE)
C            NVAL = 1
C           pbd bug fix, first time step not always at first value
            IF (DTSPOS.LT.7) THEN
C             date specified on file, use to position in data buffer
              CALL TIMDIF (LSDATE,CDATE,TU,TS,
     O                     DOFF)
              NVAL = DOFF + 1
            ELSE
C             dates not in use, reset buffer position
              NVAL = 1
            END IF
            IF (BEDATE(1).GT.0) THEN
C             determine total number of values
              CALL TIMDIF (LSDATE,BEDATE,TU,TS,
     O                     TOTVAL)
              IF (TOTVAL.EQ.0) THEN
C               at end of time period
                DONFG = 1
              ELSE IF (TOTVAL.GT.MAXBUF) THEN
C               only process max size of buffer at a time
                TOTVAL = MAXBUF
              END IF
            ELSE
C             allow max number of values
              TOTVAL = MAXBUF
            END IF
          END IF
C         put values in buffer
          CALL COPYR (NRVAL,RVAL,DBUFF(NVAL))
C         save current number of values
          NVAL = NVAL + NRVAL - 1
          LVAL = NVAL
        END IF
        IF (DONFG.EQ.1) THEN
C         done processing values
          IF (NVAL.GT.0) THEN
C           put data on WDM file
            IF (LBEFLG.EQ.1) THEN
C             back up a time interval for start date
              CALL COPYI (I6,LSDATE,TMPDAT)
              DO 90 I = 1,TS
                CALL TIMBAK (TU,
     M                       TMPDAT)
 90           CONTINUE
              CALL WDTPUT (WDMSFL,DSN,TS,TMPDAT,NVAL,
     I                     OV,QC,TU,DBUFF,
     O                     RETCOD)
            ELSE
              CALL WDTPUT (WDMSFL,DSN,TS,LSDATE,NVAL,
     I                     OV,QC,TU,DBUFF,
     O                     RETCOD)
            END IF
          ELSE
C           no values to write
            RETCOD = 1
          END IF
        END IF
      IF (DONFG.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   GFMVAL
     I                   (CFMT,BUFF,VALTYP,
     O                    IVAL,NRVAL,RVAL)
C
C     + + + PURPOSE + + +
C     Use format statement for data input to generate date
C     and data values for the current data record.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       VALTYP(*),IVAL(6),NRVAL
      REAL          RVAL(*)
      CHARACTER*64  CFMT
      CHARACTER*400 BUFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CFMT   - character string containing format for dates/data
C     BUFF   - current data record being processed
C     VALTYP - array of types for values being read,
C              1 - integer, 2 - real, 3 - character
C     IVAL   - date for data value(s) OR name for attribute value
C     NRVAL  - number of data values read from this record
C     RVAL   - array of data value(s) read
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,IPOS,XPOS,DPOS,VPOS,FPOS,APOS,RVPOS,
     $             DONFG,NSKIP,NX,ILEN,JLEN,DP,NRPT,FLEN,
     $             PARFLG,PARPOS,PARRPT,PARLEN
      CHARACTER*1  CHFSEP,LBUFF(400)
      CHARACTER*6  DATSTR
      CHARACTER*80 LFMT,TFMT,CURFMT,PARFMT
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (LFMT1,LFMT)
      CHARACTER*1  LFMT1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT, CHRINT, LENSTR, CKNBLV
      REAL         CHRDEC
C
C     + + + INTRINSICS + + +
      INTRINSIC    INDEX, CHAR, ICHAR, LEN
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZLNTXT, CHRINT, LENSTR, CKNBLV, CHRDEC
      EXTERNAL     ZLJUST, CVARAR, PARFND, FLDSEP
C
C     + + + DATA INITIALIZATIONS + + +
      DATA DATSTR /'YMDHNS'/
C
C     + + + END SPECIFICATIONS + + +
C
      NRVAL = 0
      DONFG = 0
      RVPOS = 1
C
      TFMT = CFMT
C
      CALL ZLJUST (TFMT)
      IF (TFMT(1:1).EQ.'(') THEN
C       strip leading left paren
        TFMT = TFMT(2:80)
      END IF
      IPOS = ZLNTXT(TFMT)
      IF (TFMT(IPOS:IPOS).EQ.')') THEN
C       strip trailing right paren
        TFMT(IPOS:IPOS) = ' '
      END IF
      CURFMT = TFMT
C
      PARFLG = 0
      NSKIP  = 1
 10   CONTINUE
C       parse next part of format
        IF (PARFLG.EQ.0) THEN
C         look for parentheses in format statement
          CALL PARFND (CURFMT,
     O                 PARPOS,PARFMT,PARRPT,PARLEN)
          I = INDEX(TFMT,',')
          IF (PARPOS.GT.0 .AND. (PARPOS.LT.I .OR. I.EQ.0)) THEN
C           time to process parentheses, store remainder of format for later
            LFMT = CURFMT(PARPOS+PARLEN:80)
C           look for next part of format after parentheses
            IPOS = INDEX(LFMT,',')
            IF (IPOS.GT.0) THEN
              TFMT = LFMT(IPOS+1:80)
            ELSE
              TFMT = LFMT
            END IF
C           make current format the contents of the parentheses
            CURFMT = PARFMT
            PARFLG = 1
          END IF
        END IF
        CALL ZLJUST (CURFMT)
        I = INDEX(CURFMT,',')
        IF (I.GT.0) THEN
C         find rightmost position of this segment
          IPOS = ZLNTXT(CURFMT(1:I-1))
        ELSE
C         no more commas, must be at end
          IPOS = ZLNTXT(CURFMT)
          DONFG = 1
        END IF
        LFMT = CURFMT(1:IPOS)
        XPOS = INDEX (LFMT,'X')
        DPOS = INDEX (DATSTR,LFMT(1:1))
        VPOS = INDEX (LFMT,'V')
        FPOS = INDEX (LFMT,'F')
        APOS = INDEX (LFMT,'A')
        IF (XPOS.GT.0) THEN
C         process space skip
          IF (IPOS.EQ.1) THEN
C           just an X, skip one space
            NX = 1
          ELSE IF (XPOS.EQ.1) THEN
C           X at beginning, process trailing numbers
            NX = CHRINT(IPOS-1,LFMT1(2))
          ELSE
C           process numbers preceeding X
            NX = CHRINT(IPOS-1,LFMT1(1))
          END IF
          NSKIP = NSKIP + NX
        ELSE IF (DPOS.GT.0) THEN
C         date specification
          IF (IPOS.GT.1) THEN
C           look for decimal in this element
            DP = INDEX(LFMT(2:IPOS),'.')
            IF (DP.GT.0) THEN
C             period in date format, just get field width for read
              ILEN = CHRINT(DP-1,LFMT1(2))
            ELSE
C             no period, width of field is all thats left
              ILEN = CHRINT(IPOS-1,LFMT1(2))
            END IF
          ELSE
C           no length specified, will be determined below
            ILEN = 0
          END IF
C         look for field separator to determine field width to use
          CALL FLDSEP (DONFG,PARFLG,IPOS,CURFMT,PARFMT,PARRPT,
     I                 TFMT,NSKIP,BUFF,
     O                 FLEN)
          IF (FLEN.GT.0) THEN
C           use length to field separator as field width
            ILEN = FLEN
          END IF
          IF (ILEN.LE.0) THEN
C           no length specified and no field separator, use rest of record
            ILEN = ZLNTXT(BUFF(NSKIP:LEN(BUFF)))
          END IF
          CALL CVARAR (ILEN,BUFF(NSKIP:NSKIP+ILEN-1),ILEN,LBUFF)
          IVAL(DPOS) = CHRINT(ILEN,LBUFF)
          NSKIP = NSKIP + ILEN
        ELSE IF (VPOS.GT.0) THEN
C         data value
          IF (VPOS.GT.1) THEN
C           look for repeat number
            ILEN = VPOS - 1
            CALL CVARAR (ILEN,LFMT,ILEN,LBUFF)
            NRPT = CHRINT(ILEN,LBUFF)
            IF (NRPT.EQ.0) THEN
C             not a valid repeat number
              NRPT = 1
            END IF
          ELSE
C           just get one value
            NRPT = 1
          END IF
          IF (IPOS.GT.VPOS) THEN
C           look for decimal in this element
            DP = INDEX(LFMT(VPOS+1:IPOS),'.')
            IF (DP.GT.0) THEN
C             period in value format, just get field width for read
              ILEN = CHRINT(DP-1,LFMT1(VPOS+1))
            ELSE
C             no period, width of field is all thats left
              ILEN = CHRINT(IPOS-VPOS,LFMT1(VPOS+1))
            END IF
          ELSE
C           no length specified, will be determined below
            ILEN = 0
          END IF
C         look for field separator to determine field width to use
          CALL FLDSEP (DONFG,PARFLG,IPOS,CURFMT,PARFMT,PARRPT,
     I                 TFMT,NSKIP,BUFF,
     O                 FLEN)
          IF (FLEN.GT.0) THEN
C           use length to field separator as field width
            ILEN = FLEN
          END IF
          IF (ILEN.LE.0) THEN
C           no length specified and no field separator, use rest of record
            ILEN = ZLNTXT(BUFF(NSKIP:LEN(BUFF)))
          END IF
          DO 30 I = 1,NRPT
            IF (ZLNTXT(BUFF(NSKIP:LEN(BUFF))).GT.0) THEN
C             more info on record to process
              CALL CVARAR (ILEN,BUFF(NSKIP:NSKIP+ILEN-1),ILEN,LBUFF)
              JLEN  = LENSTR(ILEN,LBUFF)
              NRVAL = NRVAL + 1
              IF (VALTYP(NRVAL).LE.2) THEN
C               integer or real
C               pbd, added to give missing value for blanks
                IF (JLEN.EQ.0) THEN
                  RVAL(RVPOS) = -99999.00
                ELSE
                  RVAL(RVPOS) = CHRDEC(JLEN,LBUFF)
                END IF
              ELSE
C               character, find start of string
                DP = CKNBLV(JLEN,LBUFF)
                DP = NSKIP + DP - 1
C               store position and length of string in buffer
                RVAL(RVPOS) = 100*DP + JLEN
              END IF
              RVPOS = RVPOS + 1
              NSKIP = NSKIP + ILEN
            ELSE
C             at end of record
              DONFG  = 1
              PARRPT = 1
            END IF
 30       CONTINUE
        ELSE IF (FPOS.GT.0) THEN
C         field delimeter specifier
          I = CHRINT(IPOS-1,LFMT1(2))
          IF (I.GT.0) THEN
C           ASCII code for field separator
            CHFSEP = CHAR(I)
          ELSE
C           actual character specified
            CHFSEP = LFMT1(2)
          END IF
C         move to position just after field separator
          I = INDEX (BUFF(NSKIP:400),CHFSEP)
          NSKIP = NSKIP + I
        ELSE IF (APOS.GT.0) THEN
C         attribute name specifier, get width of field
          ILEN = CHRINT(IPOS-APOS,LFMT1(APOS+1))
          CALL CVARAR (ILEN,BUFF(NSKIP:NSKIP+ILEN-1),ILEN,LBUFF)
          IF (ILEN.GT.6) THEN
C           only allow 6 characters for attribute name
            JLEN = 6
          ELSE
            JLEN = ILEN
          END IF
          DO 50 I = 1,JLEN
C           put up to first 6 characters into output name
            IVAL(I) = ICHAR(LBUFF(I))
 50       CONTINUE
          NSKIP = NSKIP + ILEN
        END IF
C
        IF (DONFG.EQ.0) THEN
C         move to start of next segment of format statement
          IPOS = INDEX(CURFMT,',')
          LFMT = CURFMT(IPOS+1:80)
          CURFMT = LFMT
          IF (PARFLG.EQ.0) THEN
C           current format is main format, update the main
            TFMT = CURFMT
          END IF
        ELSE IF (PARFLG.EQ.1) THEN
C         parentheses in use
          IF (PARRPT.GT.1) THEN
C           repeating parentheses
            CURFMT = PARFMT
            PARRPT = PARRPT - 1
            DONFG  = 0
          ELSE
C           all done with parentheses
            IF (ZLNTXT(TFMT).GT.0) THEN
C             more to process in original format
              CURFMT = TFMT
              PARFLG = 0
              DONFG  = 0
            END IF
          END IF
        END IF
      IF (DONFG.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   FLDSEP
     I                   (DONFG,PARFLG,IPOS,CURFMT,PARFMT,PARRPT,
     I                    TFMT,NSKIP,BUFF,
     O                    FLEN)
C
C     + + + PURPOSE + + +
C     Find length of field if a field separator follows the field.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       DONFG,PARFLG,IPOS,PARRPT,NSKIP,FLEN
      CHARACTER*80  TFMT,CURFMT,PARFMT
      CHARACTER*400 BUFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DONFG  - flag indicating if done processing current format
C     PARFLG - flag indicating if parentheses info being processed
C     IPOS   - position in current format
C     CURFMT - current format being processed
C     PARFMT - parenthese format
C     PARRPT - repeat indicator for parentheses processing
C     TFMT   - main format
C     NSKIP  - position on record being read
C     BUFF   - text buffer containing record being read
C     FLEN   - field length if field separator found (otherwise 0)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,JLEN
      CHARACTER*1 CHFSEP
      CHARACTER*6 FTMP
C
C     + + + FUNCTIONS + + +
      INTEGER     CHRINT
C
C     + + + INTRINSICS + + +
      INTRINSIC   INDEX, CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRINT, ZLJUST
C
C     + + + END SPECIFICATIONS + + +
C
      FLEN = 0
C
      IF (DONFG.EQ.0 .OR. PARFLG.EQ.1) THEN
C       more in format, see if next element is field separator
        IF (DONFG.EQ.0) THEN
C         more in current format, look at next element
          FTMP = CURFMT(IPOS+2:IPOS+7)
        ELSE IF (PARRPT.GT.1) THEN
C         done with paren, but still repeating, look at 1st element
          FTMP = PARFMT(1:6)
        ELSE
C         done with paren and repeats, look at next element in main format
          FTMP = TFMT(2:7)
        END IF
        CALL ZLJUST(FTMP)
        IF (FTMP(1:1).EQ.',') THEN
C         remove comma
          FTMP(1:1) = ' '
          CALL ZLJUST(FTMP)
        END IF
        IF (FTMP(1:1).EQ.'F') THEN
C         next element of format is field separator, remove F character
          FTMP = FTMP(2:6)
          JLEN = INDEX(FTMP,',')
          IF (JLEN.EQ.0) THEN
C           no commas, look for blank
            JLEN = INDEX(FTMP,' ')
          END IF
          IF (JLEN.GT.1) THEN
C           something follows F character
            JLEN = JLEN - 1
            I = CHRINT(JLEN,FTMP(1:JLEN))
            IF (I.GT.0) THEN
C             ASCII code for field separator
              CHFSEP = CHAR(I)
            ELSE
C             actual character specified
              CHFSEP = FTMP(1:1)
            END IF
            FLEN = INDEX(BUFF(NSKIP:400),CHFSEP) - 1
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PARFND
     I                   (TFMT,
     O                    PARPOS,PARFMT,PARRPT,PARLEN)
C
C     + + + PURPOSE + + +
C     Find any embedded parentheses in the format statement.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      PARPOS,PARRPT,PARLEN
      CHARACTER*80 TFMT,PARFMT
C
C     + + + ARGUMENT DEFINTIONS + + +
C     TFMT   - text containing current format statement
C     PARPOS - position in format of the start of the parentheses
C     PARFMT - text within the parentheses in the format
C     PARRPT - number of times to repeat format info within the parentheses
C     PARLEN - number of characters from start to end of parentheses
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IPOS,ILEN
      CHARACTER*1 LBUFF(10)
C
C     + + + FUNCTIONS + + +
      INTEGER     CHRINT, ZLNTXT
C
C     + + + INTRINSICS + + +
      INTRINSIC   INDEX
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRINT, ZLNTXT, CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      PARFMT = ' '
      PARRPT = 0
      PARPOS = INDEX(TFMT,'(')
      IF (PARPOS.GT.0) THEN
C       parenthesis embedded in format
        IF (PARPOS.GT.1) THEN
C         look for repeat indicator
          IPOS = PARPOS
 5        CONTINUE
            IPOS = IPOS - 1
          IF (TFMT(IPOS:IPOS).NE.',' .AND. IPOS.GT.1) GO TO 5
          IF (IPOS.GT.1) THEN
C           move up to position after comma
            IPOS = IPOS + 1
          END IF
          ILEN = PARPOS - IPOS
          IF (ILEN.GT.0) THEN
C           read preceeding characters
            CALL CVARAR (ILEN,TFMT(IPOS:PARPOS),ILEN,LBUFF)
            PARRPT = CHRINT(ILEN,LBUFF)
          END IF
        END IF
        IF (PARRPT.EQ.0) THEN
C         assume indefinite repeating
          PARRPT = 1000
        END IF
        IPOS = INDEX(TFMT,')')
        IF (IPOS.GT.0) THEN
C         right paren found, store contents between parentheses
          PARFMT = TFMT(PARPOS+1:IPOS-1)
        ELSE
C         did not find right paren, use remainder of format
          IPOS = ZLNTXT(TFMT)
          PARFMT = TFMT(PARPOS+1:IPOS)
        END IF
        PARLEN = IPOS - PARPOS + 1
      END IF
C
      RETURN
      END
