C
C
C
      SUBROUTINE   ADDSTR
     I                   (APLUS,
     M                    A)
C
C     + + + PURPOSE + + +
C     add string to end of existing string
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80 A, APLUS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     APLUS - base string
C     A     - string to add
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   LNGTH, LAPLS, I
      LOGICAL   GOODCH
C
C     + + + FUNCTIONS + + +
      INTEGER   LNGSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL  LNGSTR
C
C     + + + END SPECIFICATIONS + + +
C
      LNGTH = LNGSTR(A)
      LAPLS = LNGSTR(APLUS)
      IF (LAPLS .GT. 0) THEN
C       something to add
        GOODCH = .FALSE.
C
        DO 10 I = 1, LAPLS
          IF (APLUS(I:I) .NE. ' ') THEN
C           first non-blank character
            GOODCH = .TRUE.
          ENDIF
          IF (GOODCH) THEN
            LNGTH = LNGTH + 1
            IF (LNGTH .LE. 80) A(LNGTH:LNGTH) = APLUS(I:I)
          ENDIF
 10     CONTINUE
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   BMPCHR
     M                   (ANS)
C
C     + + + PURPOSE + + +
C     convert character to upper case
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*1 ANS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ANS - character to convert
C
C     + + + PARAMETERS + + +
      INCLUDE 'PCMPLR.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ICH
C
C     + + + INTRINSICS + + +
      INTRINSIC   ICHAR,CHAR
C
C     + + + END SPECIFICATIONS + + +
C
      IF (PCASCI) THEN
C       force character to be a capital (must be ASCII character types)
        ICH = ICHAR(ANS)
        IF ((ICH .GE. 97) .AND. (ICH .LE. 122)) THEN
C         character is in range of 'a'..'z'
          ANS = CHAR(ICH - 32)
        ENDIF
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CENTER
     I                   (IWDTH,
     M                    A)
C
C     + + + PURPOSE + + +
C     center mesages
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80 A
      INTEGER      IWDTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IWDTH - string to center
C     A     - allowed total width of output string
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*80 ADUM
      INTEGER      ITST, IBUF, I, J
C
C     + + + FUNCTIONS + + +
      INTEGER      LNGSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     LNGSTR
C
C     + + + END SPECIFICATIONS + + +
C
      ITST = LNGSTR(A)
      IBUF = (IWDTH - ITST) / 2
      IF (ITST .LT. IWDTH .AND. IBUF .GT. 1) THEN
        ADUM = A
        DO 10 I = 1, IBUF
          A(I:I) = ' '
 10     CONTINUE
        DO 20 I = 1, ITST
          J = I + IBUF
          A(J:J) = ADUM(I:I)
 20     CONTINUE
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLEAR
C
C     + + + PURPOSE + + +
C     clear screen for pcs and vax hardware
C     Modification date: 2/18/92 JAM
C
C     + + + PARAMETERS + + +
      INCLUDE 'PIOUNI.INC'
      INCLUDE 'PCMPLR.INC'
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*1  ESC
      CHARACTER*10 NWPAGE
C
C     + + + INTRINSICS + + +
      INTRINSIC    CHAR
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (1X,A10)
C
C     + + + END SPECIFICATIONS + + +
C
      ESC=CHAR(27)
      IF (NONPC) THEN
        NWPAGE = ' ' // ESC // '[1;1f' // ESC // '[J'
      ELSE
        NWPAGE = ESC // '[2J'
      ENDIF
      WRITE (KUOUT,2000) NWPAGE
C
      RETURN
      END
C
C
C
      SUBROUTINE   COMRD
     I                  (KFILE,
     O                   LINE)
C
C     + + + PURPOSE + + +
C     calls subroutine to check if an input line is a comment or data;
C     if not a comment, return line for data input.  line must be
C     defined as CHARACTER*80 in the calling program.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      KFILE
      CHARACTER*80 LINE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KFILE - unit number from which to read
C     LINE  - record read from file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      IERR
      LOGICAL      EOF, FATAL
      CHARACTER*2  CDUM
      CHARACTER*80 OUTMSG
C
C     + + + EXTERNALS + + +
      EXTERNAL   COMRD2,ERRCHK
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(I2)
C
C     + + + END SPECIFICATIONS + + +
C
      CALL COMRD2(
     I            KFILE,
     O            LINE, EOF)
C
      IF (EOF) THEN
C       if end of file to read from
        FATAL  = .TRUE.
        IERR   = 1560
C       convert kfile to character
        WRITE (CDUM,2000) KFILE
        OUTMSG = 'End of file on unit [' // CDUM //
     1           '] encountered'
        CALL ERRCHK(IERR, OUTMSG, FATAL)
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   COMRD2
     I                   (KFILE,
     M                    LINE,
     O                    EOF)
C
C     + + + PURPOSE + + +
C     checks whether an input line is a comment or data;
C     if not a comment, return line for data input.  line must be
C     defined as CHARACTER*80 in the calling program.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80 LINE
      LOGICAL      EOF
      INTEGER      KFILE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KFILE - unit number to read from
C     LINE  - record read from file
C     EOF   - end of file reached
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*80 OUTMSG
      LOGICAL      FATAL, COMMNT
      INTEGER*4    ICHAR, IERR
      CHARACTER*2  CDUM
      CHARACTER*1  BLANK
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A80)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(I2)
C
C     + + + INTRINSICS + + +
      INTRINSIC    INDEX
C
C     + + + EXTERNALS + + +
      EXTERNAL     ERRCHK
C
C     + + + END SPECIFICATIONS + + +
C
      IERR = 0
      EOF  = .FALSE.
C     set blank character
      BLANK = ' '
C
C     return here to continue reading until non-comment
   10 CONTINUE
C       skip leading blank characters and check for comments
        DO 50 ICHAR = 1,80
          IF(LINE(ICHAR:ICHAR) .NE. BLANK)GO TO 70
   50   CONTINUE
C       blank line, set ICHAR to 1
        ICHAR = 1
   70 CONTINUE
      COMMNT = .FALSE.
      READ(KFILE,1000,IOSTAT=IERR,ERR=991,END=992) LINE
      IF (INDEX(LINE,'***').GT.0) THEN
C       three asterisks indicate comment line
        COMMNT = .TRUE.
      ENDIF
      IF (COMMNT) GOTO 10
      GOTO 999
C
C     input error
  991 CONTINUE
        FATAL = .TRUE.
C        LNGTH  = LNGSTR(MESAGE)
C       convert kfile to character
        WRITE (CDUM,2000) KFILE
        OUTMSG =
     1    'Error in input on unit [' // CDUM //
     2    '], check compiler I/O error list for #'
        CALL ERRCHK(IERR, OUTMSG, FATAL)
C
C     end-of-file
  992 CONTINUE
        EOF = .TRUE.
C
  999 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   COMRD3
     I                   (KFILE,
     O                    LINE,STATUS)
C
C     + + + PURPOSE + + +
C     checks for comments in input data lines.
C     status returns .TRUE. for data; .FALSE. for end of block.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      KFILE
      CHARACTER*80 LINE
      LOGICAL      STATUS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KFILE  - unit number to read from
C     LINE   - record read from file
C     STATUS - true for data, false for end of block
C
C     + + + EXTERNALS + + +
      EXTERNAL     COMRD
C
C     + + + END SPECIFICATIONS + + +
C
      CALL COMRD(KFILE, LINE)
      STATUS = (LINE(1:3) .NE. 'END')
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   DYJDY
     I                        (YEAR,MONTH,DAY)
C
C     + + + PURPOSE + + +
C     Determine the Julian day from a given a calendar year/month/day
C     Added by PRH @ AQUA TERRA Consultants, 9/93
C     Addition of this function eliminates calling the include file
C     CMISC.INC every time, when only one variable from it is needed.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YEAR,MONTH,DAY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YEAR   - input year
C     MONTH  - input month
C     DAY    - input day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     LEAP,CNDMO(2,13)
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     +  +  + DATA INITIALIZATIONS +  +  +
      DATA  CNDMO/0,0,31,31,59,60,90,91,120,121,151,152,181,182,
     1          212,213,243,244,273,274,304,305,334,335,365,366/
C
C     + + + END SPECIFICATIONS + + +
C
C     check for leap year
      IF ((MOD(YEAR,4).EQ.0) .AND. (MOD(YEAR,100).NE.0)) THEN
C       leap year
        LEAP = 2
      ELSE
C       regular year
        LEAP = 1
      ENDIF
C     Julian day is cumulative number of days for month plus day number
      DYJDY = CNDMO(LEAP,MONTH)+ DAY
C
      RETURN
      END
C
C
C
      SUBROUTINE   ECHORD
     I                   (KFILE, LINCOD, FRSTRD,
     O                    LINE)
C
C     + + + PURPOSE + + +
C     read record from file and maybe echo it
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      KFILE
      CHARACTER*80 LINCOD,LINE
      LOGICAL      FRSTRD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KFILE  - file to read from
C     LINCOD - name of record being read
C     FRSTRD - first read flag
C     LINE   - record read
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CECHOT.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      LNGTH
      CHARACTER*80 MESAGE
C
C     + + + FUNCTIONS + + +
      INTEGER      LNGSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     LNGSTR,LFTJUS,PZDSPL,COMRD
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(' ',A80)
C
C     + + + END SPECIFICATIONS + + +
C
      IF ((ECHOLV .GE. 8) .AND. FRSTRD) THEN
        LINE  = LINCOD
        CALL LFTJUS(LINE)
        LNGTH = LNGSTR(LINE)
        IF (LNGTH .GE. 1) THEN
          MESAGE = 'Reading record [' // LINE(1:LNGTH) // ']'
          CALL PZDSPL(KECHOT,MESAGE)
        ENDIF
      ENDIF
C     read record
      CALL COMRD(
     I           KFILE,
     O           LINE)
C
      IF ((ECHOLV .GE. 9) .AND. FRSTRD) THEN
        LNGTH = LNGSTR(LINE)
        IF (LNGTH .GT. 65) THEN
C         only so much space
          LNGTH = 65
        ENDIF
        MESAGE = 'COMRD input [' // LINE(1:LNGTH) // ']'
        WRITE(KECHOT,2000) MESAGE
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ELPSE
     I                  (VALUE,CHR,LTOTL,
     M                   MESAGE)
C
C     + + + PURPOSE + + +
C     add trailing string and fill middle with specified character
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80 MESAGE, VALUE
      CHARACTER*1  CHR
      INTEGER      LTOTL
C
C     + + + ARGUMENT DEFINTIONS + + +
C     MESAGE - base string
C     VALUE  - trailing string
C     CHR    - fill in character
C     LTOTL  - length of output string
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*80 ADUM
      INTEGER      LNGTH,I
C
C     + + + FUNCTIONS + + +
      INTEGER      LNGSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     LNGSTR,ADDSTR
C
C     + + + END SPECIFICATIONS + + +
C
      ADUM = MESAGE
      CALL ADDSTR(VALUE,ADUM)
      LNGTH= LNGSTR(ADUM)
C
C     compute no. of fill chars. needed
      LNGTH= LTOTL - 1 - LNGTH
      ADUM = ' '
      IF (LNGTH .GT. 0) THEN
C       filler needed
        DO 10 I = 1, LNGTH
          ADUM(I:I) = CHR
 10     CONTINUE
C
        CALL ADDSTR(ADUM,MESAGE)
      ENDIF
C     add trailing string
      CALL ADDSTR(VALUE,MESAGE)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ERRCHK
     I                   (IERROR,MESAGE,FATAL)
C
C     + + + PURPOSE + + +
C     write error message
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      IERROR
      CHARACTER*80 MESAGE
      LOGICAL      FATAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IERROR - numeric code for error
C     MESAGE - text of error message
C     FATAL  - fatal error flag
C
C     + + + PARAMETERS + + +
      INCLUDE 'PIOUNI.INC'
      INCLUDE 'PMXZON.INC'
      INCLUDE 'PMXNSZ.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CTRACE.INC'
      INCLUDE 'CECHOT.INC'
      INCLUDE 'CFILEX.INC'
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*80 OUTMSG
      INTEGER      OUTLVL,LINE, I
C
C     + + + EXTERNALS + + +
      EXTERNAL     PZSCRN,TRCLIN,LFTJUS,FILCLO
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('Error number [',I6,']')
 2005 FORMAT('Compiler generated error number [',I4,']')
 2010 FORMAT('Warning [',I6,']')
 2100 FORMAT(1X,79('*')/1X,'*',T80,'*'/
     1       1X,'*',2X,'F A T A L   E R R O R [',I5,']',T80,'*'/
     2       1X,'*',T80,'*'/1X,'*',2X,A80,T80,'*'/
     3       1X,'*',T80,'*'/1X,'*',2X,A80,T80,'*'/
     4       1X,'*',T80,'*'/1X,79('*'))
 2110 FORMAT(1X,79('+')/1X,'+',T80,'+'/
     1       1X,'+',2X,'W A R N I N G [',I5,']',T80,'+'/
     2       1X,'+',T80,'+'/1X,'+',2X,A80,T80,'+'/
     3       1X,'+',T80,'+'/1X,79('+'))
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IERROR .NE. 0) THEN
        IF (FATAL) THEN
C
C         reset echo level so screen is written for fatal error
          ECHOLV = 7
          IF (IERROR .LT. 1000) THEN
            WRITE(OUTMSG,2005) IERROR
          ELSE
            WRITE(OUTMSG,2000) IERROR
          ENDIF
        ELSE
          WRITE(OUTMSG,2010) IERROR
        ENDIF
        CALL PZSCRN(1,OUTMSG)
        CALL PZSCRN(2,MESAGE)
C
C       set up trace
        OUTLVL = SUBLVL
C
        CALL TRCLIN(
     I              OUTLVL,
     O              LINE)
C
C       check for fatal error
        IF (FATAL) THEN
C         error is fatal
          CALL LFTJUS(MESAGE)
          IF (LINE .GT. 1) THEN
            WRITE(FECHO,2100) LINE  ,MESAGE, OUTSTR(1)
          ELSE
            WRITE(FECHO,2100) IERROR,MESAGE,OUTSTR(1)
          ENDIF
C
          DO 10 I = 1, LINE
            MESAGE = OUTSTR(I)
            CALL PZSCRN(2+I,MESAGE)
 10       CONTINUE
C
          OUTMSG = 'F A T A L   E R R O R   ! ! - Execution terminated'
          CALL PZSCRN(7,OUTMSG)
          CALL FILCLO
          STOP 
        ELSE
C         not fatal error
          IF (ECHOLV .GE. 5) THEN
            CALL LFTJUS(MESAGE)
            IF (LINE .GT. 1) THEN
              WRITE(FECHO,2110) LINE  ,MESAGE
            ELSE
              WRITE(FECHO,2110) IERROR,MESAGE
            ENDIF
C
          ENDIF
C
        ENDIF
      ENDIF
C
      RETURN
      END
C
C
C
      REAL*8   FUNCTION   EXPCHK
     I                          (ARG)
C
C     + + + PURPOSE + + +
C
C     checks arguments for exponential limits
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      REAL*8  ARG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ARG - argument to check
C
C     + + + PARAMETERS + + +
      INCLUDE 'PCMPLR.INC'
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*80 MESAGE
      LOGICAL      FATAL
      INTEGER      IERROR
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS,EXP
C
C     + + + EXTERNALS + + +
      EXTERNAL     ERRCHK
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('Argument [',D11.4,'] too large for EXP')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ARG .LT. EXNMX) THEN
        EXPCHK = 0.0
      ELSE IF (ABS(ARG) .LT. EXPMN) THEN
        EXPCHK = 1.0
      ELSE IF (ARG .GT. EXPMX) THEN
        WRITE(MESAGE,2000) ARG
        IERROR = 1320
        FATAL = .TRUE.
        CALL ERRCHK(IERROR,MESAGE,FATAL)
        EXPCHK = REALMX
      ELSE
        EXPCHK = EXP(ARG)
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   FILCHK
     I                   (LFILID,NUMREQ,FILUNI)
C
C     + + + PURPOSE + + +
C     check to see that required files are open
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*18 LFILID
      INTEGER      NUMREQ,FILUNI(NUMREQ)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LFILID - identifier for file input
C     NUMREQ - number of files required
C     FILUNI - file unit number array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I, IERROR, LEN
      CHARACTER*80 MESAGE
      CHARACTER*2  USTR
      LOGICAL      FATAL
C
C     + + + FUNCTIONS + + +
      INTEGER      LNGSTR
C
C     + + + EXTERNAL + + +
      EXTERNAL     LNGSTR,ERRCHK
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(I2)
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1, NUMREQ
C       loop through each zone
        IF (FILUNI(I) .LT. 0) THEN
C         file not open
          LEN = LNGSTR(LFILID(1:18))
          WRITE(USTR,2000) I
          MESAGE= 'required file [' // LFILID(1:LEN) // USTR //
     1            '] not opened before ENDFILE found.'
          IERROR= 1280
          FATAL = .TRUE.
          CALL ERRCHK( IERROR, MESAGE, FATAL)
        ENDIF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   FILCLO
C
C     + + + PURPOSE + + +
C     close multiple segment files
C     Modification date: 2/18/92 JAM
C
C     + + + PARAMETERS + + +
      INCLUDE 'PMXZON.INC'
      INCLUDE 'PMXNSZ.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CFILEX.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,RETCOD
C
C     + + + EXTERNALS + + + 
C      
      EXTERNAL WDFLCL
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1, MXZONE
        IF (FPRZIN(I) .NE. -999) THEN
C         file is open -- close it
          CLOSE (FPRZIN(I),STATUS='KEEP')
        ENDIF
        IF (FPRZRS(I) .NE. -999) THEN
C         file is open -- close it
          CLOSE (FPRZRS(I),STATUS='KEEP')
        ENDIF
        IF (FPRZOT(I) .NE. -999) THEN
C         file is open -- close it
          CLOSE (FPRZOT(I),STATUS='KEEP')
        ENDIF
        IF (FMETEO(I) .NE. -999) THEN
C         file is open -- close it
          CLOSE (FMETEO(I),STATUS='KEEP')
        ENDIF
        IF (FSPTIC(I) .NE. -999) THEN
C         file is open -- close it
          CLOSE (FSPTIC(I),STATUS='KEEP')
        ENDIF
        IF (FTMSRS(I) .NE. -999) THEN
C         file is open -- close it
          CLOSE (FTMSRS(I),STATUS='KEEP')
        ENDIF
   10 CONTINUE
C
      DO 20 I = 1, MXNSZO
        IF (FVADIN(I) .NE. -999) THEN
C         file is open -- close it
          CLOSE (FVADIN(I),STATUS='KEEP')
        ENDIF
        IF (FVADOT(I) .NE. -999) THEN
C         file is open -- close it
          CLOSE (FVADOT(I),STATUS='KEEP')
        ENDIF
        IF (FVTP10(I) .NE. -999) THEN
C         file is open -- close it
          CLOSE (FVTP10(I),STATUS='KEEP')
        ENDIF
        IF (FVRSTF(I) .NE. -999) THEN
C         file is open -- close it
          CLOSE (FVRSTF(I),STATUS='KEEP')
        ENDIF
        IF (FVRSTT(I) .NE. -999) THEN
C         file is open -- close it
          CLOSE (FVRSTT(I),STATUS='KEEP')
        ENDIF
   20 CONTINUE
C
      IF (FWDMS .NE. -999) THEN
C       file is open -- requires special routine to close
        CALL WDFLCL(FWDMS,RETCOD)
      ENDIF
      IF (FMCIN .NE. -999) THEN
C       file is open -- close it
        CLOSE (FMCIN,STATUS='KEEP')
      ENDIF
      IF (FMCOUT .NE. -999) THEN
C       file is open -- close it
        CLOSE (FMCOUT,STATUS='KEEP')
      ENDIF
      IF (FMCOU2 .NE. -999) THEN
C       file is open -- close it
        CLOSE (FMCOU2,STATUS='KEEP')
      ENDIF
      IF (FCON .NE. -999) THEN
C       file is open -- close it
        CLOSE (FCON,STATUS='KEEP')
      ENDIF
      IF (FECHO .NE. -999) THEN
C       file is open -- close it
        CLOSE (FECHO,STATUS='KEEP')
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   LFTJUS
     M                   (A)
C
C     + + + PURPOSE + + +
C     left justify a string
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80 A
C
C     + + + ARGUMENT DEFINITIONS + + +
C     A - string to left justify
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*80 ADUM
      INTEGER      IWDTH, ISTRT, I
C
C     + + + FUNCTIONS + + +
      INTEGER      LNGSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     LNGSTR
C
C     + + + END SPECIFICATIONS + + +
C
C     check to see that A is not blank or only 1 character long
      IWDTH = LNGSTR(A)
C
      IF (IWDTH .GT. 1) THEN
C       find 1st non-blank character
        ISTRT = 0
        I     = 0
 10     CONTINUE
          I = I + 1
          IF (A(I:I) .NE. ' ') THEN
            ISTRT = I
          ENDIF
        IF (ISTRT .EQ. 0 .AND. I .LT. IWDTH) GOTO 10
C       move to left
        ADUM = A(ISTRT:IWDTH)
        A    = ADUM
C
      ENDIF
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   LNCHK
     I                       (ARGX)
C
C     + + + PURPOSE + + +
C     take a natural log with error checking
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       ARGX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ARGX - variable of which to take the natural log
C
C     + + + FUNCTIONS + + +
      REAL       LOGCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL   LOGCHK
C
C     + + + END SPECIFICATIONS + + +
C
      LNCHK = 2.302585093 * LOGCHK(ARGX)
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   LNGSTR
     I                             (AMESG)
C
C     + + + PURPOSE + + +
C     determine the length of a character string
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80 AMESG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     AMESG - string of which to determine the length
C
C     + + + LOCAL VARIABLES + + +
      LOGICAL      MORFG
C
C     + + + END SPECIFICATIONS + + +
C
      MORFG  = .TRUE.
      LNGSTR = 80
C
 10   CONTINUE
        IF(AMESG(LNGSTR:LNGSTR) .EQ. ' ') THEN
          LNGSTR = LNGSTR - 1
        ELSE
          MORFG = .FALSE.
        ENDIF
        IF ((MORFG) .AND. LNGSTR .GT. 0) THEN
          GOTO 10
        ENDIF
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   LOGCHK
     I                        (ARG)
C
C     + + + PURPOSE + + +
C     take a base 10 logarithm with error checking
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      REAL ARG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ARG - variable of which to take base 10 log
C
C     + + + LOCAL VARIABLES + + +
      LOGICAL      FATAL
      INTEGER      IERROR
      CHARACTER*80 MESAGE
C
C     + + + INTRINSICS + + +
      INTRINSIC    LOG10
C
C     + + + EXTERNALS + + +
      EXTERNAL     ERRCHK
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('Negative or zero argument [',G12.3,'] to LOG')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ARG .LE. 0.0) THEN
C       problem with argument
        WRITE(MESAGE,2000) ARG
        IERROR = 1330
        FATAL = .TRUE.
        CALL ERRCHK(IERROR,MESAGE,FATAL)
      ELSE
C       okay to take log
        LOGCHK = LOG10( ARG)
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NAMFIX
     M                   (A)
C
C     + + + PURPOSE + + +
C     left justify a string and capitalize all characters
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80 A
C
C     + + + ARGUMENT DEFINITIONS + + +
C     A - string to left justify and capitalize
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4    IWDTH,I
C
C     + + + FUNCTIONS + + +
      INTEGER      LNGSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     LFTJUS,BMPCHR,LNGSTR
C
C     + + + END SPECIFICATIONS + + +
C
      CALL LFTJUS(
     M            A)
C
      IWDTH = LNGSTR(A)
C
      IF (IWDTH .GT. 0) THEN
C       bump characters to capitals
        DO 10 I = 1, IWDTH
          CALL BMPCHR(
     M                A(I:I))
 10     CONTINUE
C
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OPECHO
     I                   (FECHO,MESAGE,FLAG)
C
C     + + + PURPOSE + + +
C     flags printing utility
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      FECHO
      CHARACTER*80 MESAGE
      LOGICAL      FLAG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FECHO  - ???
C     MESAGE - ???
C     FLAG   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4    LTOTL
      CHARACTER*80 VALUE
      CHARACTER*1  CHR
C
C     + + + EXTERNALS + + +
      EXTERNAL     ELPSE
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CHR /'.'/
      DATA LTOTL / 79/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(' ',A80)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (FLAG) THEN
        VALUE = 'ON'
      ELSE
        VALUE = 'OFF'
      ENDIF
      CALL ELPSE(VALUE,CHR,LTOTL,MESAGE)
      WRITE(FECHO,2000) MESAGE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PZDSPL
     I                   (KFILE, MESAGE)
C
C     + + + PURPOSE + + +
C     display message on screen and echo file
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4    KFILE
      CHARACTER*80 MESAGE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KFILE  - echo file unit number
C     MESAGE - message to be displayed
C
C     + + + PARAMETERS + + +
      INCLUDE 'PCMPLR.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CECHOT.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + EXTERNALS + + +
      EXTERNAL   PZSCRN,CENTER
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(1X,79('.'))
 2010 FORMAT(1X,':',77A1,':')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (WINDOW) THEN
C       write to screen
        CALL PZSCRN(1,MESAGE)
      ELSE
C       center mesage
        CALL CENTER(77,MESAGE)
      ENDIF
C
      IF (ECHOLV .GE. 3) THEN
C       write to echo file
        WRITE(KFILE,2000)
        WRITE(KFILE,2010) (MESAGE(I:I),I=2,78)
        WRITE(KFILE,2000)
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PZSCRN
     I                   (IOUT, MESAGE)
C
C     + + + PURPOSE + + +
C     display to IBM-PC computers w/ANSI.SYS
C     installed in CONFIG.SYS. If this is not the system
C     being used, set the parameter WINDOW (in PCMPLR.INC) to .FALSE.
C     and the screen display will be replaced with a list-type
C     display (eg. any computer or terminal CRT or printer)
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80 MESAGE
      INTEGER      IOUT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESAGE - text to display
C     IOUT   - position of text to display
C
C     + + + PARAMETERS + + +
      INCLUDE 'PCMPLR.INC'
      INCLUDE 'PIOUNI.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CECHOT.INC'
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*80 CHTEMP
      CHARACTER*1  ESC, BRCKET, VLINE , BSPACE
      CHARACTER*4  NWPAGE
      CHARACTER*5  GOTOXY
      CHARACTER*76 HLINE, BLINE
      INTEGER      I
C
C     + + + INTRINSICS + + +
      INTRINSIC    CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL     CLEAR,CENTER
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(1X,A1,A76,A1)
 2010 FORMAT(1X,A1,A1,A75,A1)
 2020 FORMAT(1X,A4)
 2030 FORMAT(1X,A5)
 2040 FORMAT(1X,A)
 2200 FORMAT(1X,A80)
C
C     + + + END SPECIFICATIONS + + +
C
      BSPACE = ' '
      IF (WINDOW) THEN
        IF (ECHOLV .GT. 0) THEN
          ESC = CHAR(27)
          BRCKET = '['
          IF (NONPC) THEN
            VLINE = '|'
          ELSE
            VLINE = CHAR(186)
          ENDIF
          DO 10 I = 1, 76
             BLINE (I:I) = ' '
            IF (NONPC) THEN
              HLINE (I:I) = '-'
            ELSE
              HLINE (I:I) = CHAR(205)
            ENDIF
 10       CONTINUE
C
          IF (IOUT .EQ. 0) THEN
C
C           initial screen setup
              CALL CLEAR
              IF (NONPC) THEN
                WRITE(KUOUT,2000) '+', HLINE, '+'
              ELSE
                WRITE(KUOUT,2000) CHAR(201), HLINE, CHAR(187)
              ENDIF
            CALL CENTER(76,MESAGE)
C
C             write program header
              WRITE(KUOUT,2000) VLINE, MESAGE(1:76), VLINE
              IF (NONPC) THEN
                WRITE(KUOUT,2000) '+', HLINE, '+'
              ELSE
                WRITE(KUOUT,2000) CHAR(204), HLINE, CHAR(185)
              ENDIF
C
C           simulation status (eg errors, files opened, etc.)
            CHTEMP = 'Simulation Status'
            CALL CENTER(76,CHTEMP)
              WRITE(KUOUT,2000) VLINE, CHTEMP(1:76), VLINE
            CHTEMP = '---------- ------'
            CALL CENTER(76,CHTEMP)
              WRITE(KUOUT,2000) VLINE, CHTEMP(1:76), VLINE
C
C           leave 2 lines (#6 & #7) blank for future messages
              WRITE(KUOUT,2000) VLINE, BLINE, VLINE
              WRITE(KUOUT,2000) VLINE, BLINE, VLINE
C
              IF (NONPC) THEN
                WRITE(KUOUT,2000) '+', HLINE, '+'
              ELSE
                WRITE(KUOUT,2000) CHAR(204), HLINE, CHAR(185)
              ENDIF
            CHTEMP = 'Software Trace'
            CALL CENTER(77,CHTEMP)
              WRITE(KUOUT,2000) VLINE, CHTEMP(1:76), VLINE
            CHTEMP = '-------- -----'
            CALL CENTER(77,CHTEMP)
              WRITE(KUOUT,2000) VLINE, CHTEMP(1:76), VLINE
C             leave 2 lines (#11 & #12) blank for future messages
              WRITE(KUOUT,2000) VLINE, BLINE, VLINE
              WRITE(KUOUT,2000) VLINE, BLINE, VLINE
C           end of software trace
C
C           percent complete
              IF (NONPC) THEN
                WRITE(KUOUT,2000) '+', HLINE, '+'
              ELSE
                WRITE(KUOUT,2000) CHAR(204), HLINE, CHAR(185)
              ENDIF
            CHTEMP = 'Percent Complete'
            CALL CENTER(76,CHTEMP)
              WRITE(KUOUT,2000) VLINE, CHTEMP(1:76), VLINE
            CHTEMP = ' 0...................................50' //
     1               '..................................100'
            CALL CENTER(76,CHTEMP)
              WRITE(KUOUT,2000) VLINE, CHTEMP(1:76), VLINE
C
C           leave 1 line (#16) blank for future completion bar
              WRITE(KUOUT,2000) VLINE, BLINE, VLINE
C
C           end the window
              IF (NONPC) THEN
                WRITE(KUOUT,2000) '+', HLINE, '+'
              ELSE
                WRITE(KUOUT,2000) CHAR(200), HLINE, CHAR(188)
              ENDIF
C           end screen setup
C
          ELSE IF ((IOUT .EQ. 1) .OR. (IOUT .EQ. 2)) THEN
C           simulation status, start at line 6
            IF (ECHOLV .GE. 2) THEN
              IF (IOUT .EQ. 1) THEN
                IF (NONPC) THEN
                  GOTOXY = ESC // BRCKET // '6H'
                ELSE
                  NWPAGE = ESC // BRCKET // '6H'
                ENDIF
              ELSE
                IF (NONPC) THEN
                  GOTOXY = ESC // BRCKET // '7H'
                ELSE
                  NWPAGE = ESC // BRCKET // '7H'
                ENDIF
              ENDIF
              CALL CENTER(76,MESAGE)
                IF (NONPC) THEN
                  WRITE(KUOUT,2020) GOTOXY
                ELSE
                  WRITE(KUOUT,2020) NWPAGE
                ENDIF
                WRITE(KUOUT,2000) VLINE, MESAGE(1:76), VLINE
C
              IF (IOUT .EQ. 1) THEN
C               blank second line
                  WRITE(KUOUT,2000) VLINE, BLINE, VLINE
              ENDIF
            ENDIF
          ELSE IF ((IOUT .EQ. 3) .OR. (IOUT .EQ. 4)) THEN
C
C           software trace, start at line 11
            IF (IOUT .EQ. 3) THEN
              IF (NONPC) THEN
                GOTOXY = ESC // BRCKET // '11H'
              ELSE
                GOTOXY = ESC // BRCKET // '11H'
              ENDIF
                WRITE(KUOUT,2030) GOTOXY
            ELSE
              IF (NONPC) THEN
                GOTOXY = ESC // BRCKET // '12H'
              ELSE
                GOTOXY = ESC // BRCKET // '12H'
              ENDIF
                WRITE(KUOUT,2030) GOTOXY
            ENDIF
C
              WRITE(KUOUT,2010) VLINE,BSPACE, MESAGE(1:75), VLINE
C
            IF (IOUT .EQ. 3) THEN
C             blank second line
                WRITE(KUOUT,2000) VLINE, BLINE, VLINE
            ENDIF
          ELSE IF (IOUT .EQ. 5) THEN
C
C           percent complete
            IF (NONPC) THEN
              GOTOXY = ESC // BRCKET // '16H'
            ELSE
              GOTOXY = ESC // BRCKET // '16H'
            ENDIF
              WRITE(KUOUT,2030) GOTOXY
              WRITE(KUOUT,2010) VLINE,BSPACE, MESAGE(1:75), VLINE
          ELSE
C
C           write anything else below this window
            GOTOXY = ESC // BRCKET // '20H'
              WRITE(KUOUT,2030) GOTOXY
              WRITE(KUOUT,2040) MESAGE
          ENDIF
C
C         put cursor below screen so system error mesages do not destroy
          GOTOXY = ESC // BRCKET // '20H'
            WRITE(KUOUT,2030) GOTOXY
C
        ENDIF
C       end of echolv check
C
      ELSE
C       window output is not desired
        WRITE (KUOUT,2200) MESAGE
C
      ENDIF
C
      RETURN
      END
C
C
C
      REAL*8   FUNCTION   RELTST(ARG)
C
C     + + + PURPOSE + + +
C     check to see double precision argument is valid
C     as single precision
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      REAL*8       ARG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ARG - variable to test
C
C     + + + PARAMETERS + + +
      INCLUDE 'PCMPLR.INC'
C
C     + + + LOCAL VARIABLES + + +
      REAL*8       DARG
      CHARACTER*80 MESAGE
      INTEGER*4    IERROR
      LOGICAL      FATAL
C
C     + + + INTRINSICS + + +
      INTRINSIC   DABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    ERRCHK
C
C     + + + END SPECIFICATIONS + + +
C
      DARG = DABS(ARG)
C
      IF (DARG .LT. REALMN) THEN
C       too small
        RELTST = 0.0
      ELSE IF (DARG .GT. REALMX) THEN
C       too big
        MESAGE = 'Single precision overflow'
        IERROR = 1350
        FATAL  = .TRUE.
        CALL ERRCHK(IERROR,MESAGE,FATAL)
        RELTST = REALMX
      ELSE
C       just right
        RELTST = ARG
      ENDIF
C
      RETURN
      END
C
C
C
      REAL*8   FUNCTION   SQRCHK
     I                          (ARG)
C
C     + + + PURPOSE + + +
C     square root with error checking
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      REAL*8       ARG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ARG - variable of which to take the square root
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*80 MESAGE
      LOGICAL      FATAL
      INTEGER      IERROR
C
C     + + + INTRINSICS + + +
      INTRINSIC    SQRT
C
C     + + + EXTERNALS + + +
      EXTERNAL     ERRCHK
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('Negative argument[',G12.3,'] to SQRT')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ARG .LT. 0.0) THEN
C       can't take square root
        WRITE(MESAGE,2000) ARG
        IERROR = 1360
        FATAL = .TRUE.
        CALL ERRCHK(IERROR,MESAGE,FATAL)
      ELSE
C       take square root
        SQRCHK = SQRT( ARG)
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SUBIN
     I                  (MESAGE)
C
C     + + + PURPOSE + + +
C     track entry into subroutine
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80 MESAGE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESAGE - routine being retrieved
C
C     + + + PARAMETERS + + +
      INCLUDE 'PCMPLR.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CTRACE.INC'
      INCLUDE 'CECHOT.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      LINE, ILIN
      CHARACTER*80 OUTMSG
C
C     + + + EXTERNALS + + +
      EXTERNAL     TRCLIN,PZSCRN
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(' Trace: ',A80)
C
C     + + + END SPECIFICATIONS + + +
C
C     increment subroutine level
      SUBLVL = SUBLVL + 1
      IF (SUBLVL .GT. MAXSUB) THEN
C       at maximum depth of sub calls
        SUBLVL = MAXSUB
      ENDIF
C
      LOCATN(SUBLVL) = MESAGE
C
C     write out location if at proper trace level
      IF ((TRCLVL .GT. 0) .AND. (ECHOLV .GE. 4)) THEN
C       trace line to screen
        CALL TRCLIN(
     I              TRCLVL,
     O              LINE)
C       write out location to echo
        IF (TRCLVL .GE. SUBLVL) THEN
          DO 10 ILIN = 1, LINE
C           MESAGE = OUTSTR(LINE)
            OUTMSG = OUTSTR(ILIN)
            CALL PZSCRN(2+ILIN,OUTMSG)
            IF (ECHOLV .GE. 10) THEN
              WRITE(KECHOT,2000) OUTMSG
            ENDIF
 10       CONTINUE
        ENDIF
      ENDIF
C
C
      RETURN
      END
C
C
C
      SUBROUTINE   SUBOUT
C
C     + + + PURPOSE + + +
C     track exit from subroutine
C     Modification date: 2/18/92 JAM
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CTRACE.INC'
C
C     + + + END SPECIFICATIONS + + +
C
      SUBLVL = SUBLVL - 1
      IF (SUBLVL .LT. 0) THEN
        SUBLVL = 0
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TRCLIN
     I                   (LEVEL,
     O                    LINE)
C
C     + + + PURPOSE + + +
C     write trace line to display screen
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEVEL, LINE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEVEL - trace level
C     LINE  - number of trace lines written
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CTRACE.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4    LNGOUT, LNGSUB, LNGTST, ILVL, IERROR
      CHARACTER*80 OUTMSG, ADUM
      LOGICAL      FATAL
C
C     + + + FUNCTIONS + + +
      INTEGER      LNGSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     LNGSTR,ERRCHK
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('Too many lines [',I2,'] required for trace, max is ',I2)
C
C     + + + END SPECIFICATIONS + + +
C
      LINE = 1
      IF (LEVEL .GT. 0) THEN
        OUTSTR(LINE) = 'PRZM2'
        IF (SUBLVL .GT. 0) THEN
C         load output string w/ sub. names deeper
C         than (or = to) trace level
          IF (LEVEL .GE. SUBLVL) THEN
C           loop through all subroutines
            DO 10 ILVL = 1, SUBLVL
              LNGOUT = LNGSTR(OUTSTR(LINE))
              LNGSUB = LNGSTR(LOCATN(ILVL))
              LNGTST = LNGOUT + LNGSUB + 1
              IF (LNGTST .GT. 80) THEN
                LINE = LINE + 1
C
                IF (LINE .GT. MAXLIN) THEN
C                 out of trace lines
                  WRITE(OUTMSG,2000) LINE, MAXLIN
                  IERROR = 1310
                  FATAL  = .FALSE.
                  CALL ERRCHK(IERROR,OUTMSG,FATAL)
                  LINE   = MAXLIN
                  GO TO 20
                ENDIF
C
                OUTSTR(LINE) = '...'
              ENDIF
              ADUM         = OUTSTR(LINE)(1:LNGSTR(OUTSTR(LINE))) //
     1          '>' // LOCATN(ILVL)(1:LNGSUB)
              OUTSTR(LINE) = ADUM
 10         CONTINUE
          ENDIF
        ENDIF
      ENDIF
C
 20   RETURN
      END

