C     aninit.f 2.1 9/4/91
C
C
C
      SUBROUTINE   ANINIT
     I                    (WDMSFL)
C
C     + + + PURPOSE + + +
C     Initialize environment for ANNIE application and open the
C     WDM message file.   Uses MESSAGE.WDM for the name of the
C     message file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM message file
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*64  WDNAME, VERSN
C
C     + + + EXTERNALS + + +
      EXTERNAL   ANINIX
C
C     + + + END SPECIFICATIONS + + +
C
C     version info for what on unix
      INCLUDE 'fversn.inc'
C
      INCLUDE 'fmsgwd.inc'
      CALL ANINIX (WDMSFL, WDNAME)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ANINIX
     I                    (WDMSFL, WDNAME)
C
C     + + + PURPOSE + + +
C     Initialize environment for ANNIE application and open the
C     WDM message file.  Uses programmer supplied name (WDNAME)
C     for the message file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL
      CHARACTER*64 WDNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM message file
C     WDNAME - name of WDM message file
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*8 APPNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL    ANINIZ
C
C     + + + END SPECIFICATIONS + + +
C
      APPNAM= 'APPLIC  '
      CALL ANINIZ (WDMSFL, WDNAME, APPNAM)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ANINIZ
     I                    (WDMSFL, WDNAME, APPNAM)
C
C     + + + PURPOSE + + +
C     Initialize environment for ANNIE application and open the
C     WDM message file.  Uses programmer supplied name (WDNAME)
C     for the message file.  Uses programmer supplied name (APPNAM)
C     for the base name of the log file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL
      CHARACTER*8  APPNAM
      CHARACTER*64 WDNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM message file
C     WDNAME - name of WDM message file
C     APPNAM - base name for log file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cterif.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,OORC,ERRFLG,PRMIND,RDOFLG,FE,PTHLEN,APPLEN,CTYP
      CHARACTER*8  AIDPTH
      CHARACTER*12 IFLNAM
      CHARACTER*52 PTHNAM
      CHARACTER*64 FILNAM
      LOGICAL*4    LTF
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + INTRINSICS + + +
      INTRINSIC  CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBOPN, ANPRGT, COLINI, GETPTH, BLDFNM
      EXTERNAL   ZEMIFE, GETFUN, NUMINI, ZLNTXT, XOSVAR
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(1X,A1,'[?9',A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
C
C     get path for files opened here
      INCLUDE 'faidep.inc'
      CALL GETPTH (AIDPTH,
     O             PTHNAM,PTHLEN)
C     open the error file, if not already opened by ANNIE graphics routines
      FE= 99
CJK   LTF= .FALSE.
      INQUIRE (UNIT=FE, OPENED=LTF)
      IF (.NOT. LTF) THEN
C       file not already open
        IFLNAM= 'ERROR.FIL'
C       build full file name using path
        CALL BLDFNM (PTHLEN,PTHNAM,IFLNAM,
     O               FILNAM)
        OPEN (UNIT=FE, FILE=FILNAM)
      END IF
C
C     see if environment var for message file
      CALL XOSVAR (WDNAME,
     O             FILNAM)
C
C     open the old WDM message file (read only if possible)
      RDOFLG= 1
      CALL WDBOPN (WDMSFL,FILNAM,RDOFLG,
     O             ERRFLG)
      IF (ERRFLG.NE.0) THEN
C       bad wdm file
        WRITE(FE,*) 'Bad WDM file:',ERRFLG,WDNAME,FILNAM
        WRITE(*,*)  'Bad WDM file:',ERRFLG,WDNAME,FILNAM
        STOP
      END IF
C
C     get standard input and output units from users 'TERM.DAT' file
      PRMIND= 3
      CALL ANPRGT (PRMIND, TERIFL)
      TEMPFL= TERIFL
      OORC  = 1
      CALL GETFUN (OORC,LOGFL)
CPRHC     initialize the common block of unit numbers
CPRH      CALL TRMINL (TERIFL, TEMPFL, LOGFL)
C     set default colors
      CALL COLINI
C
C     initialize EMIFE stuff
      CALL ZEMIFE (WDMSFL,I0,I0)
C
C     set up log file, build full file name using path
      APPLEN= ZLNTXT(APPNAM)
      IFLNAM= APPNAM(1:APPLEN)//'.LOG'
      CALL BLDFNM (PTHLEN,PTHNAM,IFLNAM,
     O             FILNAM)
      OPEN (UNIT=LOGFL,FILE=FILNAM,STATUS='UNKNOWN',ERR=10)
      GO TO 40
 10   CONTINUE
C       get here on problem opening log file, try different name
        IFLNAM= 'ANNIE1.LOG'
        CALL BLDFNM (PTHLEN,PTHNAM,IFLNAM,
     O               FILNAM)
        OPEN (UNIT=LOGFL,FILE=FILNAM,STATUS='UNKNOWN',ERR=20)
        WRITE (99,*) 'Using file ANNIE1.LOG for logging responses'
        GO TO 30
 20     CONTINUE
C         couldn't open different named log file either
          WRITE (99,*) 'Could not open a file for logging responses'
 30     CONTINUE
 40   CONTINUE
CPRHC
CPRH      CALL TRMINL (TERIFL,TEMPFL,LOGFL)
C
      PRMIND= 1
      CALL ANPRGT (PRMIND,CTYP)
      IF (CTYP .GT. 3) THEN
C       enable mouse
        WRITE(*,2000) CHAR(27),'s'
        WRITE(*,2000) CHAR(27),'h'
        WRITE(*,*) 'MOUSE ENABLED'
      END IF
C
C     initialize machine dependent min and max numbers
      CALL NUMINI
C
      RETURN
      END
C
C
C
      SUBROUTINE   ANCLOS
     I                   (MESSFL)
C
C     + + + PURPOSE + + +
C     close message file and scratch pad file,
C     write out scratch pad file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C
C     + + + COMMON BLOCKS + + +
C     INCLUDE 'cterif.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I1,PRMIND,CTYP
      CHARACTER*1 BLNK(1)
C     INTEGER     J,NLOGFL,ERRFLG,DONFG,TLOGFL,FSCLU,SGRP,RESP
C     CHARACTER*1 TBUFF(80)
C
C     + + + FUNCTIONS + + +
      INTEGER     ZCMDON
C     INTEGER     LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC  CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZCMDON, QFCLOS, ZEMSTP, SCCLAL, SCPRBF, COLSET, ANPRGT
C     EXTERNAL    LENSTR, QRESP, QFOPEN
C
C     + + + FORMATS + + +
C1000 FORMAT (80A1)
2000  FORMAT(1X,A1,'[?9r')
C
C     + + + END SPECIFICATIONS + + +
C
C     save log file unit number
C     TLOGFL= LOGFL
C     turn off log file
C     LOGFL= 0
C
C     does user want to save log file(1-N,2-Y)
C     FSCLU= 2
C     SGRP = 61
C     CALL QRESP (MESSFL,FSCLU,SGRP,RESP)
C     IF (RESP.EQ.2) THEN
C       yes, save log file as new file name
C       SGRP= 62
C       CALL QFOPEN (MESSFL,FSCLU,SGRP,NLOGFL,ERRFLG)
C       IF (ERRFLG.EQ.0) THEN
C         new file opened ok, end of file log file and rewind it
C         ENDFILE(TLOGFL)
C         REWIND(TLOGFL)
C         DONFG= 0
C         J= 80
C10       CONTINUE
C           read from TLOGFL and write to NLOGFL
C           READ (TLOGFL,1000,END=20) TBUFF
C           WRITE (NLOGFL,1000) (TBUFF(I),I=1,LENSTR(J,TBUFF))
C           GO TO 30
C
C20         CONTINUE
C             get here on end of file
C             CALL QFCLOS(NLOGFL,DONFG)
C             DONFG= 1
C
C30         CONTINUE
C         IF (DONFG.EQ.0) GO TO 10
C       END IF
C     END IF
C
C     reset to standard colors (grey on black)
      CALL COLSET (7,0)
C     clear the screen
      CALL SCCLAL
      I1= 1
      BLNK(1)= ' '
      I = 0
      CALL SCPRBF(I1,I,I1,BLNK)
C     close down message file and log file
      I= 0
      CALL QFCLOS(MESSFL,I)
C     CALL QFCLOS(TLOGFL,I)
      I= 21
      IF (ZCMDON(I).EQ.1) THEN
C       write and close scratch pad file (if it is being used)
        CALL ZEMSTP
      END IF
C
      PRMIND= 1
      CALL ANPRGT (PRMIND,CTYP)
      IF (CTYP .GT. 3) THEN
C       enable mouse
        WRITE(*,2000) CHAR(27)
        WRITE(*,*) 'MOUSE DISABLED'
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETPTH
     I                   (PTHTYP,
     O                    PTHNAM,PTHLEN)
C
C     + + + PURPOSE + + +
C     Determine the path to files related to a given
C     type of information (WDM, AIDE, Save).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      PTHLEN
      CHARACTER*8  PTHTYP
      CHARACTER*52 PTHNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PTHTYP - type of information for which path is needed
C     PTHNAM - character string containing path name to files
C     PTHLEN - length of path name
C
C     + + + PARAMETERS + + +
      INTEGER    MAXSAV
      PARAMETER (MAXSAV=5)
C
C     + + + SAVES + + +
      INTEGER      SPTCNT,SPTLEN(MAXSAV)
      CHARACTER*8  SPTTYP(MAXSAV)
      CHARACTER*52 SPTNAM(MAXSAV)
      SAVE         SPTCNT,SPTLEN,SPTTYP,SPTNAM
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,PTHFLG
      CHARACTER*11 FILNAM
      LOGICAL*4    LEXIST
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZLNTXT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA SPTCNT,SPTLEN/0,5*0/
      DATA SPTTYP,SPTNAM/5*' ',5*' '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A52)
C
C     + + + END SPECIFICATIONS + + +
C
      PTHFLG= 0
      IF (SPTCNT.GT.0) THEN
C       see if path name already found for this type of info
        I= 0
 10     CONTINUE
C         search through paths already defined
          I= I+ 1
          IF (SPTTYP(I).EQ.PTHTYP) THEN
C           this type already defined
            PTHFLG= 1
            PTHNAM= SPTNAM(I)
            PTHLEN= SPTLEN(I)
          END IF
        IF (I.LT.SPTCNT .AND. PTHFLG.EQ.0) GO TO 10
      END IF
C
      IF (PTHFLG.EQ.0) THEN
C       need to define path for this type of info
        I= 0
 20     CONTINUE
C         check three places for file containing path
          I= I+ 1
          IF (I.EQ.1) THEN
C           try current directory under C drive
            FILNAM= 'C:'//PTHTYP
          ELSE IF (I.EQ.2) THEN
C           try root directory under C drive
            FILNAM= 'C:'//CHAR(92)//PTHTYP
          ELSE
C           try current directory under current drive
            FILNAM= PTHTYP
          END IF
          INQUIRE (FILE=FILNAM,EXIST=LEXIST,ERR=30)
          GO TO 40
 30       CONTINUE
C           get here on inquire error
            LEXIST= .FALSE.
 40       CONTINUE
        IF (.NOT. LEXIST .AND. I.LT.3) GO TO 20
        IF (LEXIST) THEN
C         file containing path name found
          OPEN (UNIT=98,FILE=FILNAM,STATUS='OLD',ERR=50)
          READ (98,1000,ERR=50) PTHNAM
          PTHLEN= ZLNTXT (PTHNAM)
C         path name and length defined
          PTHFLG= 1
          IF (SPTCNT.LT.MAXSAV) THEN
C           increment count of paths defined
            SPTCNT= SPTCNT+ 1
            SPTTYP(SPTCNT)= PTHTYP
            SPTNAM(SPTCNT)= PTHNAM
            SPTLEN(SPTCNT)= PTHLEN
          END IF
 50       CONTINUE
        END IF
      END IF
C
      IF (PTHFLG.EQ.0) THEN
C       file containing path not found
        PTHNAM= ' '
        PTHLEN= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   BLDFNM
     I                   (PTHLEN,PTHNAM,IFLNAM,
     O                    FILNAM)
C
C     + + + PURPOSE + + +
C     Given a path and an input file name,
C     build a complete file name including the path.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      PTHLEN
      CHARACTER*12 IFLNAM
      CHARACTER*52 PTHNAM
      CHARACTER*64 FILNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PTHLEN - length of path name
C     PTHNAM - path name
C     IFLNAM - input base file name
C     FILNAM - complete output file name
C
C     + + + END SPECIFICATIONS + + +
C
      IF (PTHLEN.GT.0) THEN
C       use path name
        FILNAM= PTHNAM(1:PTHLEN)//IFLNAM
      ELSE
C       just use base name
        FILNAM= IFLNAM
      END IF
C
      RETURN
      END
