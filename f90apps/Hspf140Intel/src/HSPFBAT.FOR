C
C
C
      PROGRAM   HSPFBAT
C
C     + + + PURPOSE + + +
C     Batch HSPF, file management and system dependent stuff here,
C     then call generic code shared with interactive
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      FILES(15),I,I0,I1,USRFL,MESSU,
     $             RETCOD,RCL,SCLU,SGRP,IOPT,LEN,ICOMMA,ISPACE
C      INTEGER      SDATM(8),EDATM(8),IDATM(8),DSEC
      REAL         TIMES, TIMEE
      LOGICAL      LFLAG
      CHARACTER*64 FILNAM,VERSN,HMSNAM,FNAME,CLINE
      CHARACTER*12 ACC,STAT
      CHARACTER*30 FRMT
      CHARACTER*1  COMMA,SPACE
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR,STRFND
C
C     + + + INTRINSICS + + +
      INTRINSIC    MIN
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDBFIN, XGTARG, FILOPN, FILBLK, HSPF, HDMESI, SCCLAL
      EXTERNAL     ZIPI, DSSCLO, UCIINP, HDMESC, HDMEST, WDBOPN, LENSTR
      EXTERNAL     STRFND
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A64)
C
C     + + + END SPECIFICATIONS + + +
C
C     version info and unix what info
      INCLUDE 'versn.inc'
C
C     when start?
      CALL CPU_TIME(TIMES)
C     CALL TSTUF('    ',-1.0)
C      uncomment these lines to determine f90 run times
C      CALL DATE_AND_TIME(VALUES=SDATM)
C      SDATM(4)= SDATM(5)
C      SDATM(5)= SDATM(6)
C      SDATM(6)= SDATM(7)
C
C     vax - dont print output conversion error messages
CVAX  call errset (63,.true.,.false.,.false.,.false.,15)
C     avoid some lahey math errors
      LFLAG = .TRUE.
C     CALL INVALOP (LFLAG)
C     CALL UNDFL (LFLAG)
C
      OPEN (UNIT=99,FILE='error.fil')
C
      CALL POINTERPLAYGROUND      
C
C     initialize wdm file common block
      CALL WDBFIN
C
C     get command line - this is a system-specific routine
      CLINE= 'hspfuci.inp'
      CALL XGTARG
     M            (CLINE)
C
C     parse command line to get input filename - must be first argument
      I= 64
      I1= 1
      COMMA= ','
      SPACE= ' '
      LEN= LENSTR (I,CLINE)
      ICOMMA= STRFND (LEN,CLINE,I1,COMMA)- 1
      ISPACE= STRFND (LEN,CLINE,I1,SPACE)- 1
      IF (ICOMMA .LT. 0) ICOMMA= LEN
      IF (ISPACE .LT. 0) ISPACE= LEN
      LEN= MIN (ICOMMA,ISPACE)
      FILNAM= CLINE(1:LEN)
C
C     open message file
      I = 1
      INCLUDE 'fhsmes.inc'
      CALL WDBOPN (MESSFL,HMSNAM,I,
     O             RETCOD)
      IF (RETCOD.NE.0) THEN
C       problem with opening message file, prompt for other file name
        WRITE (*,*) 'Problem:',RETCOD,MESSFL,I,' with ',HMSNAM
        WRITE (*,*)
        WRITE (*,*) 'NAME OF WDM MESSAGE FILE: '
        READ (*,1000) FNAME
        CALL WDBOPN (MESSFL,FNAME,I,
     O               RETCOD)
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       do opening message
C       clear screen
cthj        CALL SCCLAL
        IOPT = 1
        SCLU = 201
        SGRP = 50
        CALL HDMESC (IOPT,MESSFL,SCLU,SGRP,FILNAM)
C       open input file
        USRFL= 7
        ACC  = 'SEQUENTIAL'
        FRMT = 'FORMATTED'
        RCL  = 0
        STAT = 'OLD'
        CALL FILOPN
     I              (ACC,FRMT,RCL,STAT,USRFL,FILNAM,
     O               RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         input file opened, process files block in input file
C         initialize files to closed
          I = 14
          I0= 0
          CALL ZIPI (I,I0,
     O               FILES)
          FILES(15) = MESSFL
          CALL FILBLK
     I                (USRFL,
     M                 FILES,
     O                 RETCOD)
C         back to beginning of input file
          REWIND(USRFL)
C
          IF (RETCOD .EQ. 0) THEN
C           file block processed without error - beginning message
            CALL SCCLAL
            SGRP = 51
            IOPT = 1
            CALL HDMEST (IOPT,MESSFL,SCLU,SGRP)
C           read users uci file
            MESSU  = FILES(1)
            CALL UCIINP (USRFL,MESSFL,MESSU)
C           close users input file
            CLOSE (UNIT=USRFL)
C           proceed to run model
            CALL HSPF (FILES,
     O                 RETCOD)
C            CALL HSPF (FILES,
C     O                 IDATM,RETCOD)
C           simulation complete
C           clear screen
            CALL SCCLAL
            IF (RETCOD .EQ. 0) THEN
              SGRP = 52
              IOPT = 1
              CALL HDMEST (IOPT,MESSFL,SCLU,SGRP)
            ELSE IF (RETCOD .EQ. 1) THEN
C             runfg=0 in global block - must stop
              SGRP = 53
              IOPT = 1
              CALL HDMEST (IOPT,MESSFL,SCLU,SGRP)
            ELSE IF (RETCOD .EQ. 2) THEN
C             errors in input file -  must stop
              SGRP = 54
              IOPT = 1
              CALL HDMEST (IOPT,MESSFL,SCLU,SGRP)
            ELSE IF (RETCOD .EQ. 3) THEN
C             no run keyword found in input file
              SGRP = 55
              IOPT = 1
              CALL HDMEST (IOPT,MESSFL,SCLU,SGRP)
            END IF
          ELSE
C           error in files block
            SGRP = 56
            IOPT = 1
            CALL HDMESI (IOPT,MESSFL,SCLU,SGRP,RETCOD)
          END IF
        ELSE
C         error opening uci file
          SGRP = 57
          IOPT = 1
          CALL HDMESI (IOPT,MESSFL,SCLU,SGRP,RETCOD)
        END IF
C
C       close any dss files
        CALL DSSCLO
      END IF
C
C     pointer end stuff
      CALL POINTERPLAYGROUND2
C      
C     when end?
      CALL CPU_TIME(TIMEE)
      WRITE(99,*) TIMEE-TIMES, TIMES, TIMEE
C     CALL TSTUF('Done',1.0)
C      uncomment these lines for run times on f90
C      CALL DATE_AND_TIME(VALUES=EDATM)
C      EDATM(4)= EDATM(5)
C      EDATM(5)= EDATM(6)
C      EDATM(6)= EDATM(7)
C      CALL TIMDIF (SDATM,IDATM,1,1,DSEC)
C      WRITE(*,2900) 'intrp tim:',DSEC/60,':',MOD(DSEC,60)
C2900  FORMAT(1X,A10,I4,A1,I2,I6,1X,8I4)
C      CALL TIMDIF (IDATM,EDATM,1,1,DSEC)
C      WRITE(*,2900) 'run   tim:',DSEC/60,':',MOD(DSEC,60)
C      CALL TIMDIF (SDATM,EDATM,1,1,DSEC)
C      WRITE(*,2900) 'total tim:',DSEC/60,':',MOD(DSEC,60)
C
      STOP
      END
C
C
C
      SUBROUTINE POINTERPLAYGROUND2
C
      USE HIOOSV
C
      TYPE PERLND_OSV_GENERAL
         SEQUENCE
         INTEGER       :: OPT(10)
         REAL          :: VAL(5)
         INTEGER       :: FLAG, DUM(484)
      END TYPE PERLND_OSV_GENERAL
      
      TYPE PERLND_OSV
         SEQUENCE
         TYPE(PERLND_OSV_GENERAL) :: PGEN
         REAL                     :: PARM(10)   
      END TYPE PERLND_OSV
      
      INTEGER            :: IOPBASE, IREC 
      TYPE (PERLND_OSV)  :: POSV
      POINTER (P,POSV)
C
      DO IOPBASE = 1,2
        IREC = 1 + ((IOPBASE-1)*2)
        P = LOC(OSVM(1,IREC))
        WRITE (99,*) IOPBASE,
     1               POSV%PGEN%OPT(1), POSV%PGEN%VAL(1), POSV%PGEN%FLAG, 
     1               POSV%PARM(1),
     1               OSVM(1,IREC),OSVM(16,IREC)
      END DO
      END            
C
C
C
      SUBROUTINE POINTERPLAYGROUND
C
      USE HIOOSV
C
      INTEGER FINDOFFSET
C
      CHARACTER*8 VAR_NAME
C
      VAR_NAME= "PRECFP"
      WRITE(99,*) VAR_NAME, FINDOFFSET(VAR_NAME)
      VAR_NAME= "GATFP"
      WRITE(99,*) VAR_NAME, FINDOFFSET(VAR_NAME)
      VAR_NAME= "AIRTFP"
      WRITE(99,*) VAR_NAME, FINDOFFSET(VAR_NAME)
C
      CALL INITOSV(5)
      CALL PTOSVI(1,1,1)
      CALL PTOSVR(1,11,1.0)
      CALL PTOSVI(1,16,-1)
      CALL PTOSVR(2,1,10.0)
      
      CALL PTOSVI(3,1,2)  
      CALL PTOSVR(3,11,2.0)
      CALL PTOSVI(3,16,-2)
      CALL PTOSVR(4,1,20.0)
C
      END SUBROUTINE
C
C
C
      INTEGER FUNCTION FINDOFFSET(VARIABLENAME) 
C
      CHARACTER(LEN=*) :: VARIABLENAME      
C
      INCLUDE 'CPLAT.INC'
C 
      INTEGER :: ADDR
      INTEGER, SAVE :: BASE_ADDR = 0
C
      IF (BASE_ADDR .EQ. 0) THEN      
        BASE_ADDR = LOC(ASVEC)
      END IF
C                 
      SELECT CASE (VARIABLENAME)
        CASE ("GATFP")
          ADDR = LOC(GATFP)
        CASE ("PRECFP")
          ADDR = LOC(PRECFP)
        CASE ("AIRTFP")
          ADDR = LOC(AIRTFP)
      END SELECT
      FINDOFFSET= 1+ (ADDR - BASE_ADDR)/4
C      
      END FUNCTION
      