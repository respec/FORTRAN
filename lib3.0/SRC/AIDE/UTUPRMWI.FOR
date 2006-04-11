C
C
C
      SUBROUTINE   ANPRGT (PRMIND,
     O                     PRMVAL)
C
C     get user parameter value, reads TERM.DAT on first entry
C     if overlay is used, make first call to this routine from ROOT
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   PRMIND,PRMVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PRMIND - user parameter index number
C     PRMVAL - user parameter value
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxprm.inc'
      INCLUDE 'pmesfl.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cusrpm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   UFILFG
C
C     + + + EXTERNALS + + +
      EXTERNAL   ANPRRD
C
C     + + + DATA INITIALIZATIONS + + +
      SAVE      UFILFG
      DATA      UFILFG/0/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (PRMIND.GT.0) THEN
        IF (UFILFG.EQ.0) THEN
C         need to fill in the user parameter common block
          CALL ANPRRD (MESSFL,
     O                 UPRMCT,UPARMS)
          UFILFG= 1
        END IF
        IF (PRMIND.GT.UPRMCT) THEN
C         no value available
          PRMVAL= -999
        ELSE
C         return the value
          PRMVAL= UPARMS(PRMIND)
        END IF
      ELSE IF (PRMIND.EQ.-1) THEN
C       special case for message file
        PRMVAL= MESSFL
      ELSE
C       no value available
        PRMVAL= -999
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ANPRRD (MESSFL,
     O                     UPRMCT,UPARMS)
C
C     reads user specific parameters from TERM.DAT file,
C     otherwise defaults, called only first time a parameter is
C     requested
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,UPRMCT,UPARMS(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     UPRMCT - count of user parameters
C     UPARMS - values of user parameters
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxprm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     LEN,I,I6,I44,J,UPRMVL,UTRMFL,APRMCL,APRMGP,
     1            UPRMID,APCLEN,APMIN(MXPRM),APMAX(MXPRM),
     2            I64,IOS,PTHLEN,INIT,CONT
      CHARACTER*1 UPRMNM(6),APRMTY(MXPRM),APBUFF(80),
     1            APRMNM(6,MXPRM),APCHVL(44,MXPRM)
      CHARACTER*8 AIDPTH
      CHARACTER*12 IFLNAM
      CHARACTER*52 PTHNAM
      CHARACTER*64 FILNAM
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (APBUFF,UBUFF)
      CHARACTER*80 UBUFF
C
C     + + + EXTERNALS + + +
      EXTERNAL    WMSGTT, STRFND, LENSTR, CHKINT, GETPTH, BLDFNM
C
C     + + + FUNCTIONS + + +
      INTEGER     STRFND, LENSTR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  I6,I44,I64 /6,44,64/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (5X,6A1,1X,A1,1X,I10,1X,2I10)
 1010 FORMAT (5X,6A1,1X,A1,1X,2I5,1X,44A1)
 1020 FORMAT (6A1,1X,A64)
 1030 FORMAT (BN,I10)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (' setting default system parameter values')
 2010 FORMAT (' reading users system parameters from TERM.DAT')
 2020 FORMAT (' unknown system parameter name: ',6A1)
 2030 FORMAT (' incorrect character parameter value: ',64A1)
 2040 FORMAT (' incorrect integer parameter value: ',I10)
 2050 FORMAT (' optional TERM.DAT file not opened, using defaults')
C
C     + + + END SPECIFICATIONS + + +
C
C     get valid parameter names
      APRMCL= 3
      APRMGP= 1
      UPRMCT= 0
      INIT  = 1
      WRITE (99,2000)
 10   CONTINUE
        I= 64
        CALL WMSGTT (MESSFL,APRMCL,APRMGP,INIT,
     M               I,
     O               APBUFF,CONT)
        INIT  = 0
        UPRMCT= UPRMCT+ 1
C
        IF (APBUFF(13).EQ.'C') THEN
C         character type parameter
          READ (UBUFF,1010) (APRMNM(I,UPRMCT),I=1,I6),APRMTY(UPRMCT),
     1      APMIN(UPRMCT),UPARMS(UPRMCT),(APCHVL(I,UPRMCT),I=1,44)
        ELSE
C         integer parameter
          READ (UBUFF,1000) (APRMNM(I,UPRMCT),I=1,I6),APRMTY(UPRMCT),
     1      UPARMS(UPRMCT),APMIN(UPRMCT),APMAX(UPRMCT)
        END IF
      IF (CONT .EQ. 1) GO TO 10
C
      APCLEN= UPRMCT* I6
C
C     try to open and read users TERM.DAT file
      UTRMFL= 90
C     first get path for file
      INCLUDE 'faidep.inc'
      CALL GETPTH (AIDPTH,
     O             PTHNAM,PTHLEN)
      IFLNAM= 'TERM.DAT'
      CALL BLDFNM (PTHLEN,PTHNAM,IFLNAM,
     O             FILNAM)
      OPEN (UNIT=UTRMFL,FILE=FILNAM,STATUS='OLD',
     1      ERR=50,IOSTAT=IOS,BLANK='NULL')
C
      WRITE (99,2010)
 20   CONTINUE
C       get a user specified parameter value
        READ (UTRMFL,1020,END=40) UPRMNM,UBUFF
C       which parameter?
        UPRMID= STRFND(APCLEN,APRMNM,I6,UPRMNM)
C
        IF (UPRMID.EQ.0) THEN
C         bad name on user file
          WRITE (99,2020) UPRMNM
        ELSE
C         fix the index
          UPRMID= 1+ UPRMID/I6
C         check the value
          IF (APRMTY(UPRMID).EQ.'C') THEN
C           type character
            IF (LENSTR(I64,APBUFF).GT.0) THEN
C             check against valid values
              I     = LENSTR(I44,APCHVL(1,UPRMID))+ APMIN(UPRMID)
              UPRMVL= STRFND(I,APCHVL(1,UPRMID),APMIN(UPRMID),APBUFF)
              IF (UPRMVL.EQ.0) THEN
C               bad character value
                WRITE (99,2030) (APBUFF(J),J=1,APMIN(UPRMID))
                UPRMVL= -999
              ELSE
C               fix the value
                UPRMVL= 1+ UPRMVL/APMIN(UPRMID)
              END IF
            ELSE
C             special case, name present means parm value of 1
              UPRMVL= 1
            END IF
          ELSE
C           type integer
            READ (UBUFF,1030) UPRMVL
            CALL CHKINT (APMIN(UPRMID),APMAX(UPRMID),UPARMS(UPRMID),
     O                   UPRMVL,I)
            IF (I.EQ.0) THEN
C             bad integer value
              WRITE (99,2040) UPRMVL
              UPRMVL= -999
            END IF
          END IF
          IF (UPRMVL.NE.-999) THEN
C           ok value, update default
            UPARMS(UPRMID)= UPRMVL
          END IF
        END IF
      GOTO 20
C     end of user parameter file
 40   CONTINUE
        GO TO 90
 50   CONTINUE
C     get here quickly on error of open of TERM.DAT, use defaults
        WRITE (99,2050)
        GO TO 90
 90   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   GLVINI
     I                   (APRMGP)
C
C     + + + PURPOSE + + +
C     initialize global values from message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      APRMGP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     APRMGP - Group containing global values
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       CONT,INIT,SGRP,LEN,IPOS,FLEN,APRMCL,I,IVAL(64),ICNT,
     1              MULT(3),ITMP(3)
      CHARACTER*1   STRIN1(512)
      CHARACTER*8   GLOID
      CHARACTER*64  BUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (BUFF,BUF1)
      CHARACTER*1  BUF1(64)
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTT, GSTGLV
C
C     + + + INPUT FORMATS + + +
1000  FORMAT (A8,I4,4X,3I8)
1010  FORMAT (16X,3I8)
C
C     + + + END SPECIFICATIONS + + +
C
C     global valid values
      APRMCL= 3
      INIT  = 1
      IPOS  = 0
      ICNT  = 0
 10   CONTINUE
        LEN= 64
        CALL WMSGTT (MESSFL,APRMCL,APRMGP,INIT,
     M               LEN,
     O               BUF1,CONT)
        IF (INIT .EQ. 1) THEN
C         header info
          READ(BUFF,1000) GLOID,FLEN,MULT
          INIT= 0
        ELSE
          ICNT= ICNT+ 1
          DO 20 I= 1,FLEN
            IPOS= IPOS+ 1
            STRIN1(IPOS)= BUFF(I:I)
 20       CONTINUE
          READ(BUFF,1010) ITMP
          IVAL(ICNT)= (ITMP(1)* MULT(1))+ (ITMP(2)* MULT(2))+
     1                (ITMP(3)* MULT(3))
        END IF
      IF (CONT .EQ. 1) GO TO 10
C
      CALL GSTGLV (GLOID,FLEN,IPOS,STRIN1,ICNT,IVAL)
C
      RETURN
      END
