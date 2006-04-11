C
C
C
      SUBROUTINE   HSPSTA
     I                   (IOPT,NOPNS,LAST,COUNT,OPN,OMCODE,OPTNO)
C
C     + + + PURPOSE + + +
C     routine to show run status for HSPF
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IOPT,NOPNS,LAST,COUNT,OPN,OMCODE,OPTNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT   - position to output
C     NOPNS  - total number of operations
C     LAST   - last time interval
C     COUNT  - number of current time interval
C     OPN    - number of current operation number
C     OMCODE - code number of current operation
C     OPTNO  - number for this operation
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          OPCT,PCT,ILEN,OPT
      DOUBLE PRECISION BINC,LINC,BCUR,LCUR
      CHARACTER*1      CPCT,TXT(12)
      SAVE             OPCT,BINC,LINC
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TXT,LTXT)
      CHARACTER*12 LTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL    UPDWIN
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(I1,'%')
2010  FORMAT(I2,'%')
C
C     + + +  END SPECIFICATIONS + + +
C
      IF (COUNT.EQ.1 .AND. OPN.EQ.1) THEN
        BINC= FLOAT(1)/FLOAT(LAST)
        LINC= BINC * (FLOAT(1)/FLOAT(NOPNS))
        OPCT= 0
        !WRITE(*,*) 'HSPSTA:HSPSTA:BINC,LINC',BINC,LINC,NOPNS,LAST
        ILEN= 12
        OPT = 1
        LTXT= 'Executing'
        CALL UPDWIN(OPT,ILEN,TXT)
        OPT = 2
        LTXT= '  Now    '
        CALL UPDWIN(OPT,ILEN,TXT)
        OPT = 4
        LTXT= 'Complete '
        CALL UPDWIN(OPT,ILEN,TXT)
      END IF
      BCUR= FLOAT(COUNT-1) * BINC
      LCUR= FLOAT(OPN-1)   * LINC
      PCT = 100* (LCUR + BCUR)
      !WRITE(*,*) 'HSPSTA:HSPSTA:PCT',PCT,COUNT,OPN,IOPT,BCUR,LCUR
      IF (PCT .NE. OPCT) THEN
        ILEN= 1
        CPCT= CHAR(PCT)
        CALL UPDWIN(IOPT,ILEN,CPCT)
        IF (PCT < 10) THEN
          WRITE(LTXT,2000) PCT
        ELSE IF (PCT < 100) THEN
          WRITE(LTXT,2010) PCT
        ELSE
          LTXT = '99%'
        END IF
        OPT = 3
        CALL HDMES3(OPT,LTXT)
      END IF
      OPCT = PCT
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMESC
     I                   (IOPT,MESSFL,SCLU,SGRP,STRING)
C
C     + + + PURPOSE + + +
C     put message with character string to output unit
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IOPT,MESSFL,SCLU,SGRP
      CHARACTER*64  STRING
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT   - position to output(not used in batch version)
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     SGRP   - screen message group
C     STRING - character string to add to message
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I1,CLEN(1),I,J,OFUN
      CHARACTER*1  STRIN1(64)
C
C     + + + EXTERNALS + + +
      EXTERNAL    CVARAR,PMXTFC,SCCUMV
C
C     + + + END SPECIFICATIONS + + +
C
      I1     = 1
      CLEN(1)= 64
C     set cursor in right place
      I = 6
      J = 12
      CALL SCCUMV (I,J)
C
C     convert character string to array
      CALL CVARAR (CLEN(1),STRING,CLEN(1),STRIN1)
C     send message to screen
      OFUN = -1
      CALL PMXTFC (MESSFL,OFUN,SCLU,SGRP,I1,CLEN,STRIN1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMESI
     I                   (IOPT,MESSFL,SCLU,SGRP,I)
C
C     + + + PURPOSE + + +
C     put message with integer to output unit
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IOPT,MESSFL,SCLU,SGRP,I
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT   - position to output(not used in batch version)
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     SGRP   - screen message group
C     I      - integer value to add to message
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I1,IVAL(1),K,J,OFUN
C
C     + + + EXTERNALS + + +
      EXTERNAL    PMXTFI,SCCUMV
C
C     + + + END SPECIFICATIONS + + +
C
      I1     = 1
      IVAL(1)= I
C     set cursor in right place
      K = 6
      J = 12
      CALL SCCUMV (K,J)
C
C     send message to screen
      OFUN = -1
      CALL PMXTFI (MESSFL,OFUN,SCLU,SGRP,I1,IVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMEST
     I                   (IOPT,MESSFL,SCLU,SGRP)
C
C     + + + PURPOSE + + +
C     write message file text to window
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     IOPT,MESSFL,SCLU,SGRP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT   - position to output
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     SGRP   - screen message group
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     INITFG, OLEN, CONT
      CHARACTER*1 TXT(72)
C
C     + + + EXTERNALS + + +
      EXTERNAL    WMSGTT,UPDWIN
C
C     + + +  END SPECIFICATIONS + + +
C
      INITFG = 1
      OLEN   = 72
      CALL WMSGTT (MESSFL,SCLU,SGRP,INITFG,
     M             OLEN,
     O             TXT,CONT)
C
      CALL UPDWIN(IOPT,OLEN,TXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMES2
     I                   (IOPT,KTYP,OCCUR)
C
C     + + + PURPOSE + + +
C     write keyword to window
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IOPT,KTYP,OCCUR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT    - position to output
C     KTYP    - type of keyword
C     OCCUR   - number of occurances
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      ILEN
      CHARACTER*12 KNAME
C
C     + + + EXTERNALS + + +
      EXTERNAL     GETKNM, UPDWIN
C
C     + + +  END SPECIFICATIONS + + +
C
      CALL GETKNM(KTYP,OCCUR,
     O            KNAME)
C
      ILEN = 12
      CALL UPDWIN(IOPT,ILEN,KNAME)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMES3
     I                   (IOPT,TXT)
C
C     + + + PURPOSE + + +
C     write text to window
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          ::  IOPT
      CHARACTER(LEN=*) ::  TXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT    - position to output
C     TXT     - text to write
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      ILEN
C
C     + + + INTRINSICS + + +
      INTRINSIC    LEN
C
C     + + + EXTERNALS + + +
      EXTERNAL     UPDWIN
C
C     + + +  END SPECIFICATIONS + + +
C
      ILEN = LEN(TXT)
      CALL UPDWIN(IOPT,ILEN,TXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMESN
     I                   (IOPT,INUM)
C
C     + + + PURPOSE + + +
C     write integer number to window
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IOPT, INUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT   - position to output
C     INUM   - number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      ILEN
      CHARACTER*6  CNUM
C
C     + + + EXTERNALS + + +
      EXTERNAL     UPDWIN
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(I6)
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE(CNUM,2000) INUM
      ILEN = 6
      CALL UPDWIN(IOPT,ILEN,CNUM)
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION CKUSER ()
C
C     + + + PURPOSE + + +
C     ckeck user status - 1 is cancel
C
cthj      USE SCENMOD, ONLY: UPDATESTATUS
C     INTEGER    UPDATESTATUS
C     DLL_IMPORT UPDATESTATUS
C
C     + + + LOCAL VARIABLES + + +
cthj      INTEGER     IOPT,IRET
cthj      INTEGER*1   JTXT(1)
C
C     + + + END SPECIFICATIONS + + +
C
cthj      IOPT= 0
cthj      IRET= UPDATESTATUS(IOPT,JTXT)
cthj      IF (IRET .GT. 0) THEN
cthj        CKUSER = 1
cthj      ELSE
        CKUSER = 0
cthj      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE UPDWIN(IOPT,ILEN,ATXT)
C
C     + + + PURPOSE + + +
C     write status (nowait) to ms window
C
cthj      USE SCENMOD, ONLY: UPDATESTATUS
C     INTEGER    UPDATESTATUS
C     DLL_IMPORT UPDATESTATUS
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      IOPT, ILEN
      CHARACTER*1  ATXT(ILEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT    - position to output
C     ILEN    - length of text to output
C     ATXT    - text to output
C
C     + + + LOCAL VARIABLES + + +
cthj      INTEGER      OLEN, JLEN
cthj      INTEGER      I
cthj      INTEGER*1    JTXT(132)
C
C     + + + END SPECIFICATIONS + + +
C
C     text to local string
cthj      IF (IOPT .EQ. 5) THEN
cthj        OLEN= 1
cthj      ELSE
cthj        IF (IOPT.EQ.1) THEN
cthj          OLEN = 72
cthj        ELSE IF (IOPT.EQ.2 .OR. IOPT.EQ.4) THEN
cthj          OLEN = 12
cthj        ELSE IF (IOPT.EQ.3) THEN
cthj          OLEN = 6
cthj        ELSE IF (IOPT.EQ.10) THEN
cthj          OLEN = 48
cthj        ELSE IF (IOPT.EQ.6) THEN
cthj          OLEN = 80
cthj        ELSE
cthj          OLEN = 6
cthj        END IF
cthj        JLEN = ILEN
cthj        IF (JLEN .GT. OLEN) THEN
cthj          JLEN = OLEN
cthj        END IF
cthj      END IF
cthj      !WRITE(*,*)'HSPSTA:UPDWIN:stat:',IOPT,OLEN,ILEN,JLEN
C     text to I*1 array
cthj      JTXT= 32
cthj      I   = 1
cthj      DO WHILE (I.LE.ILEN)
cthj        JTXT(I)= ICHAR(ATXT(I))
cthj        I      = I+ 1
cthj      END DO
cthj      JTXT(I)= 0
cthj      !IF (IOPT .EQ. 5) WRITE(*,*) 'HSPSTA:slider',JTXT(1),JTXT(2)
cthj      I= UPDATESTATUS(IOPT,JTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE SDELAY
     I                 (HUNSEC)
C
C     + + + PURPOSE + + +
C     delay specified amount of time
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     HUNSEC
C
C     + + + ARGUMENT DEFINTIONS + + +
C     HUNSEC - hundredths of a second to delay
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     TIM, OTIM
C
C     + + + END SPECIFICATIONS + + +
C
      CALL TIMER(TIM)
      OTIM = TIM + HUNSEC
      DO WHILE (OTIM > TIM)
        CALL TIMER(TIM)
      END DO
C
      RETURN
      END
C
C
C
      SUBROUTINE   HSPF_INI
     I                      (DELT,OPST,OPND,OPNTAB,
     O                       EXUPFG,EXTWID)
C
C     + + + PURPOSE + + +
C     Passes start-up information from HSPF to integrated model driver
C     program, and returns the number of intervals to run.  If a run
C     has multiple INSPANs, the driver must be able to respond to
C     multiple messages from this subroutine, each with its own DELT.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DELT,OPST,OPND,OPNTAB(20,OPND),EXUPFG,EXTWID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT   - timestep of run, in minutes
C     OPST   - first operation of this ingroup
C     OPND   - last operation of this ingroup
C     OPNTAB - table of operation information and keys
C     EXUPFG - flag indicating whether updates are needed from
C              external driver during the run
C     EXTWID - maximum inspan width required by external driver
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,LFIL,BINADD,BINU
      CHARACTER*80  LISTNM,BINNAM
      CHARACTER*256 MSG
C
C     + + + FUNCTIONS + + +
      INTEGER  QUERY_DRIVER
C
C     + + + EXTERNALS + + +
      EXTERNAL QUERY_DRIVER,GTOSVI
C
C     + + + DATA INITIALIZATIONS + + +
CTHJ  REPLACE WITH CORRECT ADDRESS FOR BINU(1)
      DATA LFIL,BINADD/11,49/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (7X,I10)
 1010 FORMAT (7X,A80)
C
C     + + + END SPECIFICATIONS + + +
C
C     by default, there is no external inspan limit
      EXTWID= 0
C
C     determine whether we need updates
      EXUPFG= QUERY_DRIVER ()
      IF (EXUPFG .EQ. 1) THEN
C       need to poll driver for inspan width
        WRITE (*,*) '(HSPF TIMESTEP',DELT,')'
        READ (*,*) MSG
        READ (MSG,1000,ERR=10) EXTWID
        WRITE (*,*) '(HSPF NEEDBIN',')'
        READ (*,*) MSG
        READ (MSG,1010,ERR=10) LISTNM
C
        OPEN (LFIL, FILE=LISTNM, ERR=999)
C
C       look through all operations for simulation modules
        DO 10 I= OPST, OPND
          IF (OPNTAB(4,I) .LE. 3) THEN
C           found a perlnd, implnd, or rchres
C
C           we can assume BINU fits within first record
            CALL GTOSVI (OPNTAB(7,I),BINADD,
     O                   BINU)
            INQUIRE (BINU,NAME=BINNAM)
C
C           report this unit number to the driver
            WRITE (LFIL,*) '(HSPF BINOUT_ID ',OPNTAB(1,I),
     $                      OPNTAB(2,I),BINNAM,')'
C
          END IF
 10     CONTINUE
      END IF
C
 999  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   EXT_UPDATE
     I                        (WDMSFL,FOPKEY,LOPKEY,OSUPM)
C
C     + + + PURPOSE + + +
C     Pause the run each inspan, read updates from file, and alter OSV as
C     needed.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER WDMSFL(4),FOPKEY,LOPKEY,OSUPM(11,LOPKEY)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - array of WDM file unit numbers
C     FOPKEY - pointer to first operation in osuper file
C     LOPKEY - pointer to last operation in osuper file
C     OSUPM  - osuper file
C
C     + + + LOCAL VARIABLES + + +
      LOGICAL       WDOPFG(4)
      INTEGER       INFIL,RECORD,OFFSET,I,I0,NVLIB,VARADD(2),
     $              ADDR,OMCODE,OPTNO
      REAL          NEWVAL,OLDVAL
      CHARACTER*6   VARNAM,VARLIB(2)
      CHARACTER*256 MSG,FILNAM,WDNAME(4)
C
C     + + + INTRINSICS + + +
      INTRINSIC     MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL      GTOSVI,GTOSVR,PTOSVR,WDFLCL,WDBOPN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA INFIL,NVLIB,I0/11,2,0/
      DATA VARLIB/'LZS   ','LZSN  '/
CTHJ REPLACE WITH NEW ADDRESSES
      DATA VARADD/863,513/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (7X,A80)
 1010 FORMAT (A6,3I5,F10.0)
C
C     + + + END SPECIFICATIONS + + +
C
C     close all open WDM files
      DO 10 I= 1, 4
        INQUIRE (WDMSFL(I),OPENED=WDOPFG(I),NAME=WDNAME(I))
        IF (WDOPFG(I)) THEN
          CALL WDFLCL (WDMSFL(I),
     O                 RETCOD)
        END IF
 10   CONTINUE
C
C     tell driver that we are ready to begin simulating current period
      RECORD= OSUPM(7,FOPKEY)
      CALL GTOSVI (record,84,
     O             iyear)
      CALL GTOSVI (record,85,
     O             imon)
      CALL GTOSVI (record,86,
     O             iday)
      WRITE (*,*) '(HSPF NEEDUPDATE AT ',IYEAR,IMON,IDAY,')'
C
C     pause until receive filename from driver
      READ (*,*) MSG
      READ (MSG,1000,ERR=100) FILNAM
C
C     process change file
      OPEN (INFIL, FILE=FILNAM, STATUS='OLD', ERR=100)
C
C     loop through all lines in file
 20   CONTINUE
        READ (INFIL,1010,ERR=50,END=50) OMCODE,OPTNO,VARNAM,
     $                                    NEWVAL
C
C       find operation that this applies to
        RECORD= 0
        DO 30 I= FOPKEY, LOPKEY
          IF ( (OSUPM(1,I) .EQ. OMCODE) .AND.
     $         (OSUPM(2,I) .EQ. OPTNO) ) THEN
            RECORD= OSUPM(7,I)
          END IF
 30     CONTINUE
C
        IF (RECORD .GT. 0) THEN
C         found operation - now determine variable address
          DO 40 I= 1, NVLIB
            IF (VARNAM .EQ. VARLIB(I)) THEN
C             found variable name - update
C
C             compute address in terms of record and offset
              ADDR= VARADD(I)
              OFFSET= MOD (ADDR,500)
              RECORD= RECORD+ (ADDR- OFFSET)/500
C             keep old value - add archiving function in final
              CALL GTOSVR (RECORD,OFFSET,
     O                     OLDVAL)
C
C             finally update variable value
              CALL PTOSVR (RECORD,OFFSET,NEWVAL)
            END IF
 40       CONTINUE
        END IF
C
C     end of loop to process line
      GO TO 20
C
C     finished processing file  
 50   CONTINUE
      CLOSE (INFIL,STATUS='DELETE')
C
 100  CONTINUE
C
C     reopen all previusly opened WDM files
      DO 110 I= 1, 4
        IF (WDOPFG(I)) THEN
          CALL WDBOPN (WDMSFL(I),WDNAME(I),I0,
     O                 RETCOD)
        END IF
 110  CONTINUE
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   QUERY_DRIVER ()
C     temporary dummy to be removed
      QUERY_DRIVER= 0
      RETURN
      END
