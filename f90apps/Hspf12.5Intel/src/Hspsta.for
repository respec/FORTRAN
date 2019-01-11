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
C     dummy for DOS version of HSPF
C
C     + + + END SPECIFICATIONS + + +
C
      CKUSER = 0
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
C     dummy for DOS version of HSPF
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
C     + + + END SPECIFICATIONS + + +
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
      REAL    TIM, OTIM
C
C     + + + END SPECIFICATIONS + + +
C
      CALL CPU_TIME(TIM)
      OTIM = TIM + (HUNSEC/ 100)
      DO WHILE (OTIM > TIM)
        CALL CPU_TIME(TIM)
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
C     dummy for the DOS version of HSPF
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
C     + + + END SPECIFICATIONS + + +
C
      EXUPFG= 0
      EXTWID= 0
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
C     dummy for the DOS version of HSPF
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER WDMSFL(5),FOPKEY,LOPKEY,OSUPM(11,LOPKEY)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - array of WDM file unit numbers
C     FOPKEY - pointer to first operation in osuper file
C     LOPKEY - pointer to last operation in osuper file
C     OSUPM  - osuper file
C
C     + + + END SPECIFICATIONS + + +
C
      RETURN
      END
C
C
C
      SUBROUTINE LOG_MSG
     I                  (MSG)
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER(LEN=*) MSG
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*256 ERROR_FILE_NAME
      CHARACTER*1   C
      CHARACTER*10  TIME, DATE, ZONE
      CHARACTER*15  TIMEX
      INTEGER       DT(8)
      INTEGER       IO_STATUS
      LOGICAL       EXIST_FLAG
      LOGICAL       OPEN_FLAG
      LOGICAL       WRITE_FLAG 
        
      DATA ERROR_FILE_NAME /'C:\TEMP\ERROR.FIL'/
C     DATA ERROR_FILE_NAME /'ERROR.FIL'/
      DATA WRITE_FLAG/.FALSE./

      INQUIRE(FILE=ERROR_FILE_NAME, 
     1        EXIST=EXIST_FLAG, 
     1        OPENED=OPEN_FLAG, 
     1        IOSTAT=IO_STATUS,ERR=98)

C     WRITE(*,*) OPEN_FLAG,EXIST_FLAG,IO_STATUS,TRIM(MSG)

      IF (TRIM(MSG) .EQ. 'WRITE') THEN
        WRITE_FLAG = .TRUE.
      END IF

      TIMEX = ""
      CALL DATE_AND_TIME(DATE,TIME,ZONE,DT)
      TIMEX = TIME(1:2) // ":" // 
     1        TIME(3:4) // ":" // 
     1        TIME(5:10) // " : "
        
      IF (TRIM(MSG) .EQ. 'OPEN' .OR. TRIM(MSG) .EQ. 'WRITE') THEN 
        IF (.NOT. OPEN_FLAG) THEN
          IF (EXIST_FLAG) THEN 
            OPEN(UNIT=99,FILE=ERROR_FILE_NAME,
     1           POSITION='APPEND',ACTION='WRITE', 
     1           ERR=98,IOSTAT=IO_STATUS,STATUS='OLD')
          ELSE
              OPEN(UNIT=99,FILE=ERROR_FILE_NAME,
     1             POSITION='APPEND',ACTION='WRITE', 
     1             ERR=98,IOSTAT=IO_STATUS,STATUS='NEW')
          END IF
          WRITE(99,*) TIMEX // 'LOG_MSG:ERROR.FIL OPENED'
        ELSE
          WRITE(99,*) TIMEX // 'LOG_MSG:ERROR.FIL ALREADY OPEN'
        END IF
      ELSE IF (TRIM(MSG) .EQ. 'CLOSE') THEN
        IF (OPEN_FLAG) THEN
          WRITE(99,*) TIMEX // 'LOG_MSG:ERROR.FIL CLOSING'
          CLOSE(99)
          OPEN_FLAG = .FALSE.
        END IF
        WRITE_FLAG = .FALSE.
      ELSE IF (OPEN_FLAG .AND. WRITE_FLAG) THEN
        WRITE(99,*) TIMEX // TRIM(MSG)
        CALL FLUSH(99)
      END IF

      RETURN

 98   CONTINUE
        WRITE (*,*) 'Error ',MOD(IO_STATUS,16384),
     1              ' opening ERROR.FIL',OPEN_FLAG
        INQUIRE(99,ERR=99,IOSTAT=IO_STATUS,OPENED=OPEN_FLAG)
 99   CONTINUE
        WRITE (*,*) 'Status',MOD(IO_STATUS,16384),OPEN_FLAG
        READ(*,*) C
        WRITE (*,*) C

      END SUBROUTINE
