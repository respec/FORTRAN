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
      USE SCENMOD, ONLY: UPDATESTATUS
C     INTEGER    UPDATESTATUS
C     DLL_IMPORT UPDATESTATUS
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IOPT,IRET
      INTEGER*1   JTXT(1)
C
C     + + + END SPECIFICATIONS + + +
C
      IOPT= 0
      IRET= UPDATESTATUS(IOPT,JTXT)
      IF (IRET .GT. 0) THEN
        CKUSER = 1
      ELSE
        CKUSER = 0
      END IF
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
      USE SCENMOD, ONLY: UPDATESTATUS
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
      INTEGER      OLEN, JLEN
      INTEGER      I
      INTEGER*1    JTXT(256)
C
C     + + + END SPECIFICATIONS + + +
C
C     text to local string
      IF (IOPT .EQ. 5) THEN
        OLEN= 1
        JLEN= 1
      ELSE
        IF (IOPT.EQ.1) THEN
          OLEN = 72
        ELSE IF (IOPT.EQ.2 .OR. IOPT.EQ.4) THEN
          OLEN = 12
        ELSE IF (IOPT.EQ.3) THEN
          OLEN = 6
        ELSE IF (IOPT.EQ.10) THEN
          OLEN = 48
        ELSE IF (IOPT.EQ.6) THEN
          OLEN = 255
        ELSE
          OLEN = 6
        END IF
        JLEN = ILEN
        IF (JLEN .GT. OLEN) THEN
          JLEN = OLEN
        END IF
      END IF
      !WRITE(*,*)'HSPSTA:UPDWIN:stat:',IOPT,OLEN,ILEN,JLEN
C     text to I*1 array
      JTXT= 32
      I   = 1
      DO WHILE (I.LE.JLEN)
        JTXT(I)= ICHAR(ATXT(I))
        I      = I+ 1
      END DO
      JTXT(I)= 0
      !IF (IOPT .EQ. 5) WRITE(*,*) 'HSPSTA:slider',JTXT(1),JTXT(2)
      I= UPDATESTATUS(IOPT,JTXT)
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
      USE SCENMOD, ONLY: SYNC_TIME
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
C     determine whether we need updates
      CALL SYNC_TIME (DELT,OPST,OPND,OPNTAB,
     O                EXUPFG,EXTWID)
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
      USE SCENMOD, ONLY: M_GETSTRING, UPDATESTATUSX
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER WDMSFL(5),FOPKEY,LOPKEY,OSUPM(11,LOPKEY)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - array of WDM file unit numbers, 5th is message file
C     FOPKEY - pointer to first operation in osuper file
C     LOPKEY - pointer to last operation in osuper file
C     OSUPM  - osuper file
C
C     + + + LOCAL VARIABLES + + +
      LOGICAL       WDOPFG(5)
      INTEGER       INFIL,RECORD,OFFSET,I,I0,I1,NVLIB,VARADD(5),
     $              ADDR,OMCODE,OPTNO,iyear,imon,iday,ihr,imin,RETCOD,C,
     $              BINADD,BINU,ERRFLG,ARCFIL,ILEN,VARSUB,MXSUB(5)
      INTEGER       WDMXRC(5), IMXRC, IBUFF(512)
      LOGICAL       MXMSG
      REAL          NEWVAL,OLDVAL
      CHARACTER*1   FUNC
      CHARACTER*4   CTYP
      CHARACTER*6   VARNAM,VARLIB(5),OPNAME,OPLIB(3)
      CHARACTER*16  S
      CHARACTER*80  UREC
      CHARACTER*256 MSG,DMSG,FILNAM,WDNAME(5),BINNAM,ARCNAM
C
C     + + + FUNCTIONS + + +
      INTEGER       WDGIVL
C
C     + + + INTRINSICS + + +
      INTRINSIC     MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL      GTOSVI,GTOSVR,PTOSVR,WDFLCL,WDBOPN,WDBFIN,WDGIVL
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NVLIB,I0,I1/5,0,1/
      DATA VARLIB/'LZS   ','LZSN  ','LZETP ','LZETPM','INFILT'/
      DATA MXSUB / 0,       0,       0,       12     ,      0 /
      DATA OPLIB/'PERLND','IMPLND','RCHRES'/
      DATA VARADD/864,514,881,626,515/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (7X,A80)
 1005 FORMAT (A80)
 1007 FORMAT (A4)
 1010 FORMAT (A6,I5,1X,A6,I5,1X,F10.0,1X,A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (I5,4I3,1X,A6,I4,1X,A6,I5,2E12.4)
 2010 FORMAT (1X,A,2I3,1X,A)
 2020 FORMAT (1X,A,2I5)
 2030 FORMAT (1X,A,4I5,1X,A)
 2040 FORMAT (1X,A,2I5,1X,A)
 2050 FORMAT (1X,A,1X,A)
 2060 FORMAT (1X,A,I5,1X,A)
 2070 FORMAT (1X,A,5I10,1X,A)
C
C     + + + END SPECIFICATIONS + + +
C
      ERRFLG= 0
C
C     close all open WDM files
      DO 10 I= 1, 5
C        WRITE(99,2020) 'HSPSTA:EXT_UPD:closing',I,WDMSFL(I)
        IF (WDMSFL(I) .NE. 0) THEN
          INQUIRE (WDMSFL(I),OPENED=WDOPFG(I),NAME=WDNAME(I))
          IF (WDOPFG(I)) THEN
            WDMXRC(I) = WDGIVL(WDMSFL(I),1,29)
            IF (I.EQ.5) THEN
C             special case for message file
              CLOSE (WDMSFL(I))
              RETCOD= 0
            ELSE
              CALL WDFLCL (WDMSFL(I),
     O                     RETCOD)
            END IF
C            WRITE(99,2030)  'HSPSTA:EXT_UPD:closed ',
C     $                   I,WDMSFL(I),WDMXRC(I),RETCOD,TRIM(WDNAME(I))
            WRITE(MSG,2010) 'HSPSTA:EXT_UPDATE:closed ',
     $                   WDMSFL(I),RETCOD,TRIM(WDNAME(I))
            ILEN = LEN_TRIM(MSG)
            CALL UPDATESTATUSX(7,ILEN,MSG)
C
           IMXRC = WDGIVL(WDMSFL(I),1,29)
           IF (WDMXRC(I) .NE. IMXRC .AND. WDMXRC(I) .GT. 0) THEN
              OPEN(UNIT=WDMSFL(I),FILE=WDNAME(I),ACCESS='DIRECT',
     $             RECL=2048,STATUS='OLD')
              READ(UNIT=WDMSFL(I),REC=1) IBUFF
C              WRITE(99,*) ' HSPSTA:EXT_UPD:BUFFER STATE:',
C     $               IBUFF(1),IBUFF(29),IBUFF(31)
              CLOSE(UNIT=WDMSFL(I))
            END IF
C
          ELSE
C            WRITE(99,2040) 'HSPSTA:EXT_UPD:not open',
C     $                   I,WDMSFL(I),TRIM(WDNAME(I))
          END IF
        END IF
 10   CONTINUE
C     be sure io ON 99 is complete
      CALL FLUSH(99)
C
C     close binu
      IF (OSUPM(1,FOPKEY) .LE. 2) THEN
        !perlnd or implnd
        BINADD = 49
      ELSE
        !rchres
        BINADD = 48
      END IF
      CALL GTOSVI (OSUPM(7,FOPKEY),BINADD,
     1             BINU)

      INQUIRE (BINU,NAME=BINNAM,ERR=20)
      CLOSE (UNIT=BINU)
      GOTO 30
 20   CONTINUE
        WRITE (99,*) 'HSPSTA:EXT_UPDATE:COULD NOT FIND BINU FILE',BINU
 30   CONTINUE

C      INQUIRE (99,OPENED=WDOPFG(6),NAME=WDNAME(6))
C      IF (WDOPFG(6)) THEN
C        WRITE(*,*) 'ABOUT TO CLOSE 99 ',TRIM(WDNAME(6))
C        CLOSE(99)
C      END IF
C
C     tell driver that we are ready to begin simulating current period
      RECORD= OSUPM(7,FOPKEY)
      IF (OSUPM(1,FOPKEY) .EQ. 3) THEN
        I = -1
      ELSE
        I = 0
      END IF
      CALL GTOSVI (record,I+84,
     O             iyear)
      CALL GTOSVI (record,I+85,
     O             imon)
      CALL GTOSVI (record,I+86,
     O             iday)
      CALL GTOSVI (record,I+87,
     O             ihr)
      CALL GTOSVI (record,I+88,
     O             imin)
C
      write(s,'(i4,"/",i2,"/",i2,"/",i2,"/",i2)') iyear,imon,iday,
     $                                            ihr,imin
      WRITE (*,*) '(HSPF NEEDUPDATE AT ' // s // ')'
      WRITE (99,2050) 'HSPSTA:EXT_UPDATE:',
     $            '(HSPF NEEDUPDATE AT ' // s // ')'
C
C     pause until receive filename from driver
      !Write (99,*) 'Wait for Msg from Driver'
      CALL M_GETSTRING (DMSG)
C     READ (*,*) DMSG
C
C      Write(99,2050) 'HSPSTA:EXT_UPDATE:Msg from Driver:', trim(dmsg)
C
      READ (DMSG,1000,ERR=40) FILNAM
      GO TO 50
 40   CONTINUE
        Write(99,2050) 'HSPSTA:EXT_UPDATE:' // 
     $                 'Error reading filename from:',TRIM(MSG)
        ERRFLG= 1
C
 50   CONTINUE
C
      Write(MSG,2050) 'HSPSTA:EXT_UPDATE:readfile:',trim(filnam)    
      ILEN = LEN_TRIM(MSG)
      CALL UPDATESTATUSX(7,ILEN,MSG)
C      Write(99,2050) 'HSPSTA:EXT_UPDATE:readfile:',trim(filnam)
C
      IF (ERRFLG .EQ. 0) THEN
C       process change file
        INFIL= 115
        OPEN (INFIL, FILE=FILNAM, STATUS='OLD', ERR=60, IOSTAT=I)
        GO TO 70
 60     CONTINUE
          Write(*,*) '(EXT_UPDATE:FileOpenErrorUpd:',I-16384,')'
          ERRFLG= 1
C
 70     CONTINUE
C        Write(99,2060) 'HSPSTA:EXT_UPDATE:file opened',
C     $                  INFIL,TRIM(FILNAM)
        !Write(*,*) '(EXT_UPDATE:File Opened)'

        ARCFIL= 116
        ARCNAM = TRIM(FILNAM) // 's'
C        Write(99,*) '  readfile',trim(arcnam)

        OPEN (ARCFIL, FILE=ARCNAM, POSITION='APPEND',
     $        ERR=73, IOSTAT=I)

        GO TO 77
 73     CONTINUE
          Write(*,2050) '(HSPSTA:EXT_UPDATE:FileOpenErrorArc:',
     $                  I-16384,')'
          ERRFLG= 1
C
 77     CONTINUE
        !Write(99,*) '  file opened',ARCFIL,TRIM(ARCNAM)
        !Write(*,*) '(EXT_UPDATE:File Opened)'
C
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       loop through all lines in file
 80     CONTINUE
          READ (INFIL,1005,ERR=120,END=120) UREC
          !WRITE(99,*) '  read UREC:' // UREC
          WRITE(MSG,2010) 'HSPSTA:EXT_UPDATE:urec:' // TRIM(UREC)
          ILEN = LEN_TRIM(MSG)
          CALL UPDATESTATUSX(7,ILEN,MSG)
C
          READ (UREC,1007,ERR=120,END=120) CTYP
          !WRITE(99,*) '  read CTYP:' // CTYP

          IF (CTYP.EQ.'MULT') THEN
C           change multiplication factor desired
            !WRITE(99,*) '  found MULT'
            CALL MULT_UPDATE (ARCFIL,UREC,FOPKEY,LOPKEY,OSUPM,
     1                        iyear,imon,iday,ihr,imin)
          ELSE
C           change parameter value desired
            READ (UREC,1010,ERR=120,END=120) OPNAME,OPTNO,VARNAM,
     $                                       VARSUB,NEWVAL,FUNC
            DO 90 I= 1, 3
              IF (OPNAME .EQ. OPLIB(I)) THEN
                OMCODE= I
              END IF
 90         CONTINUE
C            write(99,*) '  read record from file',OPNAME,OMCODE,OPTNO,
C     $                     VARNAM,VARSUB,NEWVAL,FUNC
C
C           find operation that this applies to
            RECORD= 0
            DO 100 I= FOPKEY, LOPKEY
              IF ( (OSUPM(1,I) .EQ. OMCODE) .AND.
     $             (OSUPM(2,I) .EQ. OPTNO) ) THEN
                RECORD= OSUPM(7,I)
              END IF
 100        CONTINUE
C
            IF (RECORD .GT. 0) THEN
C             found operation - now determine variable address
              DO 110 I= 1, NVLIB
                IF (VARNAM .EQ. VARLIB(I)) THEN
C                 found variable name - update
C
C                 compute address in terms of record and offset
                  IF ((VARSUB .GE. 1) .AND. (VARSUB .LE. MXSUB(I))) THEN
                    ADDR= VARADD(I)+ VARSUB- 1
                  ELSE
                    ADDR= VARADD(I)
                  END IF
                  OFFSET= MOD (ADDR,500)
                  RECORD= RECORD+ (ADDR- OFFSET)/500
C                 keep old value - add archiving function in final
                  CALL GTOSVR (RECORD,OFFSET,
     O                         OLDVAL)
C
C                  IF (FUNC .NE. ' ') THEN
C                    WRITE(99,*) '  adjust value:',FUNC,OLDVAL,NEWVAL
C                  END IF
C
                  IF (FUNC.EQ.'+') THEN
                    NEWVAL = OLDVAL + NEWVAL
                  ELSEIF (FUNC.EQ.'-') THEN
                    NEWVAL = OLDVAL - NEWVAL
                  ELSEIF (FUNC.EQ.'*') THEN
                    NEWVAL = OLDVAL * NEWVAL
                  END IF
C
C                 finally update variable value
                  CALL PTOSVR (RECORD,OFFSET,NEWVAL)
C                  WRITE (99,*) 'Reset ',VARNAM,OLDVAL,NEWVAL
                  WRITE (ARCFIL,2000) iyear,imon,iday,ihr,imin,
     1                                OPNAME,OPTNO,
     1                                VARNAM,VARSUB,OLDVAL,NEWVAL
                END IF
 110          CONTINUE
            END IF
          END IF
C
C       end of loop to process line
        GO TO 80
C
C       finished processing file  
 120    CONTINUE
      END IF
      CLOSE (ARCFIL)
      CLOSE (INFIL,STATUS='DELETE')
C
      IF (BINU .GT. 0) THEN
C       reopen binu
        OPEN (UNIT=BINU,FILE=BINNAM,ACCESS='SEQUENTIAL',
     1        FORM='UNFORMATTED',POSITION='APPEND',ACTION='DENYNONE')
      END IF
C
C     reopen all previusly opened WDM files
      CALL WDBFIN
      DO 140 I= 5, 1, -1
        IF (WDOPFG(I)) THEN
          C= 0
          MXMSG = .FALSE.
 130      CONTINUE
            C = C+ 1
            IF (I.EQ.5) THEN
              CALL WDBOPN (WDMSFL(I),WDNAME(I),I1,
     O                     RETCOD)
            ELSE
              CALL WDBOPN (WDMSFL(I),WDNAME(I),I0,
     O                     RETCOD)
            END IF
            IF (RETCOD .EQ. 0) THEN
              IMXRC = WDGIVL(WDMSFL(I),1,29)
              IF (WDMXRC(I) .GT. IMXRC) THEN
C               file buffer problem - compiler or op sys
                IF (MXMSG .EQV. .FALSE.) THEN
                  MXMSG = .TRUE.
                  WRITE(99,2030)  'HSPSTA:EXT_UPD:MXREC PROBLEM ',
     $                       I,WDMSFL(I),WDMXRC(I),IMXRC,TRIM(WDNAME(I))            
                  CALL WDFLCL(WDMSFL(I),RETCOD)
                  OPEN(UNIT=WDMSFL(I),FILE=WDNAME(I),ACCESS='DIRECT',
     $                 RECL=2048,STATUS='OLD')
                  READ(UNIT=WDMSFL(I),REC=1) IBUFF
                  WRITE(99,*) ' HSPSTA:EXT_UPD:BUFFER STATE:',
     $                 IBUFF(1),IBUFF(29),IBUFF(31)
                  CLOSE(UNIT=WDMSFL(I))
                  RETCOD = -1
                ELSE
C                 hopeless
                  C = 100000
                  RETCOD = -1
                END IF
              END IF
            END IF
          IF (RETCOD .NE. 0 .AND. C .LT. 100000) GO TO 130
          WDMXRC(I) = IMXRC
C
C          WRITE(99,2070) ' HSPSTA:EXT_UPDATE:opnWdmFile',
C     1                I,WDMSFL(I),RETCOD,C,IMXRC,TRIM(WDNAME(I))
          IF (RETCOD .NE. 0) THEN
            WRITE(99,*) '  HSPSTA:EXT_UPDATE:error opening WDM'
C           be sure io ON 99 is complete
            CALL FLUSH(99)
CC           force buffer clear
C            CLOSE(99)
C            OPEN(99,"ERROR.FIL",ACCESS="APPEND")
          END IF
C
          WRITE(MSG,2010) 'HSPSTA:EXT_UPDATE:openwdm ',WDMSFL(I),
     $                     RETCOD,TRIM(WDNAME(I))
          ILEN = LEN_TRIM(MSG)
          CALL UPDATESTATUSX(7,ILEN,MSG)
        END IF
 140  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   MULT_UPDATE
     I                        (ARCFIL,UREC,FOPKEY,LOPKEY,OSUPM,
     I                         IYEAR,IMON,IDAY,IHR,IMIN)
C
C     + + + PURPOSE + + +
C     Read multiplication factor update, alter time series
C     instructions as needed.
C
      USE SCENMOD, ONLY: M_GETSTRING, UPDATESTATUSX
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       ARCFIL,FOPKEY,LOPKEY,OSUPM(11,LOPKEY),
     1              IYEAR,IMON,IDAY,IHR,IMIN
      CHARACTER*80  UREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ARCFIL - unit number of archive file
C     UREC   - mult factor change record
C     FOPKEY - pointer to first operation in osuper file
C     LOPKEY - pointer to last operation in osuper file
C     OSUPM  - osuper file
C     IYEAR  - year of mfactr update
C     IMON   - month of mfactr update
C     IDAY   - day of mfactr update
C     IHR    - hour of mfactr update
C     IMIN   - minute of mfactr update
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SNUM,SMEMSB(2),TNUM,TMEMSB(2),DATIM(6),ILEN
      REAL          MULT
      CHARACTER*1   FUNC
      CHARACTER*6   SVOL,SGRPN,SMEMN,TVOL,TGRPN,TMEMN
      CHARACTER*256 MSG
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (5X,A6,I4,2A6,2I2,F10.0,A1,1X,A6,I4,2A6,2I2)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (1X,A,1X,A)
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
C
      READ (UREC,1000,ERR=120,END=120) SVOL,SNUM,SGRPN,SMEMN,SMEMSB,
     $                       MULT,FUNC,TVOL,TNUM,TGRPN,TMEMN,TMEMSB
C
C      WRITE(99,2000)  ' HSPSTA:MULT_UPDATE:urec:', TRIM(UREC)
      WRITE(MSG,2000) ' HSPSTA:MULT_UPDATE:urec:', TRIM(UREC)
      ILEN = LEN_TRIM(MSG)
      CALL UPDATESTATUSX(7,ILEN,MSG)
C
      DATIM(1) = IYEAR
      DATIM(2) = IMON
      DATIM(3) = IDAY
      DATIM(4) = IHR
      DATIM(5) = IMIN
      DATIM(6) = 0
C
C     use updmft from specact to update the mfact in the tsins
      CALL UPDMFT
     I           (I0,ARCFIL,FOPKEY,LOPKEY,OSUPM,
     I            SVOL,SNUM,SGRPN,SMEMN,SMEMSB,
     I            TVOL,TNUM,TGRPN,TMEMN,TMEMSB,
     I            MULT,FUNC,DATIM,I0)
C
      GOTO 110
 120  CONTINUE
        WRITE(99,*) '*** HSPSTA:MULT_UPDATE:error reading urec ***'
        WRITE(99,*) TRIM(UREC)
        WRITE(99,*) 'xxxxxSSSSSSNNNNGGGGGGMMMMMM1122FFFFFFFFFF' //
     $              'OXTTTTTTNNNNGGGGGGMMMMMM1122'
 110  CONTINUE
C
      RETURN
      END
