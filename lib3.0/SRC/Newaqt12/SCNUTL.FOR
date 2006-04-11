C
C
C
      SUBROUTINE   DELSCN
     I                   (MESSFL,CSCEN)
C
C     + + + PURPOSE + + +
C     batch routine to delete specified scenario
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL
      CHARACTER*8   CSCEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     CSCEN  - name of scenario to delete
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I0,I1
      CHARACTER*8   CCON,CLOC
C
C     + + + EXTERNALS + + +
      EXTERNAL     TSDSMD, TSESPC, TSDSCL
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
C     mark all assoc. data sets for delete
      CLOC = '        '
      CCON = '        '
      CALL TSESPC (CSCEN,CLOC,CCON)
      CALL TSDSMD
C     check data sets, delete if needed
      CALL TSDSCL (MESSFL,I0,I1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   COPSCN
     I                   (CASCEN,CNSCEN,DSNID,RELABS)
C
C     + + + PURPOSE + + +
C     batch routine to copy specified scenario
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       DSNID,RELABS
      CHARACTER*8   CASCEN,CNSCEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DSNID  - starting data set number id
C     RELABS - relative or absolute numbering from starting dsn id
C     CASCEN - name of active scenario
C     CNSCEN - name of new scenario
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,I3,J,NEWID,I1
      CHARACTER*8   CCON,CLOC,CTSLOC,CTSCON,CBLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL      TSESPC,TSDSM,TSDSMA
      EXTERNAL      TSDSSC,NEWDSN,NEWFIL,TSRETN
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      I3 = 3
C
C     find first data set in copied scenario
      CCON = '        '
      CLOC = '        '
      I    = 0
 10   CONTINUE
C       look for data set meeting specs
        I = I + 1
        CALL TSESPC (CASCEN,CLOC,CCON)
        CALL TSDSM (I)
        IF (I.GT.0) THEN
C         found a data set that needs to be replicated
          CBLNK = ' '
          CALL TSRETN (I,
     O                 CBLNK,CTSLOC,CTSCON)
          CALL TSESPC (CNSCEN,CTSLOC,CTSCON)
C         figure out where to put this new data set
          IF (RELABS.EQ.0) THEN
C           relative numbering
            NEWID = I + DSNID
          ELSE
C           absolute numbering
            NEWID = DSNID
          END IF
          IF (NEWID.LT.2) THEN
C           force no use of dsn 1
            NEWID = 2
          END IF
C         create this data set
          CALL TSDSMA (NEWID,
     O                 J)
C         set scenario loc const names in directory
          CALL TSDSSC (I,J,CNSCEN)
C         change dsn in uci file memory
          CALL NEWDSN (I3,I1,I,J)
        END IF
      IF (J.GT.0 .AND. I.GT.0) GO TO 10
C     change file names in uci file memory
      CALL NEWFIL (CASCEN,CNSCEN)
C
      RETURN
      END
C
C
C
      SUBROUTINE   NEWDSN
     I                   (XTSCOD,WDMID,OLD,NEW)
C
C     + + + PURPOSE + + +
C     swap an old data set number for a new data set number for a
C     new scenario
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    XTSCOD,OLD,NEW,WDMID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XTSCOD - which ts block to look in (3-ext targets)
C     WDMID  - wdm id of new dsn
C     OLD    - original data set number
C     NEW    - copy's data set number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I0,I1,ID,SREC,EREC,KEY,ITMP,OLDD,NEWD,J
      CHARACTER*4   OLDWDI,NEWWDI,CTMP
      CHARACTER*80  UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL      GETSE,GETUCI,REPUCI,WID2UA
C
C     + + + INPUT FORMATS + + +
 1012 FORMAT(I4)
 1015 FORMAT(A4)
C
C     + + + OUTPUT FORMATS + + +
 2012 FORMAT(I4)
 2015 FORMAT(A4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C     convert this dsnid to a wdm id and dsn
      CALL WID2UA (I0,OLD,
     O             J,OLDD,OLDWDI)
      CALL WID2UA (I0,NEW,
     O             J,NEWD,NEWWDI)
      IF (WDMID.EQ.1) THEN
        NEWWDI = "WDM1"
      ELSEIF (WDMID.EQ.2) THEN
        NEWWDI = "WDM2"
      ELSEIF (WDMID.EQ.3) THEN
        NEWWDI = "WDM3"
      ELSEIF (WDMID.EQ.4) THEN
        NEWWDI = "WDM4"
      END IF
C
      IF (XTSCOD.EQ.3) THEN
C       look in external targets records
        ID = 8
        CALL GETSE (ID,I1,
     O              SREC,EREC)
        IF (SREC.NE.0) THEN
C         some records exist in this table
          KEY = SREC
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
 50       CONTINUE
            READ (UCIBF(44:47),1015) CTMP
            IF (CTMP.EQ.'WDM ') THEN
C             update this to new mult wdm
              CTMP = 'WDM1'
            END IF
            READ (UCIBF(50:53),1012) ITMP
C           see if this dsn is the one we want
            IF (ITMP.EQ.OLDD .AND. CTMP.EQ.NEWWDI) THEN
C             yes it is, replace it
              ITMP = NEWD
              CTMP = NEWWDI
              WRITE (UCIBF(50:53),2012) ITMP
              WRITE (UCIBF(44:47),2015) CTMP
              CALL REPUCI (KEY,UCIBF)
            END IF
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
          IF (KEY.NE.EREC) GO TO 50
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NEWFIL
     I                   (COLD,CBAS)
C
C     + + + PURPOSE + + +
C     swap an old file name for a new file name for a new scenario
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*8    CBAS,COLD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CBAS   - base name to be included in new file names
C     COLD   - old base name from previous scenario
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I8,J,I0,I1,I80,ID,SREC,EREC,KEY,IUNIT,
     1              RCL,RET,K,I,ISLASH
      CHARACTER*1   CBAS1(8),FILNM1(64),COLD1(8)
      CHARACTER*12  ACC,STAT
      CHARACTER*30  FRM
      CHARACTER*64  FILNAM
      CHARACTER*80  UCIBF,TMPBF
      LOGICAL       IOPEN
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR,STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR,CVARAR,GETSE,GETUCI,REPUCI,FILOPN,STRFND
      EXTERNAL    REPFIL,ZIPC,QUPCAS
C
C     + + + INPUT FORMATS + + +
1000  FORMAT(I3)
C
C     + + + END SPECIFICATIONS + + +
C
      I8 = 8
      I0 = 0
      I1 = 1
      I64= 64
      I80= 80
      ID = 12
      CALL GETSE (ID,I1,
     O            SREC,EREC)
      IF (SREC.NE.0) THEN
C       some records exist in files block
        KEY = SREC
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
 50     CONTINUE
          IF (UCIBF(1:6) .EQ. 'MESSU ' .OR.
     1        UCIBF(1:6) .EQ. '      ') THEN
C           see if file is open
            FILNAM = UCIBF(17:78)
            INQUIRE (FILE=FILNAM,OPENED=IOPEN)
            IF (IOPEN) THEN
C             close old file
              READ (UCIBF(11:13),1000) IUNIT
              CLOSE (IUNIT)
            END IF
C           replace file name
            CALL CVARAR (I8,COLD,I8,COLD1)
            CALL QUPCAS (I8,COLD1)
            CALL CVARAR (I8,CBAS,I8,CBAS1)
            CALL QUPCAS (I8,CBAS1)
            CALL ZIPC (I64,' ',FILNM1)
            CALL CVARAR (I64,FILNAM,I64,FILNM1)
            CALL QUPCAS (I64,FILNM1)
            L    = LENSTR (I8,COLD1)
            ITMP = STRFND(I64,FILNM1,L,COLD1)
            J = LENSTR(I64,FILNM1)
            ISLASH = 0
            DO 10 I = 1,J
C             check for a path in the name
              IF (FILNM1(I).EQ.'\') THEN
                ISLASH = I
              END IF
 10         CONTINUE
            IF ((ITMP.GT.0 .AND. ISLASH.GT.0 .AND. ITMP.GT.ISLASH)
     1          .OR. (ITMP.GT.0 .AND. ISLASH.EQ.0)) THEN
C             found the old name in this string, replace it
              J = LENSTR (I8,CBAS1)
              TMPBF = UCIBF(1:15+ITMP) // CBAS(1:J) //
     1                UCIBF(ITMP+L+16:80)
            ELSE
C             just add the new scen name
              K = LENSTR(I8,CBAS1)
              IF (ISLASH.EQ.0) THEN
C               no path
                TMPBF = UCIBF(1:16) // CBAS(1:K) // '.' //
     1                  FILNAM(1:J)
              ELSE
C               have a path name, insert after slash
                TMPBF = UCIBF(1:16) // FILNAM(1:ISLASH) // CBAS(1:K) //
     1                  '.' // FILNAM(ISLASH+1:J)
              END IF
            END IF
            CALL REPUCI (KEY,TMPBF)
C           also need to replace in hspf files common block
            CALL REPFIL (FILNAM,TMPBF(17:80))
C           try to open with new name
            FILNAM = TMPBF(17:78)
            INQUIRE (FILE=FILNAM,OPENED=IOPEN)
            IF (IOPEN) THEN
C             already open
              write(99,*) 'newfil, already open ',filnam
            ELSE
              READ (UCIBF(11:13),1000) IUNIT
              ACC=  'SEQUENTIAL'
              FRM=  'FORMATTED'
              RCL=   132
              STAT= 'UNKNOWN'
              CALL FILOPN
     I                    (ACC,FRM,RCL,STAT,IUNIT,FILNAM,
     O                     RET)
              write(99,*) 'newfil, opening ',filnam,ret,iunit
            END IF
          END IF
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
        IF (KEY.NE.EREC) GO TO 50
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   FILSET
     I                   (MESSFL,UCIFL,WDMSFL,
     M                    FILES,
     O                    ECOUNT,RETCOD)
C
C     + + + PURPOSE + + +
C     set up uci and associated files for activating a scenario
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,UCIFL,RETCOD,WDMSFL,FILES(15),ECOUNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     UCIFL  - Fortran unit number for UCI file, neg to not open files
C     RETCOD - return code, -1 - not a HSPF UCI file, -2 problem with parms
C     WDMSFL - wdm file unit number
C     FILES  - array of hspf file unit numbers
C     ECOUNT - count of errors from files block
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,SGRP,SCLU,I1,I0,MSGFL,INITFG,CLEN,
     1             KWDDIM(1),KWDTYP(1),CONT,KCNT
      CHARACTER*1  CHSTR1(20),KWDLIB(12)
      CHARACTER*4  CTEMP
      LOGICAL      IOPEN
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR1,CHSTR)
      CHARACTER*20 CHSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    QFCLOS,WMSGTT,KEYUCI,DMPKEY
      EXTERNAL    FILBLK,UCIINP,WCH2UD
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (12A1,2I4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
C     if any files are already open, close them
      DO 10 I = 1,10
C       don't do for wdm file
        IF (FILES(I).NE.0) THEN
C         already open, close and set to zero
          INQUIRE (UNIT=FILES(I),OPENED=IOPEN)
          IF (IOPEN) THEN
            CLOSE (UNIT=FILES(I))
          END IF
          FILES(I) = 0
        END IF
 10   CONTINUE
C     use hspf msg file
      FILES(15)= MESSFL
C     process files block in input file
      CALL FILBLK (UCIFL,
     M             FILES,
     O             RETCOD)
      WRITE (99,*) 'FILBLK RETCOD',RETCOD
C     back to beginning of input file
      REWIND(ABS(UCIFL))
      IF (UCIFL.LT.0) THEN
C       dont do files block
        RETCOD = 0
      END IF
C
      ECOUNT = 0
      IF (RETCOD .EQ. 0) THEN
C       file block processed without error
C       read users uci file into memory
        MSGFL = FILES(1)
        CALL UCIINP (ABS(UCIFL),MESSFL,MSGFL)
C       close UCI file
        I = 0
        CALL QFCLOS (ABS(UCIFL),I)
C       get major keywords (RUN)
        SCLU  = 201
        SGRP  = 21
        INITFG= 1
        CLEN  = 20
        CALL WMSGTT (MESSFL,SCLU,SGRP,INITFG,
     M               CLEN,
     O               CHSTR1,CONT)
        READ (CHSTR,1000)  KWDLIB,KWDDIM,KWDTYP
C       look for major keywords
        CLEN  = 4
        CALL KEYUCI (I1,CLEN,I0,I0,I1,KWDLIB,KWDDIM,KWDTYP,
     M               ECOUNT,
     O               KCNT)
        CALL DMPKEY
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCISAV
     I                   (OUTFL)
C
C     + + + PURPOSE + + +
C     Write out a UCI file to disk with no screen interaction
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OUTFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTFL  - Fortran unit number for output UCI file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       LTYPRC,NREC,INDENT
      CHARACTER*80  UCIBUF
      CHARACTER*132 LONGBF
C
C     + + + EXTERNALS + + +
      EXTERNAL     GETUCI,GETIND
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT (A80)
 2020 FORMAT (A132)
C
C     + + + END SPECIFICATIONS + + +
C
      LTYPRC= -2
      NREC  = 1
 10   CONTINUE
        CALL GETUCI (LTYPRC,
     M               NREC,
     O               UCIBUF)
        CALL GETIND (NREC,
     O               INDENT)
        IF (INDENT .LE. 0) THEN
C         no indent - write uci buffer as stored
          WRITE (OUTFL,2010) UCIBUF
        ELSE
C         add indent first
          LONGBF= ' '
          WRITE (LONGBF(INDENT+1:INDENT+80),2010) UCIBUF
          WRITE (OUTFL,2020) LONGBF
        END IF
      IF (NREC.GT.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   GTNXKW
     I                   (MESSFL,INIT,ID,
     O                    CKWD,KWDFG,CONTFG,RETID)
C
C     + + + PURPOSE + + +
C     get next keyword and check for keyword's existance in uci file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,KWDFG,INIT,ID,CONTFG,RETID
      CHARACTER*12 CKWD
C
C     + + + ARGUMENT DEFINTIONS + + +
C     MESSFL - hspf message file unit number
C     INIT   - init flag, 1-get first keyword
C     ID     - id to look for, 0-run keywords, 121to129- operation keywords
C              -1-current parm 2 columns
C     KWDFG  - keyword exist flag, 1-yes
C     CKWD   - keyword name
C     CONTFG - continue flag, 1-there is more to read
C     RETID  - return id number, 2-global, 121-perlnd, 1001-perlnd activity
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU,SGRP,CLEN,IDNO,ITEMP,
     #             KWDDIM,KWDTYP,J,RETCOD,TBINFO(3),
     #             I1,LKWDS,SREC,EREC,IOP
      CHARACTER*1  CHSTR1(80),KWDLIB(12)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR1,CHSTR)
      CHARACTER*80 CHSTR
C
C     + + + SAVES + + +
      SAVE         IOP
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTT,GETSE,CARVAR,WMSGTV
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (12A1,2I4)
 1020 FORMAT (I3,5X,12A1)
 1030 FORMAT (6A1,I4)
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
C
      CKWD= " "
C
      IF (ID.GE.0) THEN
C       keywords from hspfmsg.wdm file
        IF (ID.EQ.0) THEN
C         run block keyword info
          SCLU = 201
          SGRP = 22
        ELSEIF (ID.LT.1000) THEN
C         operation type block keyword info or timser groups
          SCLU = ID
          SGRP = 1
        ELSE
          SGRP = MOD(ID,1000)
          SCLU = (ID-SGRP)/1000
C         write(*,*) ID,SGRP,SCLU
        END IF
C
        IF (INIT.EQ.1) THEN
C         initialize count of operation keywords
          IOP = 0
        END IF
C
        CLEN  = 80
        LKWDS = 12
C
        CALL WMSGTT (MESSFL,SCLU,SGRP,INIT,
     M               CLEN,
     O               CHSTR1,CONTFG)
        IF (ID.EQ.0) THEN
C         getting run block keywords
          READ(CHSTR,1000) (KWDLIB(J),J=1,LKWDS),KWDDIM,KWDTYP
          IF (KWDTYP .EQ. 100) THEN
C           operation type keyword
            IOP = IOP + 1
            IDNO = (IOP*1000) + 1
            CALL GETSE (IDNO,I1,
     O                  SREC,EREC)
            RETID = 120 + IOP
          ELSE
C           non-operation type keyword
            CALL GETSE (KWDTYP,I1,
     O                  SREC,EREC)
            RETID = KWDTYP
          END IF
        ELSE IF (ID .LT. 140) THEN
C         operation type keywords
          IOP = IOP + 1
          READ(CHSTR,1020) ITEMP,(KWDLIB(J),J=1,LKWDS)
          RETID = ((ID-120)*1000)+IOP
          CALL GETSE (RETID,I1,
     O                SREC,EREC)
          IF (ITEMP.EQ.0) THEN
C           this is 'end' operation or 'aaaaaaaaa'
            RETID = 0
          END IF
        ELSE IF (SCLU .LT. 150) THEN
C         valid timser stuff
          READ(CHSTR,1030) (KWDLIB(J),J=1,6),RETID
          LKWDS = 6
        END IF
C       return name of keyword
        CALL CARVAR (LKWDS,KWDLIB,LKWDS,CKWD)
        IF (SREC.GT.0) THEN
C         this keyword exists
          KWDFG = 1
        ELSE
C         this keyword does not appear in this uci file
          KWDFG = 0
        END IF
      ELSE IF (ID .GT. -100) THEN
C       keyword from current parm 2 screen
        J = ABS(ID)+ 1
        CALL WMSGTV (J,INIT,
     O               CHSTR1,RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         more
          CONTFG = 1
        ELSE
C         done
          CONTFG = 0
        END IF
        J = 12
        CALL CARVAR (J,CHSTR1,J,CKWD)
        KWDFG = 1
        RETID = -1
      ELSE
        IF (ID .EQ. -102) THEN
C         ftables
          J = 4
        ELSE
C         mass link
          J = 11
        END IF
        CALL GMTBNO(J,INIT,TBINFO)
        WRITE(CKWD,*) TBINFO(1)
        RETID = TBINFO(2)*10000 + TBINFO(3)
C       WRITE(*,*) 'GTNXKW',CKWD,RETID,TBINFO
        IF (TBINFO(1) .EQ. 0) THEN
C         all done
          CONTFG= 0
        ELSE
C         more
          CONTFG= 1
        END IF
      END IF
C     IF (ID > 140) WRITE(*,*) CKWD
C
      RETURN
      END
C
C
C
      SUBROUTINE   ESTADD
     I                   (MESSFL,TCLU,TGRP,KGRP,OPERNO,ADDOFG,
     O                    SREC,EREC)
C
C     + + + PURPOSE + + +
C     add specified table to the uci file in memory
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        MESSFL,TCLU,TGRP,KGRP,OPERNO,SREC,EREC,ADDOFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     TCLU   - cluster number of table
C     TGRP   - group number of table
C     KGRP   - sequential table number
C     OPERNO - operation number
C     SREC   - starting record number
C     EREC   - ending record number
C     ADDOFG - flag indicating to add operations to this table
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      NFLDS,SCOL(30),FLEN(30),APOS(30),IMIN(30),I0,I1,I3,
     $             NMHDRW,RETCOD,IMAX(30),IDEF(30),IDNO,LGRP,I78,I12,
     $             I,KEY,J,IPOS,OPID,IM1,IRPT,ITH,SGRP,INITFG,NUMREC,
     $             OLEN,CONT
      REAL         RMIN(30),RMAX(30),RDEF(30)
      CHARACTER*1  FTYP(30),HDRBUF(78,5),HBEXT(2),BLNK,CBLNK(3),CSTR(3)
      CHARACTER*1  OBUF1(80),TNMBUF(12)
      CHARACTER*80 UCIBF
C
C     + + + FUNCTIONS + + +
      INTEGER      STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTX,GETSE,PUTUCI,CHRINS,STRFND,CHRCHR,GETNXT
      EXTERNAL     GETUCI,PUTKWD,WMSGTT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CSTR /'*','*','*'/
      DATA CBLNK/' ',' ',' '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (80A1)
 2010 FORMAT (2X,12A1)
 2020 FORMAT ('  END ',12A1)
 2040 FORMAT (I5,75X)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      IM1= -1
      I1 = 1
      I3 = 3
      I12= 12
      I78= 78
      BLNK = ' '
C
      SREC = 0
      IF (KGRP.GT.1) THEN
C       figure out where to put it
        LGRP = KGRP + 1
 10     CONTINUE
          IF (LGRP.GT.1) THEN
            LGRP = LGRP-1
            IDNO = (OPERNO*1000)+LGRP
            IF (ADDOFG.LT.0) THEN
C             want the nth table
              I = -1 * ADDOFG
            ELSE
              I = 1
            END IF
            CALL GETSE (IDNO,I,
     O                  SREC,EREC)
          END IF
        IF (SREC.EQ.0 .AND. LGRP.GT.1) GO TO 10
      END IF
      IF (SREC.EQ.0) THEN
C       no other tables exist in this block, or this is the first
C       want to put after start of main block header
        IDNO = 100
        ITH  = TCLU - 120
C       see how many operations exist before this one
        IRPT = 0
        DO 15 I = 1,ITH
          CALL GETSE (IDNO,I,
     O                SREC,EREC)
          IF (SREC.GT.0) THEN
C           this operation exists
            IRPT = IRPT + 1
          END IF
 15     CONTINUE
        IF (IRPT.EQ.0) THEN
C         make call with irpt equals 1 to flag problem
          IRPT = 1
        END IF
C       now irpt contains the occurance number of an operation
        CALL GETSE (IDNO,IRPT,
     O              SREC,EREC)
        EREC = SREC
      END IF
C     need to start this table after erec
      IDNO= (OPERNO*1000)+KGRP
C
C     get this table header
      SGRP  = 1
      INITFG= 1
      NUMREC= 0
  5   CONTINUE
        OLEN= 80
        CALL WMSGTT (MESSFL,TCLU,SGRP,INITFG,
     M               OLEN,
     O               OBUF1,CONT)
        NUMREC= NUMREC+ 1
        INITFG= 0
C       loop back if more
      IF (CONT.EQ.1 .AND. NUMREC.LT.KGRP) GO TO 5
      CALL CHRCHR (I12,OBUF1(9),TNMBUF)
C
C     put table header
      WRITE (UCIBF,2010) (TNMBUF(I),I=1,12)
      KEY = EREC
      CALL PUTUCI (UCIBF,I0,KEY)
C     get next record number
      CALL GETUCI (I0,
     M             KEY,
     O             UCIBF)
      SREC = KEY
C
C     put header lines for this table
C     get this tables format info
      CALL WMSGTX (MESSFL,TCLU,TGRP,
     O             NFLDS,SCOL,FLEN,FTYP,APOS,IMIN,IMAX,IDEF,
     O             RMIN,RMAX,RDEF,
     O             NMHDRW,HDRBUF,RETCOD)
      DO 100 I= 1,NMHDRW
        IF (FLEN(1).EQ.4) THEN
C         insert 2 blanks in header lines to restore to original
          J= 1
          HBEXT(1)= HDRBUF(77,I)
          HBEXT(2)= HDRBUF(78,I)
          CALL CHRINS (I78,J,BLNK,HDRBUF(1,I))
          CALL CHRINS (I78,J,BLNK,HDRBUF(1,I))
        ELSE
          HBEXT(1)= BLNK
          HBEXT(2)= BLNK
        END IF
        IF (STRFND(I78,HDRBUF(1,I),I3,CSTR).EQ.0) THEN
C         no comment string found, add one
          IPOS= STRFND(I78,HDRBUF(1,I),I3,CBLNK)
          IF (IPOS.EQ.0) THEN
C           no space, try first pos
            IPOS = 1
          END IF
          CALL CHRCHR (I3,CSTR,HDRBUF(IPOS,I))
        END IF
        WRITE (UCIBF,2000)
     1        (HDRBUF(J,I),J=1,78),HBEXT
        CALL PUTUCI (UCIBF,IM1,KEY)
C       get next record number
        CALL GETUCI (IM1,
     M               KEY,
     O               UCIBF)
 100  CONTINUE
C
C     put a line for each operation
      OPID = 0
      CALL GETNXT (OPERNO,
     M             OPID)
 200  CONTINUE
C       put operation id line to uci file in memory
        WRITE (UCIBF,2040) OPID
        CALL PUTUCI (UCIBF,I0,KEY)
C       get next record number
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
C       get next operation id
        CALL GETNXT (OPERNO,
     M               OPID)
        IF (ADDOFG.EQ.0) THEN
C         do not want to add each operation here
          OPID = 0
        END IF
      IF (OPID.GT.0) GO TO 200
C
C     put end table header
      WRITE (UCIBF,2020) (TNMBUF(I),I=1,12)
      CALL PUTUCI (UCIBF,I0,KEY)
      CALL GETUCI (I0,
     M             KEY,
     O             UCIBF)
      EREC = KEY
C
C     data structure needs to know that this table now exists
      IF (TCLU.EQ.130 .AND. TGRP.EQ.112) THEN
        CALL PUTKWD (I3,IDNO,TNMBUF,SREC,EREC,
     O               RETCOD)
      ELSE
        CALL PUTKWD (I1,IDNO,TNMBUF,SREC,EREC,
     O               RETCOD)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTGLO
     I                   (SDATIM,EDATIM,OUTLEV,SPOUT,
     I                    RESMFG,RUNFG,UNIT,RNINFO)
C
C     + + + PURPOSE + + +
C     put global block back into memory uci file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      OUTLEV,RESMFG,RUNFG,SPOUT,
     $             SDATIM(5),EDATIM(5),UNIT
      CHARACTER*80 RNINFO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     OUTLEV - run interpreter output level
C     SPOUT  - special actions output level
C     RESMFG - resume flag - 0:standard mode, 1:resume mode
C     RUNFG  - run flag - 1:run with no errors
C                         0:just interp
C     UNIT   - english/metric units flag (english-1,metric-2)
C     RNINFO - character string of run information
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,ITYP,KEY,KEYST,KEYND
      CHARACTER*80 UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL  GETUCI,GETSE,REPUCI
C
C     + + + INPUT FORMATS + + +
 2030 FORMAT (2X,'START   ',I8,4(1X,I2),'  END',I8,4(1X,I2))
 2040 FORMAT (2X,'RUN INTERP OUTPUT LEVEL',2I5)
 2050 FORMAT (2X,'RESUME ',I5,' RUN ',I5,'            ',
     $        '       UNIT SYSTEM ',I5)
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
C     find table in uci (type 2 from hspf.seq, grp 22, col 3
      ITYP= 2
      CALL GETSE(ITYP,I1,
     O           KEYST,KEYND)
      IF (KEYST.NE.0) THEN
C       putting global block
        KEY    = KEYST
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        CALL REPUCI (KEY,RNINFO)
C       put dates and times
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        WRITE (UCIBF,2030,ERR=30) SDATIM,EDATIM
        CALL REPUCI (KEY,UCIBF)
 30     CONTINUE
C       put the "level" for quantity of run interpreter output
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        WRITE (UCIBF,2040,ERR=40)  OUTLEV,SPOUT
        CALL REPUCI (KEY,UCIBF)
 40     CONTINUE
C       ascertain functions to be performed in this run
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        WRITE (UCIBF,2050,ERR=60)  RESMFG, RUNFG, UNIT
 60     CONTINUE
        CALL REPUCI (KEY,UCIBF)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   XBLOCK
     I                    (BLKNO,INIT,
     O                     RETKEY,CBUFF,RETCOD)
C
C     + + + PURPOSE + + +
C     return one line of selected block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       BLKNO,INIT,RETCOD,RETKEY
      CHARACTER*80  CBUFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BLKNO  - block id number
C     CBUFF  - character string containing line of block
C     INIT   - initialization flag (get first line of block)
C     RETKEY - key of returned record
C     RETCOD - return flag  -1 does not exist
C                            1 returned header
C                            2 returned normal record
C                           10 no more to return
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        LRTKEY,RECTYP
C
C     + + + EXTERNALS + + +
      EXTERNAL       XBLOCKEX
C
C     + + + END SPECIFICATIONS + + +
C
      LRTKEY = 0
      CALL XBLOCKEX (BLKNO,INIT,
     M               LRTKEY,
     O               CBUFF,RECTYP,RETCOD)
      RETKEY = LRTKEY
C
      RETURN
      END
C
C
C
      SUBROUTINE   XBLOCKEX
     I                      (BLKNO,INIT,
     M                       RETKEY,
     O                       CBUFF,RECTYP,RETCOD)
C
C     + + + PURPOSE + + +
C     return one line of selected block, extended to include rec type
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       BLKNO,INIT,RETCOD,RETKEY,RECTYP
      CHARACTER*80  CBUFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BLKNO  - block id number
C     CBUFF  - character string containing line of block
C     INIT   - initialization flag (get first line of block)
C     RETKEY - input-types of record to read
C              output - key of returned record
C     RECTYP - type of record returned   0 - normal record
C                                       -1 - comment
C                                       -2 - blank
C     RETCOD - return flag  -1 does not exist
C                            2 returned normal record
C                           10 no more to return
C
C     + + + SAVE VARIABLES + + +
      INTEGER       EREC,KEY
      SAVE          EREC,KEY
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I1,I0,SREC,LEV
      CHARACTER*1   OBUF1(80)
      CHARACTER*80  UCIBF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE   (OBUFF,OBUF1)
      CHARACTER*80   OBUFF
C
C     + + + EXTERNALS + + +
      EXTERNAL       GETSE,GETUCIEX
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
      IF (RETKEY .LT. 0) THEN
        LEV = RETKEY
      ELSE
C       user has not asked for comments/blanks thru RETKEY
        LEV = 0
      END IF
C
      RETCOD = 0
      RETKEY = 0
      RECTYP = 0
C
      IF (INIT.NE.0) THEN
        IF (INIT.EQ.1) THEN
C         see if this table exists, start and end
          CALL GETSE (BLKNO,I1,
     O                SREC,EREC)
          KEY = SREC
        ELSE
C         already know start and end of mass-link or ftable
          EREC = MOD(INIT,10000)
          SREC = (INIT-EREC)/10000
C         skip first record
          KEY = SREC
          CALL GETUCIEX (I0,
     M                   KEY,
     O                   UCIBF,RECTYP)
C         WRITE(*,*) 'XBLOCK:',SREC,EREC,KEY,UCIBF
        END IF
        IF (SREC.EQ.0) THEN
C         this table does not exist
          RETCOD = -1
        END IF
      END IF
C
      IF (RETCOD.GE.0) THEN
C       get record
        CALL GETUCIEX (LEV,
     M                 KEY,
     O                 UCIBF,RECTYP)
C       WRITE(*,*) 'XBLOCK:',SREC,EREC,KEY,UCIBF
        CBUFF  = UCIBF
        IF (KEY.EQ.EREC) THEN
C         done with table
          RETCOD = 10
        ELSE
C         normal record
          RETCOD = 2
        END IF
        RETKEY = KEY
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   XTABLE
     I                    (MSGFL,OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR,
     O                     RETKEY,CBUFF,RETCOD)
C
C     + + + PURPOSE + + +
C     return one line of selected table
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MSGFL,TABNO,UUNITS,OMCODE,INIT,ADDFG,OCCUR,
     #              RETCOD,RETKEY
      CHARACTER*80  CBUFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - message file unit number
C     OMCODE - operation code  121-perlnd,122-implnd
C     TABNO  - table id number  1-activity
C     UUNITS - system of units   1-english, 2-metric
C     CBUFF  - character string containing line of table
C     INIT   - initialization flag (get first line of table)
C     ADDFG  - add table if missing
C     OCCUR  - occurance of table
C     RETKEY - key of record returned
C     RETCOD - return flag  -1 does not exist
C                            1 returned header
C                            2 returned normal record
C                           10 no more to return
C                         -999 bad table number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        LRTKEY,RECTYP
C
C     + + + EXTERNALS + + +
      EXTERNAL       XTABLEEX
C
C     + + + END SPECIFICATIONS + + +
C
      LRTKEY = 0
      CALL XTABLEEX (MSGFL,OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR,
     M               LRTKEY,
     O               CBUFF,RECTYP,RETCOD)
      RETKEY = LRTKEY
C
      RETURN
      END
C
C
C
      SUBROUTINE   XTABLEEX
     I                      (MSGFL,OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR,
     M                       RETKEY,
     O                       CBUFF,RECTYP,RETCOD)
C
C     + + + PURPOSE + + +
C     return one line of selected table, extended to include rec type
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MSGFL,TABNO,UUNITS,OMCODE,INIT,ADDFG,OCCUR,
     #              RETCOD,RETKEY,RECTYP
      CHARACTER*80  CBUFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - message file unit number
C     OMCODE - operation code  121-perlnd,122-implnd
C     TABNO  - table id number  1-activity
C     UUNITS - system of units   1-english, 2-metric
C     CBUFF  - character string containing line of table
C     INIT   - initialization flag (get first line of table)
C     ADDFG  - add table if missing
C     OCCUR  - occurance of table
C     RETKEY - input-types of record to read
C              output - key of returned record
C     RECTYP - type of record returned   0 - normal record
C                                       -1 - comment
C                                       -2 - blank
C     RETCOD - return flag  -1 does not exist
C                            1 returned header
C                            2 returned normal record
C                            3 returned comment
C                           10 no more to return
C                         -999 bad table number
C
C     + + + PARAMETERS + + +
      INTEGER    MXFLD
      PARAMETER (MXFLD=30)
C
C     + + + SAVE VARIABLES + + +
      INTEGER       NFLDS,SCOL(MXFLD),FLEN(MXFLD),APOS(MXFLD),
     1              IMIN(MXFLD),IMAX(MXFLD),IDEF(MXFLD),NMHDRW,
     2              HEDCNT,SREC,EREC,KEY,TABGRP(4)
      REAL          RMIN(MXFLD),RMAX(MXFLD),RDEF(MXFLD)
      CHARACTER*1   HDRBUF(78,5),FTYP(MXFLD)
      SAVE          NFLDS,SCOL,FLEN,APOS,IMIN,IMAX,IDEF,NMHDRW,
     1              RMIN,RMAX,RDEF,HDRBUF,FTYP,HEDCNT,SREC,EREC,KEY
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,SGRP,INITFG,CONT,NUMREC,OLEN,OPERNO
      INTEGER       ITMP,I1,I0,I78,LEV
      CHARACTER*1   OBUF1(80)
      CHARACTER*80  UCIBF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE   (OBUFF,OBUF1)
      CHARACTER*80   OBUFF
C
C     + + + EXTERNALS + + +
      EXTERNAL       WMSGTT,WMSGTX,GETSE,GETUCI,RNGCHK,CARVAR,ESTADD
C
C     + + + INPUT FORMATS + + +
 1030 FORMAT (22X,4(4X,I3))
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I78= 78
C
      RECTYP = 0
      RETCOD = 0
      IF (RETKEY .LT. 0) THEN
        LEV = RETKEY
      ELSE
C       user has not asked for comments/blanks thru RETKEY
        LEV = 0
      END IF
      RETKEY = 0
C
      OPERNO = OMCODE - 120
C
C     write(*,*) 'XTABLEEX:',MSGFL,OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR
      IF (INIT.EQ.1) THEN
C       first read addresses for this table, english and metric
        SGRP= 1
        INITFG= 1
        NUMREC= 0
 10     CONTINUE
          OLEN= 80
          CALL WMSGTT (MSGFL,OMCODE,SGRP,INITFG,
     M                 OLEN,
     O                 OBUF1,CONT)
          NUMREC= NUMREC+ 1
          IF (NUMREC.EQ.TABNO) THEN
            READ (OBUFF,1030) (TABGRP(I),I= 1, 4)
          END IF
          INITFG= 0
C         loop back if more
        IF (CONT.EQ.1 .AND. NUMREC.NE.TABNO) GO TO 10
C
        IF (NUMREC.NE.TABNO) THEN
C         big problem here, requested table number invalid
          WRITE (*,*) 'XTABLE problem, invalid table ',OMCODE,TABNO
          RETCOD = -999
        ELSE
          HEDCNT = 0
C         see if this table exists
          I = (OPERNO*1000)+TABNO
          CALL GETSE (I,OCCUR,
     O                SREC,EREC)
          SGRP= TABGRP(UUNITS)
C         WRITE(*,*) 'XTABLE:found:',SREC,EREC,SGRP,ADDFG
          IF (SREC.EQ.0) THEN
            IF (ADDFG.EQ.1) THEN
C             this table does not exist, add it
              CALL ESTADD (MSGFL,OMCODE,SGRP,TABNO,OPERNO,I1,
     O                   SREC,EREC)
C             WRITE(*,*) 'XTABLE:added:',SREC,EREC
            ELSE
              RETCOD = -1
            END IF
          END IF
          IF (SREC.GT.0) THEN
C           read info for this type of table from message file
            KEY = SREC
            CALL WMSGTX (MSGFL,OMCODE,SGRP,
     O                   NFLDS,SCOL,FLEN,FTYP,APOS,IMIN,IMAX,IDEF,
     O                   RMIN,RMAX,RDEF,NMHDRW,HDRBUF,ITMP)
C           WRITE(*,*) 'XTABLE:',KEY,SREC,EREC,NMHDRW,HEDCNT
          END IF
        END IF
      END IF
C
      IF (RETCOD .EQ. 0) THEN
        IF (NMHDRW.GT.HEDCNT) THEN
C         return a header row here
          HEDCNT= HEDCNT + 1
          CBUFF = ' '
          CALL CARVAR (I78,HDRBUF(1,HEDCNT),I78,CBUFF)
          RETCOD = 1
          RETKEY = -1
        ELSE
C         no more header rows to return, get regular record
          CALL GETUCIEX (LEV,
     M                   KEY,
     O                   UCIBF,RECTYP)
          IF (KEY.EQ.EREC) THEN
C           done with table
            RETCOD = 10
          ELSE IF (RECTYP.EQ.0) THEN
C           expand range of operations if needed
            CALL RNGCHK (OPERNO,KEY,
     M                   UCIBF)
            CBUFF  = UCIBF
            RETCOD = 2
            RETKEY = KEY
          ELSE IF (RECTYP.EQ.-1) THEN
C           comment
            CBUFF  = UCIBF
            RETCOD = 3
            RETKEY = KEY
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   XCLUGP
     I                    (MSGFL,OMCODE,TABNO,UUNITS,CHKEST,
     O                     SCLU,SGRP,ISECT,IREPT,RETCOD)
C
C     + + + PURPOSE + + +
C     return group and cluster number of selected table
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MSGFL,TABNO,UUNITS,OMCODE,RETCOD,SCLU,SGRP,IREPT
      INTEGER       CHKEST
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - message file unit number
C     OMCODE - operation code  121-perlnd,122-implnd,3-opseq,4-ftab
C     TABNO  - table id number  1-activity
C     UUNITS - system of units   1-english, 2-metric
C     RETCOD - return flag  -1 block does not exist
C     SGRP   - screen group
C     SCLU   - screen cluster
C     CHKEST - check existance of this table flag
C     ISECT  - section number to which this table belongs
C     IREPT  - number of times this table can occur
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,INITFG,CONT,NUMREC,OLEN,OPERNO
      INTEGER       TABGRP(4),I1,SREC,EREC
      CHARACTER*1   OBUF1(80)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE   (OBUFF,OBUF1)
      CHARACTER*80   OBUFF
C
C     + + + EXTERNALS + + +
      EXTERNAL       WMSGTT,GETSE,ESTADD
C
C     + + + INPUT FORMATS + + +
 1030 FORMAT (22X,4(4X,I3))
 1040 FORMAT (3X,I3,51X,I3)
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
C
      RETCOD = 0
      ISECT = 0
C
      IF (OMCODE.GE.100) THEN
C       operation type table
C       first read addresses for this table, english and metric
        LGRP= 1
        INITFG= 1
        NUMREC= 0
 10     CONTINUE
          OLEN= 80
          CALL WMSGTT (MSGFL,OMCODE,LGRP,INITFG,
     M                 OLEN,
     O                 OBUF1,CONT)
          NUMREC= NUMREC+ 1
          IF (NUMREC.EQ.TABNO) THEN
            READ (OBUFF,1030) (TABGRP(I),I= 1, 4)
            READ (OBUFF,1040) IREPT,ISECT
          END IF
          INITFG= 0
C         loop back if more
        IF (CONT.EQ.1 .AND. NUMREC.NE.TABNO) GO TO 10
        SGRP= TABGRP(UUNITS)
        SCLU= OMCODE
C
        IF (NUMREC.NE.TABNO) THEN
C         big problem here, requested table number invalid
          WRITE (99,*) 'XCLUGP problem, invalid table ',OMCODE,TABNO
          RETCOD = -999
        ELSE IF (CHKEST.GT.0) THEN
C         see if this table exists
          OPERNO = OMCODE - 120
          I = (OPERNO*1000)+TABNO
          CALL GETSE (I,I1,
     O                SREC,EREC)
          IF (SREC.EQ.0) THEN
C           this table does not exist, add it
            CALL ESTADD (MSGFL,OMCODE,SGRP,TABNO,OPERNO,I1,
     O                   SREC,EREC)
          END IF
        END IF
      ELSE
C       block type table
C       see if this table exists, start and end
        IF (CHKEST.GT.0) THEN
          CALL GETSE (OMCODE,I1,
     O                SREC,EREC)
          IF (SREC.EQ.0) THEN
C           this table does not exist
            RETCOD = -1
          END IF
        END IF
        SCLU = 53
        IF (OMCODE.EQ.3) THEN
C         opn sequence
          SGRP = 21
        ELSE IF (OMCODE.EQ.4) THEN
C         ftables
          SGRP = 36
        ELSE IF (OMCODE.EQ.5) THEN
C         ext sources
          SGRP = 150
        ELSE IF (OMCODE.EQ.6) THEN
C         formats
          SGRP = -99
          RETCOD = -1
        ELSE IF (OMCODE.EQ.7) THEN
C         network
          SGRP = 170
        ELSE IF (OMCODE.EQ.8) THEN
C         ext targets
          SGRP = 180
        ELSE IF (OMCODE.EQ.9) THEN
C         spec-actions
          SGRP = 41
        ELSE IF (OMCODE.EQ.10) THEN
C         schematic
          SGRP = 190
        ELSE IF (OMCODE.EQ.11) THEN
C         mass-link
          SGRP = 200
        ELSE IF (OMCODE.EQ.12) THEN
C         files
          SGRP = 50
        ELSE IF (OMCODE.EQ.13) THEN
C         category
          SGRP = 55
        ELSE IF (OMCODE.EQ.14) THEN
C         month-data
          SGRP = 60
        ELSE IF (OMCODE.EQ.15) THEN
C         pathnames
          SGRP = 65
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   RNGCHK
     I                   (OPERNO,KEY,
     M                    UCIBF)
C
C     + + + PURPOSE + + +
C     check to see if this record specifies a range of operations,
C     if so send back one record for each operation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        OPERNO,KEY
      CHARACTER*80   UCIBF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPERNO - operation number
C     UCIBF  - record read from in-memory uci
C     KEY    - key corresponding to this record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,SNO,ENO,TMPKEY,OPID,COUNT
      CHARACTER*5  CENO,BLNK5
      CHARACTER*80 TMPBF
C
C     + + + EXTERNALS + + +
      EXTERNAL     REPUCI,GETNXT,PUTUCI,GETUCI
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (2I5,70X)
 1010 FORMAT (5X,A5,70X)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (I5)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      BLNK5 = '     '
C
C     check to see if ending number has been entered in range
      READ (UCIBF,1010) CENO
      IF (CENO.NE.BLNK5) THEN
C       something in end of range field, check it
        READ (UCIBF,1000) SNO,ENO
        IF (ENO.NE.SNO) THEN
C         need to expand out this range
          UCIBF(6:10) = BLNK5
C         look for operation of this type
          TMPBF = UCIBF
          TMPKEY= KEY
          OPID  = SNO-1
          COUNT = 0
 10       CONTINUE
            CALL GETNXT (OPERNO,
     M                   OPID)
            IF (OPID.GT.0 .AND. OPID.LE.ENO) THEN
C             found an operation in the range
              COUNT = COUNT + 1
              WRITE (CENO,2000) OPID
              TMPBF(1:5) = CENO
              IF (COUNT.EQ.1) THEN
C               first occurance, replace record read
                CALL REPUCI (TMPKEY,TMPBF)
                UCIBF = TMPBF
              ELSE
C               add this record to in-memory uci
                CALL PUTUCI (TMPBF,I0,TMPKEY)
C               get next record number
                CALL GETUCI (I0,
     M                       TMPKEY,
     O                       TMPBF)
              END IF
            END IF
          IF (OPID.NE.0 .AND. OPID.LT.ENO) GO TO 10
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GMTBNO
     I                   (BID,INIT,
     O                    TBINFO)
C
C     + + + PURPOSE + + +
C     get id numbers of multiple table blocks (ftables and mass-links)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     BID,INIT,TBINFO(3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BID    - block id number 11-masslink 4-ftables
C     INIT   - init flag, start from beginning of block
C     TBINFO - array of info about each table (1)=table id
C                                             (2)=starting key
C                                             (3)=ending key
C
C     + + + FUNCTIONS + + +
      INTEGER      STRFND, CHRINT
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRFND,GETSE,GETUCI,CVARAR,CHRINT
C
C     + + + SAVE VARIABLES + + +
      INTEGER      SAVREC,SREC,EREC
      SAVE         SAVREC,SREC,EREC
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      KEY,SAVKEY
      CHARACTER*80 UCIBF
      CHARACTER*6  CNAME
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (12X,I3)
 1010 FORMAT (12X,I8)
C
C     + + + END SPECIFICATIONS + + +
C
      TBINFO(1) = 0
      TBINFO(2) = 0
      TBINFO(3) = 0
C
      IF (INIT.EQ.1) THEN
        CALL GETSE (BID,INIT,
     O              SREC,EREC)
C       WRITE(*,*) 'GMTBNO:srec:',SREC,EREC
        IF (SREC .GT. 0) THEN
C         at least one exists
          SAVREC = SREC
          IF (BID .EQ. 4) THEN
            CNAME= 'FTABLE'
          ELSE
            CNAME= 'MASS-L'
          END IF
        ELSE
          SAVREC = 0
        END IF
      END IF
C
      IF (SAVREC .GT. 0) THEN
        KEY = SAVREC
 10     CONTINUE
C         look at records in in-memory uci for start and end of table
          SAVKEY = KEY
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
C         WRITE(*,*) 'GMTBNO:',SAVKEY,KEY,UCIBF
          IF (UCIBF(3:8) .EQ. CNAME) THEN
C           found beginning of desired table
            TBINFO(2) = SAVKEY
            IF (BID .EQ. 4) THEN
              READ(UCIBF,1000) TBINFO(1)
            ELSE
              READ(UCIBF,1010) TBINFO(1)
            END IF
C           WRITE(*,*) 'GMTBNO:fnd start',TBINFO(1)
          ELSE IF (UCIBF(3:5) .EQ. 'END') THEN
C           found end of block, so done
            TBINFO(3) = SAVKEY
C           WRITE(*,*) 'GMTBNO:fnd end  ',TBINFO(1)
          END IF
        IF (KEY.NE.EREC .AND. TBINFO(3).EQ.0) GO TO 10
        SAVREC = KEY
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETOCR
     I                   (ID,
     O                    NOCCUR)
C
C     + + + PURPOSE + + +
C     get number of occurrances of this table
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ID,NOCCUR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ID     - table id number
C     NOCCUR - number of occurrances of this table
C
C     + + + EXTERNALS + + +
      EXTERNAL    GETSE
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,SREC,EREC
C
C     + + + END SPECIFICATIONS + + +
C
      I = 0
 10   CONTINUE
        I = I + 1
        CALL GETSE (ID,I,
     O              SREC,EREC)
      IF (SREC .GT. 0) GO TO 10
C
      NOCCUR = I - 1
C
      RETURN
      END
