C
C
C
      SUBROUTINE   INFLTS
     I                   (MESSFL,WDMFL)
C
C     + + + PURPOSE + + +
C     Input time-series data from a flat file to WDM.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     WDMFL  - Fortran unit number for WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'catrib.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I0,I1,I4,I6,I64,SCLU,SGRP,RESP,IRET,INUM,CNUM,
     $             IVAL(15),IAR(1),CVAL(6),DSTYPE,DSSTAT,DSCFRM,
     $             SFILE,IHEAD,CDSN,QC,OV,SDATE(6),EDATE(6),
     $             BEFLG,RETCOD,VERS,SPACE(5),TIME(7)
      CHARACTER*1  TBUFF(64)
      CHARACTER*8  PTHNAM(1)
      CHARACTER*64 DAFMT,FNAME(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL     QRESP, ZSTCMA, Q1INIT, Q1EDIT, QGETF, QGETI, QUPCAS
      EXTERNAL     INFREE, QSETOP, QSETI, QGETOP, QSTCTF, QGTCTF
      EXTERNAL     QSETCO, QGETCO, QSETR, QGETR, QSETFN, COPYI, ZIPI
      EXTERNAL     CVARAR, CARVAR, DISTOP, PRNTXT, QFCLOS, GETFUN
      EXTERNAL     INITAT, INDSLC, INDSSP, INDSGT, TSFLAT, PMXTXI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  SPACE,           TIME
     $    / 35,70,150,1,1,   4,6,1,1,1,1900,1 /
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 125
C
      I0  = 0
      I1  = 1
      I4  = 4
      I6  = 6
      I64 = 64
C
      FNAME(1) = ' '
      SFILE  = 0
      IHEAD  = 0
      OV     = 0
      QC     = 0
      BEFLG  = 0
      DAFMT  = ' '
      CDSN   = 0
      CALL ZIPI (I6,I0,SDATE)
      CALL ZIPI (I6,I0,EDATE)
      DSSTAT = 1
      DSCFRM = 3
C
C     initialize attribute arrays
      VERS = 4
      CALL INITAT (VERS,SPACE,TIME)
C
 10   CONTINUE
C       select a time-series input option
        SGRP = 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
        CALL ZSTCMA (I4,I1)
        IF (RESP.EQ.1) THEN
C         specify source flat file
          SGRP = 10
          CALL Q1INIT (MESSFL,SCLU,SGRP)
          IF (SFILE.GT.0) THEN
C           file already open, save file name
            INQUIRE (SFILE,NAME=FNAME(1))
C           put opened file name in field as default
            CALL QSETFN (I1,FNAME)
C           close file for now
            CALL QFCLOS (SFILE,I0)
          END IF
C         header lines
          IVAL(1) = IHEAD
          CALL QSETI (I1,IVAL)
          CALL Q1EDIT (IRET)
          IF (IRET.EQ.1) THEN
C           get file name and number of header lines
            CALL QGETF (I1,
     O                  IVAL)
            SFILE = IVAL(1)
C           header stuff
            CALL QGETI (I1,
     O                  IVAL)
            IHEAD = IVAL(1)
          ELSE IF (FNAME(1).NE.' ') THEN
C           previous command selected, reopen old file
            CALL GETFUN (I1,SFILE)
            OPEN (SFILE,FILE=FNAME(1))
          END IF
        ELSE IF (RESP.EQ.2) THEN
C         specify format
          IF (SFILE.GT.0) THEN
C           source file open, get options
            SGRP = 20
            CALL Q1INIT (MESSFL,SCLU,SGRP)
C           set format
            CALL CVARAR (I64,DAFMT,I64,TBUFF)
            CALL QSTCTF (I1,I64,TBUFF)
C           display top of file
            I = 10
            CALL DISTOP (SFILE,IHEAD,I)
C           edit options
            CALL Q1EDIT (IRET)
            IF (IRET.EQ.1) THEN
C             continue, get format
              CALL QGTCTF (I1,I64,TBUFF)
C             convert format to upper case
              CALL QUPCAS (I64,TBUFF)
              CALL CARVAR (I64,TBUFF,I64,DAFMT)
            END IF
          ELSE
C           source file not open yet
            SGRP = 21
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
        ELSE IF (RESP.EQ.3) THEN
C         define target data set
          SGRP = 30
          CALL Q1INIT (MESSFL,SCLU,SGRP)
C         determine firts free data set
          CALL INFREE (WDMFL,I1,I1,I1,
     O                 IVAL(1),RETCOD)
C         set data-set number
          CALL QSETI (I1,IVAL)
C         set data-set option to default of NEW
          IAR(1) = 1
          CALL QSETOP (I1,I1,IAR,IAR,IAR)
C         overwrite flag
          CVAL(1) = OV + 1
          CALL QSETCO (I1,CVAL)
C         edit options
          CALL Q1EDIT (IRET)
          IF (IRET.EQ.1) THEN
C           continue, get dsn
            CALL QGETI (I1,
     O                  IVAL)
            CDSN = IVAL(1)
C           get data-set option
            CALL QGETOP (I1,
     O                   IAR)
            DSSTAT = IAR(1)
C           overwrite flag
            CALL QGETCO (I1,
     O                   CVAL)
            OV = CVAL(1) - 1
          END IF
        ELSE IF (RESP.EQ.4) THEN
C         location and description parameters
          PTHNAM(1) = 'T       '
          CALL INDSLC (MESSFL,I1,PTHNAM,
     O                 IRET)
        ELSE IF (RESP.EQ.5) THEN
C         time parameters
          SGRP = 50
          CALL Q1INIT (MESSFL,SCLU,SGRP)
C         tsbyr, tsstep, quality code, dates
          IVAL(1) = AIVAL(6)
          IVAL(2) = AIVAL(7)
          IVAL(3) = QC
          CALL COPYI (I6,SDATE,IVAL(4))
          CALL COPYI (I6,EDATE,IVAL(10))
          INUM = 15
          CALL QSETI (INUM,IVAL)
C         tcode, tgroup (adjusted), tsform, compfg, vbtime
          AIVAL(2) = AIVAL(2) - 2
          I = 5
          CALL COPYI (I,AIVAL,CVAL)
          CVAL(6) = BEFLG + 1
          CNUM = 6
          CALL QSETCO (CNUM,CVAL)
C         tsfill
          CALL QSETR (I1,ARVAL(1))
          CALL Q1EDIT (IRET)
          IF (IRET.EQ.1) THEN
C           continue, get integer values
            CALL QGETI (INUM,
     O                  IVAL)
            AIVAL(6) = IVAL(1)
            AIVAL(7) = IVAL(2)
            QC = IVAL(3)
            CALL COPYI (I6,IVAL(4),SDATE)
            CALL COPYI (I6,IVAL(10),EDATE)
C           get character values
            CALL QGETCO (CNUM,
     O                   CVAL)
            I = 5
            CALL COPYI (I,CVAL,AIVAL)
            AIVAL(2) = AIVAL(2) + 2
            BEFLG = CVAL(6) - 1
C           get real
            CALL QGETR (I1,
     O                  ARVAL)
          END IF
        ELSE IF (RESP.EQ.6) THEN
C         space parameters
C         fake a table data set to get only space parms (no time parms)
          DSTYPE = 2
          PTHNAM(1) = 'T       '
          CALL INDSSP (MESSFL,I1,PTHNAM,DSTYPE,
     O                 IRET)
        ELSE IF (RESP.EQ.7) THEN
C         process input flat file
          IF (SFILE.GT.0 .AND. DAFMT.NE.' ') THEN
C           source file open, activate data set
            I = 99
            DSTYPE = 1
            PTHNAM(1) = 'IT      '
            CALL INDSGT (MESSFL,WDMFL,I,I1,PTHNAM,
     I                   DSSTAT,DSCFRM,DSTYPE,I1,
     M                   CDSN,
     O                   RETCOD)
            IF (RETCOD.EQ.1) THEN
C             process flat file
              SGRP = 70
              IVAL(1) = CDSN
              CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,I1,I1,IVAL)
              CALL TSFLAT (WDMFL,CDSN,SFILE,IHEAD,DAFMT,BEFLG,
     I                     AIVAL(7),AIVAL(1),QC,OV,ARVAL(1),SDATE,EDATE,
     O                     IRET)
              REWIND SFILE
            ELSE
C             problem activating data set
              SGRP = 73
              CALL PRNTXT (MESSFL,SCLU,SGRP)
            END IF
          ELSE IF (SFILE.EQ.0) THEN
C           source file not open yet
            SGRP = 71
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          ELSE
C           no format entered yet
            SGRP = 72
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
        END IF
        CALL ZSTCMA (I4,I0)
      IF (RESP.NE.8) GO TO 10
C
      IF (SFILE.GT.0) THEN
C       close source file
        CALL QFCLOS (SFILE,I0)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DISTOP
     I                   (SFILE,IH,MAXLIN)
C
C     + + + PURPOSE + + +
C     Display top of source flat file on screen to assist
C     in Format specification.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SFILE,IH,MAXLIN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SFILE  - Fortran unit number for source flat file
C     IH     - number of header lines in source file
C     MAXLIN - max number of lines to display
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I0,IM1,I78,I155,NH,NLINES,ILEN
      CHARACTER*1 TBUFF(156)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, ZBLDWR
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (156A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      IM1  = -1
      I78  = 78
      I155 = 155
C
      IF (IH.GT.3) THEN
C       only display last 3 header lines
        NH = IH
 10     CONTINUE
C         read and ignore lines before last 3
          READ (SFILE,1000,ERR=99,END=99) TBUFF
          NH = NH - 1
        IF (NH.GT.3) GO TO 10
      END IF
C
      NLINES = 0
      DO 50 I = 1,5
C       display up to 5 records
        IF (NLINES.LT.MAXLIN) THEN
C         read record and display it
          READ (SFILE,1000,ERR=99,END=99) TBUFF
          ILEN = LENSTR(I155,TBUFF)
          IF (ILEN.GT.78) THEN
C           need two lines for this record
            CALL ZBLDWR (I78,TBUFF,I0,IM1,J)
            NLINES = NLINES + 1
            IF (NLINES.LT.MAXLIN) THEN
C             put symbol in text to indicate word wrapping
              TBUFF(78) = '>'
              ILEN = ILEN - 77
              CALL ZBLDWR (ILEN,TBUFF(78),I0,IM1,J)
            END IF
          ELSE
C           record will fit on this line
            CALL ZBLDWR (ILEN,TBUFF,I0,IM1,J)
          END IF
          NLINES = NLINES + 1
        END IF
 50   CONTINUE
C
 99   CONTINUE
C       get here on read error or end of file
C       (sorry about the hard exits from the above loops,
C        but at least they only go to one place)
C
C     reset to start of source file
      REWIND SFILE
C
      RETURN
      END
