C
C
C
      SUBROUTINE   ADDBAT
     I                   (MESSFL,SCLU,TGRP,IDNO,IPRMPT,TNAME,
     M                    SREC,EREC)
C
C     + + + PURPOSE + + +
C     create a block if it does not exist, return starting and
C     ending records of created block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        MESSFL,SCLU,TGRP,IDNO,IPRMPT,SREC,EREC
      CHARACTER*12   TNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - cluster number of screen messages
C     TGRP   - group number of table
C     IDNO   - id number for this block
C     IPRMPT - flag to prompt user with question (1-ask,0-just do it)
C     SREC   - starting record of new block
C     EREC   - ending record of new block
C     TNAME  - name of table to add
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      NFLDS,SCOL(30),FLEN(30),APOS(30),IMIN(30),I0,I1,I3,
     $             NMHDRW,RETCOD,IMAX(30),IDEF(30),I78,
     $             I,KEY,J,IPOS,IM1,TMPID,I12
      REAL         RMIN(30),RMAX(30),RDEF(30)
      CHARACTER*1  FTYP(30),HDRBUF(78,5),HBEXT(2),BLNK,CBLNK(3),CSTR(3),
     $             TNMBUF(12),TMPBUF(78)
      CHARACTER*80 UCIBF,CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER      STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTX,GETSE,PUTUCI,STRFND,CHRCHR
      EXTERNAL     GETUCI,PUTKWD,CVARAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CSTR /'*','*','*'/
      DATA CBLNK/' ',' ',' '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (80A1)
 2010 FORMAT (A12)
 2020 FORMAT ('END ',A12)
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
      IF (IPRMPT.GE.0) THEN
C       figure out where to put it
        TMPID = IDNO
 10     CONTINUE
          IF (TMPID.GT.1) THEN
            TMPID= TMPID - 1
            CALL GETSE (TMPID,I1,
     O                  SREC,EREC)
          END IF
        IF (SREC.EQ.0 .AND. TMPID.GT.1) GO TO 10
        IF (TMPID.EQ.1) THEN
C         only found run keyword, want new block after srec
          EREC = SREC
        END IF
      END IF
C
C     build table header
      WRITE (UCIBF,2010) TNAME
C     need to start this table after erec
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
      CALL WMSGTX (MESSFL,SCLU,TGRP,
     O             NFLDS,SCOL,FLEN,FTYP,APOS,IMIN,IMAX,IDEF,
     O             RMIN,RMAX,RDEF,
     O             NMHDRW,HDRBUF,RETCOD)
      DO 100 I= 1,NMHDRW
C       need to move header over cuz 0th field not to go on uci file
        J = 75
        CALL CHRCHR (J,HDRBUF(4,I),TMPBUF(1))
        CALL CHRCHR (J,TMPBUF(1),HDRBUF(1,I))
        HDRBUF(76,I) = ' '
        HDRBUF(77,I) = ' '
        HDRBUF(78,I) = ' '
        IF (STRFND(I78,HDRBUF(1,I),I3,CSTR).EQ.0) THEN
C         no comment string found, add one
          IPOS= STRFND(I78,HDRBUF(1,I),I3,CBLNK)
          CALL CHRCHR (I3,CSTR,HDRBUF(IPOS,I))
        END IF
        HBEXT(1)= BLNK
        HBEXT(2)= BLNK
        WRITE (UCIBF,2000)
     1        (HDRBUF(J,I),J=1,78),HBEXT
        CALL PUTUCI (UCIBF,IM1,KEY)
C       get next record number
        CALL GETUCI (IM1,
     M               KEY,
     O               UCIBF)
 100  CONTINUE
C
      IF (IPRMPT.EQ.1) THEN
C       user asked for this block, put one line
        UCIBF = ' '
        CALL PUTUCI (UCIBF,I0,KEY)
C       get next record number
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
      END IF
C     put end table header
      WRITE (UCIBF,2020) TNAME
      CALL PUTUCI (UCIBF,I0,KEY)
C     get next record number
      CALL GETUCI (I0,
     M             KEY,
     O             UCIBF)
      EREC = KEY
C
C     data structure needs to know that this block now exists
      CALL CVARAR (I12,TNAME,I12,TNMBUF)
      CALL PUTKWD (I1,IDNO,TNMBUF,SREC,EREC,
     O             RETCOD)
C
      RETURN
      END
C
C
C
      SUBROUTINE   DELBLK
     I                   (IDNO)
C
C     + + + PURPOSE + + +
C     delete this block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        IDNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDNO   - id number for this block
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I1,SREC,EREC,KEY,SAVKEY
      CHARACTER*80 UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL     GETUCI,DELUCI,GETSE
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      CALL GETSE (IDNO,I1,
     O            SREC,EREC)
      KEY= SREC
      CALL DELUCI (KEY)
      I  = -2
C     delete all remaining records from this block
 10   CONTINUE
        CALL GETUCI (I,
     M               KEY,
     O               UCIBF)
        SAVKEY = KEY
        CALL DELUCI (KEY)
      IF (SAVKEY.NE.EREC) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   COMUCI
     I                   (KEY,
     M                    UCIBF)
C
C     + + + PURPOSE + + +
C     Comment out a uci line
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      KEY
      CHARACTER*80 UCIBF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEY    - record to comment
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I80,I3,IPOS
      CHARACTER*1  IBUF1(80),CMNT(3),CBLNK(3)
C
C     + + + FUNCTIONS + + +
      INTEGER   STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL  CVARAR,STRFND,CHRCHR,CARVAR,COMKEY
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CMNT/'*','*','*'/
      DATA CBLNK/' ',' ',' '/
C
C     + + + END SPECIFICATIONS + + +
C
      I3  = 3
      I80 = 80
      CALL CVARAR (I80,UCIBF,I80,IBUF1)
C
C     want to comment this record
      IF (STRFND(I80,IBUF1,I3,CMNT).EQ.0) THEN
C       no comment string found, add one
        IPOS = STRFND(I80,IBUF1,I3,CBLNK)
        CALL CHRCHR (I3,CMNT,IBUF1(IPOS))
C       change record type in in-memory uci
        CALL COMKEY (KEY)
      END IF
C
      CALL CARVAR (I80,IBUF1,I80,UCIBF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKADJ
     I                   (MESSFL,SCLU,SGRP,
     O                    SRTFLG)
C
C     + + + PURPOSE + + +
C     check to see if nessessary to adjust the length of an
C     80 character string to 78 characters for edit screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        MESSFL,SCLU,SGRP,SRTFLG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - cluster number
C     SGRP   - group number for screen to check
C     SRTFLG - shorten flag, zero if not needed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     NFLDS,SCOL(30),FLEN(30),APOS(30),IMIN(30),
     $            NMHDRW,RETCOD,IMAX(30),IDEF(30)
      REAL        RMIN(30),RMAX(30),RDEF(30)
      CHARACTER*1 FTYP(30),HDRBUF(78,5)
C
C     + + + EXTERNALS + + +
      EXTERNAL    WMSGTX
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WMSGTX (MESSFL,SCLU,SGRP,
     O             NFLDS,SCOL,FLEN,FTYP,APOS,IMIN,IMAX,IDEF,
     O             RMIN,RMAX,RDEF,NMHDRW,HDRBUF,RETCOD)
      IF (FLEN(1).EQ.4 .OR. FLEN(1).EQ.8) THEN
C       need to remove two characters from this string
        SRTFLG = 1
      ELSE
        SRTFLG = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ADJLEN
     I                   (ADJFLG,
     M                    STRG)
C
C     + + + PURPOSE + + +
C     adjust the length of an 80 character string to
C     78 characters for edit screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        ADJFLG
      CHARACTER*80   STRG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ADJFLG - shorten(0) or lengthen(1) flag
C     STRG   - character string to adjust
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J
      CHARACTER*80 TMP
C
C     + + + INPUT FORMATS + + +
1000  FORMAT(2I5)
1010  FORMAT(2I4)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ADJFLG.EQ.0) THEN
C       need to remove two characters from this string
        READ(STRG,1000,ERR=10) I,J
C       numeric, remove 1 and 6
        TMP = STRG(2:5) // STRG(7:80) // '  '
        GO TO 20
 10     CONTINUE
C         not number, remove 9 and 10
          TMP = STRG(1:8) // STRG(11:80) // '  '
 20     CONTINUE
      ELSE IF (ADJFLG.EQ.1) THEN
C       need to add two characters to this string
        READ(STRG,1010,ERR=30) I,J
C       numeric, add at 1 and 6
        TMP = ' ' // STRG(1:4) // ' ' // STRG(5:78)
        GO TO 40
 30     CONTINUE
C         not number, add at 9 and 10
          TMP = STRG(1:8) // '  ' // STRG(9:78)
 40     CONTINUE
      END IF
C
      STRG= TMP
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETDEP
     O                   (DEPSUM)
C
C     + + + PURPOSE + + +
C     get sum of deepfr values to set subjective parm in expert sys
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   DEPSUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DEPSUM - sum of deepfr values from perlnd
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I0,I1,ID,SREC,EREC,KEY
      REAL          RTMP
      CHARACTER*80  UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL      GETSE,GETUCI,RNGCHK
C
C     + + + INPUT FORMATS + + +
 1012 FORMAT(F10.3)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
      DEPSUM = 0.0
      ID = 1012
      CALL GETSE (ID,I1,
     O            SREC,EREC)
      IF (SREC.NE.0) THEN
C       some records exist in this table
        KEY = SREC
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
 50     CONTINUE
          READ (UCIBF(51:60),1012) RTMP
          DEPSUM = DEPSUM + RTMP
          CALL RNGCHK (I1,KEY,
     M                 UCIBF)
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
      SUBROUTINE   ADDOPB
     I                   (IDNO,TNAME,
     M                    SREC,EREC)
C
C     + + + PURPOSE + + +
C     add an operation-type block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        IDNO,SREC,EREC
      CHARACTER*12   TNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDNO   - id number for this block
C     TNAME  - name of table to add
C     SREC   - starting record of new block
C     EREC   - ending record of new block
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,RETCOD,KEY,TMPID,I12,IM2
      CHARACTER*1  TNMBUF(12)
      CHARACTER*80 UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL     GETSE,PUTUCI
      EXTERNAL     GETUCI,PUTKWD,CVARAR
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT (A12)
 2020 FORMAT ('END ',A12)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      IM2= -2
      I12= 12
C
C     figure out where to put it
      TMPID = IDNO
 10   CONTINUE
        IF (TMPID.GT.1) THEN
          TMPID= TMPID - 1
          CALL GETSE (TMPID,I1,
     O                SREC,EREC)
        END IF
      IF (SREC.EQ.0 .AND. TMPID.GT.1) GO TO 10
      IF (TMPID.EQ.1) THEN
C       only found run keyword, want new block after srec
        EREC = SREC
      END IF
C
C     put blank line
      UCIBF = ' '
      KEY   = EREC
      CALL PUTUCI (UCIBF,IM2,KEY)
C     get next record number
      CALL GETUCI (IM2,
     M             KEY,
     O             UCIBF)
C
C     build table header
      WRITE (UCIBF,2010) TNAME
C     need to start this table after erec
      CALL PUTUCI (UCIBF,I0,KEY)
C     get next record number
      CALL GETUCI (I0,
     M             KEY,
     O             UCIBF)
      SREC = KEY
C
C     put end table header
      WRITE (UCIBF,2020) TNAME
      CALL PUTUCI (UCIBF,I0,KEY)
C     get next record number
      CALL GETUCI (I0,
     M             KEY,
     O             UCIBF)
      EREC = KEY
C
C     put blank line
      UCIBF = ' '
      CALL PUTUCI (UCIBF,IM2,KEY)
C
C     data structure needs to know that this block now exists
      CALL CVARAR (I12,TNAME,I12,TNMBUF)
      CALL PUTKWD (I1,IDNO,TNMBUF,SREC,EREC,
     O             RETCOD)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ADDFIL
     I                   (FNAME,
     M                    IFUN)
C
C     + + + PURPOSE + + +
C     add a file to the files block with this unit number
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        IFUN
      CHARACTER*64   FNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FNAME  - name of file
C     IFUN   - unit number to use, if this number is already used returns
C              number actually used
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        I1,I0,BLID,SREC,EREC,KEY,FOUND,IUNIT
      CHARACTER*80   UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL       GETUCI,PUTUCI,GETSE,PREUCI
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (8X,I5,67X)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (8X,I5,3X,A64)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
      BLID = 12
      CALL GETSE (BLID,I1,
     O            SREC,EREC)
C
      IF (SREC.NE.0) THEN
C       look through each record of files block
 20     CONTINUE
          KEY = SREC
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          FOUND = 0
 10       CONTINUE
C           look at starting number for this record
            READ (UCIBF,1000) IUNIT
            IF (IUNIT.EQ.IFUN) THEN
C             this number is already used
              FOUND= 1
            END IF
C           get next record
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
          IF (KEY.NE.EREC .AND. FOUND.EQ.0) GO TO 10
          IF (FOUND.EQ.1) THEN
C           try next unit number
            IFUN = IFUN + 1
          END IF
        IF (FOUND.EQ.1) GO TO 20
C       found a unit number to use
        CALL PREUCI (KEY)
        WRITE (UCIBF,2000) IFUN,FNAME
        CALL PUTUCI (UCIBF,I1,KEY)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GFILNM
     I                   (IFUN,
     O                    FNAME)
C
C     + + + PURPOSE + + +
C     get a file name from the files block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        IFUN
      CHARACTER*64   FNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IFUN   - unit number
C     FNAME  - name of file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        I1,I0,BLID,SREC,EREC,KEY,FOUND,IUNIT
      CHARACTER*80   UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL       GETUCI,GETSE
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (8X,I5,3X,A64)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
      BLID = 12
      CALL GETSE (BLID,I1,
     O            SREC,EREC)
C
      FOUND = 0
      IF (SREC.NE.0) THEN
C       look through each record of files block
        KEY = SREC
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
 10     CONTINUE
C         look at starting number for this record
          READ (UCIBF,1000) IUNIT,FNAME
          IF (IUNIT.EQ.IFUN) THEN
C           found this number
            FOUND= 1
          END IF
C         get next record
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
        IF (KEY.NE.EREC .AND. FOUND.EQ.0) GO TO 10
      END IF
      IF (FOUND.EQ.0) THEN
C       this file was not found
        FNAME = ' '
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PFILNM
     I                   (IFUN,FNAME)
C
C     + + + PURPOSE + + +
C     put a file name to the files block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        IFUN
      CHARACTER*64   FNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IFUN   - unit number
C     FNAME  - name of file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        I1,I0,BLID,SREC,EREC,KEY,FOUND,IUNIT
      CHARACTER*80   UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL       GETUCI,GETSE,REPUCI
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (8X,I5,67X)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (8X,I5,3X,A64)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
      BLID = 12
      CALL GETSE (BLID,I1,
     O            SREC,EREC)
C
      IF (SREC.NE.0) THEN
C       look through each record of files block
        KEY = SREC
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        FOUND = 0
 10     CONTINUE
C         look at starting number for this record
          READ (UCIBF,1000) IUNIT
          IF (IUNIT.EQ.IFUN) THEN
C           found this number
            FOUND= 1
            WRITE (UCIBF,2000) IFUN,FNAME
            CALL REPUCI (KEY,UCIBF)
          END IF
C         get next record
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
        IF (KEY.NE.EREC .AND. FOUND.EQ.0) GO TO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DELFIL
     I                   (IFUN)
C
C     + + + PURPOSE + + +
C     delete a file from the files block with this unit number
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        IFUN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IFUN   - unit number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        I1,I0,BLID,SREC,EREC,KEY,FOUND,IUNIT
      CHARACTER*80   UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL       GETUCI,DELUCI,GETSE
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (8X,I5,67X)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
      BLID = 12
      CALL GETSE (BLID,I1,
     O            SREC,EREC)
C
      IF (SREC.NE.0) THEN
C       look through each record of files block
        KEY = SREC
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        FOUND = 0
 10     CONTINUE
C         look at starting number for this record
          READ (UCIBF,1000) IUNIT
          IF (IUNIT.EQ.IFUN) THEN
C           yes, this is the one we want
            CALL DELUCI (KEY)
            FOUND= 1
          END IF
C         get next record
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
        IF (KEY.NE.EREC .AND. FOUND.EQ.0) GO TO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETML
     I                  (IDTYPE,
     R                   MLNO)
C
C     + + + PURPOSE + + +
C     Get a mass-link id number for a particular operation combination,
C     add it if one does not exist
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     IDTYPE,MLNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDTYPE - mass link type
C              1 - perlnd to bmprac
C              2 - implnd to bmprac
C              3 - bmprac to rchres
C              4 - perlnd to report
C              5 - implnd to report
C              6 - rchres to report (pos)
C              7 - bmprac to report
C              8 - perlnd to rchres
C              9 - implnd to rchres
C             10 - rchres to rchres
C             11 - rchres to report (neg)
C     MLNO   - number corresponding to this mass link type
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,SREC,EREC,KEY,ID,I2,IM1,
     2             MCNT,MLTBID(50),I80,I9,COMKEY,PREEND,
     3             I,TMPKEY,TMPID,RETCOD,CONFIL,IKEEP
      REAL         RAREA
      CHARACTER*1  CMASS(9),IBUF1(80),TNMBUF(12)
      CHARACTER*12 TNAME
      CHARACTER*80 UCIBF,TMPBF
C
C     + + + FUNCTIONS + + +
      INTEGER      STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRFND,PUTUCI,PUTKWD,PREUCI
      EXTERNAL    GETSE,GETUCI,CVARAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CMASS/'M','A','S','S','-','L','I','N','K'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (12X,I8)
 1005 FORMAT (A80)
 1050 FORMAT (28X,F10.3,42X)
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT ('  MASS-LINK    ',I5)
 2020 FORMAT ('  END MASS-LINK',I5)
 2030 FORMAT (A80)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I2 = 2
      I9 = 9
      I80= 80
      IM1= -1
C
      MCNT = 0
      MLNO = 0
C     does the mass-link block exist?
      ID = 11
      CALL GETSE (ID,I1,
     O            SREC,EREC)
      PREEND = EREC
      CALL PREUCI(PREEND)
      IF (SREC.GT.0) THEN
C       at least one mass-link table exists
        KEY = SREC
 10     CONTINUE
C         look at records in in-memory uci for start and end of table
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          CALL CVARAR (I80,UCIBF,I80,IBUF1)
          IF (STRFND(I9,IBUF1(3),I9,CMASS) .GT. 0) THEN
C           found beginning of a mass-link table
            MCNT = MCNT + 1
            READ (UCIBF,1000) ITMP
            MLTBID(MCNT) = ITMP
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
            IF (IDTYPE.EQ.1) THEN
C             1 - perlnd to bmprac
              IF (UCIBF(1:6).EQ.'PERLND' .AND.
     1          UCIBF(44:49).EQ.'BMPRAC') THEN
                MLNO = ITMP
              END IF
            ELSE IF (IDTYPE.EQ.2) THEN
C             2 - implnd to bmprac
              IF (UCIBF(1:6).EQ.'IMPLND' .AND.
     1          UCIBF(44:49).EQ.'BMPRAC') THEN
                MLNO = ITMP
              END IF
            ELSE IF (IDTYPE.EQ.3) THEN
C             3 - bmprac to rchres
              IF (UCIBF(1:6).EQ.'BMPRAC' .AND.
     1          UCIBF(44:49).EQ.'RCHRES') THEN
                MLNO = ITMP
              END IF
            ELSE IF (IDTYPE.EQ.4) THEN
C             4 - perlnd to report
              IF (UCIBF(1:6).EQ.'PERLND' .AND.
     1          UCIBF(44:49).EQ.'REPORT') THEN
                MLNO = ITMP
              END IF
            ELSE IF (IDTYPE.EQ.5) THEN
C             5 - implnd to report
              IF (UCIBF(1:6).EQ.'IMPLND' .AND.
     1          UCIBF(44:49).EQ.'REPORT') THEN
                MLNO = ITMP
              END IF
            ELSE IF (IDTYPE.EQ.6) THEN
C             6 - rchres to report (pos)
              IF (UCIBF(1:6).EQ.'RCHRES' .AND.
     1          UCIBF(44:49).EQ.'REPORT') THEN
                READ(UCIBF,1050) RAREA
                IF (RAREA.GT.-1.0E-6) THEN
                  MLNO = ITMP
                END IF
              END IF
            ELSE IF (IDTYPE.EQ.7) THEN
C             7 - bmprac to report
              IF (UCIBF(1:6).EQ.'BMPRAC' .AND.
     1          UCIBF(44:49).EQ.'REPORT') THEN
                MLNO = ITMP
              END IF
            ELSE IF (IDTYPE.EQ.8) THEN
C             8 - perlnd to rchres
              IF (UCIBF(1:6).EQ.'PERLND' .AND.
     1          UCIBF(44:49).EQ.'RCHRES') THEN
                MLNO = ITMP
              END IF
            ELSE IF (IDTYPE.EQ.9) THEN
C             9 - implnd to rchres
              IF (UCIBF(1:6).EQ.'IMPLND' .AND.
     1          UCIBF(44:49).EQ.'RCHRES') THEN
                MLNO = ITMP
              END IF
            ELSE IF (IDTYPE.EQ.10) THEN
C             10 - rchres to rchres
              IF (UCIBF(1:6).EQ.'RCHRES' .AND.
     1          UCIBF(44:49).EQ.'RCHRES') THEN
                MLNO = ITMP
              END IF
            ELSE IF (IDTYPE.EQ.11) THEN
C             11 - rchres to report (neg)
              IF (UCIBF(1:6).EQ.'RCHRES' .AND.
     1          UCIBF(44:49).EQ.'REPORT') THEN
                READ(UCIBF,1050) RAREA
                IF (RAREA.LT.0.0) THEN
                  MLNO = ITMP
                END IF
              END IF
            END IF
          END IF
C         look ahead one record for end of block
          TMPKEY = KEY
          CALL GETUCI (I0,
     M                 TMPKEY,
     O                 TMPBF)
        IF (TMPKEY.NE.EREC .AND. TMPKEY.NE.PREEND
     1      .AND. MLNO.EQ.0) GO TO 10
C       does not find end of ml block correctly!!!
      ELSE
C       no mass link block exists, add it
        TMPID = ID
 20     CONTINUE
          IF (TMPID.GT.1) THEN
            TMPID= TMPID - 1
            CALL GETSE (TMPID,I1,
     O                  SREC,EREC)
          END IF
        IF (SREC.EQ.0 .AND. TMPID.GT.1) GO TO 20
        IF (TMPID.EQ.1) THEN
C         only found run keyword, want new block after srec
          EREC = SREC
        END IF
C       need to start this table after erec
        KEY = EREC
        UCIBF = 'MASS-LINK'
        CALL PUTUCI (UCIBF,I0,KEY)
C       get next record number
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        SREC = KEY
C       put end table header
        UCIBF = 'END MASS-LINK'
        CALL PUTUCI (UCIBF,I0,KEY)
C       get next record number
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        EREC = KEY
C       data structure needs to know that this block now exists
        TNAME = 'MASS-LINK   '
        CALL CVARAR (I12,TNAME,I12,TNMBUF)
        CALL PUTKWD (I1,ID,TNMBUF,SREC,EREC,
     O               RETCOD)
      END IF
C
      IF (MLNO.EQ.0) THEN
C       need to add a mass-link
        IF (MCNT.EQ.0) THEN
          MLNO = 1
        ELSE
C         find a valid number
 40       CONTINUE
            MLNO = MLNO + 1
            IPROB = 0
            DO 30 I = 1,MCNT
              IF (MLTBID(I).EQ.MLNO) THEN
C               problem, not unique number
                IPROB = 1
              END IF
 30         CONTINUE
          IF (IPROB.NE.0) GOTO 40
        END IF
C       have a valid number
        KEY = SREC
        WRITE(UCIBF,2010) MLNO
        CALL PUTUCI (UCIBF,I0,KEY)
        COMKEY = KEY
C
        IF (IDTYPE.EQ.4 .OR. IDTYPE.EQ.5 .OR. IDTYPE.EQ.6 .OR.
     1      IDTYPE.EQ.7 .OR. IDTYPE.EQ.11) THEN
C         will want to add some text to the mass link
C         try to open cons file
          CONFIL = 91
          OPEN(UNIT=CONFIL,FILE='repcon.inp',STATUS='OLD',ERR=90)
          READ(CONFIL,1000) NCONS
          DO 65 J = 1,NCONS
C           read each line of constituents
            READ(CONFIL,1005) TMPBF
 65       CONTINUE
 91       CONTINUE
            READ(CONFIL,1005,END=99) UCIBF
            IKEEP = 0
            IF (IDTYPE.EQ.4) THEN
C             4 - perlnd to report
              IF (UCIBF(1:6).EQ.'PERLND' .AND.
     1          UCIBF(44:49).EQ.'REPORT') THEN
                IKEEP = 1
              END IF
            ELSE IF (IDTYPE.EQ.5) THEN
C             5 - implnd to report
              IF (UCIBF(1:6).EQ.'IMPLND' .AND.
     1          UCIBF(44:49).EQ.'REPORT') THEN
                IKEEP = 1
              END IF
            ELSE IF (IDTYPE.EQ.6) THEN
C             6 - rchres to report (pos)
              IF (UCIBF(1:6).EQ.'RCHRES' .AND.
     1          UCIBF(44:49).EQ.'REPORT') THEN
                READ(UCIBF,1050) RAREA
                IF (RAREA.GT.-1.0E-6) THEN
                  IKEEP = 1
                END IF
              END IF
            ELSE IF (IDTYPE.EQ.7) THEN
C             7 - bmprac to report
              IF (UCIBF(1:6).EQ.'BMPRAC' .AND.
     1          UCIBF(44:49).EQ.'REPORT') THEN
                IKEEP = 1
              END IF
            ELSE IF (IDTYPE.EQ.11) THEN
C             11 - rchres to report (neg)
              IF (UCIBF(1:6).EQ.'RCHRES' .AND.
     1          UCIBF(44:49).EQ.'REPORT') THEN
                READ(UCIBF,1050) RAREA
                IF (RAREA.LT.0.0) THEN
                  IKEEP = 1
                END IF
              END IF
            END IF
            IF (IKEEP.EQ.1) THEN
C             write this record to uci
              CALL GETUCI (I0,
     M                     KEY,
     O                     TMPBF)
              CALL PUTUCI (UCIBF,I0,KEY)
            END IF
          GOTO 91
 99       CONTINUE
          CLOSE(UNIT=CONFIL)
C         continue here if file does not exist
 90       CONTINUE
        END IF
C
C        IF (IDTYPE.EQ.1 .OR. IDTYPE.EQ.2 .OR. IDTYPE.EQ.3) THEN
C          UCIBF = 'DEFAULT MASS-LINK RECORDS'
C          CALL GETUCI (I0,
C     M                 KEY,
C     O                 TMPBF)
C          CALL PUTUCI (UCIBF,I0,KEY)
C        END IF
C
C       put end mass-link
C       get next record number
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        WRITE(UCIBF,2020) MLNO
        CALL PUTUCI (UCIBF,I0,KEY)
C
C       now put comment headers
        KEY = COMKEY
C       get next record number
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        UCIBF = '<Name>            <Name> x x<-factor->strg ' //
     1          '<Name>                <Name> x x  ***'
        CALL PUTUCI (UCIBF,IM1,KEY)
        UCIBF = '<-Volume-> <-Grp> <-Member-><--Mult-->Tran ' //
     1          '<-Target vols> <-Grp> <-Member->  ***'
        CALL PUTUCI (UCIBF,IM1,KEY)
C
        IF (IDTYPE.EQ.1 .OR. IDTYPE.EQ.2 .OR. IDTYPE.EQ.3) THEN
C         will want to add default mass link for bmprac
          MLNO = -1 * MLNO
        END IF
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DEFML
     I                  (IDTYPE,MLNO)
C
C     + + + PURPOSE + + +
C     Default a mass-link for a bmp operation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     IDTYPE,MLNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDTYPE - mass link type
C              1 - perlnd to bmprac
C              2 - implnd to bmprac
C              3 - bmprac to rchres
C     MLNO   - mass link number to default
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,SREC,EREC,KEY,ID,IM1,
     2             I80,I9,PREEND,COPYID,
     3             TMPKEY,COPYK,COPYE,NEWK,COPYNO
      CHARACTER*1  CMASS(9),IBUF1(80)
      CHARACTER*80 UCIBF,TMPBF
C
C     + + + FUNCTIONS + + +
      INTEGER      STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRFND,PUTUCI,PREUCI
      EXTERNAL    GETSE,GETUCI,CVARAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CMASS/'M','A','S','S','-','L','I','N','K'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (12X,I8)
C
C     + + + END SPECIFICATIONS + + +
C
      IM1= -1
      I0 = 0
      I1 = 1
      I9 = 9
      I80= 80
C
      IF (IDTYPE.EQ.1) THEN
C       defaulting perlnd to bmprac, copy from perlnd to rchres
        COPYID = 8
      ELSE IF (IDTYPE.EQ.2) THEN
C       defaulting implnd to bmprac, copy from implnd to rchres
        COPYID = 9
      ELSE IF (IDTYPE.EQ.3) THEN
C       defaulting bmprac to rchres, simple default
        COPYID = 10
      END IF
C
C     get the mass link id from which to copy
      CALL GETML (COPYID,
     O            COPYNO)
C
      COPYK = 0
      COPYE = 0
      IF (COPYNO.GT.0) THEN
C       locate start of masslink to copy from
C       does the mass-link block exist?
        ID = 11
        CALL GETSE (ID,I1,
     O              SREC,EREC)
        PREEND = EREC
        CALL PREUCI(PREEND)
        IF (SREC.GT.0) THEN
C         at least one mass-link table exists
          KEY = SREC
 10       CONTINUE
C           look at records in in-memory uci for start and end of table
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
            CALL CVARAR (I80,UCIBF,I80,IBUF1)
            IF (STRFND(I9,IBUF1(3),I9,CMASS) .GT. 0) THEN
C             found beginning of a mass-link table
              IF (COPYK.EQ.0) THEN
                READ (UCIBF,1000) ITMP
                IF (ITMP.EQ.COPYNO) THEN
C                 this is the one were looking for
                  COPYK = KEY
                END IF
              ELSE IF (COPYE.EQ.0) THEN
                COPYE = KEY
                CALL PREUCI(COPYE)
                CALL PREUCI(COPYE)
              END IF
            END IF
C           look ahead one record for end of block
            TMPKEY = KEY
            CALL GETUCI (I0,
     M                   TMPKEY,
     O                   TMPBF)
          IF (TMPKEY.NE.EREC .AND. TMPKEY.NE.PREEND) GO TO 10
        END IF
      END IF
C
      IF (COPYK.GT.0) THEN
C       found key to copy from, find key to copy to
        NEWK = 0
        IF (MLNO.GT.0) THEN
C         locate start of new mass link
C         does the mass-link block exist?
          ID = 11
          CALL GETSE (ID,I1,
     O                SREC,EREC)
          PREEND = EREC
          CALL PREUCI(PREEND)
          IF (SREC.GT.0) THEN
C           at least one mass-link table exists
            KEY = SREC
 20         CONTINUE
C             look at records in in-memory uci for start and end of table
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
              CALL CVARAR (I80,UCIBF,I80,IBUF1)
              IF (STRFND(I9,IBUF1(3),I9,CMASS) .GT. 0) THEN
C               found beginning of a mass-link table
                READ (UCIBF,1000) ITMP
                IF (ITMP.EQ.MLNO) THEN
C                 this is the one were looking for
                  NEWK = KEY
                END IF
              END IF
C             look ahead one record for end of block
              TMPKEY = KEY
              CALL GETUCI (I0,
     M                     TMPKEY,
     O                     TMPBF)
            IF (TMPKEY.NE.EREC .AND. TMPKEY.NE.PREEND) GO TO 20
          END IF
        END IF
      END IF
C
      IF (NEWK.GT.0 .AND. COPYK.GT.0) THEN
C       found start of each masslink, go ahead and copy
        CALL GETUCI (IM1,
     M               NEWK,
     O               TMPBF)
        CALL GETUCI (IM1,
     M               NEWK,
     O               TMPBF)
 30     CONTINUE
          CALL GETUCI (I0,
     M                 COPYK,
     O                 UCIBF)
          IF (COPYK.NE.COPYE) THEN
C           modify this record as appropriate
            IF (IDTYPE.EQ.1 .OR. IDTYPE.EQ.2) THEN
              TMPBF = UCIBF(1:43) // 'BMPRAC' // UCIBF(50:80)
              IF (UCIBF(66:71).EQ.'OXIF  ') THEN
                TMPBF(66:71) = 'IOX   '
              ELSE IF (UCIBF(66:71).EQ.'NUIF1 ') THEN
                TMPBF(66:71) = 'IDNUT '
              ELSE IF (UCIBF(66:71).EQ.'PKIF  ') THEN
                TMPBF(66:71) = 'IPLK  '
              END IF
            ELSE IF (IDTYPE.EQ.3) THEN
              TMPBF = 'BMPRAC' // UCIBF(7:80)
            END IF
            CALL PUTUCI (TMPBF,I0,NEWK)
            CALL GETUCI (I0,
     M                   NEWK,
     O                   TMPBF)
          END IF
        IF (COPYK.NE.COPYE) GO TO 30
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DELML
     I                  (MLNO)
C
C     + + + PURPOSE + + +
C     delete a mass-link
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MLNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MLNO   - mass link id number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,SREC,EREC,KEY,ID,TSREC,TEREC,
     2             MCNT,I80,I9,TMPKEY,I3
      CHARACTER*1  CMASS(9),IBUF1(80),CEND(3)
      CHARACTER*80 UCIBF,TMPBF
C
C     + + + FUNCTIONS + + +
      INTEGER      STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRFND
      EXTERNAL    GETSE,GETUCI,CVARAR,DELBLK,DELKWD,DELUCI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CEND/'E','N','D'/
      DATA CMASS/'M','A','S','S','-','L','I','N','K'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (12X,I8)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I3 = 3
      I9 = 9
      I80= 80
C
      MCNT = 0
C     does the mass-link block exist?
      ID = 11
      CALL GETSE (ID,I1,
     O            SREC,EREC)
      IF (SREC.GT.0) THEN
C       at least one mass-link table exists
        KEY = SREC
        TSREC = 0
        TEREC = 0
 10     CONTINUE
C         look at records in in-memory uci for start and end of table
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          CALL CVARAR (I80,UCIBF,I80,IBUF1)
          IF (STRFND(I9,IBUF1(3),I9,CMASS) .GT. 0) THEN
C           found beginning of a mass-link table
            MCNT = MCNT + 1
            READ (UCIBF,1000) ITMP
            IF (ITMP.EQ.MLNO) THEN
C             save where this starts
              TSREC = KEY
            END IF
          END IF
          IF (STRFND(I3,IBUF1(3),I3,CEND) .GT. 0 .AND. TSREC.GT.0) THEN
C           found end of this mass-link
            IF (TEREC.EQ.0) THEN
              TEREC = KEY
            END IF
          END IF
C         look ahead one record for end of block
          TMPKEY = KEY
          CALL GETUCI (I0,
     M                 TMPKEY,
     O                 TMPBF)
        IF (TMPKEY.NE.EREC) GO TO 10
C
        IF (TSREC.GT.0 .AND. TEREC.GT.0) THEN
          IF (MCNT.EQ.1) THEN
C           this is the only one
            CALL DELBLK (ID)
            CALL DELKWD (ID)
          ELSE
C           delete only this one
 20         CONTINUE
              CALL DELUCI (TEREC)
            IF (TEREC.NE.TSREC) GO TO 20
            CALL DELUCI (TEREC)
          END IF
        END IF
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GTINS
     I                  (INIT,IDNO,RORB,
     O                   CTXT,RAREA)
C
C     + + + PURPOSE + + +
C     get an input segment into this reach or bmprac from schematic block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INIT,IDNO,RORB
      REAL          RAREA
      CHARACTER*10  CTXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INIT   - flag specifying first time through (1-first)
C     IDNO   - id number of specific operation desired
C     RORB   - flag indicating reach(1) or bmprac(2)
C     CTXT   - text of input segment
C     RAREA  - area contributing to this reach or bmprac
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,FOUND,I1,BLKID
      CHARACTER*80 UCIBF
C
C     + + + SAVES + + +
      INTEGER      SREC,EREC,IDKEY
      SAVE         SREC,EREC,IDKEY
C
C     + + + EXTERNALS + + +
      EXTERNAL     GETUCI,GETSE
C
C     + + + INPUT FORMATS + + +
 1040 FORMAT (49X,I4,27X)
 1050 FORMAT (28X,F10.3,42X)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
      IF (INIT.EQ.1) THEN
C       first time through for this item
C       get starting and ending keys
        BLKID = 10
        CALL GETSE (BLKID,I1,
     O              SREC,EREC)
        IDKEY = SREC
      END IF
C
C     start here if not first time through for this item
C     find input operations
      CTXT = '       -99'
      FOUND = 0
      IF (SREC.NE.0) THEN
C       this table exists
        CALL GETUCI (I0,
     M               IDKEY,
     O               UCIBF)
 85     CONTINUE
C         look for this reach or bmprac
          IF ((UCIBF(44:49).EQ.'RCHRES'.AND.RORB.EQ.1) .OR.
     1        (UCIBF(44:49).EQ.'BMPRAC'.AND.RORB.EQ.2)) THEN
C           check to see if this is the right r or b
            READ (UCIBF,1040) ITMP
            IF (ITMP.EQ.IDNO) THEN
              IF (UCIBF(1:6).EQ.'PERLND' .OR.
     1            UCIBF(1:6).EQ.'IMPLND') THEN
                CTXT = UCIBF(1:10)
                READ (UCIBF,1050) RAREA
                FOUND= 1
              ELSE IF (UCIBF(1:6).EQ.'RCHRES') THEN
                CTXT = UCIBF(1:10)
                RAREA= 1.0
                FOUND= 1
              ELSE IF (UCIBF(1:6).EQ.'BMPRAC') THEN
                CTXT = UCIBF(1:10)
                READ (UCIBF,1050) RAREA
                FOUND= 1
              END IF
            END IF
          END IF
          IF (FOUND.EQ.0 .AND. IDKEY.NE.EREC) THEN
C           get next record
            CALL GETUCI (I0,
     M                   IDKEY,
     O                   UCIBF)
          END IF
        IF (IDKEY.NE.EREC .AND. FOUND.EQ.0) GO TO 85
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PBMPAR
     I                   (CINOP,INID,RAREA,COUTOP,OUTID,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     put bmp info to uci file in memory schematic and opn seq block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INID,OUTID,RETCOD
      CHARACTER*6   CINOP,COUTOP
      REAL          RAREA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CINOP  - input operation
C     INID   - input operation id
C     RAREA  - area factor
C     COUTOP - output operation
C     OUTID  - output operation id
C     RETCOD - return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,FOUND,I1,BLKID,SREC,EREC,KEY,MLTYPE,MLNO,
     1             IEXIST,OPTYP,OPID,LEN,IDC,ISD
      CHARACTER*1  STR(10)
      CHARACTER*80 UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL     GETUCI,GETSE,REPUCI,PUTUCI,PREUCI,GETML
      EXTERNAL     HAOPSQ,GETNXT,DECCHX
C
C     + + + INPUT FORMATS + + +
 1030 FORMAT (6X,I4,70X)
 1040 FORMAT (49X,I4,27X)
 1050 FORMAT (55X,I5,20X)
C
C     + + + OUTPUT FORMATS + + +
 2060 FORMAT (A6,I4,18X,10A1,5X,A6,I4,2X,I5,20X)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      RETCOD = 0
C
      IF (CINOP.EQ.'BMPRAC' .AND. COUTOP.EQ.'RCHRES') THEN
C       see if we need to add this bmp to opn seq block
        OPTYP= 10
        OPID = 0
        IEXIST = 0
 10     CONTINUE
          CALL GETNXT (OPTYP,
     M                 OPID)
          IF (OPID.EQ.INID) THEN
C           this bmp already exists
            IEXIST = 1
          END IF
        IF (OPID.GT.0) GO TO 10
C
        IF (IEXIST.EQ.1) THEN
C         already in opn seq block
        ELSE
C         new bmp, add to opn seq block
          CALL HAOPSQ (CINOP,INID,COUTOP,OUTID)
          RETCOD = -1
        END IF
      END IF
C
C     get starting and ending keys for schematic block
      BLKID = 10
      CALL GETSE (BLKID,I1,
     O            SREC,EREC)
      KEY = SREC
C
C     find input operations
      FOUND = 0
      IF (SREC.NE.0) THEN
C       this table exists
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
 85     CONTINUE
C         look for this record
          IF (UCIBF(1:6).EQ.CINOP .AND.
     1        UCIBF(44:49).EQ.COUTOP) THEN
C           check to see if these are the right ops
            READ (UCIBF,1030) ITMP1
            READ (UCIBF,1040) ITMP2
            IF (ITMP1.EQ.INID .AND. ITMP2.EQ.OUTID) THEN
C             found the record we're looking for
              IF (RAREA.GT.1.0E-5) THEN
                READ (UCIBF,1050) MLNO
                LEN = 10
                ISD = 6
                IDC = 2
                CALL DECCHX (RAREA,LEN,ISD,IDC,
     O                       STR)
                WRITE (UCIBF,2060) CINOP,INID,STR,COUTOP,OUTID,MLNO
                CALL REPUCI (KEY,UCIBF)
              ELSE
C               the area term is zero
                CALL DELUCI (KEY)
              END IF
              FOUND = 1
            END IF
          END IF
          IF (FOUND.EQ.0) THEN
C           get next record
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
          END IF
        IF (KEY.NE.EREC .AND. FOUND.EQ.0) GO TO 85
C
        IF (FOUND.EQ.0 .AND. RAREA.GT.1.0E-5) THEN
C         this record does not exist, will need to add it
          IF (CINOP.EQ.'PERLND' .AND. COUTOP.EQ.'BMPRAC') THEN
            MLTYPE = 1
          ELSE IF (CINOP.EQ.'IMPLND' .AND. COUTOP.EQ.'BMPRAC') THEN
            MLTYPE = 2
          ELSE IF (CINOP.EQ.'BMPRAC' .AND. COUTOP.EQ.'RCHRES') THEN
            MLTYPE = 3
          ELSE IF (CINOP.EQ.'PERLND' .AND. COUTOP.EQ.'RCHRES') THEN
            MLTYPE = 8
          ELSE IF (CINOP.EQ.'IMPLND' .AND. COUTOP.EQ.'RCHRES') THEN
            MLTYPE = 9
          ELSE IF (CINOP.EQ.'RCHRES' .AND. COUTOP.EQ.'RCHRES') THEN
            MLTYPE = 10
          END IF
          CALL GETML (MLTYPE,
     O                MLNO)
          IF (MLNO.LT.0) THEN
C           default the masslink block for bmp
            MLNO = -1 * MLNO
            CALL DEFML (MLTYPE,MLNO)
          END IF
          LEN = 10
          ISD = 6
          IDC = 2
          CALL DECCHX (RAREA,LEN,ISD,IDC,
     O                 STR)
          WRITE (UCIBF,2060) CINOP,INID,STR,COUTOP,OUTID,MLNO
          CALL PREUCI (EREC)
          CALL PUTUCI (UCIBF,I1,EREC)
        END IF
      ELSE
        RETCOD = 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DELBMP
     I                   (BMPID)
C
C     + + + PURPOSE + + +
C     delete a bmp
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       BMPID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BMPID  - bmp id
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       OPTYP,IVAL
      CHARACTER*8   ITMNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL      HGETI,DELFIL,HDELOP
C
C     + + + END SPECIFICATIONS + + +
C
      OPTYP = 10
C     get associated file unit number english
      ITMNAM = 'BMPUNITE'
      CALL HGETI (ITMNAM,BMPID,
     O            IVAL)
      IF (IVAL.NE.0) THEN
C       delete this file from files block
        CALL DELFIL (IVAL)
      END IF
C     get associated file unit number metric
      ITMNAM = 'BMPUNITM'
      CALL HGETI (ITMNAM,BMPID,
     O            IVAL)
      IF (IVAL.NE.0) THEN
C       delete this file from files block
        CALL DELFIL (IVAL)
      END IF
C     now delete the operation
      CALL HDELOP (OPTYP,BMPID)
C
      RETURN
      END
C
C
C
      SUBROUTINE ADDBMP
     I                 (MESSFL,BMPID)
C
C     + + + PURPOSE + + +
C     add bmp tables
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,BMPID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     BMPID  - bmp id to add
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        OPTYP,IFOUND,IDNO,SREC,EREC,I1,I0,
     1               TCLU,TGRP,KGRP,RETCOD,SNO
      CHARACTER*12   TNAME
      CHARACTER*70   CSTR
      CHARACTER*80   UCIBF
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (I5,75X)

C     + + + OUTPUT FORMATS + + +
 2005 FORMAT (I5,5X,A70)

C     + + + EXTERNALS + + +
      EXTERNAL     ADDOPB, GETSE, ESTADD
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
C     does the bmp block exist yet?
      OPTYP = 10
      ITYPE = (1000 * OPTYP) + 1
      CALL GETSE (ITYPE,I1,
     O            SREC,EREC)
      IF (SREC.GT.0) THEN
C       this block already exists
        write(99,*) 'bmp block already exists'
      ELSE
C       need to add bmp block
        IDNO = 100
        TNAME= 'BMPRAC'
        SREC = 0
        EREC = 0
        CALL ADDOPB (IDNO,TNAME,
     M               SREC,EREC)
C       bmp block has been added
        write(99,*) 'bmp block had been added'
      END IF
C
C     add bmp tables if necessary
      DO 30 I = 1,19
C       go through each of the bmp tables
        TCLU   = 130
        IFOUND = 0
C       check that this table exists
        IDNO = (OPTYP*1000)+ I
        CALL GETSE (IDNO,I1,
     O              SREC,EREC)
        IF (SREC.EQ.0) THEN
C         this table does not exist, add it
          TGRP  = 100 + I
          KGRP  = I
          CALL ESTADD (MESSFL,TCLU,TGRP,KGRP,OPTYP,I1,
     O                 SREC,EREC)
          KEY = SREC
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          IFOUND = KEY
          write(99,*) 'bmp table has been added',i
        ELSE
C         this table already exists
          write(99,*) 'bmp table already exists',i
C         check to see if this record is already in table
          INIT  = 1
 40       CONTINUE
            CALL XTABLE (MESSFL,TCLU,I,I1,INIT,I0,I1,
     O                   KEY,UCIBF,RETCOD)
            IF (RETCOD.EQ.2) THEN
              READ (UCIBF,1000) SNO
              IF (BMPID.EQ.SNO) THEN
C               this record is in the table already
                RETCOD = 10
                IFOUND = 1
              END IF
            END IF
            INIT = 0
          IF (RETCOD.NE.10) GO TO 40
          IF (IFOUND.EQ.0) THEN
            KEY = EREC
            CALL PREUCI (KEY)
          END IF
        END IF
C
        IF (IFOUND.EQ.1) THEN
C         table already existed with this record, do nothing
          write(99,*) 'bmp record already existed'
        ELSE
C         need to build this record
C
          write(99,*) 'building bmp record for bmp ',bmpid
          IF (I.EQ.1) THEN
            CSTR = '    5    5    5    5    5    5    5' //
     1             '    5         1   12'
          ELSE IF (I.EQ.2) THEN
            CSTR = 'BMP N                   1    1    3' //
     1             '         1    1'
          ELSE IF (I.EQ.3) THEN
            CSTR = '    0'
          ELSE IF (I.EQ.4) THEN
            CSTR = '      0.00'
          ELSE IF (I.EQ.5) THEN
            CSTR = '    0'
          ELSE IF (I.EQ.6) THEN
            CSTR = '                 TDS                0.00'
          ELSE IF (I.EQ.7) THEN
            CSTR = '    0'
          ELSE IF (I.EQ.8) THEN
            CSTR = '      0.00'
          ELSE IF (I.EQ.9) THEN
            CSTR = '    0    0    0'
          ELSE IF (I.EQ.10) THEN
            CSTR = '      0.00      0.00      0.00'
          ELSE IF (I.EQ.11) THEN
            CSTR = '    0    0    0    0    0    0    0    0' //
     1             '    0    0    0    0'
          ELSE IF (I.EQ.12) THEN
            CSTR = '               FECAL                0.00' //
     1             '      0.00      0.00      0.00'
          ELSE IF (I.EQ.13) THEN
            CSTR = '    0    0'
          ELSE IF (I.EQ.14) THEN
            CSTR = '      0.00      0.00'
          ELSE IF (I.EQ.15) THEN
            CSTR = '    0    0    0    0    0    0    0    0' //
     1             '    0    0'
          ELSE IF (I.EQ.16) THEN
            CSTR = '      0.00      0.00      0.00      0.00'
          ELSE IF (I.EQ.17) THEN
            CSTR = '      0.00      0.00      0.00      0.00' //
     1             '      0.00      0.00'
          ELSE IF (I.EQ.18) THEN
            CSTR = '    0    0    0    0    0'
          ELSE IF (I.EQ.19) THEN
            CSTR = '      0.00      0.00      0.00      0.00' //
     1             '      0.00'
          END IF
C
          WRITE(UCIBF,2005) BMPID,CSTR
          IF (IFOUND.GT.1) THEN
            CALL REPUCI(IFOUND,UCIBF)
            IFOUND = 0
          ELSE
            CALL PUTUCI(UCIBF,I1,KEY)
            CALL GETUCI (I1,
     M                   KEY,
     O                   UCIBF)
          END IF
C
        END IF
 30   CONTINUE
C
C     need to add nquals tables
      TGRP  = 112
      KGRP  = 12
      CALL ESTADD (MESSFL,TCLU,TGRP,KGRP,OPTYP,I1,
     O             SREC,EREC)
      KEY = SREC
      CALL GETUCI (I0,
     M             KEY,
     O             UCIBF)
      CSTR = '                LEAD                0.00' //
     1       '      0.00      0.00      0.00'
      WRITE(UCIBF,2005) BMPID,CSTR
      CALL REPUCI(KEY,UCIBF)
C
      I = -2
      CALL ESTADD (MESSFL,TCLU,TGRP,KGRP,OPTYP,I,
     O             SREC,EREC)
      KEY = SREC
      CALL GETUCI (I0,
     M             KEY,
     O             UCIBF)
      CSTR = '              COPPER                0.00' //
     1       '      0.00      0.00      0.00'
      WRITE(UCIBF,2005) BMPID,CSTR
      CALL REPUCI(KEY,UCIBF)
C
      RETURN
      END
