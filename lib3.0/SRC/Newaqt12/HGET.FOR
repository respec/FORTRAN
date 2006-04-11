C
C
C
      SUBROUTINE   HGETIA
     I                   (ITMNAM,NUM,IDNO,
     O                    IVAL)
C
C     + + + PURPOSE + + +
C     get an integer item from hspf card image data structure
C     for all operations of one type
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NUM,IVAL(NUM),IDNO(NUM)
      CHARACTER*8   ITMNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITMNAM - name of item to get from data structure
C     NUM    - number of ids to get data for
C     IDNO   - ids to get data for
C     IVAL   - array of integer values retrieved
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + EXTERNALS + + +
      EXTERNAL  HGETI
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,NUM
C       get value from hspf data structure for each operation
        CALL HGETI (ITMNAM,IDNO(I),
     O              IVAL(I))
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   HGETI
     I                  (ITMNAM,IDNO,
     O                   IVAL)
C
C     + + + PURPOSE + + +
C     get one integer item from hspf card image data structure
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IVAL,IDNO
      CHARACTER*8   ITMNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITMNAM - name of item to get from data structure
C     IDNO   - id to get data for
C     IVAL   - integer value retrieved
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       LEN,IDUM
      CHARACTER*80  CTXT
C
C     + + + FUNCTIONS + + +
      INTEGER    CHRINT
C
C     + + + EXTERNALS + + +
      EXTERNAL   HGET,CHRINT
C
C     + + + END SPECIFICATIONS + + +
C
      CALL HGET (ITMNAM,IDNO,IDUM,
     O           LEN,CTXT)
      IVAL = CHRINT(LEN,CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HGETRA
     I                   (ITMNAM,NUM,IDNO,IDUM,
     O                    RVAL)
C
C     + + + PURPOSE + + +
C     get a real item from hspf card image data structure
C     for all operations of one type
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NUM,IDNO(NUM),IDUM
      REAL          RVAL(NUM)
      CHARACTER*8   ITMNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITMNAM - name of item to get from data structure
C     NUM    - number of ids to get data for
C     IDNO   - ids to get data for
C     RVAL   - array of real values retrieved
C     IDUM   - dummy integer value used to pass oper id when needed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + EXTERNALS + + +
      EXTERNAL   HGETR
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,NUM
C       get value from hspf data structure for each operation
        CALL HGETR (ITMNAM,IDNO(I),IDUM,
     O              RVAL(I))
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   HGETR
     I                  (ITMNAM,IDNO,IDUM,
     O                   RVAL)
C
C     + + + PURPOSE + + +
C     get one real item from hspf card image data structure
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IDNO,IDUM
      REAL          RVAL
      CHARACTER*8   ITMNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITMNAM - name of item to get from data structure
C     IDNO   - ids to get data for
C     RVAL   - real value retrieved
C     IDUM   - dummy integer value used to pass oper id when needed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       LEN
      CHARACTER*80  CTXT
C
C     + + + FUNCTIONS + + +
      REAL       CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL   HGET,CHRDEC
C
C     + + + END SPECIFICATIONS + + +
C
C     get value from hspf data structure
      CALL HGET (ITMNAM,IDNO,IDUM,
     O           LEN,CTXT)
      RVAL = CHRDEC(LEN,CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HGETCA
     I                   (ITMNAM,NUM,IDNO,
     O                    CTXT)
C
C     + + + PURPOSE + + +
C     get a character item from hspf card image data structure
C     for all operations of this type
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NUM,IDNO(NUM)
      CHARACTER*8   ITMNAM
      CHARACTER*(*) CTXT(NUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITMNAM - name of item to get from data structure
C     IDNO   - ids to get data for
C     CTXT   - character value retrieved
C     NUM    - number of values to retrieve
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + EXTERNALS + + +
      EXTERNAL   HGETC
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,NUM
C       get value from hspf data structure for each operation
        CALL HGETC (ITMNAM,IDNO(I),
     O              CTXT(I))
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   HGETC
     I                  (ITMNAM,IDNO,
     O                   CTXT)
C
C     + + + PURPOSE + + +
C     get one character item from hspf card image data structure
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IDNO
      CHARACTER*8   ITMNAM
      CHARACTER*(*) CTXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITMNAM - name of item to get from data structure
C     IDNO   - ids to get data for
C     CTXT   - character value retrieved
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       LEN,IDUM
      CHARACTER*80  CTMP
C
C     + + + EXTERNALS + + +
      EXTERNAL   HGET
C
C     + + + END SPECIFICATIONS + + +
C
      write(99,*) 'in hgetc, looking for ',itmnam,idno
C     get value from hspf data structure
      CALL HGET (ITMNAM,IDNO,IDUM,
     O           LEN,CTMP)
      CTXT = CTMP(1:LEN)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HPUTIA
     I                   (ITMNAM,NUM,IDNO,IVAL)
C
C     + + + PURPOSE + + +
C     put an integer item from hspf card image data structure
C     for all operations of one type
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NUM,IVAL(NUM),IDNO(NUM)
      CHARACTER*8   ITMNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITMNAM - name of item to put to data structure
C     NUM    - number of ids to put
C     IDNO   - ids to put data for
C     IVAL   - array of integer values to put
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + EXTERNALS + + +
      EXTERNAL  HPUTI
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,NUM
C       put value to hspf data structure for each operation
        CALL HPUTI (ITMNAM,IDNO(I),IVAL(I))
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   HPUTI
     I                  (ITMNAM,IDNO,IVAL)
C
C     + + + PURPOSE + + +
C     put one integer item to hspf card image data structure
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IVAL,IDNO
      CHARACTER*8   ITMNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITMNAM - name of item to put to data structure
C     IDNO   - id to put data for
C     IVAL   - integer value put to data structure
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       IDUM,ILEN,I,I0,VTYPE
      CHARACTER*1   CSTR1(20)
      CHARACTER*80  CTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL   HPUT,INTCHR,CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      ILEN = 20
      CALL INTCHR (IVAL,ILEN,I0,I,CSTR1)
      CTXT = ' '
      CALL CARVAR (ILEN,CSTR1,ILEN,CTXT)
      VTYPE = 1
      CALL HPUT (ITMNAM,IDNO,IDUM,VTYPE,CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HPUTRA
     I                   (ITMNAM,NUM,IDNO,IDUM,RVAL)
C
C     + + + PURPOSE + + +
C     put a real item from hspf card image data structure
C     for all operations of one type
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NUM,IDNO(NUM),IDUM
      REAL          RVAL(NUM)
      CHARACTER*8   ITMNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITMNAM - name of item to put to data structure
C     NUM    - number of ids to put data for
C     IDNO   - ids to put data for
C     RVAL   - array of real values to put
C     IDUM   - dummy integer value used to pass oper id when needed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + EXTERNALS + + +
      EXTERNAL   HPUTR
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,NUM
C       put value to hspf data structure for each operation
        CALL HPUTR (ITMNAM,IDNO(I),IDUM,RVAL(I))
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   HPUTR
     I                  (ITMNAM,IDNO,IDUM,
     O                   RVAL)
C
C     + + + PURPOSE + + +
C     get one real item from hspf card image data structure
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IDNO,IDUM
      REAL          RVAL
      CHARACTER*8   ITMNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITMNAM - name of item to get from data structure
C     IDNO   - ids to get data for
C     RVAL   - real value to put
C     IDUM   - dummy integer value used to pass oper id when needed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       ILEN,I0,I,VTYPE
      CHARACTER*1   CSTR1(20)
      CHARACTER*80  CTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL   HPUT,CARVAR,DECCHR
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      ILEN = 20
      CALL DECCHR (RVAL,ILEN,I0,I,CSTR1)
      CTXT = ' '
      CALL CARVAR (ILEN,CSTR1,ILEN,CTXT)
C     put value to hspf data structure
      VTYPE = 2
      CALL HPUT (ITMNAM,IDNO,IDUM,VTYPE,CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HPUTCA
     I                   (ITMNAM,NUM,IDNO,CTXT)
C
C     + + + PURPOSE + + +
C     put a character item to hspf card image data structure
C     for all operations of this type
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NUM,IDNO(NUM)
      CHARACTER*8   ITMNAM
      CHARACTER*(*) CTXT(NUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITMNAM - name of item to put to data structure
C     IDNO   - ids to put data for
C     CTXT   - character value to put
C     NUM    - number of values to put
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + EXTERNALS + + +
      EXTERNAL   HPUTC
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,NUM
C       put value to hspf data structure for each operation
        CALL HPUTC (ITMNAM,IDNO(I),CTXT(I))
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   HPUTC
     I                  (ITMNAM,IDNO,CTXT)
C
C     + + + PURPOSE + + +
C     put one character item to hspf card image data structure
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IDNO
      CHARACTER*8   ITMNAM
      CHARACTER*(*) CTXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITMNAM - name of item to put to data structure
C     IDNO   - ids to put data for
C     CTXT   - character value to put
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       IDUM,VTYPE
      CHARACTER*80  CTMP
C
C     + + + EXTERNALS + + +
      EXTERNAL   HPUT
C
C     + + + END SPECIFICATIONS + + +
C
      CTMP = ' '
      CTMP = CTXT
C     put value to hspf data structure
      VTYPE = 3
      CALL HPUT (ITMNAM,IDNO,IDUM,VTYPE,CTMP)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HPUT
     I                 (LITEM,IDNO,IDUM,VTYPE,CTXT)
C
C     + + + PURPOSE + + +
C     put an item to hspf card image data structure
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IDNO,IDUM,VTYPE
      CHARACTER*8   LITEM
      CHARACTER*80  CTXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LITEM  - name of item to put to data structure
C     IDNO   - id number of specific operation desired
C     IDUM   - dummy integer value used to pass oper id when needed
C     VTYPE  - variable type 1-integer,2-real,3-character
C              if -1 drop this type of record
C     CTXT   - text of data item
C
C     + + + INCLUDE FILES + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,FOUND,KEY,ISTART,SEDAT(10),I4,I20,I10,JUST,DSN,
     $             SCLU,SGRP,IFIELD,I,I1,ITMP,TSREC,TEREC,RECCNT,TLEN
      REAL         RTMP
      CHARACTER*1  CSTR1(80)
      CHARACTER*4  MEMNAM,WDID
      CHARACTER*6  OPNAME
      CHARACTER*12 TNAME
      CHARACTER*80 UCIBF,CTMP
C
C     + + + SAVES + + +
      CHARACTER*8  ITMNAM
      SAVE         ITMNAM
      CHARACTER*80 CDEF
      SAVE         CDEF
      INTEGER      OPERNO,LEN,STPOS,SREC,EREC,BLKID,ITAB,ISAV
      SAVE         OPERNO,LEN,STPOS,SREC,EREC,BLKID,ITAB,ISAV
C     CDEF   - character string containing default values
C     OPERNO - operation number (0 if not an operation)
C     LEN    - actual length to write
C     STPOS  - starting position to write
C     SREC   - starting record of this table
C     EREC   - ending record of this table
C     BLKID  - block id (global-2,opnseq-3,operation-100)
C     ITAB   - current operation table number, also used as flag
C     ISAV   - int value to save for future calculations, used for
C              storing ingrp time step for ext targs mfactr
C
C     + + + FUNCTIONS + + +
      INTEGER      CHRINT
      REAL         CHRDEC
C
C     + + + INTRINSICS + + +
      INTRINSIC    FLOAT
C
C     + + + EXTERNALS + + +
      EXTERNAL     HGETLC,GETUCI,GETSE,CVARAR,CHRDEC,DECCHR
      EXTERNAL     CARVAR,INTCHR,CHRINT,ADDBAT,WID2UA
      EXTERNAL     REPUCI,DELUCI,PUTUCI,PREUCI,DELKWD,DELBLK
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (I5,75X)
 1050 FORMAT (10X,I8,4(1X,I2),5X,I8,4(1X,I2))
 1100 FORMAT (33X,I2)
C
C     + + + OUTPUT FORMATS + + +
 2030 FORMAT (2X,'START   ',I8,4(1X,I2),'  END',I8,4(1X,I2))
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  ITMNAM/'        '/
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I4 = 4
      I10= 10
      I20= 20
C
      IF (LITEM.NE.ITMNAM) THEN
C       first time through for this item name,
C       look up this name in data dictionary to find out details
        ITMNAM = LITEM
        CALL HGETLC (ITMNAM,
     O               BLKID,OPERNO,LEN,STPOS,CDEF,ITAB,SGRP,IFIELD)
C       get starting and ending keys
        IF (BLKID.NE.100) THEN
C         not an operation table
          CALL GETSE (BLKID,I1,
     O                SREC,EREC)
        ELSE
C         an operation table
          I = OPERNO*1000+ITAB
          CALL GETSE (I,I1,
     O                SREC,EREC)
        END IF
      END IF
C
C     start here if not first time through for this item
      IF (BLKID.EQ.100) THEN
C       operation table to put
        IF (SREC.NE.0) THEN
C         this table exists
          KEY = SREC
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          FOUND = 0
 10       CONTINUE
C           look at starting number for this record
            write(99,*) 'In HPUT ',ucibf
            READ (UCIBF,1000) ISTART
            IF (ISTART.EQ.IDNO) THEN
C             yes, this is the one we want
C             write desired info
              IF (VTYPE.EQ.1 .OR. VTYPE.EQ.2) THEN
C               integer or real value right justified
                UCIBF(STPOS:STPOS+LEN-1) = CTXT(20-LEN+1:20)
              ELSE IF (VTYPE.EQ.3) THEN
C               character left justified
                UCIBF(STPOS:STPOS+LEN-1) = CTXT(1:LEN)
              END IF
              FOUND= 1
C             replace this record in memory
              CALL REPUCI (KEY,UCIBF)
            END IF
C           get next record
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
          IF (KEY.NE.EREC .AND. FOUND.EQ.0) GO TO 10
        END IF
      ELSE IF (BLKID.EQ.2) THEN
C       put info to global block
        KEY = SREC
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        IF (LEN.EQ.80) THEN
C         put description
          UCIBF = CTXT
C         replace this record in memory
          CALL REPUCI (KEY,UCIBF)
        ELSE IF (LEN.EQ.30) THEN
C         put dates and times
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          READ (UCIBF,1050) SEDAT
          CALL CVARAR(LEN,CTXT,LEN,CSTR1)
          I = 5
          SEDAT(1) = CHRINT(I,CSTR1(1))
          SEDAT(2) = CHRINT(I,CSTR1(6))
          SEDAT(3) = CHRINT(I,CSTR1(11))
          SEDAT(6) = CHRINT(I,CSTR1(16))
          SEDAT(7) = CHRINT(I,CSTR1(21))
          SEDAT(8) = CHRINT(I,CSTR1(26))
          WRITE (UCIBF,2030) SEDAT
          CALL REPUCI (KEY,UCIBF)
        END IF
      ELSE IF (BLKID.EQ.5) THEN
C       external sources, put if evap or prec considered
        IF (VTYPE.EQ.-1) THEN
C         drop this type of record
          IF (SREC.NE.0) THEN
C           this table exists
            KEY = SREC
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
            RECCNT = 0
            IF (KEY.NE.EREC) THEN
C             we're not at end
 82           CONTINUE
C               look for a wdm to reach record
                IF (UCIBF(1:3).EQ.'WDM' .AND.
     1              UCIBF(44:49).EQ.'RCHRES') THEN
C                 drop this record
                  CALL DELUCI (KEY)
                ELSE
C                 count number of records left
                  RECCNT = RECCNT + 1
                END IF
C               get next record
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
              IF (KEY.NE.EREC) GO TO 82
            END IF
            IF (RECCNT.EQ.0) THEN
C             no records left in this block, delete it
              CALL DELBLK (BLKID)
              CALL DELKWD (BLKID)
              SREC = 0
              EREC = 0
            END IF
          END IF
        ELSE
C         add a record for wdm to rchres connection
          IF (SREC.EQ.0) THEN
C           ext sources block does not exist, add it
            SCLU = 53
            SGRP = 150
            TNAME= 'EXT SOURCES '
            CALL ADDBAT (MESSFL,SCLU,SGRP,BLKID,I0,TNAME,
     M                   SREC,EREC)
          END IF
          IF (STPOS.EQ.-1) THEN
C           want a precip record
            UCIBF = 'WDM        PRCP     ENGL                '//
     1              '   RCHRES         EXTNL  PREC           '
          ELSE IF (STPOS.EQ.-2) THEN
C           add a evap record
            UCIBF = 'WDM        EVAP     ENGL                '//
     1              '   RCHRES         EXTNL  POTEV          '
          END IF
C         put target rchres id into record
          UCIBF(50:50+LEN-1) = CTXT(20-LEN+1:20)
C         put wdm data set num into record
          CALL WID2UA (I0,IDNO,
     O                 I,DSN,WDID)
          CALL INTCHR (DSN,I4,I0,I,CSTR1)
          CALL CARVAR (I4,CSTR1,I4,CTMP)
          UCIBF(7:10) = CTMP(1:4)
          UCIBF(1:4)  = WDID(1:4)
          KEY = EREC
          CALL PREUCI (KEY)
          CALL PUTUCI (UCIBF,I1,KEY)
        END IF
      ELSE IF (BLKID.EQ.7) THEN
C       network block
        IF (STPOS.EQ.0) THEN
C         delete old reach to reach connections
          IF (SREC.NE.0) THEN
C           this table exists
            KEY = SREC
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
            IF (KEY.NE.EREC) THEN
C             we're not at end
 62           CONTINUE
C               look for a reach to reach record
                IF (UCIBF(1:6).EQ.'RCHRES' .AND.
     2              UCIBF(44:49).EQ.'RCHRES') THEN
C                 drop this record
                  CALL DELUCI (KEY)
                END IF
C               get next record
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
              IF (KEY.NE.EREC) GO TO 62
            END IF
          ELSE
C           look in schematic block
            ITMP = 10
            CALL GETSE (ITMP,I1,
     O                  TSREC,TEREC)
            IF (TSREC.NE.0) THEN
C             this table exists
              KEY = TSREC
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
              IF (KEY.NE.EREC) THEN
C               we're not at end
 72             CONTINUE
C                 look for a reach to reach record
                  IF (UCIBF(1:6).EQ.'RCHRES' .AND.
     2                UCIBF(44:49).EQ.'RCHRES') THEN
C                   drop this record
                    CALL DELUCI (KEY)
                  END IF
C                 get next record
                  CALL GETUCI (I0,
     M                         KEY,
     O                         UCIBF)
                IF (KEY.NE.EREC) GO TO 72
              END IF
            END IF
          END IF
        ELSE IF (STPOS.EQ.7) THEN
C         put source/target of reach to reach connections
          IF (SREC.NE.0) THEN
C           this table exists
            UCIBF = 'RCHRES     HYDR   ROVOL  1              '//
     1              '   RCHRES         EXTNL  IVOL           '
            UCIBF(STPOS:STPOS+LEN-1) = CTXT(20-LEN+1:20)
C           put target rchres id into record
            CALL INTCHR (IDNO,I4,I0,I,CSTR1)
            CALL CARVAR (I4,CSTR1,I4,CTMP)
            UCIBF(50:53) = CTMP(1:4)
            KEY = EREC
            CALL PREUCI (KEY)
            CALL PUTUCI (UCIBF,I1,KEY)
          ELSE
C           look in schematic block
            ITMP = 10
            CALL GETSE (ITMP,I1,
     O                  TSREC,TEREC)
            IF (TSREC.NE.0) THEN
C             this table exists
              UCIBF = 'RCHRES     HYDR   ROVOL  1              '//
     1                '   RCHRES         EXTNL  IVOL           '
C             put source id in
              UCIBF(STPOS:STPOS+LEN-1) = CTXT(20-LEN+1:20)
C             put target rchres id into record
              CALL INTCHR (IDNO,I4,I0,I,CSTR1)
              CALL CARVAR (I4,CSTR1,I4,CTMP)
              UCIBF(50:53) = CTMP(1:4)
              KEY = EREC
              CALL PREUCI (KEY)
              CALL PUTUCI (UCIBF,I1,KEY)
            END IF
          END IF
        ELSE IF (STPOS.EQ.29) THEN
C         put reaches with this perlnd/implnd as source for land areas
          IF (ITAB.EQ.-1) THEN
            OPNAME= 'PERLND'
            MEMNAM= 'PERO'
          ELSE IF (ITAB.EQ.-2) THEN
            OPNAME= 'IMPLND'
            MEMNAM= 'SURO'
          END IF
          IF (VTYPE.EQ.-1) THEN
C           drop this type of record
            IF (SREC.NE.0) THEN
C             this table exists
              KEY = SREC
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
              IF (KEY.NE.EREC) THEN
C               we're not at end
 42             CONTINUE
C                 look for a perlnd/implnd to reach record
                  IF (UCIBF(1:6).EQ.OPNAME .AND.
     1                UCIBF(19:22).EQ.MEMNAM .AND.
     2                UCIBF(44:49).EQ.'RCHRES') THEN
C                   drop this record
                    CALL DELUCI (KEY)
                  END IF
C                 get next record
                  CALL GETUCI (I0,
     M                         KEY,
     O                         UCIBF)
                IF (KEY.NE.EREC) GO TO 42
              END IF
            ELSE
C             look in schematic block
              ITMP = 10
              CALL GETSE (ITMP,I1,
     O                    TSREC,TEREC)
              IF (TSREC.NE.0) THEN
C               this table exists
                KEY = TSREC
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
                IF (KEY.NE.EREC) THEN
C                 we're not at end
 52               CONTINUE
C                   look for a perlnd/implnd to reach record
                    IF (UCIBF(1:6).EQ.OPNAME .AND.
     1                  UCIBF(19:22).EQ.MEMNAM .AND.
     2                  UCIBF(44:49).EQ.'RCHRES') THEN
C                     drop this record
                      CALL DELUCI (KEY)
                    END IF
C                   get next record
                    CALL GETUCI (I0,
     M                           KEY,
     O                           UCIBF)
                  IF (KEY.NE.EREC) GO TO 52
                END IF
              END IF
            END IF
          ELSE
C           put perlnd/implnd to reach record
            IF (SREC.NE.0) THEN
C             this table exists
              IF (ITAB.EQ.-1) THEN
                UCIBF = 'PERLND     PWATER PERO                  '//
     1                  '   RCHRES         EXTNL  IVOL           '
              ELSE IF (ITAB.EQ.-2) THEN
                UCIBF = 'IMPLND     IWATER SURO                  '//
     1                  '   RCHRES         EXTNL  IVOL           '
              END IF
              CALL CVARAR (I20,CTXT,I20,CSTR1)
              RTMP = CHRDEC(I20,CSTR1)
              IF (RTMP.GT.0.0) THEN
C               we have a connection here
                TLEN = 10
                UCIBF(STPOS:STPOS+TLEN-1) = CTXT(20-TLEN+1:20)
C               put implnd/perlnd id into record
                CALL INTCHR (IDUM,I4,I0,I,CSTR1)
                CALL CARVAR (I4,CSTR1,I4,CTMP)
                UCIBF(7:10) = CTMP(1:4)
C               put rchres id into record
                CALL INTCHR (IDNO,I4,I0,I,CSTR1)
                CALL CARVAR (I4,CSTR1,I4,CTMP)
                UCIBF(50:53) = CTMP(1:4)
                KEY = EREC
                CALL PREUCI (KEY)
                CALL PUTUCI (UCIBF,I1,KEY)
              END IF
            ELSE
C             look in schematic block
              ITMP = 10
              CALL GETSE (ITMP,I1,
     O                    TSREC,TEREC)
              IF (TSREC.NE.0) THEN
C               this table exists
                IF (ITAB.EQ.-1) THEN
                  UCIBF = 'PERLND     PWATER PERO                  '//
     1                    '   RCHRES         EXTNL  IVOL           '
                ELSE IF (ITAB.EQ.-2) THEN
                  UCIBF = 'IMPLND     IWATER SURO                  '//
     1                    '   RCHRES         EXTNL  IVOL           '
                END IF
                CALL CVARAR (I20,CTXT,I20,CSTR1)
                RTMP = CHRDEC(I20,CSTR1)
                IF (RTMP.GT.0.0) THEN
C                 we have a connection here
                  TLEN = 10
                  UCIBF(STPOS:STPOS+TLEN-1) = CTXT(20-TLEN+1:20)
C                 put implnd/perlnd id into record
                  CALL INTCHR (IDUM,I4,I0,I,CSTR1)
                  CALL CARVAR (I4,CSTR1,I4,CTMP)
                  UCIBF(7:10) = CTMP(1:4)
C                 put rchres id into record
                  CALL INTCHR (IDNO,I4,I0,I,CSTR1)
                  CALL CARVAR (I4,CSTR1,I4,CTMP)
                  UCIBF(50:53) = CTMP(1:4)
                  KEY = EREC
                  CALL PREUCI (KEY)
                  CALL PUTUCI (UCIBF,I1,KEY)
                END IF
              END IF
            END IF
          END IF
        END IF
      ELSE IF (BLKID.EQ.8) THEN
C       external targets block
        IF (VTYPE.EQ.-1) THEN
C         drop this type of record
          RECCNT = 0
          IF (SREC.NE.0) THEN
C           this table exists
            KEY = SREC
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
            IF (KEY.NE.EREC) THEN
C             we're not at end
 32           CONTINUE
C               look for a reach to wdm record
                IF (UCIBF(1:6).EQ.'RCHRES' .AND.
     1              UCIBF(44:46).EQ.'WDM') THEN
C                 drop this record
                  CALL DELUCI (KEY)
                ELSE
C                 count this record as a record not dropped
                  RECCNT = RECCNT + 1
                END IF
C               get next record
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
              IF (KEY.NE.EREC) GO TO 32
            END IF
            IF (RECCNT.EQ.0) THEN
C             no records left in this block, delete it
              CALL DELBLK (BLKID)
              CALL DELKWD (BLKID)
              SREC = 0
              EREC = 0
            END IF
          END IF
C         get ingroup time step to use in putting ext targs records
          I = 3
          CALL GETSE (I,I1,
     O                TSREC,TEREC)
          IF (TSREC.NE.0) THEN
C           this block exists to modify
            KEY = TSREC
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
C           get from this line ingrp time step
            READ (UCIBF,1100) ISAV
          END IF
        ELSE
C         put wdm data set for analysis
          CALL CVARAR (I20,CTXT,I20,CSTR1)
          IF (CHRINT(I20,CSTR1).NE.0) THEN
C           we want a rchres to wdm record for this reach
            IF (SREC.EQ.0) THEN
C             ext targets block does not exist, add it
              SCLU = 53
              SGRP = 180
              TNAME= 'EXT TARGETS '
              CALL ADDBAT (MESSFL,SCLU,SGRP,BLKID,I0,TNAME,
     M                     SREC,EREC)
            END IF
            UCIBF = 'RCHRES     HYDR   ROVOL  1 0          SAME WDM'//
     1              '        SFLO     ENGL      REPL   '
            CALL WID2UA (I0,CHRINT(I20,CSTR1),
     O                   I,DSN,WDID)
            UCIBF(44:47) = WDID(1:4)
            JUST = 0
            CALL INTCHR (DSN,I20,JUST,I,CSTR1)
            CALL CARVAR (I20,CSTR1,I20,CTMP)
            UCIBF(STPOS:STPOS+LEN-2) = CTMP(22-LEN:20)
C           put rchres id into record
            JUST= 0
            CALL INTCHR (IDNO,I4,JUST,I,CSTR1)
            CALL CARVAR (I4,CSTR1,I4,CTMP)
            UCIBF(7:10) = CTMP(1:4)
C           build mfactor from ingroup time step
            RTMP= 12.1*(60/FLOAT(ISAV))
            CALL DECCHR (RTMP,I10,JUST,I,CSTR1)
            CALL CARVAR (I10,CSTR1,I10,CTMP)
            UCIBF(29:38) = CTMP(1:10)
C           build record to in-memory uci
            KEY = EREC
            CALL PREUCI (KEY)
            CALL PUTUCI (UCIBF,I1,KEY)
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   HGET
     I                 (LITEM,IDNO,IDUM,
     O                  ILEN,CTXT)
C
C     + + + PURPOSE + + +
C     get an item from hspf card image data structure
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IDNO,ILEN,IDUM
      CHARACTER*8   LITEM
      CHARACTER*80  CTXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LITEM  - name of item to get from data structure
C     IDNO   - id number of specific operation desired
C     IDUM   - dummy integer value used to pass oper id when needed
C     ILEN   - actual length of item
C     CTXT   - text of data item
C
C     + + + INCLUDE FILES + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,FOUND,KEY,ISTART,SEDAT(10),J,IFRST,TLEN,
     $             NFLDS,SCOL(30),FLEN(30),APOS(30),IMIN(30),
     $             NMHDRW,RETCOD,IMAX(30),IDEF(30),DSN,ID,I4,I5,
     $             SGRP,IFIELD,SCLU,I,I1,JUST,ITMP,ITMP2,
     $             TKEY,SSREC,SEREC,
     $             TSREC,TEREC
      REAL         RMIN(30),RMAX(30),RDEF(30),RTMP
      CHARACTER*1  FTYP(30),HDRBUF(78,5),CSTR1(80),CTMP1(80)
      CHARACTER*4  MEMNAM,CMET
      CHARACTER*6  COPER,OPNAME
      CHARACTER*80 UCIBF,RNINFO,CTMP
C
C     + + + SAVES + + +
      CHARACTER*8  ITMNAM
      SAVE         ITMNAM
      CHARACTER*80 CDEF
      SAVE         CDEF
      INTEGER      OPERNO,LEN,STPOS,SREC,EREC,IDKEY,BLKID,ITAB
      SAVE         OPERNO,LEN,STPOS,SREC,EREC,IDKEY,BLKID,ITAB
C     CDEF   - character string containing default values
C     OPERNO - operation number (0 if not an operation)
C     LEN    - actual length to read
C     STPOS  - starting position to read
C     SREC   - starting record of this table
C     EREC   - ending record of this table
C     BLKID  - block id (global-2,opnseq-3,operation-100)
C     IDKEY  - current key for uci get routines
C     ITAB   - current operation table number, also used as flag
C
C     + + + FUNCTIONS + + +
      INTEGER      CHRINT
      REAL         CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL     HGETLC,GETUCI,RNGCHK,GETSE,CVARAR,RTSCHK,CHRINT
      EXTERNAL     WMSGTX,DECCHR,CARVAR,INTCHR,CHRDEC,WUA2ID
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (I5,75X)
 1010 FORMAT (6X,I4,70X)
 1020 FORMAT (6X,I4,39X,I4,27X)
 1040 FORMAT (49X,I4,27X)
 1050 FORMAT (10X,I8,4(1X,I2),5X,I8,4(1X,I2))
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  ITMNAM/'        '/
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I4 = 4
      I5 = 5
C
      IFRST = 0
      IF (LITEM.NE.ITMNAM) THEN
C       first time through for this item name,
C       look up this name in data dictionary to find out details
        ITMNAM = LITEM
        CALL HGETLC (ITMNAM,
     O               BLKID,OPERNO,LEN,STPOS,CDEF,ITAB,SGRP,IFIELD)
        IF (SGRP.NE.0) THEN
C         need to see about getting default values
          IF (OPERNO.GT.0) THEN
C           look up blkid, operno, sgrp, fldno
            SCLU = 120+OPERNO
            CALL WMSGTX (MESSFL,SCLU,SGRP,
     O                   NFLDS,SCOL,FLEN,FTYP,APOS,IMIN,IMAX,IDEF,
     O                   RMIN,RMAX,RDEF,NMHDRW,HDRBUF,RETCOD)
            LEN  = FLEN(IFIELD)
            STPOS= SCOL(IFIELD)
            IF (FTYP(IFIELD).EQ.'R') THEN
C             set default for real field
              JUST = 0
              CALL DECCHR (RDEF(IFIELD),LEN,JUST,I,CSTR1)
              CALL CARVAR (LEN,CSTR1,LEN,CDEF)
            ELSE IF (FTYP(IFIELD).EQ.'I') THEN
C             set default for integer field
              JUST = 0
              CALL INTCHR (IDEF(IFIELD),LEN,JUST,I,CSTR1)
              CALL CARVAR (LEN,CSTR1,LEN,CDEF)
            ELSE IF (FTYP(IFIELD).EQ.'C') THEN
C             no default for character field
              CDEF = ' '
            END IF
          END IF
        ELSE
C         no default to get
          CDEF = ' '
        END IF
C       get starting and ending keys
        IF (BLKID.NE.100) THEN
C         not an operation table
          CALL GETSE (BLKID,I1,
     O                SREC,EREC)
        ELSE
C         an operation table
          I = OPERNO*1000+ITAB
          CALL GETSE (I,I1,
     O                SREC,EREC)
        END IF
        IDKEY = SREC
        IFRST = 1
      END IF
C
C     start here if not first time through for this item
      ILEN = LEN
      IF (BLKID.EQ.100) THEN
C       operation table to read
C       initialize to take default
        CTXT = CDEF
        IF (SREC.NE.0) THEN
C         this table exists
          KEY = SREC
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          FOUND = 0
 10       CONTINUE
C           expand range of operations if needed
            CALL RNGCHK (OPERNO,KEY,
     M                   UCIBF)
C           look at starting number for this record
            READ (UCIBF,1000) ISTART
            IF (ISTART.EQ.IDNO) THEN
C             yes, this is the one we want
C             read desired info
              CTXT = UCIBF(STPOS:STPOS+LEN-1)
              FOUND= 1
            END IF
C           get next record
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
          IF (KEY.NE.EREC .AND. FOUND.EQ.0) GO TO 10
        END IF
      ELSE IF (BLKID.EQ.3) THEN
C       look at operation sequence block
        IF (OPERNO.GT.0) THEN
C         want to return an operation id
          IF (OPERNO.EQ.1) THEN
C           look for perlnd
            COPER = 'PERLND'
          ELSE IF (OPERNO.EQ.2) THEN
C           look for implnd
            COPER = 'IMPLND'
          ELSE IF (OPERNO.EQ.3) THEN
C           look for rchres
            COPER = 'RCHRES'
          ELSE IF (OPERNO.EQ.10) THEN
C           look for bmprac
            COPER = 'BMPRAC'
          ELSE IF (OPERNO.EQ.11) THEN
C           look for report
            COPER = 'REPORT'
          END IF
 20       CONTINUE
            FOUND = 0
            CALL GETUCI (I0,
     M                   IDKEY,
     O                   UCIBF)
            IF (SREC.NE.0 .AND. IDKEY.NE.EREC) THEN
C             this table exists and we're not at end
              IF (UCIBF(7:12).EQ.COPER) THEN
C               we've found an operation of this type
C               return starting number for this record
                CTXT = UCIBF(STPOS:STPOS+LEN-1)
                FOUND= 1
              END IF
            ELSE
C             this table does not exist or we're at end
              CTXT = '  -99'
            END IF
          IF (IDKEY.NE.EREC .AND. FOUND.EQ.0) GO TO 20
          IF (FOUND.EQ.0) THEN
C           no more to find
            CTXT = '  -99'
          END IF
        END IF
      ELSE IF (BLKID.EQ.2) THEN
C       get info from global block
        KEY = SREC
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        RNINFO = UCIBF
C       read dates and times, as user supplied them
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        READ (UCIBF,1050) SEDAT
        IF (LEN.EQ.30) THEN
C         get dates
          JUST= 0
          I   = 5
          CALL INTCHR(SEDAT(1),I,JUST,J,CSTR1(1))
          JUST = 0
          CALL INTCHR(SEDAT(2),I,JUST,J,CSTR1(6))
          JUST = 0
          CALL INTCHR(SEDAT(3),I,JUST,J,CSTR1(11))
          JUST = 0
          CALL INTCHR(SEDAT(6),I,JUST,J,CSTR1(16))
          JUST = 0
          CALL INTCHR(SEDAT(7),I,JUST,J,CSTR1(21))
          JUST = 0
          CALL INTCHR(SEDAT(8),I,JUST,J,CSTR1(26))
          CALL CARVAR(LEN,CSTR1,LEN,CTXT)
        ELSE IF (LEN.EQ.80) THEN
C         get description
          CTXT = RNINFO
        END IF
      ELSE IF (BLKID.EQ.5) THEN
C       external sources, see if evap or prec considered
        CTXT = ' -99'
        IF (SREC.NE.0) THEN
C         this table exists
          KEY = SREC
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          IF (KEY.NE.EREC) THEN
C           some records exist
C           if range of operations here expand
            CALL RTSCHK (KEY,
     M                   UCIBF)
            FOUND = 0
 100        CONTINUE
C             look for this rchres id
              READ (UCIBF,1040) ITMP
              IF (UCIBF(1:3).EQ.'WDM' .AND. ITMP.EQ.IDNO .AND.
     1            UCIBF(44:49).EQ.'RCHRES') THEN
C               yes, this is the one we want
                IF (UCIBF(12:15).EQ.'PRCP') THEN
C                 precip considered
                  IF (CTXT(1:4).EQ.' -99') THEN
                    CTXT = '   1'
C                   precip only so far
                  ELSE IF (CTXT(1:4).EQ.'   2') THEN
C                   precip and evap considered
                    CTXT = '   3'
                  END IF
                ELSE IF (UCIBF(12:15).EQ.'EVAP') THEN
C                 evap considered
                  IF (CTXT(1:4).EQ.' -99') THEN
C                   evap only so far
                    CTXT = '   2'
                  ELSE IF (CTXT(1:4).EQ.'   1') THEN
C                   precip and evap considered
                    CTXT = '   3'
                  END IF
                END IF
              END IF
C             get next record
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
C             if range of operations here expand
              CALL RTSCHK (KEY,
     M                     UCIBF)
            IF (KEY.NE.EREC) GO TO 100
          END IF
        END IF
      ELSE IF (BLKID.EQ.7) THEN
C       network block
        IF (STPOS.EQ.0) THEN
C         find number of reach to reach connections
          FOUND = 0
          IF (SREC.NE.0) THEN
C           this table exists
            KEY = SREC
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
C           if range of operations here expand
            CALL RTSCHK (KEY,
     M                   UCIBF)
 60         CONTINUE
C             look for a reach to reach connection
              IF (UCIBF(1:6).EQ.'RCHRES'
     1            .AND. UCIBF(44:49).EQ.'RCHRES') THEN
C               yes, this is one
                FOUND = FOUND + 1
              END IF
C             get next record
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
C             if range of operations here expand
              CALL RTSCHK (KEY,
     M                     UCIBF)
            IF (KEY.NE.EREC) GO TO 60
          ELSE
C           look in schematic block
            ITMP = 10
            CALL GETSE (ITMP,I1,
     O                  TSREC,TEREC)
            IF (TSREC.NE.0) THEN
C             this table exists
              KEY = TSREC
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
 70           CONTINUE
C               look for reach to reach connection
                IF (UCIBF(1:6).EQ.'RCHRES'
     1              .AND. UCIBF(44:49).EQ.'RCHRES') THEN
C                 yes, this is one
                  FOUND = FOUND + 1
                END IF
C               get next record
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
              IF (KEY.NE.TEREC) GO TO 70
            END IF
          END IF
          JUST = 0
          CALL INTCHR (FOUND,LEN,JUST,I,CTMP1)
          CALL CARVAR (LEN,CTMP1,LEN,CTXT)
        ELSE IF (STPOS.EQ.7 .OR. STPOS.EQ.50) THEN
C         find source/target of reach to reach connections
          IF (SREC.NE.0) THEN
C           this table exists
            CALL GETUCI (I0,
     M                   IDKEY,
     O                   UCIBF)
            FOUND = 0
 80         CONTINUE
C             look for a reach to reach connection
              IF (UCIBF(1:6).EQ.'RCHRES'
     1            .AND. UCIBF(44:49).EQ.'RCHRES') THEN
C               store source/target of this reach to reach conn
                CTXT = UCIBF(STPOS:STPOS+LEN-1)
                FOUND= 1
              END IF
              IF (FOUND.EQ.0) THEN
C               get next record
                CALL GETUCI (I0,
     M                       IDKEY,
     O                       UCIBF)
              END IF
            IF (IDKEY.NE.EREC .AND. FOUND.EQ.0) GO TO 80
          ELSE
C           look in schematic block
            ITMP = 10
            CALL GETSE (ITMP,I1,
     O                  TSREC,TEREC)
            IF (TSREC.NE.0) THEN
C             this table exists
              KEY = TSREC
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
 90           CONTINUE
C               look for reach to reach connection
                FOUND = 1
                IF (UCIBF(1:6).EQ.'RCHRES'
     1              .AND. UCIBF(44:49).EQ.'RCHRES') THEN
C                 found one
                  FOUND = FOUND + 1
                  IF (FOUND.EQ.IDNO) THEN
C                   store source/target of this reach to reach conn
                    CTXT = UCIBF(STPOS:STPOS+LEN-1)
                  END IF
                END IF
C               get next record
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
              IF (KEY.NE.TEREC .AND. FOUND.NE.IDNO) GO TO 90
            END IF
          END IF
        ELSE IF (STPOS.EQ.1) THEN
C         find input operations to this reach
          CTXT = '       -99'
          FOUND = 0
          IF (SREC.NE.0) THEN
C           this table exists
            CALL GETUCI (I0,
     M                   IDKEY,
     O                   UCIBF)
 95         CONTINUE
C             look for this reach
              IF (UCIBF(44:49).EQ.'RCHRES') THEN
C               store input to this reach
                READ (UCIBF,1040) ITMP
                IF (ITMP.EQ.IDNO) THEN
                  CTXT = UCIBF(1:10)
                  FOUND= 1
                END IF
              END IF
              IF (FOUND.EQ.0) THEN
C               get next record
                CALL GETUCI (I0,
     M                       IDKEY,
     O                       UCIBF)
              END IF
            IF (IDKEY.NE.EREC .AND. FOUND.EQ.0) GO TO 95
          END IF
        ELSE IF (STPOS.EQ.29) THEN
C         find reach with perlnd/implnd as source for land areas
          IF (ITAB.EQ.-1) THEN
            OPNAME= 'PERLND'
            MEMNAM= 'PERO'
          ELSE IF (ITAB.EQ.-2) THEN
            OPNAME= 'IMPLND'
            MEMNAM= 'SURO'
          END IF
          IF (SREC.NE.0) THEN
C           this table exists
            CALL GETUCI (I0,
     M                   IDKEY,
     O                   UCIBF)
            FOUND = 0
 40         CONTINUE
C             look for rchres
              IF (UCIBF(1:6).EQ.OPNAME .AND. UCIBF(19:22).EQ.MEMNAM
     1            .AND. UCIBF(44:49).EQ.'RCHRES') THEN
C               read mfactr
                FOUND = 1
                TLEN  = 10
                CTMP  = UCIBF(STPOS:STPOS+TLEN-1)
                CALL CVARAR (TLEN,CTMP,TLEN,CTMP1)
                RTMP  = CHRDEC(TLEN,CTMP1)*12
                JUST  = 0
                CALL DECCHR (RTMP,TLEN,JUST,I,CTMP1)
                CALL CARVAR (TLEN,CTMP1,TLEN,CTMP)
                CTXT  = ' ' // UCIBF(50:53) // ' ' // UCIBF(7:10) //
     1                  CTMP(1:10)
              END IF
              IF (FOUND.EQ.0 .AND. IDKEY.NE.EREC) THEN
C               get next record
                CALL GETUCI (I0,
     M                       IDKEY,
     O                       UCIBF)
              END IF
            IF (FOUND.EQ.0 .AND. IDKEY.NE.EREC) GO TO 40
            IF (FOUND.EQ.0) THEN
C             no more of this type available
              CTXT = '  -99'
            END IF
          ELSE
C           look in schematic block
            ITMP = 10
            CALL GETSE (ITMP,I1,
     O                  TSREC,TEREC)
            IF (TSREC.NE.0) THEN
C             this table exists
              IF (IFRST.EQ.1) THEN
C               first time through
                IDKEY = TSREC
              END IF
              CALL GETUCI (I0,
     M                     IDKEY,
     O                     UCIBF)
              FOUND = 0
 50           CONTINUE
C               look for perlnd/implnd to rchres
                IF (UCIBF(1:6).EQ.OPNAME .AND. UCIBF(19:22).EQ.MEMNAM
     1              .AND. UCIBF(44:49).EQ.'RCHRES') THEN
C                 read mfactr
                  FOUND = 1
                  CTXT  = ' ' // UCIBF(50:53) // ' ' // UCIBF(7:10) //
     1                    UCIBF(29:38)
                END IF
                IF (FOUND.EQ.0 .AND. IDKEY.NE.EREC) THEN
C                 get next record
                  CALL GETUCI (I0,
     M                         IDKEY,
     O                         UCIBF)
                END IF
              IF (FOUND.EQ.0 .AND. IDKEY.NE.EREC) GO TO 50
              IF (FOUND.EQ.0) THEN
C               no more of this type available
                CTXT = '  -99'
              END IF
            END IF
          END IF
        ELSE IF (STPOS.EQ.-1 .OR. STPOS.EQ.-2) THEN
C         look for met segments associated with reaches
          CTXT = '  -99'
          IF (STPOS.EQ.-1) THEN
            CMET = 'PRCP'
          ELSE IF (STPOS.EQ.-2) THEN
            CMET = 'EVAP'
          END IF
          IF (SREC.NE.0) THEN
C           this table exists
            KEY = SREC
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
            IF (KEY.NE.EREC) THEN
C             some records exist
 120          CONTINUE
C               look for this rchres id
                READ (UCIBF,1020) IDUM,ITMP2
                IF (UCIBF(44:49).EQ.'RCHRES' .AND. ITMP2.EQ.IDNO) THEN
C                 found this reach in network block
                  OPNAME = UCIBF(1:6)
C                 now look in external sources block for this land segment
                  ITMP = 5
                  CALL GETSE (ITMP,I1,
     O                        TSREC,TEREC)
                  IF (TSREC.NE.0) THEN
C                   this table exists
                    TKEY = TSREC
                    CALL GETUCI (I0,
     M                           TKEY,
     O                           UCIBF)
 130                CONTINUE
C                     look for this land segment id
                      READ (UCIBF,1040) ITMP
                      IF (UCIBF(1:3).EQ.'WDM' .AND. ITMP.EQ.IDUM .AND.
     1                    UCIBF(44:49).EQ.OPNAME .AND.
     2                    UCIBF(12:15).EQ.CMET) THEN
C                       yes, found this land segment
                        CALL CVARAR (I4,UCIBF(7:10),I4,CSTR1)
                        DSN = CHRINT(I4,CSTR1)
C                       convert dsn to id
                        IF (UCIBF(4:4).EQ.' ') THEN
                          UCIBF(4:4) = '1'
                        END IF
                        CALL WUA2ID (I,DSN,UCIBF(1:4),
     O                               ID)
                        JUST = 0
                        CALL INTCHR (ID,I5,JUST,
     O                               I,CSTR1)
                        CALL CARVAR (I5,CSTR1,I5,CTMP)
                        IF (CTXT(1:5).EQ.'  -99') THEN
C                         only connection so far
                          CTXT(1:5) = CTMP(1:5)
                        ELSE IF (CTXT(1:5).NE.' -99' .AND.
     1                      CTXT(1:5).NE.CTMP(1:5)) THEN
C                         another met segment already found for this rch
                          CTXT = '    0'
                        END IF
                      END IF
C                     get next record
                      CALL GETUCI (I0,
     M                             TKEY,
     O                             UCIBF)
                    IF (TKEY.NE.TEREC) GO TO 130
                  END IF
                END IF
C               get next record
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
              IF (KEY.NE.EREC) GO TO 120
            END IF
          ELSE
C           look in schematic block
            ITMP = 10
            CALL GETSE (ITMP,I1,
     O                  SSREC,SEREC)
            IF (SSREC.NE.0) THEN
C             this table exists
              KEY = SSREC
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
              IF (KEY.NE.SEREC) THEN
C               some records exist
 140            CONTINUE
C                 look for this rchres id
                  READ (UCIBF,1020) IDUM,ITMP2
                  IF (UCIBF(44:49).EQ.'RCHRES' .AND. ITMP2.EQ.IDNO) THEN
C                   found this reach in network block
                    OPNAME = UCIBF(1:6)
C                   now look in external sources block for this land segment
                    ITMP = 5
                    CALL GETSE (ITMP,I1,
     O                          TSREC,TEREC)
                    IF (TSREC.NE.0) THEN
C                     this table exists
                      TKEY = TSREC
                      CALL GETUCI (I0,
     M                             TKEY,
     O                             UCIBF)
 150                  CONTINUE
C                       look for this land segment id
                        READ (UCIBF,1040) ITMP
                        IF (UCIBF(1:3).EQ.'WDM' .AND. ITMP.EQ.IDUM .AND.
     1                      UCIBF(44:49).EQ.OPNAME .AND.
     2                      UCIBF(12:15).EQ.CMET) THEN
C                         yes, found this land segment
                          CALL CVARAR (I4,UCIBF(7:10),I4,CSTR1)
                          DSN = CHRINT(I4,CSTR1)
C                         convert dsn to id
                          IF (UCIBF(4:4).EQ.' ') THEN
                            UCIBF(4:4) = '1'
                          END IF
                          CALL WUA2ID (I,DSN,UCIBF(1:4),
     O                                 ID)
                          JUST = 0
                          CALL INTCHR (ID,I5,JUST,
     O                                 I,CSTR1)
                          CALL CARVAR (I5,CSTR1,I5,CTMP)
                          IF (CTXT(1:5).EQ.'  -99') THEN
C                           only connection so far
                            CTXT(1:5) = CTMP(1:5)
                          ELSE IF (CTXT(1:5).NE.' -99' .AND.
     1                        CTXT(1:5).NE.CTMP(1:5)) THEN
C                           another met segment already found for this rch
                            CTXT = '    0'
                          END IF
                        END IF
C                       get next record
                        CALL GETUCI (I0,
     M                               TKEY,
     O                               UCIBF)
                      IF (TKEY.NE.TEREC) GO TO 150
                    END IF
                  END IF
C                 get next record
                  CALL GETUCI (I0,
     M                         KEY,
     O                         UCIBF)
                IF (KEY.NE.SEREC) GO TO 140
              END IF
            END IF
          END IF
          IF (CTXT(1:4).EQ.' -99' .OR. CTXT(1:4).EQ.'   0') THEN
C           still havent associated this reach with a met segment,
C           check for a direct wdm to reach association
            ITMP = 5
            CALL GETSE (ITMP,I1,
     O                  TSREC,TEREC)
            IF (TSREC.NE.0) THEN
C             this table exists
              TKEY = TSREC
              CALL GETUCI (I0,
     M                     TKEY,
     O                     UCIBF)
 160          CONTINUE
C               look for this rchres id
                READ (UCIBF,1040) ITMP
                IF (UCIBF(1:3).EQ.'WDM' .AND. ITMP.EQ.IDNO .AND.
     1              UCIBF(44:49).EQ.'RCHRES' .AND.
     2              UCIBF(12:15).EQ.CMET) THEN
C                 yes, found this rchres id
                  CALL CVARAR (I4,UCIBF(7:10),I4,CSTR1)
                  DSN = CHRINT(I4,CSTR1)
C                 convert dsn to id
                  IF (UCIBF(4:4).EQ.' ') THEN
                    UCIBF(4:4) = '1'
                  END IF
                  CALL WUA2ID (I,DSN,UCIBF(1:4),
     O                         ID)
                  JUST = 0
                  CALL INTCHR (ID,I5,JUST,
     O                         I,CSTR1)
                  CALL CARVAR (I5,CSTR1,I5,CTXT)
                END IF
C               get next record
                CALL GETUCI (I0,
     M                       TKEY,
     O                       UCIBF)
              IF (TKEY.NE.TEREC) GO TO 160
            END IF
          END IF
        END IF
      ELSE IF (BLKID.EQ.8) THEN
C       external targets block, get wdm data set for analysis
        CTXT = '    0'
        IF (SREC.NE.0) THEN
C         this table exists
          KEY = SREC
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          IF (KEY.NE.EREC) THEN
C           some records exist
            FOUND = 0
 30         CONTINUE
C             look for this rchres id
              READ (UCIBF,1010) ITMP
              IF (UCIBF(1:6).EQ.'RCHRES' .AND. ITMP.EQ.IDNO .AND.
     1            UCIBF(44:46).EQ.'WDM') THEN
C               yes, this is the one we want
C               read wdm data set number
                CALL CVARAR (I4,UCIBF(STPOS:STPOS+3),I4,CSTR1)
                DSN = CHRINT(I4,CSTR1)
C               convert dsn to id
                IF (UCIBF(47:47).EQ.' ') THEN
                  UCIBF(47:47) = '1'
                END IF
                CALL WUA2ID (I,DSN,UCIBF(44:47),
     O                       ID)
                JUST = 0
                CALL INTCHR (ID,I5,JUST,
     O                       I,CSTR1)
                CALL CARVAR (I5,CSTR1,I5,CTXT)
                FOUND= 1
              END IF
C             get next record
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
            IF (KEY.NE.EREC .AND. FOUND.EQ.0) GO TO 30
          END IF
        END IF
      ELSE IF (BLKID.EQ.10) THEN
C       schematic block
        IF (STPOS.EQ.1) THEN
C         find input operations to this reach
          CTXT = '       -99'
          FOUND = 0
          IF (SREC.NE.0) THEN
C           this table exists
            CALL GETUCI (I0,
     M                   IDKEY,
     O                   UCIBF)
 85         CONTINUE
C             look for this reach
              IF (UCIBF(44:49).EQ.'RCHRES') THEN
C               store input to this reach
                READ (UCIBF,1040) ITMP
                IF (ITMP.EQ.IDNO) THEN
                  CTXT = UCIBF(1:10)
                  FOUND= 1
                END IF
              END IF
              IF (FOUND.EQ.0) THEN
C               get next record
                CALL GETUCI (I0,
     M                       IDKEY,
     O                       UCIBF)
              END IF
            IF (IDKEY.NE.EREC .AND. FOUND.EQ.0) GO TO 85
          END IF
        END IF
      ELSE
C       fill in other types of blocks here
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   RTSCHK
     I                   (KEY,
     M                    UCIBF)
C
C     + + + PURPOSE + + +
C     check to see if this time series record specifies a range of
C     operations, if so send back one record for each operation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        KEY
      CHARACTER*80   UCIBF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UCIBF  - record read from in-memory uci
C     KEY    - key corresponding to this record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,SNO,ENO,TMPKEY,OPID,COUNT,OPERNO
      CHARACTER*4  CENO,BLNK4
      CHARACTER*80 TMPBF
C
C     + + + EXTERNALS + + +
      EXTERNAL     REPUCI,GETNXT,PUTUCI,GETUCI
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (49X,2I4,23X)
 1010 FORMAT (53X,A4,23X)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (I4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      BLNK4 = '    '
C
C     check to see if ending number has been entered in range
      READ (UCIBF,1010) CENO
      IF (CENO.NE.BLNK4) THEN
C       something in end of range field, check it
        READ (UCIBF,1000) SNO,ENO
        IF (ENO.NE.SNO .AND. (UCIBF(44:49).EQ.'PERLND' .OR.
     1    UCIBF(44:49).EQ.'IMPLND' .OR. UCIBF(44:49).EQ.'RCHRES')) THEN
C         need to expand out this range
          UCIBF(54:57) = BLNK4
C         look for operation of this type
          IF (UCIBF(44:49).EQ.'PERLND') THEN
            OPERNO = 1
          ELSE IF (UCIBF(44:49).EQ.'IMPLND') THEN
            OPERNO = 2
          ELSE IF (UCIBF(44:49).EQ.'RCHRES') THEN
            OPERNO = 3
          END IF
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
              TMPBF(50:53) = CENO
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
      SUBROUTINE   HGETLC
     I                   (ITMNAM,
     O                    BLKID,OPERNO,LEN,STPOS,CDEF,ITAB,SGRP,IFIELD)
C
C     + + + PURPOSE + + +
C     get location of an item in the hspf card image data structure
C     and vital details from the data dictionary
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       BLKID,OPERNO,LEN,STPOS,ITAB,SGRP,IFIELD
      CHARACTER*8   ITMNAM
      CHARACTER*80  CDEF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITMNAM - item name to find in data dictionary
C     BLKID  - block id (global-2,opnseq-3,operation-100)
C     LEN    - actual length to read
C     STPOS  - starting position to read
C     OPERNO - operation number (0 if not an operation)
C     CDEF   - character string containing default values
C     ITAB   - table number for this operation
C     SGRP   - group number for default values, 0 if none available
C     IFIELD - field number for variable desired
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ITMNAM.EQ.'REPUNIT ') THEN
C       getting report file unit numbers
        BLKID = 100
        OPERNO= 11
        ITAB  = 1
        LEN   = 5
        STPOS = 11
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'BMPUNITE') THEN
C       getting bmp file unit numbers english units
        BLKID = 100
        OPERNO= 10
        ITAB  = 2
        LEN   = 5
        STPOS = 61
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'BMPUNITM') THEN
C       getting bmp file unit numbers metric units
        BLKID = 100
        OPERNO= 10
        ITAB  = 2
        LEN   = 5
        STPOS = 66
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'RCHID   ') THEN
C       getting rchres ids
        BLKID = 3
        OPERNO= 3
        LEN   = 5
        STPOS = 16
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'PERID   ') THEN
C       getting perlnd ids
        BLKID = 3
        OPERNO= 1
        LEN   = 5
        STPOS = 16
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'IMPID   ') THEN
C       getting implnd ids
        BLKID = 3
        OPERNO= 2
        LEN   = 5
        STPOS = 16
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'BMPID   ') THEN
C       getting bmp ids
        BLKID = 3
        OPERNO= 10
        LEN   = 5
        STPOS = 16
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'RCHNAMES') THEN
C       getting rchres names
        BLKID = 100
        OPERNO= 3
        ITAB  = 3
        LEN   = 8
        STPOS = 11
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'RCHDESC ') THEN
C       getting full rchres description
        BLKID = 100
        OPERNO= 3
        ITAB  = 3
        LEN   = 20
        STPOS = 11
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'IMPDESC ') THEN
C       getting full implnd description
        BLKID = 100
        OPERNO= 2
        ITAB  = 3
        LEN   = 20
        STPOS = 11
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'PERDESC ') THEN
C       getting full perlnd description
        BLKID = 100
        OPERNO= 1
        ITAB  = 3
        LEN   = 20
        STPOS = 11
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'BMPDESC ') THEN
C       getting full bmprac description
        BLKID = 100
        OPERNO= 10
        ITAB  = 2
        LEN   = 20
        STPOS = 11
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'RCHLEN  ') THEN
C       getting rchres lengths
        BLKID = 100
        OPERNO= 3
        ITAB  = 5
        LEN   = 10
        STPOS = 21
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'RCHSLO  ') THEN
C       getting rchres slopes
        BLKID = 100
        OPERNO= 3
        ITAB  = 5
        LEN   = 10
        STPOS = 31
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'RCHVOL  ') THEN
C       getting rchres initial volumes
        BLKID = 100
        OPERNO= 3
        ITAB  = 7
        LEN   = 10
        STPOS = 11
        SGRP  = 107
        IFIELD= 2
      ELSE IF (ITMNAM.EQ.'PERLUTYP') THEN
C       getting perlnd land use types
        BLKID = 100
        OPERNO= 1
        ITAB  = 3
        LEN   = 20
        STPOS = 11
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'UCIDESCR') THEN
C       getting uci file description from global block
        BLKID = 2
        LEN   = 80
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'SEDATES ') THEN
C       getting start and end dates from global block
        BLKID = 2
        LEN   = 30
        SGRP  = 0
      ELSE IF (ITMNAM.EQ.'ANAWDM  ') THEN
C       getting output wdm data set numbers from ext-targets block
        BLKID = 8
        LEN   = 5
        SGRP  = 0
        STPOS = 50
      ELSE IF (ITMNAM.EQ.'LANDAC  ') THEN
C       for each perlnd, fill array of land acreage to each reach
        BLKID = 7
        LEN   = 20
C       use itab to indicate perlnd or implnd desired
        ITAB  = -1
        SGRP  = 0
        STPOS = 29
      ELSE IF (ITMNAM.EQ.'IMPACR  ') THEN
C       for each implnd, fill array of land acreage to each reach
        BLKID = 7
        LEN   = 20
C       use itab to indicate perlnd or implnd desired
        ITAB  = -2
        SGRP  = 0
        STPOS = 29
      ELSE IF (ITMNAM.EQ.'NUMR2R  ') THEN
C       find number of reach to reach connections
        BLKID = 7
        LEN   = 5
        SGRP  = 0
        STPOS = 0
      ELSE IF (ITMNAM.EQ.'R2RSRC  ') THEN
C       find source of reach to reach connections
        BLKID = 7
        LEN   = 4
        SGRP  = 0
        STPOS = 7
      ELSE IF (ITMNAM.EQ.'R2RTAR  ') THEN
C       find target of reach to reach connections
        BLKID = 7
        LEN   = 4
        SGRP  = 0
        STPOS = 50
      ELSE IF (ITMNAM.EQ.'INOPERS ') THEN
C       find operations input to this reach in the schematic block
        BLKID = 10
        LEN   = 10
        SGRP  = 0
        STPOS = 1
      ELSE IF (ITMNAM.EQ.'INOPERN ') THEN
C       find operations input to this reach in the network block
        BLKID = 7
        LEN   = 10
        SGRP  = 0
        STPOS = 1
      ELSE IF (ITMNAM.EQ.'PRECEVAP') THEN
C       find out if prec or evap to be considered
        BLKID = 5
        LEN   = 4
        SGRP  = 0
        STPOS = 0
      ELSE IF (ITMNAM.EQ.'PREC4RCH') THEN
C       find prcp segment associated with a reach inferred
        BLKID = 7
        LEN   = 5
        SGRP  = 0
        STPOS = -1
      ELSE IF (ITMNAM.EQ.'EVAP4RCH') THEN
C       find evap segment associated with a reach inferred
        BLKID = 7
        LEN   = 5
        SGRP  = 0
        STPOS = -2
      ELSE IF (ITMNAM.EQ.'WDMPREC ') THEN
C       find prcp segment associated with a reach direct in ext sources
        BLKID = 5
        LEN   = 4
        SGRP  = 0
        STPOS = -1
      ELSE IF (ITMNAM.EQ.'WDMEVAP ') THEN
C       find evap segment associated with a reach direct in ext sources
        BLKID = 5
        LEN   = 4
        SGRP  = 0
        STPOS = -2
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE    HDELOP
     I                    (OPERNO,IDNO)
C
C     + + + PURPOSE + + +
C     delete an operation id from hspf data structure
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      OPERNO,IDNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPERNO - operation to which this id belongs (1-per,2-imp,3-rch)
C     IDNO   - id number to delete
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       LEN,I8,I0,I,BLKID,I4,I3,SREC,EREC
      CHARACTER*1   CSTR1(20)
      CHARACTER*12  CSTR12
      CHARACTER*20  CSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL      CVARAR,INTCHR,HDEL,DELOPN,GETNXT,GETSE
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I3 = 3
      I4 = 4
      I8 = 8
C
C     eliminate from operation sequence block
      IF (OPERNO.EQ.3) THEN
C       delete a rchres
        CSTR = 'RCHRES              '
      ELSE IF (OPERNO.EQ.10) THEN
C       delete a bmprac
        CSTR = 'BMPRAC              '
      ELSE IF (OPERNO.EQ.11) THEN
C       delete a report
        CSTR = 'REPORT              '
      END IF
      LEN  = 14
      CALL CVARAR (LEN,CSTR,LEN,CSTR1)
      CALL INTCHR (IDNO,I8,I0,I,CSTR1(7))
      BLKID = 3
      CALL HDEL (BLKID,LEN,CSTR1)
      CALL DELOPN (OPERNO,IDNO)
C
C     eliminate from each operation table
      CSTR = '                    '
      LEN  = 5
      CALL INTCHR (IDNO,LEN,I0,I,CSTR1(1))
      BLKID = OPERNO * 100
      CALL HDEL (BLKID,LEN,CSTR1)
C
C     eliminate from time series records
      IF (OPERNO.EQ.3) THEN
C       delete a rchres
        CSTR = 'RCHRES              '
      ELSE IF (OPERNO.EQ.10) THEN
C       delete a bmprac
        CSTR = 'BMPRAC              '
      ELSE IF (OPERNO.EQ.11) THEN
C       delete a report
        CSTR = 'REPORT              '
      END IF
      LEN  = 10
      CALL CVARAR (LEN,CSTR,LEN,CSTR1)
      CALL INTCHR (IDNO,I4,I0,I,CSTR1(7))
      BLKID = 5
      CALL HDEL (BLKID,LEN,CSTR1)
      BLKID = 7
      CALL HDEL (BLKID,LEN,CSTR1)
      BLKID = 8
      CALL HDEL (BLKID,LEN,CSTR1)
      BLKID = 10
      CALL HDEL (BLKID,LEN,CSTR1)
C
C     eliminate from special actions
      IF (OPERNO.EQ.3) THEN
C       delete a rchres
        CSTR = 'RCHRES              '
      ELSE IF (OPERNO.EQ.10) THEN
C       delete a bmprac
        CSTR = 'BMPRAC              '
      ELSE IF (OPERNO.EQ.11) THEN
C       delete a report
        CSTR = 'REPORT              '
      END IF
      LEN  = 9
      CALL CVARAR (LEN,CSTR,LEN,CSTR1)
      CALL INTCHR (IDNO,I3,I0,I,CSTR1(7))
      BLKID = 9
      CALL HDEL (BLKID,LEN,CSTR1)
C
C     do any more operations of this type exist
      I = 0
      CALL GETNXT (OPERNO,
     M             I)
      IF (I.EQ.0) THEN
C       this was the last one
        CALL GETSEO (OPERNO,
     O               SREC,EREC)
 10     CONTINUE
          CALL DELUCI (EREC)
        IF (EREC.NE.SREC) GO TO 10
        CALL DELUCI (EREC)
C       eliminate information about this keyword
        I = 100
        CSTR12 = CSTR
        CALL DELKWO (I,CSTR12)
        CALL DELKWD (OPERNO*1000)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE    HDEL
     I                  (BLKID,LEN,CSTR1)
C
C     + + + PURPOSE + + +
C     delete an operation id from one block of the hspf data structure
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      BLKID,LEN
      CHARACTER*1  CSTR1(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BLKID  - block to search
C     LEN    - length of string
C     CSTR1  - character string to search for
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I1,SREC,EREC,KEY,I0,I
      CHARACTER*20  CSTR
      CHARACTER*80  UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL      GETSE,CARVAR,DELUCI,GETUCI,GETSEO
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
      IF (BLKID.LT.100) THEN
C       find starting and ending records for this block
        CALL GETSE (BLKID,I1,
     O              SREC,EREC)
      ELSE IF (BLKID.EQ.300) THEN
C       find starting and ending records for this operation type block
        I = 3
        CALL GETSEO (I,
     O               SREC,EREC)
      ELSE IF (BLKID.EQ.1100) THEN
C       find starting and ending records for this operation type block
        I = 11
        CALL GETSEO (I,
     O               SREC,EREC)
      ELSE IF (BLKID.EQ.1000) THEN
C       find starting and ending records for this operation type block
        I = 10
        CALL GETSEO (I,
     O               SREC,EREC)
      END IF
C
      IF (SREC.NE.0) THEN
C       this block exists
        CSTR = '                    '
C       convert to character string for searching
        CALL CARVAR (LEN,CSTR1,LEN,CSTR)
        KEY = SREC
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
 10     CONTINUE
          IF (BLKID.EQ.3) THEN
C           in operation sequence block
            IF (UCIBF(7:7+LEN-1).EQ.CSTR(1:LEN)) THEN
C             delete this record
              CALL DELUCI (KEY)
            END IF
          ELSE IF (BLKID.GT.99) THEN
C           in operation type blocks
            IF (UCIBF(1:LEN).EQ.CSTR(1:LEN) .AND.
     1          (UCIBF(6:6+LEN-1).EQ.CSTR(1:LEN) .OR.
     1           UCIBF(6:6+LEN-1).EQ.'     ')) THEN
C             delete this record
              CALL DELUCI (KEY)
            END IF
          ELSE IF (BLKID.EQ.5) THEN
C           in external sources block
            IF (UCIBF(44:44+LEN-1).EQ.CSTR(1:LEN) .AND.
     1          (UCIBF(54:57).EQ.CSTR(7:10) .OR.
     2           UCIBF(54:57).EQ.'    ')) THEN
C             delete this record
              CALL DELUCI (KEY)
            END IF
          ELSE IF (BLKID.EQ.7 .OR. BLKID.EQ.10) THEN
C           in network block or schematic block
            IF ((UCIBF(44:44+LEN-1).EQ.CSTR(1:LEN) .AND.
     1          (UCIBF(54:57).EQ.CSTR(7:10) .OR.
     2           UCIBF(54:57).EQ.'    ')) .OR.
     3           UCIBF(1:LEN).EQ.CSTR(1:LEN)) THEN
C             delete this record
              CALL DELUCI (KEY)
            END IF
          ELSE IF (BLKID.EQ.8) THEN
C           in external targets block
            IF (UCIBF(1:LEN).EQ.CSTR(1:LEN)) THEN
C             delete this record
              CALL DELUCI (KEY)
            END IF
          ELSE IF (BLKID.EQ.9) THEN
C           in spec actions block
            IF (UCIBF(3:3+LEN-1).EQ.CSTR(1:LEN) .AND.
     1          (UCIBF(11:13).EQ.CSTR(7:9) .OR.
     1           UCIBF(10:13).EQ.'    ')) THEN
C             delete this record
              CALL DELUCI (KEY)
            END IF
          END IF
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
        IF (KEY.NE.EREC) GO TO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE    HDELFT
     I                    (IDNO,NTH)
C
C     + + + PURPOSE + + +
C     delete an ftable for a reach
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      IDNO,NTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDNO   - id number of operation
C     NTH    - which occurance of ftable to delete
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I1,BLKID,SREC,EREC,KEY,
     1              I80,I0,IM1,I3,FTABNO,FOUND,
     2              TMPKEY,MOREFG,ICNT
      CHARACTER*1   IBUF1(80),CEND(3)
      CHARACTER*80  UCIBF
C
C     + + + FUNCTIONS + + +
      INTEGER       CHRINT,STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETSE,CVARAR,GETUCI,CHRINT,STRFND,DELUCI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CEND/'E','N','D'/
C
C     + + + END SPECIFICATIONS + + +
C
      IM1= -1
      I0 = 0
      I1 = 1
      I3 = 3
      I80= 80
C
C     find ftables block
      BLKID = 4
C     find starting and ending records for this block
      CALL GETSE (BLKID,I1,
     O            SREC,EREC)
C
      ICNT = 0
      IF (SREC.NE.0) THEN
C       this block exists
        KEY = SREC
 10     CONTINUE
C         look at records in in-memory uci for start and end of table
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          CALL CVARAR (I80,UCIBF,I80,IBUF1)
C         look at ftable number
          FTABNO = CHRINT (I3,IBUF1(13))
          FOUND  = 0
          IF (FTABNO.EQ.IDNO .AND. UCIBF(3:5).NE.'END') THEN
C           found the right number
            ICNT = ICNT + 1
            IF (ICNT.EQ.NTH) THEN
C             this is the one we want to delete
              FOUND = 1
              CALL DELUCI (KEY)
 30           CONTINUE
                CALL GETUCI (IM1,
     M                       KEY,
     O                       UCIBF)
                CALL DELUCI (KEY)
                CALL CVARAR (I80,UCIBF,I80,IBUF1)
                IF (STRFND(I3,IBUF1(3),I3,CEND) .GT. 0) THEN
C                 found end of this ftable
                  MOREFG = 0
                ELSE
                  MOREFG = 1
                END IF
              IF (MOREFG.EQ.1) GO TO 30
            END IF
          END IF
C         look ahead one record for end of block
          TMPKEY = KEY
          CALL GETUCI (I0,
     M                 TMPKEY,
     O                 UCIBF)
        IF (TMPKEY.NE.EREC .AND. FOUND.EQ.0) GO TO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE    HADDOP
     I                    (OPERNO,IDNO,DOWNST,UPST,LIKEOP)
C
C     + + + PURPOSE + + +
C     add an operation to hspf data structure
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      OPERNO,IDNO,DOWNST(7),UPST(7),LIKEOP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPERNO - operation type (3-reach)
C     IDNO   - id number of new operation
C     DOWNST - array of downstream reaches
C     UPST   - array of upstream reaches
C     LIKEOP - operation from which to take defaults for new operation
C
C     + + + EXTERNALS + + +
      EXTERNAL    HAROPS,HADD
C
C     + + + END SPECIFICATIONS + + +
C
C     add this operation to the operation sequence block
      CALL HAROPS (OPERNO,IDNO,DOWNST,UPST)
C
C     add this operation to all operation tables of this type
      CALL HADD (OPERNO,IDNO,LIKEOP)
C
      RETURN
      END
C
C
C
      SUBROUTINE    HAROPS
     I                    (OPERNO,IDNO,DOWNST,UPST)
C
C     + + + PURPOSE + + +
C     add a reach to hspf operation sequence block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      OPERNO,IDNO,DOWNST(7),UPST(7)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPERNO - operation type (3-reach)
C     IDNO   - id number of new operation
C     DOWNST - array of downstream reaches
C     UPST   - array of upstream reaches
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I1,BLKID,SREC,EREC,UPOS,K,LEN,I,DONE,KEY,DPOS,I8,I0
      CHARACTER*1   CSTR1(80)
      CHARACTER*20  CSTR
      CHARACTER*80  UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETSE,CVARAR,INTCHR,CARVAR,GETUCI,PUTUCI
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I8 = 8
C
C     add entry in opn seq block
      BLKID = 3
C     find starting and ending records for this block
      CALL GETSE (BLKID,I1,
     O            SREC,EREC)
C
      IF (SREC.NE.0) THEN
C       this block exists
        UPOS = 0
        DO 30 K = 1,7
          IF (UPST(K).GT.0) THEN
C           look for upstream reaches
            IF (OPERNO.EQ.3) THEN
C             delete a rchres
              CSTR = 'RCHRES              '
            END IF
            LEN  = 14
            CALL CVARAR (LEN,CSTR,LEN,CSTR1)
            CALL INTCHR (UPST(K),I8,I0,I,CSTR1(7))
C           convert to character string for searching
            CALL CARVAR (LEN,CSTR1,LEN,CSTR)
            I = 0
            DONE = 0
            KEY = SREC
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
 40         CONTINUE
              I = I + 1
              IF (UCIBF(7:7+LEN-1).EQ.CSTR(1:LEN)) THEN
C               found this upstream record
C               save position
                IF (I.GT.UPOS) THEN
                  UPOS = I
                END IF
                DONE = 1
              END IF
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
            IF (KEY.NE.EREC .AND. DONE.EQ.0) GO TO 40
          END IF
 30     CONTINUE
        DPOS = 0
        DO 50 K = 1,7
          IF (DOWNST(K).GT.0) THEN
C           look for downstream reaches
            IF (OPERNO.EQ.3) THEN
C             delete a rchres
              CSTR = 'RCHRES              '
            END IF
            LEN  = 14
            CALL CVARAR (LEN,CSTR,LEN,CSTR1)
            CALL INTCHR (DOWNST(K),I8,I0,I,CSTR1(7))
C           convert to character string for searching
            CALL CARVAR (LEN,CSTR1,LEN,CSTR)
            I = 0
            DONE = 0
            KEY = SREC
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
 60         CONTINUE
              I = I + 1
              IF (UCIBF(7:7+LEN-1).EQ.CSTR(1:LEN)) THEN
C               found this downstream record
C               save position
                IF (I.LT.DPOS) THEN
                  DPOS = I
                END IF
                DONE = 1
              END IF
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
            IF (KEY.NE.EREC .AND. DONE.EQ.0) GO TO 60
          END IF
 50     CONTINUE
C       have to put new record in opn seq block between upos and dpos
        IF (UPOS.EQ.0) THEN
C         no upstream records, put in first position
          UPOS = 1
        END IF
        IF (UPOS.GE.DPOS .AND. DPOS.NE.0) THEN
C         error, cant write out new record
          WRITE (99,*) 'CANNOT FIND PLACE TO PUT NEW REACH IN OPN SEQ'
        ELSE
C         put this opn seq record
          KEY = SREC
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          I = 0
          DONE = 0
 80       CONTINUE
            I = I + 1
            IF (UPOS.EQ.I) THEN
C             put record after this one
              IF (OPERNO.EQ.3) THEN
C               delete a rchres
                UCIBF = '      RCHRES            '
              END IF
              LEN  = 80
              CALL CVARAR (LEN,UCIBF,LEN,CSTR1)
              CALL INTCHR (IDNO,I8,I0,I,CSTR1(13))
C             convert to character string for searching
              CALL CARVAR (LEN,CSTR1,LEN,UCIBF)
              CALL PUTUCI (UCIBF,I1,KEY)
            END IF
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
          IF (KEY.NE.EREC .AND. DONE.EQ.0) GO TO 80
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE    HADDFT
     I                    (IDNO,LIKEOP)
C
C     + + + PURPOSE + + +
C     add an ftable for a new reach like an existing reach
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      IDNO,LIKEOP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDNO   - id number of new operation
C     LIKEOP - which ftable to make this new one like
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I1,BLKID,SREC,EREC,KEY,
     1              I80,I0,IM1,I3,FTABNO,FOUND,DATKEY(2),
     2              TMPKEY,MOREFG,SKEY,TKEY
      CHARACTER*1   CMNT(3),IBUF1(80),CEND(3)
      CHARACTER*80  UCIBF
C
C     + + + FUNCTIONS + + +
      INTEGER       CHRINT,STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETSE,CVARAR,GETUCI,PUTUCI,CHRINT,STRFND
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('  FTABLE     ',I2,65X)
 2010 FORMAT ('  END FTABLE ',I2,65X)
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CEND/'E','N','D'/
      DATA CMNT/'*','*','*'/
C
C     + + + END SPECIFICATIONS + + +
C
      IM1= -1
      I0 = 0
      I1 = 1
      I3 = 3
      I80= 80
C
C     add entry in ftables block
      BLKID = 4
C     find starting and ending records for this block
      CALL GETSE (BLKID,I1,
     O            SREC,EREC)
C
      IF (SREC.NE.0) THEN
C       this block exists
        KEY = SREC
 10     CONTINUE
C         look at records in in-memory uci for start and end of table
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          CALL CVARAR (I80,UCIBF,I80,IBUF1)
C         look at ftable number
          FTABNO = CHRINT (I3,IBUF1(13))
          FOUND  = 0
          IF (FTABNO.EQ.LIKEOP) THEN
C           this is the one we want to copy
            FOUND = 1
            DATKEY(1) = KEY
 30         CONTINUE
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
              CALL CVARAR (I80,UCIBF,I80,IBUF1)
              IF (STRFND(I3,IBUF1(3),I3,CEND) .GT. 0) THEN
C               found end of this ftable
                DATKEY(2) = KEY
                MOREFG = 0
              ELSE
                MOREFG = 1
              END IF
            IF (MOREFG.EQ.1) GO TO 30
C           now we know all we need to about this ftable
          END IF
C         look ahead one record for end of block
          TMPKEY = KEY
          CALL GETUCI (I0,
     M                 TMPKEY,
     O                 UCIBF)
        IF (TMPKEY.NE.EREC .AND. FOUND.EQ.0) GO TO 10
C
        IF (FOUND.EQ.1) THEN
C         found ftable to copy, now do it
C         put first record
          SKEY = DATKEY(1)
          TKEY = DATKEY(2)
          WRITE (UCIBF,2000) IDNO
          CALL PUTUCI (UCIBF,I0,TKEY)
          CALL GETUCI (IM1,
     M                 TKEY,
     O                 UCIBF)
          CALL GETUCI (IM1,
     M                 SKEY,
     O                 UCIBF)
 20       CONTINUE
            CALL CVARAR (I80,UCIBF,I80,IBUF1)
            IF (STRFND(I80,IBUF1,I3,CMNT).GT.0) THEN
C             found a comment
              CALL PUTUCI (UCIBF,IM1,TKEY)
            ELSE
C             no comment
              CALL PUTUCI (UCIBF,I0,TKEY)
            END IF
            CALL GETUCI (IM1,
     M                   TKEY,
     O                   UCIBF)
            CALL GETUCI (IM1,
     M                   SKEY,
     O                   UCIBF)
          IF (SKEY.NE.DATKEY(2)) GO TO 20
C         now put last record
          WRITE (UCIBF,2010) IDNO
          CALL PUTUCI (UCIBF,I0,TKEY)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE    HADDFF
     I                    (IDNO,FUNIT,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     add an ftable for a reach from a file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      IDNO,FUNIT,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDNO   - id number of new operation
C     FUNIT  - file unit number containing ftable
C     RETCOD - return code to indicate if read successfully
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I1,BLKID,SREC,EREC,KEY,IFIRST,ILAST,
     1              I80,I0,IM1,I3,MOREFG,EKEY
      CHARACTER*1   CMNT(3),IBUF1(80),CEND(3)
      CHARACTER*80  UCIBF
C
C     + + + FUNCTIONS + + +
      INTEGER       STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETSE,CVARAR,GETUCI,PUTUCI,STRFND,DELUCI
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A80)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('  FTABLE     ',I2,65X)
 2010 FORMAT ('  END FTABLE ',I2,65X)
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CEND/'E','N','D'/
      DATA CMNT/'*','*','*'/
C
C     + + + END SPECIFICATIONS + + +
C
      IM1= -1
      I0 = 0
      I1 = 1
      I3 = 3
      I80= 80
C
C     add entry in ftables block
      BLKID = 4
C     find starting and ending records for this block
      CALL GETSE (BLKID,I1,
     O            SREC,EREC)
C
      IF (SREC.NE.0) THEN
C       this block exists
        KEY = SREC
C       read first record
        READ (FUNIT,1000,ERR=50) UCIBF
C       put first record
        WRITE (UCIBF,2000) IDNO
        CALL PUTUCI (UCIBF,I0,KEY)
        CALL GETUCI (IM1,
     M               KEY,
     O               UCIBF)
        IFIRST = KEY
 20     CONTINUE
          READ (FUNIT,1000,ERR=50) UCIBF
          CALL CVARAR (I80,UCIBF,I80,IBUF1)
          IF (STRFND(I80,IBUF1,I3,CMNT).GT.0) THEN
C           found a comment
            CALL PUTUCI (UCIBF,IM1,KEY)
            MOREFG = 1
          ELSE
C           no comment
            IF (STRFND(I3,IBUF1(3),I3,CEND) .GT. 0) THEN
C             found end of this ftable
              MOREFG = 0
              EKEY = KEY
            ELSE
              MOREFG = 1
              CALL PUTUCI (UCIBF,I0,KEY)
            END IF
          END IF
          CALL GETUCI (IM1,
     M                 KEY,
     O                 UCIBF)
        IF (MOREFG.EQ.1) GO TO 20
C       now put last record
        WRITE (UCIBF,2010) IDNO
        CALL PUTUCI (UCIBF,I0,EKEY)
      END IF
C
      RETCOD = 0
      GO TO 60
 50   CONTINUE
C       error reading ftable
        RETCOD = 1
        ILAST  = KEY
C       remove any part of an ftable added already
        KEY = IFIRST
        CALL DELUCI (KEY)
        IF (IFIRST.NE.ILAST) THEN
C         more records to delete
 70       CONTINUE
            CALL GETUCI (IM1,
     M                   KEY,
     O                   UCIBF)
            MOREFG = 0
            IF (KEY.NE.ILAST) THEN
              MOREFG = 1
            END IF
            CALL DELUCI (KEY)
          IF (MOREFG.EQ.1) GO TO 70
        END IF
 60   CONTINUE
      CLOSE (UNIT=FUNIT)
C
      RETURN
      END
C
C
C
      SUBROUTINE    HADD
     I                  (OPERNO,IDNO,LIKEOP)
C
C     + + + PURPOSE + + +
C     add an operation to all tables of an operation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      OPERNO,IDNO,LIKEOP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPERNO - operation type (3-reach,11-report)
C     IDNO   - id number of new operation
C     LIKEOP - operation from which to take defaults
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I1,SREC,EREC,KEY,I0,I,LEN,ICNT,FOUND
      CHARACTER*1   CSTR1(8)
      CHARACTER*8   LSTR,ISTR
      CHARACTER*80  UCIBF
C
C     + + + FUNCTIONS + + +
      INTEGER       CHRINT
C
C     + + + EXTERNALS + + +
      EXTERNAL      GETSE,CARVAR,GETUCI,INTCHR,RNGCHK,PUTUCI
      EXTERNAL      CHRINT,CVARAR,PREUCI
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
C     set like string to search for
      LSTR = ' '
      LEN  = 5
      CALL INTCHR (LIKEOP,LEN,I0,I,CSTR1(1))
      CALL CARVAR (LEN,CSTR1,LEN,LSTR)
C     set new string to replace it with
      ISTR = ' '
      LEN  = 5
      CALL INTCHR (IDNO,LEN,I0,I,CSTR1(1))
      CALL CARVAR (LEN,CSTR1,LEN,ISTR)
C
C     find starting and ending records for this operation type block
      I   = 100
      ICNT= 0
      FOUND= 0
 5    CONTINUE
        ICNT= ICNT + 1
        CALL GETSE (I,ICNT,
     O              SREC,EREC)
C
        IF (SREC.NE.0) THEN
C         a block exists, check for right type
          KEY = SREC
C         back up to previous record
          CALL PREUCI (KEY)
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
C         see if this record contains the keyword we desire
          IF ((OPERNO.EQ.3 .AND. UCIBF(1:6).EQ.'RCHRES') .OR.
     1        (OPERNO.EQ.10 .AND. UCIBF(1:6).EQ.'BMPRAC') .OR.
     1        (OPERNO.EQ.11 .AND. UCIBF(1:6).EQ.'REPORT')) THEN
C           yes, found keyword we want
            FOUND= 1
            KEY = SREC
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
 10         CONTINUE
C             in operation type blocks
              CALL CVARAR (LEN,UCIBF,LEN,CSTR1)
              IF (CHRINT(LEN,CSTR1).GT.0) THEN
C               this looks like an operation line
                CALL RNGCHK (OPERNO,KEY,
     M                       UCIBF)
                IF (UCIBF(1:LEN).EQ.LSTR(1:LEN) .AND.
     1              (UCIBF(6:6+LEN-1).EQ.LSTR(1:LEN) .OR.
     1               UCIBF(6:6+LEN-1).EQ.'     ')) THEN
C                 found a record to copy for the new operation
                  UCIBF(1:LEN) = ISTR(1:LEN)
                  UCIBF(6:6+LEN-1) = '     '
                  CALL PUTUCI (UCIBF,I1,KEY)
                END IF
              END IF
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
            IF (KEY.NE.EREC) GO TO 10
          END IF
        END IF
C       if this block not yet found, return and try next one
      IF (FOUND.EQ.0 .AND. SREC.NE.0) GO TO 5
C
      RETURN
      END
C
C
C
      SUBROUTINE    GETSEO
     I                    (OPERNO,
     O                     SREC,EREC)
C
C     + + + PURPOSE + + +
C     get the start and end of an operation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      OPERNO,SREC,EREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPERNO - operation type (1-PERLND,3-RCHRES)
C     SREC   - starting record
C     EREC   - ending record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I1,KEY,J,I
      CHARACTER*80  UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL      GETSE,PREUCI,GETUCI
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
C
      J= 1
 10   CONTINUE
        I = 100
        CALL GETSE (I,J,
     O              SREC,EREC)
        KEY = SREC
        CALL PREUCI (KEY)
        CALL GETUCI (I1,KEY,UCIBF)
        IF ((OPERNO.EQ.1 .AND. UCIBF(1:6).EQ.'PERLND') .OR.
     1      (OPERNO.EQ.3 .AND. UCIBF(1:6).EQ.'RCHRES') .OR.
     1      (OPERNO.EQ.10 .AND. UCIBF(1:6).EQ.'BMPRAC') .OR.
     1      (OPERNO.EQ.11 .AND. UCIBF(1:6).EQ.'REPORT')) THEN
          J = 12
        ELSE
          J = J + 1
          SREC = 0
          EREC = 0
        END IF
      IF (J.LT.12) GOTO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE    HAOPSQ
     I                    (COPNO,IDNO,CB4OP,B4ID)
C
C     + + + PURPOSE + + +
C     add an operation to the operation sequence block, before this op
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      IDNO,B4ID
      CHARACTER*6  COPNO,CB4OP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     COPNO  - operation type
C     IDNO   - id number of new operation
C     CB4OP  - operation to put this one before
C     B4ID   - operation id to put this one before
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I1,BLKID,SREC,EREC,LEN,I,KEY,I8,I0
      CHARACTER*1   CSTR1(80)
      CHARACTER*80  UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETSE,CVARAR,INTCHR,CARVAR,GETUCI,PUTUCI,ADDOPN
      EXTERNAL   PREUCI
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (12X,I8,60X)

C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I8 = 8
C
C     add entry in opn seq block
      BLKID = 3
C     find starting and ending records for this block
      CALL GETSE (BLKID,I1,
     O            SREC,EREC)
C
      IF (SREC.NE.0) THEN
C       this block exists
        CALL PREUCI(EREC)
        KEY = SREC
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        FOUND = 0
 10     CONTINUE
          READ(UCIBF,1010) ITMP
          IF (ITMP.EQ.B4ID .AND. UCIBF(7:12).EQ.CB4OP) THEN
C           this is the record
            FOUND = 1
          END IF
          IF (FOUND.EQ.0) THEN
C           get next record
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
          END IF
        IF (KEY.NE.EREC .AND. FOUND.EQ.0) GO TO 10
C
        IF (FOUND.GT.0) THEN
          UCIBF = '      ' // COPNO // '            '
          LEN  = 80
          CALL CVARAR (LEN,UCIBF,LEN,CSTR1)
          CALL INTCHR (IDNO,I8,I0,I,CSTR1(13))
          CALL CARVAR (LEN,CSTR1,LEN,UCIBF)
          CALL PREUCI(KEY)
          CALL PUTUCI (UCIBF,I1,KEY)
          IF (COPNO.EQ.'PERLND') THEN
            OPERNO = 1
          ELSE IF (COPNO.EQ.'IMPLND') THEN
            OPERNO = 2
          ELSE IF (COPNO.EQ.'RCHRES') THEN
            OPERNO = 3
          ELSE IF (COPNO.EQ.'BMPRAC') THEN
            OPERNO = 10
          ELSE IF (COPNO.EQ.'REPORT') THEN
            OPERNO = 11
          END IF
          CALL ADDOPN (OPERNO,IDNO)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE    HADDTS
     I                    (OPERNO,IDNO,LIKEID)
C
C     + + + PURPOSE + + +
C     add time series records for a new operation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      OPERNO,IDNO,LIKEID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPERNO - operation to which this id belongs (1-per,2-imp,3-rch)
C     IDNO   - id number
C     LIKEID - operation number that this one is like
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       LEN,I0,I,BLKID,I4
      CHARACTER*1   NSTR1(20),LSTR1(20)
      CHARACTER*20  CSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL      CVARAR,INTCHR,HCOPTS
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I4 = 4
C
      IF (OPERNO.EQ.11) THEN
C       add a report
        CSTR = 'REPORT              '
      ELSE IF (OPERNO.EQ.10) THEN
C       add a bmprac
        CSTR = 'BMPRAC              '
      END IF
      LEN  = 10
      CALL CVARAR (LEN,CSTR,LEN,LSTR1)
      CALL INTCHR (LIKEID,I4,I0,I,LSTR1(7))
      CALL CVARAR (LEN,CSTR,LEN,NSTR1)
      CALL INTCHR (IDNO,I4,I0,I,NSTR1(7))
C
      BLKID = 5
      CALL HCOPTS (BLKID,LEN,LSTR1,NSTR1)
      BLKID = 7
      CALL HCOPTS (BLKID,LEN,LSTR1,NSTR1)
      BLKID = 8
      CALL HCOPTS (BLKID,LEN,LSTR1,NSTR1)
      BLKID = 10
      CALL HCOPTS (BLKID,LEN,LSTR1,NSTR1)
C
      RETURN
      END
C
C
C
      SUBROUTINE    HCOPTS
     I                    (BLKID,LEN,LSTR1,NSTR1)
C
C     + + + PURPOSE + + +
C     copy time series records for a new operation like an existing op
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      BLKID,LEN
      CHARACTER*1  LSTR1(LEN),NSTR1(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BLKID  - block to search
C     LEN    - length of string
C     LSTR1  - character string to search for
C     NSTR1  - new character string
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I1,SREC,EREC,KEY,I0
      CHARACTER*20  CSTR,NSTR
      CHARACTER*80  UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL      GETSE,CARVAR,PUTUCI,GETUCI,GETSEO
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
C     find starting and ending records for this block
      CALL GETSE (BLKID,I1,
     O            SREC,EREC)
      IF (SREC.NE.0) THEN
C       this block exists
        CSTR = '                    '
        NSTR = '                    '
C       convert to character string for searching
        CALL CARVAR (LEN,LSTR1,LEN,CSTR)
        CALL CARVAR (LEN,NSTR1,LEN,NSTR)
        KEY = SREC
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
 10     CONTINUE
          IF (BLKID.EQ.5) THEN
C           in external sources block
            IF (UCIBF(44:44+LEN-1).EQ.CSTR(1:LEN) .AND.
     1          (UCIBF(54:57).EQ.CSTR(7:10) .OR.
     2           UCIBF(54:57).EQ.'    ')) THEN
C             found one, copy it
              UCIBF(44:44+LEN-1) = NSTR(1:LEN)
              CALL PUTUCI (UCIBF,I1,KEY)
            END IF
          ELSE IF (BLKID.EQ.7 .OR. BLKID.EQ.10) THEN
C           in network block or schematic block
            IF (UCIBF(44:44+LEN-1).EQ.CSTR(1:LEN) .AND.
     1          (UCIBF(54:57).EQ.CSTR(7:10) .OR.
     2           UCIBF(54:57).EQ.'    ')) THEN
C             found one, copy it
              UCIBF(44:44+LEN-1) = NSTR(1:LEN)
              CALL PUTUCI (UCIBF,I1,KEY)
            ELSE IF (UCIBF(1:LEN).EQ.CSTR(1:LEN)) THEN
C             found one, copy it
              UCIBF(1:LEN) = NSTR(1:LEN)
              CALL PUTUCI (UCIBF,I1,KEY)
            END IF
          ELSE IF (BLKID.EQ.8) THEN
C           in external targets block
            IF (UCIBF(1:LEN).EQ.CSTR(1:LEN)) THEN
C             found one, copy it
              UCIBF(1:LEN) = NSTR(1:LEN)
              CALL PUTUCI (UCIBF,I1,KEY)
            END IF
          END IF
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
        IF (KEY.NE.EREC) GO TO 10
      END IF
C
      RETURN
      END
