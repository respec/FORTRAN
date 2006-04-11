C
C
C
      INTEGER FUNCTION REPEXT
     I                       (REPID)
C
C     + + + PURPOSE + + +
C     check to see if a standard report exists
C       1-exists, 0-does not exist
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   REPID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     REPID - report id to check
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        OPID,OPTYP
C
C     + + + EXTERNALS + + +
      EXTERNAL     GETNXT
C
C     + + + END SPECIFICATIONS + + +
C
C     does this report operation exist?
      OPID  = 0
      OPTYP = 11
      REPEXT= 0
 10   CONTINUE
        CALL GETNXT (OPTYP,
     M               OPID)
        IF (OPID.GT.0 .AND. OPID.EQ.REPID) THEN
C         found the report we're looking for
          REPEXT = 1
        END IF
      IF (OPID.GT.0 .AND. REPEXT.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE ADDREPT
     I                  (MESSFL,REPID)
C
C     + + + PURPOSE + + +
C     add a standard report
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,REPID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     REPID  - report id to add
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        OPID,OPTYP,IFOUND,IDNO,SREC,EREC,I1,I8,I0,LEN,
     1               BLKID,TCLU,TGRP,KGRP,RETCOD,SNO,NRCH,RCHID(100),
     2               NBMP,BMPID(100),NPER,PERID(100),NIMP,IMPID(100),
     3               NLOAD,NUIMP,NUPER,ICNT,I4,NCONS,CONFIL
      CHARACTER*4    CTMP4,CTSTYP(50),CCONS4
      CHARACTER*8    ITMNAM
      CHARACTER*12   TNAME
      CHARACTER*20   CRCH(100),CBMP(100),CPER(100),CIMP(100),
     1               CUPER(100),CUIMP(100),CLOAD(100)
      CHARACTER*20   CTEMP(50)
      CHARACTER*80   UCIBF
      CHARACTER*1    CSTR1(80)
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (I5,75X)
 1005 FORMAT (A4,6X,A20)
 1040 FORMAT (49X,I4,27X)

C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (I5,5X,A20,' SUM     5    3')
 2010 FORMAT (I5,5X,A20)

C     + + + EXTERNALS + + +
      EXTERNAL     GETNXT, ADDOPB, HAOPSQ, GETSE, ESTADD, HGETI, HGETC
      EXTERNAL     REPMLSC
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I4 = 4
      I8 = 8
C
C     does this report operation exist?
      OPID = 0
      OPTYP= 11
      IFOUND= 0
 10   CONTINUE
        CALL GETNXT (OPTYP,
     M               OPID)
        IF (OPID.GT.0) THEN
C         found a report
          IF (OPID.EQ.REPID) THEN
C           found the report we're looking for
            IFOUND = 1
          END IF
        END IF
      IF (OPID.GT.0 .AND. IFOUND.EQ.0) GO TO 10
C
      IF (IFOUND.EQ.1) THEN
C       we found this report and don't need to add it
        write(99,*) 'already have report ',repid
      ELSE
C       need to add this report, start collecting info
        write(99,*) 'adding report ',repid
C
C       number of reaches
        NRCH = 0
 50     CONTINUE
          ITMNAM = 'RCHID   '
          CALL HGETI (ITMNAM,I0,
     O                OPID)
          IF (OPID.GT.0) THEN
C           found a reach
            NRCH  = NRCH + 1
            RCHID(NRCH) = OPID
          END IF
        IF (OPID.NE.-99) GO TO 50
        IF (NRCH.GT.0) THEN
C         get reach descriptions
          DO 501 I = 1,NRCH
            ITMNAM= 'RCHDESC '
            CALL HGETC (ITMNAM,RCHID(I),
     O                  CRCH(I))
 501      CONTINUE
        END IF
C
C       number of BMPs
        NBMP = 0
 51     CONTINUE
          ITMNAM = 'BMPID   '
          CALL HGETI (ITMNAM,I0,
     O                OPID)
          IF (OPID.GT.0) THEN
C           found a BMP
            NBMP  = NBMP + 1
            BMPID(NBMP) = OPID
          END IF
        IF (OPID.NE.-99) GO TO 51
        IF (NBMP.GT.0) THEN
C         get bmp descriptions
          DO 511 I = 1,NBMP
            ITMNAM= 'BMPDESC '
            CALL HGETC (ITMNAM,BMPID(I),
     O                  CBMP(I))
 511      CONTINUE
        END IF
C
C       number of perlnds
        NPER = 0
 52     CONTINUE
          ITMNAM = 'PERID   '
          CALL HGETI (ITMNAM,I0,
     O                OPID)
          IF (OPID.GT.0) THEN
C           found a PER
            NPER  = NPER + 1
            PERID(NPER) = OPID
          END IF
        IF (OPID.NE.-99) GO TO 52
        IF (NPER.GT.0) THEN
C         get perlnd descriptions
          DO 521 I = 1,NPER
            ITMNAM= 'PERDESC '
            CALL HGETC (ITMNAM,PERID(I),
     O                  CPER(I))
 521      CONTINUE
        END IF
        IF (NPER.GT.1) THEN
C         need unique names only
C         first string is unique
          NUPER       = 1
          CUPER(NUPER)= CPER(1)
          DO 522 I= 2,NPER
C           look thru other strings
            MATCH= 0
            DO 523 J= 1,NUPER
C             look thru existing uniques
              IF (CPER(I) .EQ. CUPER(J)) THEN
C               not unique
                MATCH= 1
              END IF
 523        CONTINUE
            IF (MATCH.EQ.0) THEN
C             unique perlnd
              NUPER= NUPER + 1
              CUPER(NUPER)= CPER(I)
            END IF
 522      CONTINUE
        END IF
C
C       number of implnds
        NIMP = 0
 53     CONTINUE
          ITMNAM = 'IMPID   '
          CALL HGETI (ITMNAM,I0,
     O                OPID)
          IF (OPID.GT.0) THEN
C           found a IMP
            NIMP  = NIMP + 1
            IMPID(NIMP) = OPID
          END IF
        IF (OPID.NE.-99) GO TO 53
        IF (NIMP.GT.0) THEN
C         get implnd descriptions
          DO 531 I = 1,NIMP
            ITMNAM= 'IMPDESC '
            CALL HGETC (ITMNAM,IMPID(I),
     O                  CIMP(I))
 531      CONTINUE
        END IF
        IF (NIMP.GT.1) THEN
C         need unique names only
C         first string is unique
          NUIMP       = 1
          CUIMP(NUIMP)= CIMP(1)
          DO 532 I= 2,NIMP
C           look thru other strings
            MATCH= 0
            DO 533 J= 1,NUIMP
C             look thru existing uniques
              IF (CIMP(I) .EQ. CUIMP(J)) THEN
C               not unique
                MATCH= 1
              END IF
 533        CONTINUE
            IF (MATCH.EQ.0) THEN
C             unique implnd
              NUIMP= NUIMP + 1
              CUIMP(NUIMP)= CIMP(I)
            END IF
 532      CONTINUE
        END IF
C
C       number of point loads
        NLOAD = 0
        BLKID = 5
        CALL GETSE (BLKID,I1,
     O              SREC,EREC)
        IF (SREC.NE.0) THEN
C         external sources block exists
          write(99,*) 'ext src block exists'
          KEY = SREC
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          IF (KEY.NE.EREC) THEN
C           some records exist
            ITMP  = 0
 100        CONTINUE
C             look for this rchres id
              IF (UCIBF(1:3).EQ.'WDM' .AND.
     1            UCIBF(44:49).EQ.'RCHRES' .AND.
     1            UCIBF(59:64).EQ.'INFLOW') THEN
C               this looks like a point load
C               if range of operations here expand
                CALL RTSCHK (KEY,
     M                       UCIBF)
                READ (UCIBF,1040) IDNO
                IF (IDNO.NE.ITMP) THEN
C                 this appears to be a point load for a different reach
                  CALL INTCHR (IDNO,I4,I1,K,CSTR1(1))
                  CALL CARVAR (I4,CSTR1,I4,CTMP4)
                  NLOAD = NLOAD + 1
                  CLOAD(NLOAD) = 'Pt Load to Rch ' // CTMP4
                  ITMP = IDNO
                END IF
              END IF
C             get next record
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
            IF (KEY.NE.EREC) GO TO 100
          END IF
        END IF
C
C       read default cons
        NCONS = 13
        CTEMP(1)  = 'SEDIMENT(TONS)      '
        CTEMP(2)  = 'NO3(LBS)            '
        CTEMP(3)  = 'NH3(LBS)            '
        CTEMP(4)  = 'NO2(LBS)            '
        CTEMP(5)  = 'NO23(LBS)           '
        CTEMP(6)  = 'ORGANIC N(LBS)      '
        CTEMP(7)  = 'TOTAL N(LBS)        '
        CTEMP(8)  = 'PO4(LBS)            '
        CTEMP(9)  = 'ORGP(LBS)           '
        CTEMP(10) = 'TOTP(LBS)           '
        CTEMP(11) = 'FECAL COLI(#M ORGS) '
        CTEMP(12) = 'DO(LBS)             '
        CTEMP(13) = 'BOD(LBS)            '
        CTSTYP(1) = 'TSS '
        CTSTYP(2) = '    '
        CTSTYP(3) = 'NH3 '
        CTSTYP(4) = '    '
        CTSTYP(5) = '    '
        CTSTYP(6) = '    '
        CTSTYP(7) = '    '
        CTSTYP(8) = 'PHOS'
        CTSTYP(9) = '    '
        CTSTYP(10) = 'PHOS'
        CTSTYP(11) = 'FECA'
        CTSTYP(12) = 'DO  '
        CTSTYP(13) = 'BOD5'
C
C       try to open cons file
        CONFIL = 91
        OPEN(UNIT=CONFIL,FILE='repcon.inp',STATUS='OLD',ERR=90)
        READ(CONFIL,1000) NCONS
        DO 65 J = 1,NCONS
C         read each line of cons file
          READ(CONFIL,1005) CTSTYP(J),CTEMP(J)
 65     CONTINUE
        CLOSE(UNIT=CONFIL)
C       continue here if file does not exist
 90     CONTINUE
C
C       finished collecting info
        write(99,*) 'finished collecting info'
C
C
C       add this new report to opn seq block
        BLKID = 3
C       find starting and ending records for this block
        CALL GETSE (BLKID,I1,
     O              SREC,EREC)
        IF (SREC.NE.0) THEN
C         this block exists
          CALL PREUCI(EREC)
          KEY = SREC
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          IFOUND = 0
 20       CONTINUE
            IF (UCIBF(7:12).EQ.'RCHRES') THEN
C             found a rchres
              IFOUND = KEY
            END IF
C           get next record
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
          IF (KEY.NE.EREC) GO TO 20
          IF (IFOUND.GT.0) THEN
C           add report operation to operation seq block
            UCIBF = '      REPORT            '
            LEN  = 80
            CALL CVARAR (LEN,UCIBF,LEN,CSTR1)
            CALL INTCHR (REPID,I8,I0,I,CSTR1(13))
            CALL CARVAR (LEN,CSTR1,LEN,UCIBF)
            CALL PUTUCI (UCIBF,I1,IFOUND)
            CALL ADDOPN (OPTYP,REPID)
          END IF
        END IF
        write(99,*) 'added to opn seq block'

C       does the report block exist yet?
        ITYPE = (1000 * OPTYP) + 1
        CALL GETSE (ITYPE,I1,
     O              SREC,EREC)
        IF (SREC.GT.0) THEN
C         this block already exists
          write(99,*) 'this block already exists'
        ELSE
C         need to add report block
          write(99,*) 'need to add report block'
          IDNO = 100
          TNAME= 'REPORT'
          SREC = 0
          EREC = 0
          CALL ADDOPB (IDNO,TNAME,
     M                 SREC,EREC)
C         report block has been added
          write(99,*) 'report block has been added'
        END IF
C
C       add report tables if necessary
        DO 30 I = 1,5
C         go through each of the 5 report tables
          TCLU   = 131
          IFOUND = 0
C         check that this table exists
          IDNO = (OPTYP*1000)+ I
          CALL GETSE (IDNO,I1,
     O                SREC,EREC)
          IF (SREC.EQ.0) THEN
C           this table does not exist, add it
            TGRP  = 100 + I
            KGRP  = I
            write(99,*) 'adding report table ',i
            CALL ESTADD (MESSFL,TCLU,TGRP,KGRP,OPTYP,I1,
     O                   SREC,EREC)
            KEY = SREC
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
            IFOUND = KEY
          ELSE
C           this table already exists
C           check to see if this record is already in table
            INIT  = 1
 40         CONTINUE
              CALL XTABLE (MESSFL,TCLU,I,I1,INIT,I0,I1,
     O                     KEY,UCIBF,RETCOD)
              IF (RETCOD.EQ.2) THEN
                READ (UCIBF,1000) SNO
                IF (REPID.EQ.SNO) THEN
C                 this record is in the table already
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
C           table already existed with this record, do nothing
          ELSE
C           need to build this record
C
C
            IF (I.EQ.1) THEN
C             flags table
C             build record
              IF (REPID.GE.1 .AND. REPID.LE.3) THEN
                ICNT = NUPER + NUIMP
              ELSE IF (REPID.EQ.4 .OR. REPID.EQ.5) THEN
                ICNT = NRCH
              ELSE IF (REPID.EQ.6) THEN
                ICNT = NLOAD
              ELSE
                ICNT = NBMP
              END IF
              CALL INTCHR (ICNT,I4,I0,K,CSTR1(1))
              CALL CARVAR (I4,CSTR1,I4,CTMP4)
              CALL INTCHR (NCONS,I4,I0,K,CSTR1(1))
              CALL CARVAR (I4,CSTR1,I4,CCONS4)
              IF (REPID.EQ.1) THEN
                UCIBF = '    1        91 ' // CCONS4 // ' ' // CTMP4 //
     1                  '    0   16 1000 1000    5   12'
              ELSE IF (REPID.EQ.2) THEN
                UCIBF = '    2        92 ' // CCONS4 // ' ' // CTMP4 //
     1                  '    0   16 1000 1000    5   12'
              ELSE IF (REPID.EQ.3) THEN
                UCIBF = '    3        93 ' // CCONS4 // ' ' // CTMP4 //
     1                  '    0   16 1000 1000    5   12'
              ELSE IF (REPID.EQ.4) THEN
                UCIBF = '    4        94 ' // CCONS4 // ' ' // CTMP4 //
     1                  '    0   20 1000 1000    5   12'
              ELSE IF (REPID.EQ.5) THEN
                UCIBF = '    5        95 ' // CCONS4 // ' ' // CTMP4 //
     1                  '    0   20 1000 1000    5   12'
              ELSE IF (REPID.EQ.6) THEN
                UCIBF = '    6        96 ' // CCONS4 // ' ' // CTMP4 //
     1                  '    0   20 1000 1000    5   12'
              ELSE IF (REPID.EQ.7) THEN
                UCIBF = '    7        97 ' // CCONS4 // ' ' // CTMP4 //
     1                  '    0   20 1000 1000    5   12'
              END IF
              IF (IFOUND.EQ.0) THEN
C               table already existed, but this record did not, add it
                CALL PUTUCI(UCIBF,I1,KEY)
                CALL GETUCI (I1,
     M                       KEY,
     O                       UCIBF)
              ELSE IF (IFOUND.GT.1) THEN
C               just added this table with this key
                CALL REPUCI(IFOUND,UCIBF)
              END IF
C
            ELSE IF (I.EQ.2) THEN
C             title table
              IF (REPID.EQ.1) THEN
                UCIBF = '    1     Land Surface total-loads       '
              ELSE IF (REPID.EQ.2) THEN
                UCIBF = '    2     Land Surface percent loads     '
              ELSE IF (REPID.EQ.3) THEN
                UCIBF = '    3     Land Surface unit loads        '
              ELSE IF (REPID.EQ.4) THEN
                UCIBF = '    4     Reach total loads              '
              ELSE IF (REPID.EQ.5) THEN
                UCIBF = '    5     Reach total change(+gain,-loss)'
              ELSE IF (REPID.EQ.6) THEN
                UCIBF = '    6     Point Loads                    '
              ELSE IF (REPID.EQ.7) THEN
                UCIBF = '    7     BMP Removals                   '
              END IF
              IF (IFOUND.EQ.0) THEN
C               table already existed, but this record did not, add it
                CALL PUTUCI(UCIBF,I1,KEY)
                CALL GETUCI (I1,
     M                       KEY,
     O                       UCIBF)
              ELSE IF (IFOUND.GT.1) THEN
C               just added this table with this key
                CALL REPUCI(IFOUND,UCIBF)
              END IF
C
            ELSE IF (I.EQ.3) THEN
C             src table
              UCIBF = ' '
              IF (REPID.GE.1 .AND. REPID.LE.3) THEN
                DO 70 J = 1,NUPER
                  WRITE(UCIBF,2010) REPID,CUPER(J)
                  IF (J.EQ.1 .AND. IFOUND.GT.1) THEN
                    CALL REPUCI(IFOUND,UCIBF)
                    IFOUND = 0
                  ELSE
                    CALL PUTUCI(UCIBF,I1,KEY)
                    CALL GETUCI (I1,
     M                           KEY,
     O                           UCIBF)
                  END IF
 70             CONTINUE
                DO 71 J = 1,NUIMP
                  WRITE(UCIBF,2010) REPID,CUIMP(J)
                  IF (J.EQ.1 .AND. IFOUND.GT.1) THEN
                    CALL REPUCI(IFOUND,UCIBF)
                    IFOUND = 0
                  ELSE
                    CALL PUTUCI(UCIBF,I1,KEY)
                    CALL GETUCI (I1,
     M                           KEY,
     O                           UCIBF)
                  END IF
 71             CONTINUE
              ELSE IF (REPID.EQ.4 .OR. REPID.EQ.5) THEN
                DO 72 J = 1,NRCH
                  WRITE(UCIBF,2010) REPID,CRCH(J)
                  IF (J.EQ.1 .AND. IFOUND.GT.1) THEN
                    CALL REPUCI(IFOUND,UCIBF)
                    IFOUND = 0
                  ELSE
                    CALL PUTUCI(UCIBF,I1,KEY)
                    CALL GETUCI (I1,
     M                           KEY,
     O                           UCIBF)
                  END IF
 72             CONTINUE
              ELSE IF (REPID.EQ.6) THEN
                DO 73 J = 1,NLOAD
                  WRITE(UCIBF,2010) REPID,CLOAD(J)
                  IF (J.EQ.1 .AND. IFOUND.GT.1) THEN
                    CALL REPUCI(IFOUND,UCIBF)
                    IFOUND = 0
                  ELSE
                    CALL PUTUCI(UCIBF,I1,KEY)
                    CALL GETUCI (I1,
     M                           KEY,
     O                           UCIBF)
                  END IF
 73             CONTINUE
              ELSE IF (REPID.EQ.7) THEN
                DO 74 J = 1,NBMP
                  WRITE(UCIBF,2010) REPID,CBMP(J)
                  IF (J.EQ.1 .AND. IFOUND.GT.1) THEN
                    CALL REPUCI(IFOUND,UCIBF)
                    IFOUND = 0
                  ELSE
                    CALL PUTUCI(UCIBF,I1,KEY)
                    CALL GETUCI (I1,
     M                           KEY,
     O                           UCIBF)
                  END IF
 74             CONTINUE
              END IF
C
            ELSE IF (I.EQ.4) THEN
C             con table
              DO 60 J = 1,NCONS
                WRITE(UCIBF,2000) REPID,CTEMP(J)
                IF (IFOUND.EQ.0) THEN
C                 table already existed, but this record did not, add it
                  CALL PUTUCI(UCIBF,I1,KEY)
                  CALL GETUCI (I1,
     M                         KEY,
     O                         UCIBF)
                ELSE IF (IFOUND.GT.1) THEN
C                 just added this table with this key
                  CALL REPUCI(IFOUND,UCIBF)
                  IFOUND = 0
                END IF
 60           CONTINUE
C
            ELSE IF (I.EQ.5) THEN
C             summ table
              IF (REPID.EQ.1) THEN
                UCIBF = '    1           Total Load     SUM  Average'//
     1                  '              AVER    1'
              ELSE IF (REPID.EQ.2) THEN
                UCIBF = '    2             Pct Load     PCT  Average'//
     1                  '              AVER    1'
              ELSE IF (REPID.EQ.3) THEN
                UCIBF = '    3            Unit Load     AVER Average'//
     1                  '              AVER    1'
              ELSE IF (REPID.EQ.4) THEN
                UCIBF = '    4           Ignore         SUM  Average'//
     1                  '              AVER    1'
              ELSE IF (REPID.EQ.5) THEN
                UCIBF = '    5          Total Delta     AVER Average'//
     1                  '              AVER    1'
              ELSE IF (REPID.EQ.6) THEN
                UCIBF = '    6           Ignore         AVER Average'//
     1                  '              AVER    1'
              ELSE IF (REPID.EQ.7) THEN
                UCIBF = '    7           Ignore         AVER Average'//
     1                  '              AVER    1'
              END IF
              IF (IFOUND.EQ.0) THEN
C               table already existed, but this record did not, add it
                CALL PUTUCI(UCIBF,I1,KEY)
                CALL GETUCI (I1,
     M                       KEY,
     O                       UCIBF)
              ELSE IF (IFOUND.GT.1) THEN
C               just added this table with this key
                CALL REPUCI(IFOUND,UCIBF)
              END IF
            END IF
C
          END IF
 30     CONTINUE
C
        IF (IFOUND.NE.1) THEN
          IF (REPID.NE.6) THEN
C           now call routine to add appropriate mass-link/schematics
            CALL REPMLSC (REPID,NRCH,RCHID,NBMP,BMPID,NUPER,CUPER,
     1                    NUIMP,CUIMP,NPER,PERID,CPER,NIMP,IMPID,CIMP)
          ELSE
C           need to add external sources for point loads
            CALL REPEXSO (NCONS,CTSTYP)
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
      SUBROUTINE DELREPT
     I                  (REPID)
C
C     + + + PURPOSE + + +
C     delete a standard report
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   REPID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     REPID - report id to delete
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        OPTYP,LEN,I8,I0,I,BLKID
      CHARACTER*1    CSTR1(20)
      CHARACTER*20   CSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     CVARAR, INTCHR, HDEL, DELOPN
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I8 = 8
C
      OPTYP = 11
      CSTR = 'REPORT              '
      LEN  = 14
      CALL CVARAR (LEN,CSTR,LEN,CSTR1)
      CALL INTCHR (REPID,I8,I0,I,CSTR1(7))
C     BLKID = 3
C     CALL HDEL (BLKID,LEN,CSTR1)
C     CALL DELOPN (OPTYP,REPID)
      CALL HDELOP (OPTYP,REPID)
C
      RETURN
      END
C
C
C
      SUBROUTINE DELALLR
C
C     + + + PURPOSE + + +
C     delete all reports
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      OPTYP,OPID
C
C     + + + EXTERNALS + + +
      EXTERNAL     GETNXT, HDELOP
C
C     + + + END SPECIFICATIONS + + +
C
C     do any report operations exist?
      OPID = 0
      OPTYP= 11
 10   CONTINUE
        CALL GETNXT (OPTYP,
     M               OPID)
        IF (OPID.GT.0) THEN
C         found a report, delete it
          CALL HDELOP (OPTYP,OPID)
        END IF
      IF (OPID.GT.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE REPMLSC
     I                  (REPID,NRCH,RCHID,NBMP,BMPID,NUPER,CUPER,
     I                   NUIMP,CUIMP,NPER,PERID,CPER,NIMP,IMPID,CIMP)
C
C     + + + PURPOSE + + +
C     add appropriate mass-link and schematics for a standard report
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       REPID,NRCH,RCHID(NRCH),NBMP,BMPID(NBMP),
     1              NUPER,NUIMP,NPER,PERID(NPER),NIMP,IMPID(NIMP)
      CHARACTER*20  CUPER(NUPER),CUIMP(NUIMP),CPER(NPER),CIMP(NIMP)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     REPID - report id to add
C     NRCH  - number of reaches
C     RCHID - array of reach ids
C     NBMP  - number of bmps
C     BMPID - array of bmp ids
C     NUPER - number of unique perlnds
C     CUPER - unique perlnd descriptions
C     NUIMP - number of unique implnds
C     CUIMP - unique implnd descriptions
C     NPER  - number of perlnds
C     PERID - array of perlnd ids
C     CPER  - array of perlnd descriptions
C     NIMP  - number of implnds
C     IMPID - array of implnd ids
C     CIMP  - array of implnd descriptions
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        I0,I1,I4,I5,BLKID,SREC,EREC,IDKEY,MLNO,I6,I,J,
     1               ITEMP,I7,ACNT,I10,KEY,K,L,TPID(50),I11
      REAL           RAREA(50),SUM,TAREA
      CHARACTER*1    CSTR1(10)
      CHARACTER*4    CSTR
      CHARACTER*10   CSTR10
      CHARACTER*80   UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL       GETUCI,GETSE,GETML,INTCHR,CARVAR,PUTUCI,DECCHR
C
C     + + + INPUT FORMATS + + +
 1030 FORMAT (6X,I4,60X)
 1040 FORMAT (49X,I4,27X)
 1050 FORMAT (28X,F10.3,42X)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I4 = 4
      I5 = 5
      I6 = 6
      I7 = 7
      I10= 10
      I11= 11
C
C     get starting and ending keys
      BLKID = 10
      CALL GETSE (BLKID,I1,
     O            SREC,EREC)
C
      IF (SREC.NE.0) THEN
C       this table exists

        IF (REPID.EQ.1 .OR. REPID.EQ.2) THEN
          IDKEY = SREC
          CALL GETUCI (I0,
     M                 IDKEY,
     O                 UCIBF)
 85       CONTINUE
C           look for this reach or bmprac
            IF ((UCIBF(1:6).EQ.'PERLND').AND.
     1          (UCIBF(44:49).EQ.'BMPRAC' .OR.
     2           UCIBF(44:49).EQ.'RCHRES')) THEN
C             we need a copy of this record going to this report
C             look for perlnd to report mass link
              CALL GETML (I4,MLNO)
              UCIBF(44:49) = 'REPORT'
              CALL INTCHR (REPID,I4,I0,I,CSTR1(1))
              CALL CARVAR (I4,CSTR1,I4,CSTR)
              UCIBF(50:53) = CSTR(1:4)
              CALL INTCHR (MLNO,I4,I0,I,CSTR1(1))
              CALL CARVAR (I4,CSTR1,I4,CSTR)
              UCIBF(57:60) = CSTR(1:4)
C             figure out source id
              READ(UCIBF,1030) ITEMP
              K = 0
              DO 10 I = 1,NPER
                IF (PERID(I).EQ.ITEMP) THEN
                  DO 15 J = 1,NUPER
                    IF (CPER(I).EQ.CUPER(J)) THEN
                      K = J
                    END IF
 15               CONTINUE
                END IF
 10           CONTINUE
              CALL INTCHR (K,I4,I0,L,CSTR1(1))
              CALL CARVAR (I4,CSTR1,I4,CSTR)
              UCIBF(72:75) = CSTR(1:4)
              CALL PUTUCI(UCIBF,I1,IDKEY)
            ELSE IF ((UCIBF(1:6).EQ.'IMPLND').AND.
     1          (UCIBF(44:49).EQ.'BMPRAC' .OR.
     2           UCIBF(44:49).EQ.'RCHRES')) THEN
              CALL GETML (I5,MLNO)
              UCIBF(44:49) = 'REPORT'
              CALL INTCHR (REPID,I4,I0,I,CSTR1(1))
              CALL CARVAR (I4,CSTR1,I4,CSTR)
              UCIBF(50:53) = CSTR(1:4)
              CALL INTCHR (MLNO,I4,I0,I,CSTR1(1))
              CALL CARVAR (I4,CSTR1,I4,CSTR)
              UCIBF(57:60) = CSTR(1:4)
C             figure out source id
              READ(UCIBF,1030) ITEMP
              K = 0
              DO 20 I = 1,NIMP
                IF (IMPID(I).EQ.ITEMP) THEN
                  DO 25 J = 1,NUIMP
                    IF (CIMP(I).EQ.CUIMP(J)) THEN
                      K = J + NUPER
                    END IF
 25               CONTINUE
                END IF
 20           CONTINUE
              CALL INTCHR (K,I4,I0,L,CSTR1(1))
              CALL CARVAR (I4,CSTR1,I4,CSTR)
              UCIBF(72:75) = CSTR(1:4)
              CALL PUTUCI(UCIBF,I1,IDKEY)
            END IF
            IF (IDKEY.NE.EREC) THEN
C             get next record
              CALL GETUCI (I0,
     M                     IDKEY,
     O                     UCIBF)
            END IF
          IF (IDKEY.NE.EREC) GO TO 85
        ELSE IF (REPID.EQ.3) THEN
C         add report 3 schematic for perlnds
          IDKEY = EREC
          CALL PREUCI (IDKEY)
          CALL GETML (I4,MLNO)
          DO 30 I = 1,NUPER
C           look for each unique perlnd name
            ACNT= 0
            DO 32 J = 1,NPER
              IF (CPER(J).EQ.CUPER(I)) THEN
C               find records with areas
                KEY = SREC
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
 35             CONTINUE
C                 look for this reach or bmprac
                  CALL INTCHR (PERID(J),I4,I0,K,CSTR1(1))
                  CALL CARVAR (I4,CSTR1,I4,CSTR)
                  IF ((UCIBF(1:6).EQ.'PERLND'.AND.
     1                 UCIBF(7:10).EQ.CSTR(1:4)) .AND.
     1                (UCIBF(44:49).EQ.'BMPRAC' .OR.
     1                 UCIBF(44:49).EQ.'RCHRES')) THEN
C                   we need to know this area
                    ACNT = ACNT + 1
                    READ(UCIBF,1050) RAREA(ACNT)
                    TPID(ACNT) = PERID(J)
                  END IF
                  IF (KEY.NE.EREC) THEN
C                   get next record
                    CALL GETUCI (I0,
     M                           KEY,
     O                           UCIBF)
                  END IF
                IF (KEY.NE.EREC) GO TO 35
              END IF
 32         CONTINUE
            IF (ACNT.GT.0) THEN
C             sum the areas for this perlnd name
              SUM = 0.0
              DO 36 K = 1,ACNT
                SUM = SUM + RAREA(K)
 36           CONTINUE
C             build the new records
              UCIBF = 'PERLND                       '//
     1                '              REPORT   3'
              DO 37 K = 1,ACNT
                CALL INTCHR (TPID(K),I4,I0,L,CSTR1(1))
                CALL CARVAR (I4,CSTR1,I4,CSTR)
                UCIBF(7:10) = CSTR(1:4)
                TAREA = RAREA(K) / SUM
                CALL DECCHR (TAREA,I10,I0,L,CSTR1(1))
                CALL CARVAR (I10,CSTR1,I10,CSTR10)
                UCIBF(29:38) = CSTR10(1:10)
                CALL INTCHR (MLNO,I4,I0,L,CSTR1(1))
                CALL CARVAR (I4,CSTR1,I4,CSTR)
                UCIBF(57:60) = CSTR(1:4)
                CALL INTCHR (I,I4,I0,L,CSTR1(1))
                CALL CARVAR (I4,CSTR1,I4,CSTR)
                UCIBF(72:75) = CSTR(1:4)
                CALL PUTUCI(UCIBF,I1,IDKEY)
 37           CONTINUE
            END IF
 30       CONTINUE
C         add report 3 schematic for implnds
          IDKEY = EREC
          CALL PREUCI (IDKEY)
          CALL GETML (I5,MLNO)
          DO 300 I = 1,NUIMP
C           look for each unique implnd name
            ACNT= 0
            DO 320 J = 1,NIMP
              IF (CIMP(J).EQ.CUIMP(I)) THEN
C               find records with areas
                KEY = SREC
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
 350            CONTINUE
C                 look for this reach or bmprac
                  CALL INTCHR (IMPID(J),I4,I0,K,CSTR1(1))
                  CALL CARVAR (I4,CSTR1,I4,CSTR)
                  IF ((UCIBF(1:6).EQ.'IMPLND'.AND.
     1                 UCIBF(7:10).EQ.CSTR(1:4)) .AND.
     1                (UCIBF(44:49).EQ.'BMPRAC' .OR.
     1                 UCIBF(44:49).EQ.'RCHRES')) THEN
C                   we need to know this area
                    ACNT = ACNT + 1
                    READ(UCIBF,1050) RAREA(ACNT)
                    TPID(ACNT) = IMPID(J)
                  END IF
                  IF (KEY.NE.EREC) THEN
C                   get next record
                    CALL GETUCI (I0,
     M                           KEY,
     O                           UCIBF)
                  END IF
                IF (KEY.NE.EREC) GO TO 350
              END IF
 320        CONTINUE
            IF (ACNT.GT.0) THEN
C             sum the areas for this implnd name
              SUM = 0.0
              DO 360 K = 1,ACNT
                SUM = SUM + RAREA(K)
 360          CONTINUE
C             build the new records
              UCIBF = 'IMPLND                       '//
     1                '              REPORT   3'
              DO 370 K = 1,ACNT
                CALL INTCHR (TPID(K),I4,I0,L,CSTR1(1))
                CALL CARVAR (I4,CSTR1,I4,CSTR)
                UCIBF(7:10) = CSTR(1:4)
                TAREA = RAREA(K) / SUM
                CALL DECCHR (TAREA,I10,I0,L,CSTR1(1))
                CALL CARVAR (I10,CSTR1,I10,CSTR10)
                UCIBF(29:38) = CSTR10(1:10)
                CALL INTCHR (MLNO,I4,I0,L,CSTR1(1))
                CALL CARVAR (I4,CSTR1,I4,CSTR)
                UCIBF(57:60) = CSTR(1:4)
                ITEMP = I + NUPER
                CALL INTCHR (ITEMP,I4,I0,L,CSTR1(1))
                CALL CARVAR (I4,CSTR1,I4,CSTR)
                UCIBF(72:75) = CSTR(1:4)
                CALL PUTUCI(UCIBF,I1,IDKEY)
 370          CONTINUE
            END IF
 300      CONTINUE
        ELSE IF (REPID.EQ.4 .OR. REPID.EQ.5) THEN
C         standard reports 4 or 5
          IDKEY = EREC
          CALL PREUCI (IDKEY)
          IF (REPID.EQ.4) THEN
            CALL GETML (I6,MLNO)
          ELSE
            CALL GETML (I11,MLNO)
          END IF
          UCIBF = 'RCHRES                              1.     REPORT'
          DO 40 I = 1,NRCH
            CALL INTCHR (RCHID(I),I4,I0,J,CSTR1(1))
            CALL CARVAR (I4,CSTR1,I4,CSTR)
            UCIBF(7:10) = CSTR(1:4)
            CALL INTCHR (REPID,I4,I0,J,CSTR1(1))
            CALL CARVAR (I4,CSTR1,I4,CSTR)
            UCIBF(50:53) = CSTR(1:4)
            CALL INTCHR (MLNO,I4,I0,J,CSTR1(1))
            CALL CARVAR (I4,CSTR1,I4,CSTR)
            UCIBF(57:60) = CSTR(1:4)
            ITEMP = NRCH-I+1
            CALL INTCHR (ITEMP,I4,I0,J,CSTR1(1))
            CALL CARVAR (I4,CSTR1,I4,CSTR)
            UCIBF(72:75) = CSTR(1:4)
            CALL PUTUCI(UCIBF,I1,IDKEY)
 40       CONTINUE
        ELSE IF (REPID.EQ.7) THEN
          IDKEY = EREC
          CALL PREUCI (IDKEY)
          CALL GETML (I7,MLNO)
          UCIBF = 'BMPRAC                              1.     REPORT'
          DO 70 I = 1,NBMP
            CALL INTCHR (BMPID(I),I4,I0,J,CSTR1(1))
            CALL CARVAR (I4,CSTR1,I4,CSTR)
            UCIBF(7:10) = CSTR(1:4)
            CALL INTCHR (REPID,I4,I0,J,CSTR1(1))
            CALL CARVAR (I4,CSTR1,I4,CSTR)
            UCIBF(50:53) = CSTR(1:4)
            CALL INTCHR (MLNO,I4,I0,J,CSTR1(1))
            CALL CARVAR (I4,CSTR1,I4,CSTR)
            UCIBF(57:60) = CSTR(1:4)
            ITEMP = NBMP-I+1
            CALL INTCHR (ITEMP,I4,I0,J,CSTR1(1))
            CALL CARVAR (I4,CSTR1,I4,CSTR)
            UCIBF(72:75) = CSTR(1:4)
            CALL PUTUCI(UCIBF,I1,IDKEY)
 70       CONTINUE
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE REPEXSO
     I                  (NCONS,CTSTYP)
C
C     + + + PURPOSE + + +
C     add appropriate ext src entries for point loads
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NCONS
      CHARACTER*4   CTSTYP(NCONS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NCONS - number of constituents
C     CTSTYP- array constituent time series types
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        BLKID,SREC,EREC,I,KEY,I1,I2,I0
      CHARACTER*1    CSTR1(2)
      CHARACTER*80   UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL       GETUCI,GETSE,INTCHR,PUTUCI
C
C     + + + INPUT FORMATS + + +
 1030 FORMAT (6X,I4,60X)
 1040 FORMAT (49X,I4,27X)
 1050 FORMAT (28X,F10.3,42X)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I2 = 2
C
      BLKID = 5
      CALL GETSE (BLKID,I1,
     O            SREC,EREC)
      IF (SREC.NE.0) THEN
C       external sources block exists
        DO 10 I = 1,NCONS
C         look for an external source of each constituent
          IF (CTSTYP(I).NE.'    ') THEN
            KEY = SREC
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
            IF (KEY.NE.EREC) THEN
C             some records exist
 100          CONTINUE
C               look for this rchres id
                IF (UCIBF(1:3).EQ.'WDM' .AND.
     1              UCIBF(44:49).EQ.'RCHRES' .AND.
     1              UCIBF(59:64).EQ.'INFLOW' .AND.
     1              UCIBF(12:15).EQ.CTSTYP(I)) THEN
C                 this looks like a point load
                  UCIBF(44:71) = 'REPORT   6     INPUT  TIMSER'
                  CALL INTCHR (I,I2,I0,K,CSTR1(1))
                  UCIBF(72:72) = CSTR1(1)
                  UCIBF(73:73) = CSTR1(2)
                  CALL PUTUCI(UCIBF,I1,KEY)
                END IF
C               get next record
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
              IF (KEY.NE.EREC) GO TO 100
            END IF
          END IF
 10     CONTINUE
      END IF
C
      RETURN
      END
