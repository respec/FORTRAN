C
C
C
      SUBROUTINE   UCIMAK
     I                    (MESSFL,MESSU,
     I                     EXPFG)
C
C     + + + PURPOSE + + +
C     make a uci file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,MESSU,EXPFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - unit number of message file containing info needed
C     MESSU  - unit number of output file
C     EXPFG  - expert system flag
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       ID,SREC,EREC,I1,I0,SGRP,SCLU,PREREC,TYPREC,I8,
     1              INITFG,CLEN,CONT,KWDDIM(1),KWDTYP(1),KCNT,PCNT,
     2              ECOUNT,IM1,I12,ICNT,RETCOD,KEY,IM2,I,J,COPCNT,
     3              KGRP,OPERNO,ENDSAV,K,KEY1,KEY2,TMPDSN,IFOUND,
     4              EXTTAR,TMPKEY,MLREC,ITMP,MLEND,DSN,FSTART,
     5              ORCH(MXRCH)
      REAL          AREASM(MXLAND),TAREA,MFACT,RDIF
      CHARACTER*1   CHSTR1(20),KWDLIB(12),TNMBUF(12),CSCEN1(8)
      CHARACTER*4   CHR4,CEND
      CHARACTER*12  TNAME
      CHARACTER*80  TXT
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR1,CHSTR)
      CHARACTER*20 CHSTR
C
C     + + + FUNCTIONS + + +
      INTEGER       LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL      INIUCI,ADDBLB,PUTUCI,WMSGTT,KEYUCI,DMPKEY,PUTGLO
      EXTERNAL      GETUCI,PUTKWD,CVARAR,REPUCI,GETSE,RCHORD
      EXTERNAL      PUTNXT,ADDTAB,DELUCI,LENSTR,WRFTAB
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (12A1,2I4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('      PERLND     ',I3)
 2010 FORMAT ('      IMPLND     ',I3)
 2020 FORMAT ('      RCHRES     ',I3)
 2090 FORMAT ('PERLND ',I3,18X,F10.2,'     RCHRES ',I3,'    ',I3)
 2100 FORMAT ('IMPLND ',I3,18X,F10.2,'     RCHRES ',I3,'    ',I3)
 2110 FORMAT ('RCHRES ',I3,33X,'RCHRES ',I3,'      3')
 2120 FORMAT ('RCHRES ',I3,' HYDR   RO       ',F10.7,' SUM  WDM   ',
     1        I4,' FLOW     ENGL AGGR REPL')
 2122 FORMAT ('RCHRES ',I3,' ROFLOW ROVOL  1  ',G10.4,'     WDM   ',
     1        I4,' SIMQ     ENGL AGGR REPL')
 2123 FORMAT ('COPY   ',I3,' OUTPUT MEAN   1  ',G10.4,'     WDM   ',
     1        I4,' SURO     ENGL AGGR REPL')
 2124 FORMAT ('COPY   ',I3,' OUTPUT MEAN   2  ',G10.4,'     WDM   ',
     1        I4,' IFWO     ENGL AGGR REPL')
 2125 FORMAT ('COPY   ',I3,' OUTPUT MEAN   3  ',G10.4,'     WDM   ',
     1        I4,' AGWO     ENGL AGGR REPL')
 2126 FORMAT ('COPY   ',I3,' OUTPUT MEAN   4  ',G10.4,'     WDM   ',
     1        I4,' PETX     ENGL AGGR REPL')
 2127 FORMAT ('COPY   ',I3,' OUTPUT MEAN   5  ',G10.4,'     WDM   ',
     1        I4,' SAET     ENGL AGGR REPL')
 2128 FORMAT ('COPY   ',I3,' OUTPUT MEAN   6  ',G10.4,'AVER WDM   ',
     1        I4,' UZSX     ENGL AGGR REPL')
 2129 FORMAT ('COPY   ',I3,' OUTPUT MEAN   7  ',G10.4,'AVER WDM   ',
     1        I4,' LZSX     ENGL AGGR REPL')
 2131 FORMAT ('WDM  ',I5,1X,A4,'     ENGL                   PERLND ',
     1        I3,'     EXTNL  PREC')
 2140 FORMAT ('WDM  ',I5,1X,A4,'     ENGL                   IMPLND ',
     1        I3,'     EXTNL  PREC')
 2145 FORMAT ('WDM  ',I5,' FLOW     ENGL      1.983471DIV  RCHRES ',I3,
     1        '     EXTNL  IVOL')
 2147 FORMAT ('WDM  ',I5,' FLOW     ENGL    ',F10.6,'DIV  RCHRES ',I3,
     1        '     EXTNL  IVOL')
 2150 FORMAT ('WDM  ',I5,' EVAP     ENGL          0.75DIV  PERLND ',
     1        I3,'     EXTNL  PETINP')
 2160 FORMAT ('WDM  ',I5,' EVAP     ENGL          0.75DIV  IMPLND ',
     1        I3,'     EXTNL  PETINP')
 2170 FORMAT ('      COPY       ',I3)
 2180 FORMAT (I5,'         0    7')
 2190 FORMAT ('PERLND ',I3,18X,F10.2,'     COPY   ',I3,'      4')
 2200 FORMAT ('IMPLND ',I3,18X,F10.2,'     COPY   ',I3,'      5')
C
C     + + + END SPECIFICATIONS + + +
C
      IM2= -2
      IM1= -1
      I0 = 0
      I1 = 1
      I8 = 8
      I12= 12
C
C     initialize uci file in memory
      CALL INIUCI (MESSFL,MESSU,
     O             CEND)
C
C
C     build uci skeleton, starting with 'run' and 'end run'
      PREREC= 1
      TXT = 'RUN'
      TYPREC= 0
C     save the record
      CALL PUTUCI(TXT,TYPREC,PREREC)
      PREREC= PREREC+ 1
C     put blank line
      TXT = ' '
      CALL PUTNXT (IM2,
     M             TXT,PREREC)
C     put end run line
      TXT = 'END RUN'
      TYPREC= 0
C     save the record
      CALL PUTUCI(TXT,TYPREC,PREREC)
C
C
C     get major keywords (RUN)
      SCLU  = 201
      SGRP  = 21
      INITFG= 1
      CLEN  = 20
      CALL WMSGTT (MESSFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1000)  KWDLIB,KWDDIM,KWDTYP
C     look for major keywords
      CLEN  = 4
      CALL KEYUCI (I1,CLEN,I0,I0,I1,KWDLIB,KWDDIM,KWDTYP,
     M             ECOUNT,
     O             KCNT)
      CALL DMPKEY
C
C
C     global block
      KEY = 3
      TXT = 'GLOBAL'
      CALL PUTNXT (I0,
     M             TXT,KEY)
      SREC= KEY
C     put four dummy lines
      DO 8 I = 1,4
        TXT = 'to be global block line'
        CALL PUTNXT (I0,
     M               TXT,KEY)
 8    CONTINUE
C     put end global
      TXT = 'END GLOBAL'
      CALL PUTNXT (I0,
     M             TXT,KEY)
      EREC= KEY
C     data structure needs to know that global block now exists
      ID = 2
      TNAME = 'GLOBAL      '
      CALL CVARAR (I12,TNAME,I12,TNMBUF)
      CALL PUTKWD (I1,ID,TNMBUF,SREC,EREC,
     O             RETCOD)
      CALL PUTGLO (SDATIM,EDATIM,OUTLEV,SPOUT,
     I             RESMFG,RUNFG,UNIT,RNINFO)
C     put blank line
      TXT = ' '
      CALL PUTNXT (IM2,
     M             TXT,EREC)
C
C
C     files block
      ID   = 12
      SCLU = 53
      SGRP = 50
      CALL ADDBLB (MESSFL,SCLU,SGRP,ID,
     M             SREC,EREC)
      KEY = SREC
      CALL GETUCI (IM1,
     M             KEY,
     O             TXT)
      TXT = 'WDM        24   '//WDMNAM
      CALL PUTNXT (I0,
     M             TXT,KEY)
      CALL CVARAR(I8,CSCEN,I8,CSCEN1)
      TXT = 'MESSU      25   '//CSCEN(1:LENSTR(I8,CSCEN1))//'.ech'
      CALL PUTNXT (I0,
     M             TXT,KEY)
      TXT = '           26   '//CSCEN(1:LENSTR(I8,CSCEN1))//'.out'
      CALL PUTUCI (TXT,I0,KEY)
C     put blank line
      TXT = ' '
      CALL PUTNXT (IM2,
     M             TXT,EREC)
C
C
C     operation sequence block
      ID   = 3
      SCLU = 53
      SGRP = 21
      CALL ADDBLB (MESSFL,SCLU,SGRP,ID,
     M             SREC,EREC)
      KEY = SREC
      CALL GETUCI (IM1,
     M             KEY,
     O             TXT)
      TXT = '    INGRP              INDELT 01:00'
      CALL REPUCI (KEY,TXT)
      IF (NLANDU.GT.0 .AND. EXPFG.EQ.1) THEN
C       have some land segments to add to block
        J = 0
 40     CONTINUE
          J = J + 1
          PCNT = 0
          ICNT = 0
          DO 30 I = 1,NLANDU
            IF (LANDPI(I).EQ.1) THEN
C             perlnd
              PCNT = PCNT + 1
              WRITE (TXT,2000) (J*100)+PCNT
            ELSE
C             implnd
              ICNT = ICNT + 1
              WRITE (TXT,2010) (J*100)+ICNT
            END IF
            CALL PUTNXT (I0,
     M                   TXT,KEY)
 30       CONTINUE
        IF (J.LT.NMETSG) GO TO 40
      END IF
      IF (NREACH.GT.0) THEN
C       have some reaches to add to block
C       need to order reaches properly
        CALL RCHORD (NREACH,
     O               ORCH)
        DO 10 I = 1,NREACH
          WRITE (TXT,2020) ORCH(I)
          CALL PUTNXT (I0,
     M                 TXT,KEY)
 10     CONTINUE
      END IF
      TXT = '    END INGRP'
      CALL PUTUCI (TXT,I0,KEY)
C     put blank line
      TXT = ' '
      CALL PUTNXT (IM2,
     M             TXT,EREC)
      ENDSAV = EREC
C
C
C     perlnd block
      IF (PCNT.GT.0 .AND. EXPFG.EQ.1) THEN
        KEY = ENDSAV
        TXT = 'PERLND'
        CALL PUTNXT (I0,
     M               TXT,KEY)
        SREC = KEY
        TXT = 'END PERLND'
        CALL PUTNXT (I0,
     M               TXT,KEY)
        ENDSAV = KEY
C       data structure needs to know that perlnd block now exists
        ID = 100
        TNAME = 'PERLND      '
        CALL CVARAR (I12,TNAME,I12,TNMBUF)
        CALL PUTKWD (I1,ID,TNMBUF,SREC,ENDSAV,
     O               RETCOD)
C       put blank line
        TXT = ' '
        CALL PUTNXT (IM2,
     M               TXT,ENDSAV)
C       perlnd activity table
        OPERNO= 1
        KGRP  = 1
        CALL ADDTAB (MESSFL,OPERNO,KGRP)
C       gen info table
        KGRP = 3
        CALL ADDTAB (MESSFL,OPERNO,KGRP)
C       pwat-parm2 table
        KGRP = 11
        CALL ADDTAB (MESSFL,OPERNO,KGRP)
C       pwat-parm4 table
        KGRP = 13
        CALL ADDTAB (MESSFL,OPERNO,KGRP)
      END IF
C
C
      IF (ICNT.GT.0 .AND. EXPFG.EQ.1) THEN
C       implnd block
        KEY = ENDSAV
        TXT = 'IMPLND'
        CALL PUTNXT (I0,
     M               TXT,KEY)
        SREC = KEY
        TXT = 'END IMPLND'
        CALL PUTNXT (I0,
     M               TXT,KEY)
        ENDSAV = KEY
C       data structure needs to know that implnd block now exists
        ID = 100
        TNAME = 'IMPLND      '
        CALL CVARAR (I12,TNAME,I12,TNMBUF)
        CALL PUTKWD (I1,ID,TNMBUF,SREC,ENDSAV,
     O               RETCOD)
C       put blank line
        TXT = ' '
        CALL PUTNXT (IM2,
     M               TXT,ENDSAV)
C       implnd activity table
        OPERNO= 2
        KGRP  = 1
        CALL ADDTAB (MESSFL,OPERNO,KGRP)
C       gen info table
        KGRP = 3
        CALL ADDTAB (MESSFL,OPERNO,KGRP)
C       iwat-parm2 table
        KGRP = 11
        CALL ADDTAB (MESSFL,OPERNO,KGRP)
      END IF
C
C
      IF (NREACH.GT.0) THEN
C       rchres block
        KEY = ENDSAV
        TXT = 'RCHRES'
        CALL PUTNXT (I0,
     M               TXT,KEY)
        SREC = KEY
        TXT = 'END RCHRES'
        CALL PUTNXT (I0,
     M               TXT,KEY)
        ENDSAV = KEY
C       data structure needs to know that rchres block now exists
        ID = 100
        TNAME = 'RCHRES      '
        CALL CVARAR (I12,TNAME,I12,TNMBUF)
        CALL PUTKWD (I1,ID,TNMBUF,SREC,ENDSAV,
     O               RETCOD)
C       put blank line
        TXT = ' '
        CALL PUTNXT (IM2,
     M               TXT,ENDSAV)
C       rchres activity table
        OPERNO= 3
        KGRP  = 1
        CALL ADDTAB (MESSFL,OPERNO,KGRP)
C       print info table
        KGRP = 2
        CALL ADDTAB (MESSFL,OPERNO,KGRP)
C       gen info table
        KGRP = 3
        CALL ADDTAB (MESSFL,OPERNO,KGRP)
C       hydr-parm1 table
        KGRP = 4
        CALL ADDTAB (MESSFL,OPERNO,KGRP)
C       hydr-parm2 table
        KGRP = 5
        CALL ADDTAB (MESSFL,OPERNO,KGRP)
      END IF
      EREC = ENDSAV
C
C
C     external sources block
      IF ((NLANDU.GT.0 .AND. EXPFG.EQ.1) .OR. NUMEXT.GT.0 .OR.
     1    NDSNIN.GT.0) THEN
        ID   = 5
        SCLU = 53
        SGRP = 150
        CALL ADDBLB (MESSFL,SCLU,SGRP,ID,
     M               SREC,EREC)
        KEY = SREC
        CALL GETUCI (IM1,
     M               KEY,
     O               TXT)
        CALL GETUCI (IM1,
     M               KEY,
     O               TXT)
        IF (NLANDU.GT.0 .AND. EXPFG.EQ.1) THEN
C         do records for precip ext sources
          J = 0
          K = 0
 200      CONTINUE
C           do for each perlnd and implnd
            J = J + 1
            IFOUND = 0
 201        CONTINUE
C             look for jth precip dsn
              K = K + 1
              IF (PRCEVP(K).EQ.1) THEN
                TMPDSN = METDSN(K)
                CHR4   = CTSTYP(K)
                IFOUND = 1
              END IF
            IF (IFOUND.EQ.0) GO TO 201
            PCNT = 0
            ICNT = 0
            DO 210 I = 1,NLANDU
C             add a record for each land use
              IF (LANDPI(I).EQ.1) THEN
                PCNT = PCNT + 1
                WRITE (TXT,2131) TMPDSN,CHR4,(J*100)+PCNT
              ELSE
                ICNT = ICNT + 1
                WRITE (TXT,2140) TMPDSN,CHR4,(J*100)+ICNT
              END IF
              CALL PUTNXT (I0,
     M                     TXT,KEY)
 210        CONTINUE
          IF (J.LT.NMETSG) GO TO 200
C         do records for evap ext sources
          K = 0
          J = 0
          TMPDSN = 0
 220      CONTINUE
C           do for each perlnd and implnd
            J = J + 1
            IFOUND = 0
 202        CONTINUE
C             look for jth evap dsn
              K = K + 1
              IF (PRCEVP(K).EQ.2) THEN
                TMPDSN = METDSN(K)
                IFOUND = 1
              END IF
            IF (IFOUND.EQ.0 .AND. K.LT.NEVAP+NMETSG) GO TO 202
            PCNT = 0
            ICNT = 0
            DO 230 I = 1,NLANDU
C             add a record for each land use
              IF (LANDPI(I).EQ.1) THEN
                PCNT = PCNT + 1
                WRITE (TXT,2150) TMPDSN,(J*100)+PCNT
              ELSE
                ICNT = ICNT + 1
                WRITE (TXT,2160) TMPDSN,(J*100)+ICNT
              END IF
              CALL PUTNXT (I0,
     M                     TXT,KEY)
 230        CONTINUE
          IF (J.LT.NMETSG) GO TO 220
        END IF
        IF (NUMEXT.GT.0) THEN
C         add a record for each external source flow data set
          DO 205 I = 1,NUMEXT
            WRITE (TXT,2145) EXTSRC(I,1),EXTSRC(I,2)
            CALL PUTNXT (I0,
     M                   TXT,KEY)
 205      CONTINUE
        END IF
        IF (NDSNIN.GT.0 .AND. EXPFG.EQ.0) THEN
C         add a record for each local inflow
          DO 207 I = 1,NREACH
C           determine data set to use
            IF (NDSNIN.GT.1) THEN
              RDIF= ABS(AREAIN(1) - LAREA(I))
              DSN = 1
              DO 208 J = 2,NDSNIN
                IF (ABS(AREAIN(J) - LAREA(I)).LT.RDIF) THEN
C                 use this data set
                  RDIF= ABS(AREAIN(J) - LAREA(I))
                  DSN = J
                END IF
 208          CONTINUE
            ELSE
              DSN = 1
            END IF
C           calculate area ratio
            RDIF = 1.983471*LAREA(I)/AREAIN(DSN)
            WRITE (TXT,2147) DSNIN(DSN),RDIF,I
            CALL PUTNXT (I0,
     M                   TXT,KEY)
 207      CONTINUE
        END IF
C       put blank line
        TXT = ' '
        CALL PUTNXT (IM2,
     M               TXT,EREC)
      END IF
C
C
C     schematic block
      ID   = 10
      SCLU = 53
      SGRP = 190
      CALL ADDBLB (MESSFL,SCLU,SGRP,ID,
     M             SREC,EREC)
C      DO 142 I = 1,NREACH
CC       sum land use areas for each reach
C        AREASM(I) = 0
C        DO 143 J = 1,NLANDU
C          AREASM(I) = AREASM(I) + AREALR(J,I)
C 143    CONTINUE
C 142  CONTINUE
      TAREA = 0
      DO 142 J = 1,NLANDU
C       sum land use areas for each reach
        AREASM(J) = 0
        DO 143 I = 1,NREACH
          AREASM(J) = AREASM(J) + AREALR(J,I)
          TAREA = TAREA + AREALR(J,I)
 143    CONTINUE
 142  CONTINUE
      IF (TAREA.LT.1.0E-6 .AND. TAREA.GT.-1.0E-6) THEN
C       zero total area, force to a unit area
        TAREA = 1.0
      END IF
      KEY = SREC
      CALL GETUCI (IM1,
     M             KEY,
     O             TXT)
      CALL GETUCI (IM1,
     M             KEY,
     O             TXT)
      IF (NLANDU.GT.0 .AND. EXPFG.EQ.1) THEN
C       have some perlnd/implnd records to put
        ITMP = 0
        DO 145 K = 1,NREACH
          IF (RCHDSN(K,1).NE.0) THEN
C           this will be a copy operation
            ITMP = ITMP + 1
          END IF
C
          DO 148 M = 1,NMETSG
C           check for contributing area from each met segment
            PCNT = 0
            ICNT = 0
            DO 147 I = 1,NLANDU
              IF (UPAREL(I,K,M).LT.-1.0E-6 .OR.
     1            UPAREL(I,K,M).GT.1.0E-6) THEN
C               have a value to add to schematic block
                IF (LANDPI(I).EQ.1) THEN
                  PCNT = PCNT + 1
                  IF (RCHDSN(K,1).NE.0) THEN
C                   need perlnd to copy records here
                    WRITE (TXT,2190) (M*100)+PCNT,
     1                                UPAREL(I,K,M),ITMP
                    CALL PUTNXT (I0,
     M                           TXT,KEY)
                  END IF
                ELSE
                  ICNT = ICNT + 1
                  IF (RCHDSN(K,1).NE.0) THEN
C                   need implnd to copy records here
                    WRITE (TXT,2200) (M*100)+ICNT,
     1                                UPAREL(I,K,M),ITMP
                    CALL PUTNXT (I0,
     M                           TXT,KEY)
                  END IF
                END IF
              END IF
 147        CONTINUE
 148      CONTINUE
C
 145    CONTINUE
        DO 150 K = 1,NREACH
          PCNT = 0
          ICNT = 0
          DO 170 I = 1,NLANDU
            IF (LANDPI(I).EQ.1) THEN
C             look to add perlnd to rchres records
              PCNT = PCNT + 1
              IF (AREALR(I,K).GT.1.0E-6 .OR.
     1            AREALR(I,K).LT.-1.0E-6) THEN
C               area is not zero
                WRITE (TXT,2090) (RCHMET(K)*100)+PCNT,
     1                           AREALR(I,K),K,LANDPI(I)
                CALL PUTNXT (I0,
     M                       TXT,KEY)
              END IF
            ELSE
C             look to add implnd to rchres records
              ICNT = ICNT + 1
              IF (AREALR(I,K).GT.1.0E-6 .OR.
     1            AREALR(I,K).LT.-1.0E-6) THEN
C               area is not zero
                WRITE (TXT,2100) (RCHMET(K)*100)+ICNT,
     1                           AREALR(I,K),K,LANDPI(I)
                CALL PUTNXT (I0,
     M                       TXT,KEY)
              END IF
            END IF
 170      CONTINUE
 150    CONTINUE
      END IF
      IF (NRCONN.GT.0) THEN
C       now put reach to reach connections
        DO 180 K = 1,NRCONN
          WRITE (TXT,2110) R2RCON(K,1),R2RCON(K,2)
          CALL PUTNXT (I0,
     M                 TXT,KEY)
 180    CONTINUE
      END IF
C     put blank line
      TXT = ' '
      CALL PUTNXT (IM2,
     M             TXT,EREC)
C
C
C     mass-link block
      ID   = 11
      SCLU = 53
      SGRP = 200
      CALL ADDBLB (MESSFL,SCLU,SGRP,ID,
     M             SREC,EREC)
C     now add individual mass link headers
      KEY = SREC
      TXT = '  MASS-LINK        3'
      CALL PUTNXT (I0,
     M             TXT,KEY)
      CALL GETUCI (IM1,
     M             KEY,
     O             TXT)
      CALL GETUCI (IM1,
     M             KEY,
     O             TXT)
      TXT = 'RCHRES     ROFLOW                          '//
     1      'RCHRES         INFLOW'
      CALL PUTNXT (I0,
     M             TXT,KEY)
      TXT = '  END MASS-LINK    3'
      CALL PUTNXT (I0,
     M             TXT,KEY)
      MLREC= KEY
      IF (ICNT.GT.0 .AND. EXPFG.EQ.1) THEN
C       put a mass link for implnd
        KEY1 = SREC
        KEY2 = SREC
        CALL ADDBLB (MESSFL,SCLU,SGRP,ID,
     M               KEY1,KEY2)
        TXT = '  MASS-LINK        2'
        CALL REPUCI (KEY1,TXT)
        TXT = '  END MASS-LINK    2'
        CALL REPUCI (KEY2,TXT)
        CALL GETUCI (IM1,
     M               KEY1,
     O               TXT)
        CALL GETUCI (IM1,
     M               KEY1,
     O               TXT)
        TXT = 'IMPLND     IWATER SURO       0.0833333     RCHRES'//
     1        '         INFLOW IVOL'
        CALL PUTUCI (TXT,I0,KEY1)
      END IF
      IF (PCNT.GT.0 .AND. EXPFG.EQ.1) THEN
C       put a mass link for perlnd
        KEY1 = SREC
        KEY2 = SREC
        CALL ADDBLB (MESSFL,SCLU,SGRP,ID,
     M               KEY1,KEY2)
        TXT = '  MASS-LINK        1'
        CALL REPUCI (KEY1,TXT)
        TXT = '  END MASS-LINK    1'
        CALL REPUCI (KEY2,TXT)
        CALL GETUCI (IM1,
     M               KEY1,
     O               TXT)
        CALL GETUCI (IM1,
     M               KEY1,
     O               TXT)
        TXT = 'PERLND     PWATER PERO       0.0833333     RCHRES'//
     1        '         INFLOW IVOL'
        CALL PUTUCI (TXT,I0,KEY1)
      END IF
C     put blank line
      TXT = ' '
      CALL PUTNXT (IM2,
     M             TXT,EREC)
      MLEND = EREC
C
C
C     external targets block
      EXTTAR = 0
      DO 189 K = 1,NREACH
        IF (RCHDSN(K,1).NE.0) THEN
C         need this table
          EXTTAR = 1
        END IF
 189  CONTINUE
      IF (EXTTAR.EQ.1) THEN
C       need to add external targets block
        ID   = 8
        SCLU = 53
        SGRP = 180
        CALL ADDBLB (MESSFL,SCLU,SGRP,ID,
     M               SREC,EREC)
        KEY = SREC
        CALL GETUCI (IM1,
     M               KEY,
     O               TXT)
        CALL GETUCI (IM1,
     M               KEY,
     O               TXT)
        DO 190 K = 1,NREACH
          IF (RCHDSN(K,1).NE.0) THEN
C           add records for output from reaches
            MFACT = 1.000
C           MFACT = 12/TAREA
            WRITE (TXT,2120) K,MFACT,RCHDSN(K,1)
            CALL PUTNXT (I0,
     M                   TXT,KEY)
          END IF
 190    CONTINUE
        KEY = SREC
        CALL GETUCI (IM1,
     M               KEY,
     O               TXT)
        CALL GETUCI (IM1,
     M               KEY,
     O               TXT)
        COPCNT = 0
        IF (EXPFG.EQ.1) THEN
C         may want to put records for copy operations
          DO 195 K = 1,NREACH
            IF (RCHDSN(K,1).NE.0) THEN
C             will want to add records here
              COPCNT = COPCNT + 1
C               add records for copy operations
                MFACT = 12/UPAREA(K)
                WRITE (TXT,2122) K,MFACT,RCHDSN(K,2)
                CALL PUTNXT (I0,
     M                       TXT,KEY)
                MFACT = 1/UPAREA(K)
                WRITE (TXT,2123) COPCNT,MFACT,RCHDSN(K,3)
                CALL PUTNXT (I0,
     M                       TXT,KEY)
                WRITE (TXT,2124) COPCNT,MFACT,RCHDSN(K,4)
                CALL PUTNXT (I0,
     M                       TXT,KEY)
                WRITE (TXT,2125) COPCNT,MFACT,RCHDSN(K,5)
                CALL PUTNXT (I0,
     M                       TXT,KEY)
                WRITE (TXT,2126) COPCNT,MFACT,RCHDSN(K,6)
                CALL PUTNXT (I0,
     M                       TXT,KEY)
                WRITE (TXT,2127) COPCNT,MFACT,RCHDSN(K,7)
                CALL PUTNXT (I0,
     M                       TXT,KEY)
                WRITE (TXT,2128) COPCNT,MFACT,RCHDSN(K,8)
                CALL PUTNXT (I0,
     M                       TXT,KEY)
                WRITE (TXT,2129) COPCNT,MFACT,RCHDSN(K,9)
                CALL PUTNXT (I0,
     M                       TXT,KEY)
            END IF
 195      CONTINUE
        END IF
C       put blank line
        TXT = ' '
        CALL PUTNXT (IM2,
     M               TXT,EREC)
        MLEND = EREC
        IF (COPCNT.GT.0 .AND. EXPFG.EQ.1) THEN
C         need to insert copy records in several places
C         need to add copy operations to opn seq block
          ID = 3
          CALL GETSE (ID,I1,
     O                SREC,EREC)
          KEY = SREC
          CALL GETUCI (IM1,
     M                 KEY,
     O                 TXT)
 197      CONTINUE
C           scan through block looking for 1st rchres
            TMPKEY = KEY
            CALL GETUCI (I0,
     M                   KEY,
     O                   TXT)
          IF (TXT(7:12).NE.'RCHRES') GO TO 197
C         put copy operations before first rchres
          KEY = TMPKEY
          DO 198 I = 1,COPCNT
            WRITE (TXT,2170) I
            CALL PUTNXT (I0,
     M                   TXT,KEY)
 198      CONTINUE
C         need to add copy operation block
          KEY = ENDSAV
          TXT = 'COPY'
          CALL PUTNXT (I0,
     M                 TXT,KEY)
          SREC = KEY
          TXT = '  TIMESERIES'
          CALL PUTNXT (I0,
     M                 TXT,KEY)
          WRITE (TXT,2180) COPCNT
          CALL PUTNXT (I0,
     M                 TXT,KEY)
          TXT = '  END TIMESERIES'
          CALL PUTNXT (I0,
     M                 TXT,KEY)
          TXT = 'END COPY'
          CALL PUTNXT (I0,
     M                 TXT,KEY)
          ENDSAV = KEY
C         data structure needs to know that copy block now exists
          ID = 100
          TNAME = 'COPY        '
          CALL CVARAR (I12,TNAME,I12,TNMBUF)
          CALL PUTKWD (I1,ID,TNMBUF,SREC,ENDSAV,
     O                 RETCOD)
C         put blank line
          TXT = ' '
          CALL PUTNXT (IM2,
     M                 TXT,ENDSAV)
          IF (ICNT.GT.0) THEN
C           put a mass link for implnd-copy
            ID   = 11
            SCLU = 53
            SGRP = 200
            KEY1 = MLREC
            KEY2 = MLREC
            CALL ADDBLB (MESSFL,SCLU,SGRP,ID,
     M                   KEY1,KEY2)
            TXT = '  MASS-LINK        5'
            CALL REPUCI (KEY1,TXT)
            TXT = '  END MASS-LINK    5'
            CALL REPUCI (KEY2,TXT)
            CALL GETUCI (IM1,
     M                   KEY1,
     O                   TXT)
            CALL GETUCI (IM1,
     M                   KEY1,
     O                   TXT)
            TXT = 'IMPLND     IWATER SURO                     COPY  '//
     1            '         INPUT  MEAN   1'
            CALL PUTNXT (I0,
     M                   TXT,KEY1)
            TXT = 'IMPLND     IWATER PET                      COPY  '//
     1            '         INPUT  MEAN   5'
            CALL PUTNXT (I0,
     M                   TXT,KEY1)
            TXT = 'IMPLND     IWATER IMPEV                    COPY  '//
     1            '         INPUT  MEAN   6'
            CALL PUTUCI (TXT,I0,KEY1)
          END IF
          IF (PCNT.GT.0) THEN
C           put a mass link for perlnd-copy
            ID   = 11
            SCLU = 53
            SGRP = 200
            KEY1 = MLREC
            KEY2 = MLREC
            CALL ADDBLB (MESSFL,SCLU,SGRP,ID,
     M                   KEY1,KEY2)
            TXT = '  MASS-LINK        4'
            CALL REPUCI (KEY1,TXT)
            TXT = '  END MASS-LINK    4'
            CALL REPUCI (KEY2,TXT)
            CALL GETUCI (IM1,
     M                   KEY1,
     O                   TXT)
            CALL GETUCI (IM1,
     M                   KEY1,
     O                   TXT)
            TXT = 'PERLND     PWATER SURO                     COPY  '//
     1            '         INPUT  MEAN   1'
            CALL PUTNXT (I0,
     M                   TXT,KEY1)
            TXT = 'PERLND     PWATER IFWO                     COPY  '//
     1            '         INPUT  MEAN   2'
            CALL PUTNXT (I0,
     M                   TXT,KEY1)
            TXT = 'PERLND     PWATER AGWO                     COPY  '//
     1            '         INPUT  MEAN   3'
            CALL PUTNXT (I0,
     M                   TXT,KEY1)
            TXT = 'PERLND     PWATER PET                      COPY  '//
     1            '         INPUT  MEAN   4'
            CALL PUTNXT (I0,
     M                   TXT,KEY1)
            TXT = 'PERLND     PWATER TAET                     COPY  '//
     1            '         INPUT  MEAN   5'
            CALL PUTNXT (I0,
     M                   TXT,KEY1)
            TXT = 'PERLND     PWATER UZS                      COPY  '//
     1            '         INPUT  MEAN   6'
            CALL PUTNXT (I0,
     M                   TXT,KEY1)
            TXT = 'PERLND     PWATER LZS                      COPY  '//
     1            '         INPUT  MEAN   7'
            CALL PUTUCI (TXT,I0,KEY1)
          END IF
        END IF
      END IF
C
C
C     ftables block
      ID   = 4
      SCLU = 53
      SGRP = 34
      SREC = MLEND
      EREC = MLEND
      CALL ADDBLB (MESSFL,SCLU,SGRP,ID,
     M             SREC,EREC)
      FSTART = SREC
C     delete unwanted ftables comment lines
      CALL GETUCI (IM1,
     M             SREC,
     O             TXT)
      CALL DELUCI (SREC)
      CALL GETUCI (IM1,
     M             SREC,
     O             TXT)
      CALL DELUCI (SREC)
C     create ftables
      CALL WRFTAB (MXRCH,MAXRAT,MAXROW,UPAREA,AREA,NUMRAT,
     I             I0,FSTART,FTABNO,NUMROW,NREACH,LEN,RAT)
C
C     put blank line
      TXT = ' '
      CALL PUTNXT (IM2,
     M             TXT,EREC)
C
      RETURN
      END
C
C
C
      SUBROUTINE   RCHORD
     I                    (NCNT,
     O                     ORCH)
C
C     + + + PURPOSE + + +
C     order reaches for operation sequence block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NCNT,ORCH(NCNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NCNT   - number of reaches
C     ORCH   - ordered array of reaches
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,K,SRCPOS,TARPOS,IOKAY
C
C     + + + END SPECIFICATIONS + + +
C
C     fill array with default reach order
      DO 5 I = 1,NREACH
        ORCH(I) = I
 5    CONTINUE
C
 7    CONTINUE
C       keep coming back here until order checks out
        IOKAY = 1
        DO 10 K = 1,NRCONN
C         check each reach to reach connection
          DO 15 I = 1,NREACH
            IF (ORCH(I).EQ.R2RCON(K,1)) THEN
              SRCPOS = I
            END IF
            IF (ORCH(I).EQ.R2RCON(K,2)) THEN
              TARPOS = I
            END IF
 15       CONTINUE
          IF (SRCPOS.GT.TARPOS) THEN
C           the source needs to be before the target
            IOKAY = 0
            ORCH(TARPOS) = R2RCON(K,1)
            ORCH(SRCPOS) = R2RCON(K,2)
          END IF
 10     CONTINUE
      IF (IOKAY.EQ.0) GO TO 7
C
      RETURN
      END
C
C
C
      SUBROUTINE   ADDTAB
     I                    (MESSFL,OPERNO,KGRP)
C
C     + + + PURPOSE + + +
C     add a table for this operation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,OPERNO,KGRP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     OPERNO - operation number (1-perlnd,2-implnd)
C     KGRP   - table number (1-activity,2-print info)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       TCLU,TGRP,I12,SREC,EREC,KEY,I0,J,I,CNT,
     1              I1,IDNO,NGRP
      CHARACTER*1   TNMBUF(12)
      CHARACTER*12  TNAME
      CHARACTER*80  TXT
C
C     + + + EXTERNALS + + +
      EXTERNAL      CVARAR,ESTADD,REPUCI,PUTNXT,GETUCI,GETSE
C
C     + + + OUTPUT FORMATS + + +
 2030 FORMAT (2X,I3,'         0    0    1',9('    0'))
 2040 FORMAT (2X,I3,5X,A12,'            1    1    1    1    0    0')
 2050 FORMAT (2X,I3,'         0    0    1',3('    0'))
 2060 FORMAT (2X,I3,5X,A12,'            1    1    1    0    0')
 2070 FORMAT (2X,I3,'         1',9('    0'))
 2075 FORMAT (2X,I3,7X,I3,52X,I3)
 2080 FORMAT (2X,I3,5X,A12,10X,I3,'    1    1    1   26',
     1        '    0  ',I3)
 2210 FORMAT (2X,I3,5X,7F10.3)
 2220 FORMAT (2X,I3,6X,4I3,2X,5I3,5X,5I3,5X,5I3)
 2230 FORMAT (2X,I3,10X,I5,5F10.2)
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      I1   = 1
      I12  = 12
C
      TCLU = 120 + OPERNO
      TGRP = 100 + KGRP
      NGRP = KGRP
C
      IF (OPERNO.EQ.1 .AND. KGRP.EQ.1) THEN
C       set table name
        TNAME = 'ACTIVITY    '
      ELSE IF (OPERNO.EQ.1 .AND. KGRP.EQ.3) THEN
        TNAME = 'GEN-INFO    '
      ELSE IF (OPERNO.EQ.1 .AND. KGRP.EQ.11) THEN
        TNAME = 'PWAT-PARM2  '
        NGRP  = 14
      ELSE IF (OPERNO.EQ.1 .AND. KGRP.EQ.13) THEN
        TNAME = 'PWAT-PARM4  '
        NGRP  = 16
      ELSE IF (OPERNO.EQ.2 .AND. KGRP.EQ.1) THEN
        TNAME = 'ACTIVITY    '
      ELSE IF (OPERNO.EQ.2 .AND. KGRP.EQ.3) THEN
        TNAME = 'GEN-INFO    '
      ELSE IF (OPERNO.EQ.2 .AND. KGRP.EQ.11) THEN
        TNAME = 'IWAT-PARM2  '
        NGRP  = 13
      ELSE IF (OPERNO.EQ.3 .AND. KGRP.EQ.1) THEN
        TNAME = 'ACTIVITY    '
      ELSE IF (OPERNO.EQ.3 .AND. KGRP.EQ.2) THEN
        TNAME = 'PRINT-INFO  '
      ELSE IF (OPERNO.EQ.3 .AND. KGRP.EQ.3) THEN
        TNAME = 'GEN-INFO    '
      ELSE IF (OPERNO.EQ.3 .AND. KGRP.EQ.4) THEN
        TNAME = 'HYDR-PARM1  '
      ELSE IF (OPERNO.EQ.3 .AND. KGRP.EQ.5) THEN
        TNAME = 'HYDR-PARM2  '
      END IF
      CALL CVARAR (I12,TNAME,I12,TNMBUF)
C
C     does this table exist, if not add it
      IDNO = (OPERNO*1000)+NGRP
      CALL GETSE (IDNO,I1,
     O            SREC,EREC)
      IF (SREC.EQ.0) THEN
C       this table does not exist
        CALL ESTADD (MESSFL,TCLU,TGRP,NGRP,OPERNO,I0,
     O               SREC,EREC)
      END IF
C
C     add lines to table for each land or reach segment
      KEY = SREC
      CALL GETUCI (I0,
     M             KEY,
     O             TXT)
      IF (OPERNO.EQ.1 .OR. OPERNO.EQ.2) THEN
C       adding a land segment table
        J = 0
 10     CONTINUE
          J = J + 1
          CNT = 0
          DO 20 I = 1,NLANDU
            IF (OPERNO.EQ.1 .AND. LANDPI(I).EQ.1) THEN
              CNT = CNT + 1
              IF (KGRP.EQ.1) THEN
                WRITE (TXT,2030) (J*100)+CNT
              ELSE IF (KGRP.EQ.3) THEN
                WRITE (TXT,2040) (J*100)+CNT,CLAND(I)
              ELSE IF (KGRP.EQ.11) THEN
                WRITE (TXT,2210) (J*100)+CNT,FOREST(I,J),LZSN(I,J),
     1            INFILT(I,J),LSUR(I,J),SLSUR(I,J),KVARY(I,J),AGWRC(I,J)
              ELSE IF (KGRP.EQ.13) THEN
                WRITE (TXT,2210) (J*100)+CNT,CEPSC(I,J),UZSN(I,J),
     1            NSUR(I,J),INTFW(I,J),IRC(I,J),LZETP(I,J)
              END IF
              IF (CNT.EQ.1 .AND. J.EQ.1) THEN
                CALL REPUCI (KEY,TXT)
              ELSE
                CALL PUTNXT (I0,
     M                       TXT,KEY)
              END IF
            ELSE IF (OPERNO.EQ.2 .AND. LANDPI(I).EQ.2) THEN
              CNT = CNT + 1
              IF (KGRP.EQ.1) THEN
                WRITE (TXT,2050) (J*100)+CNT
              ELSE IF (KGRP.EQ.3) THEN
                WRITE (TXT,2060) (J*100)+CNT,CLAND(I)
              ELSE IF (KGRP.EQ.11) THEN
                WRITE (TXT,2210) (J*100)+CNT,LSUR(I,J),SLSUR(I,J),
     1            NSUR(I,J),RETSC(I,J)
              END IF
              IF (CNT.EQ.1 .AND. J.EQ.1) THEN
                CALL REPUCI (KEY,TXT)
              ELSE
                CALL PUTNXT (I0,
     M                       TXT,KEY)
              END IF
            END IF
 20       CONTINUE
        IF (J.LT.NMETSG) GO TO 10
      ELSE IF (OPERNO.EQ.3) THEN
C       adding a rchres table
        DO 30 I = 1,NREACH
          IF (KGRP.EQ.1) THEN
            WRITE (TXT,2070) I
          ELSE IF (KGRP.EQ.2) THEN
            WRITE (TXT,2075) I,R2HYDR(I),R2PYR(I)
          ELSE IF (KGRP.EQ.3) THEN
            WRITE (TXT,2080) I,CRCH(I),R3NEXI(I),R3LKFG(I)
          ELSE IF (KGRP.EQ.4) THEN
            WRITE (TXT,2220) I,(R4HYFG(J,I),J=1,19)
          ELSE IF (KGRP.EQ.5) THEN
            WRITE (TXT,2230) I,FTABNO(I),LEN(I),DELTH(I),STCOR(I),
     1                       KS(I),DB50(I)
          END IF
          IF (I.EQ.1) THEN
            CALL REPUCI (KEY,TXT)
          ELSE
            CALL PUTNXT (I0,
     M                   TXT,KEY)
          END IF
 30     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ADDBLB
     I                   (MESSFL,SCLU,TGRP,IDNO,
     M                    SREC,EREC)
C
C     + + + PURPOSE + + +
C     create a block if it does not exist, return starting and
C     ending records of created block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        MESSFL,SCLU,TGRP,IDNO,SREC,EREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - cluster number of tables
C     TGRP   - group number of table
C     IDNO   - id number for this block
C     SREC   - starting record of new block
C     EREC   - ending record of new block
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      CLEN,CONT,SGRP,I,IM1,INITFG,
     $             KWDTYP(25),TCLU
      CHARACTER*12 TNAME
      CHARACTER*80 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTT,ADDBAT
C
C     + + + INPUT FORMATS + + +
 1080 FORMAT (16X,I4)
C
C     + + + END SPECIFICATIONS + + +
C
      IM1= -1
C
C     find corresponding keyword
      SGRP  = 22
      TCLU  = 201
      INITFG= 1
      CLEN  = 80
      I     = 0
      TNAME = ' '
 20   CONTINUE
        I= I+ 1
        CALL WMSGTT (MESSFL,TCLU,SGRP,INITFG,
     M               CLEN,
     O               CHSTR1,CONT)
        READ(CHSTR,1080) KWDTYP(I)
        IF (KWDTYP(I).EQ.IDNO) THEN
C         found keyword we need
          TNAME = CHSTR(1:12)
        END IF
        INITFG= 0
        CLEN  = 80
      IF (CONT.EQ.1 .AND. TNAME.EQ.'            ') GO TO 20
C
      CALL ADDBAT (MESSFL,SCLU,TGRP,IDNO,IM1,TNAME,
     M             SREC,EREC)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTNXT
     I                    (ITYPE,
     M                     TXT,IREC)
C
C     + + + PURPOSE + + +
C     put a line to uci file in memory and advance to next line
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       ITYPE,IREC
      CHARACTER*80  TXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITYPE  - type of record to put
C     IREC   - record number to write after, returned as this record
C     TXT    - text to put
C
C     + + + EXTERNALS + + +
      EXTERNAL   PUTUCI,GETUCI
C
C     + + + END SPECIFICATIONS + + +
C
      CALL PUTUCI (TXT,ITYPE,IREC)
      CALL GETUCI (ITYPE,
     M             IREC,
     O             TXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SIMNET
     I                   (MXRCH,MXLU,NLANDU,DBGLEV,
     I                    CRANK,CLARGE,CSMALL,AREALR,
     I                    ONR,ONR2R,OCON,ONAME,
     O                    OLAREA,OTAREA,
     O                    SNR,SNR2R,SCON,SNAME,
     O                    SLAREA,STAREA,SLAA,STAA,OTAA,
     O                    XID,XDOWN,XUP,XRANK,RETCOD)
C
C     + + + PURPOSE + + +
C     simplify the reach network by:
C       - eliminating reaches with few reaches upstream
C       - maintaining as seperate reaches those that are named,
C         major branches, or exceed a critical percentage of the
C         entire basin
C       - eliminating reaches smaller than a given percentage of
C         the entire basin
C     input parameters describe the original reach network,
C     output parameters describe the simlified reach network.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MXRCH,MXLU,ONR,NLANDU,ONR2R,DBGLEV,CRANK,
     1              OCON(MXRCH,2),SNR,XID(MXRCH),XDOWN(MXRCH),
     2              XUP(MXRCH),SNR2R,SCON(MXRCH,2),XRANK(MXRCH),RETCOD
      REAL          CLARGE,AREALR(10,MXRCH),OLAREA(MXRCH),OTAREA(MXRCH),
     1              SLAREA(MXRCH),STAREA(MXRCH),CSMALL,
     2              SLAA(MXLU,MXRCH),OTAA(MXLU,MXRCH),STAA(MXLU,MXRCH)
      CHARACTER*12  ONAME(MXRCH),SNAME(MXRCH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MXRCH  - maximum number of reaches
C     MXLU   - maximum number of land uses
C     NLANDU - number of land uses
C     DBGLEV - debug level flag: 0-none 1-minimal 5-lots
C     CRANK  - number of upstream segments which defines a segment rank
C              significant enough to keep, suggested value is 5.
C     CLARGE - critical percentage of drainage area of the entire basin
C              at which a new reach is to be created, suggested 0.2
C     CSMALL - critical percentage of drainage area of the entire basin,
C              if less than this value a reach is included in the downstream
C              reach, suggested 0.02
C     AREALR - area of each land use contributing to each original reach
C     ONR    - original number of reach segments
C     ONR2R  - original number of reach to reach connections
C     OCON   - original array of source/target pairs for each connection
C     ONAME  - array of reach names for each segment in original network
C     OLAREA - local area upstream of each original reach
C     OTAREA - total area upstream of each original reach
C     SNR    - simplified number of reaches
C     SNR2R  - number of connections in simplified network
C     SCON   - array of reach to reach connections in simplified network
C     SNAME  - array of reach names for each segment in simplified network
C     SLAREA - local area upstream of each reach in simplified network
C     STAREA - total area upstream of each reach in simplified network
C     SLAA   - array of area of each land use for each reach in new network
C     STAA   - array of total area upstream for each reach in new network
C     OTAA   - array of total area upstream for each reach in orig network
C     XID    - for each reach in original network, reach number of associated
C              reach in simplified network, 0 if dropped from network
C     XDOWN  - for each reach of simplified network, id number of
C              downstream most segment in original network
C     XUP    - for each reach of simplified network, id number of
C              upstream most segment in original network
C     XRANK  - rank for each reach in original network, defined as highest
C              number of upstream reaches in a single path
C     RETCOD - return code  2 - multiple outlets
C                           3 - triple junction
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,K,RUP,RDN,OUTLET,RCURR,NUP,NAMED,JLEN,
     #              TMPDN,TMPUP,DONEFG,NEWNUM,RCUP(2,10),NDN(10),
     #              NORD,ITEMP,IDOWN,IDEND,NTARG,L
      REAL          RTEMP,RTARG,ASUM,AUP,ASUMA(10),AUPA(10)
      CHARACTER*1   CTEMP(12)
      CHARACTER*12  CTMP12
C
C     + + + INTRINSICS + + +
      INTRINSIC     INT,FLOAT
C
C     + + + EXTERNALS + + +
C     from hass library 'util'
      EXTERNAL    ZIPR,ZIPI,INTCHR,CARVAR
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (I12)
C
C     + + + END SPECIFICATIONS + + +
C
C     WRITE (*,*) 'UCICRE:SIMNET'
C     initialize variables to zero
      CALL ZIPR(MXRCH,0.0,OTAREA)
      CALL ZIPR(MXRCH,0.0,OLAREA)
      CALL ZIPI(MXRCH,0,XRANK)
      CALL ZIPI(MXRCH,0,XID)
      CALL ZIPR(MXRCH*MXLU,0.0,SLAA)
      CALL ZIPR(MXRCH*MXLU,0.0,STAA)
      CALL ZIPR(MXRCH*MXLU,0.0,OTAA)
      SNR2R = 0
      RETCOD= 0
C
C     calculate local and cumulative areas for each reach, count highest
C     number of upstream segments along one path, and identify outlet
      OUTLET= 0
      DO 20 J = 1,ONR
C       do for each reach in original network
        DO 10 I = 1,NLANDU
C         sum areas for each land use contributing to each reach
          OLAREA(J) = OLAREA(J)+ AREALR(I,J)
 10     CONTINUE
        OTAREA(J) = OTAREA(J)+ OLAREA(J)
        DO 14 L = 1,NLANDU
          OTAA(L,J) = OTAA(L,J)+ AREALR(L,J)
 14     CONTINUE
        RUP = J
        I   = 1
        K   = 0
 15     CONTINUE
C         look through each reach to reach connection
          IF (RUP .EQ. OCON(I,1)) THEN
            RDN = OCON(I,2)
            OTAREA(RDN) = OTAREA(RDN)+ OLAREA(J)
            DO 16 L = 1,NLANDU
              OTAA(L,RDN) = OTAA(L,RDN)+ AREALR(L,J)
 16         CONTINUE
            K   = K + 1
            IF (K .GT. XRANK(RDN)) THEN
              XRANK(RDN) = K
            END IF
            RUP = RDN
            I   = 1
          ELSE
            I= I + 1
          END IF
        IF (I .LE. ONR2R) GO TO 15
C       been through every reach to reach connection
        IF (OUTLET .EQ. 0) THEN
C         save this reach number as the outlet
          OUTLET = RUP
        ELSE IF (OUTLET .NE. RUP) THEN
C          WRITE (*,*) 'UCICRE:SIMNET:multiple outlets found',OUTLET,RUP
          RETCOD = 2
        END IF
 20   CONTINUE
C
      IF (RETCOD.EQ.0) THEN
        IF (DBGLEV.GE.5) THEN
C         write debug info
C          DO 30 J = 1,ONR
C            WRITE(99,*)'UCICRE:SIMNET:R:L,T,O:',
C     #                  J,OLAREA(J),OTAREA(J),XRANK(J)
C 30       CONTINUE
        END IF
C
C       eliminate needless reaches
        IF (DBGLEV.GE.1) THEN
C         write debug info
          WRITE(99,*) 'UCICRE:SIMNET:OUTLET at:',OUTLET
        END IF
C       first reach is the outlet
        RCURR      = OUTLET
        SNR        = 1
        SLAREA(SNR)= OLAREA(RCURR)
        STAREA(SNR)= OTAREA(RCURR)
        DO 32 L = 1,NLANDU
          SLAA(L,SNR) = AREALR(L,RCURR)
          STAA(L,SNR) = OTAA(L,RCURR)
 32     CONTINUE
        XDOWN(SNR) = RCURR
C       check to see if it is named
        READ (ONAME(RCURR),1000,ERR=42) ITEMP
        CALL INTCHR (SNR,12,1,JLEN,CTEMP)
        CALL CARVAR (12,CTEMP,12,SNAME(SNR))
        GO TO 43
 42     CONTINUE
          SNAME(SNR) = ONAME(RCURR)
 43     CONTINUE
C       find sources to this reach
        NORD     = 1
        NDN(NORD)= RCURR
 50     CONTINUE
          XID(RCURR)= SNR
          NUP = 0
          DO 40 I = 1,ONR2R
C           look through each connection
            IF (OCON(I,2).EQ.RCURR) THEN
C             found a source to the current reach
              RUP = OCON(I,1)
C             check to see if it is named
              READ (ONAME(OCON(I,1)),1000,ERR=45) ITEMP
              NAMED = 0
              GO TO 47
 45           CONTINUE
C                write(*,*) 'named',ONAME(OCON(I,1))
C                write(*,*) 'rank',xrank(rup)
                NAMED = 1
 47           CONTINUE
              IF (XRANK(RUP).LT.CRANK) THEN
C               too small, add area to this reach
                SLAREA(SNR) = SLAREA(SNR) + OTAREA(OCON(I,1))
                DO 48 L = 1,NLANDU
                  SLAA(L,SNR) = SLAA(L,SNR)+ OTAA(L,OCON(I,1))
 48             CONTINUE
C                IF (DBGLEV.GE.5) THEN
C                  WRITE(99,*) 'UCICRE:SIMNET:local',
C     #                      RUP,XRANK(RUP),SLAREA(SNR),OTAREA(OCON(I,1))
C                END IF
              ELSE
C               count number of upstream reaches
                NUP = NUP + 1
                IF (NUP.EQ.3) THEN
                  WRITE(99,*) 'UCICRE:SIMNET:Problem:Triple Junc',rcurr
                  RETCOD = 3
                ELSEIF (NUP.EQ.2) THEN
C                 two upstream reaches of significant size
                  IF (XRANK(RUP).LT.XRANK(RCUP(1,NORD))) THEN
                    RCUP(2,NORD) = RCUP(1,NORD)
                    RCUP(1,NORD) = OCON(I,1)
C                    IF (DBGLEV.GE.5) THEN
C                      WRITE(99,*) 'UCICRE:SIMNET:trib ',
C     #                             RUP,XRANK(RUP),NUP
C                    END IF
                  ELSE
                    RCUP(2,NORD) = OCON(I,1)
C                    IF (DBGLEV.GE.5) THEN
C                      WRITE(99,*) 'UCICRE:SIMNET:main ',
C     #                             RUP,XRANK(RUP),NUP
C                    END IF
                  END IF
                ELSE
                  RCUP(1,NORD) = OCON(I,1)
C                  IF (DBGLEV.GE.5) THEN
C                    WRITE(99,*) 'UCICRE:SIMNET:main?',
C     #                         NORD,RUP,I,RCUP(1,NORD),XRANK(RUP),NUP
C                  END IF
                END IF
              END IF
            END IF
 40       CONTINUE
          IF (RETCOD.EQ.0) THEN
C           finished looking through all reach connections
            IF (NUP.EQ.0) THEN
C             top of (sub)watershed here
C              IF (DBGLEV.GE.1) THEN
C                WRITE(99,*) 'UCICRE:SIMNET:new*1',
C     #                SNR,NORD,RCURR,NDN(NORD),STAREA(SNR),SLAREA(SNR)
C              END IF
              SNR = SNR + 1
              NORD= NORD - 1
              IF (NORD .GT. 0) THEN
C               new reach here
                XUP(SNR-1)  = RCURR
                RCURR       = RCUP(2,NORD)
                NDN(NORD)   = RCURR
                SLAREA(SNR) = OLAREA(RCURR)
                STAREA(SNR) = OTAREA(RCURR)
                DO 51 L = 1,NLANDU
                  SLAA(L,SNR) = AREALR(L,RCURR)
                  STAA(L,SNR) = OTAA(L,RCURR)
 51             CONTINUE
C               check to see if it is named
                READ (ONAME(RCURR),1000,ERR=52) ITEMP
                CALL INTCHR (SNR,12,1,JLEN,CTEMP)
                CALL CARVAR (12,CTEMP,12,SNAME(SNR))
                GO TO 53
 52             CONTINUE
                  SNAME(SNR) = ONAME(RCURR)
 53             CONTINUE
                XDOWN(SNR)  = RCURR
              ELSE
                XUP(SNR-1) = RCURR
C                IF (DBGLEV.GE.5) THEN
C                  WRITE(99,*) 'at the big top'
C                END IF
              END IF
            ELSE IF (NUP.EQ.1) THEN
C             no reach boundary here
              IF (NAMED.EQ.1) THEN
C               named reach
C                write(*,*) 'named, dont add'
C                IF (DBGLEV.GE.5) THEN
C                  WRITE(99,*) 'UCICRE:SIMNET:named',RCUP(1,NORD)
C                END IF
C                IF (DBGLEV.GE.1) THEN
C                  WRITE(99,*) 'UCICRE:SIMNET:named',
C     #                  SNR,NORD,RCURR,NDN(NORD),STAREA(SNR),SLAREA(SNR)
C                END IF
                XUP(SNR)= RCURR
                SNR     = SNR + 1
                RCURR   = RCUP(1,NORD)
                SLAREA(SNR) = OLAREA(RCURR)
                STAREA(SNR) = OTAREA(RCURR)
                DO 54 L = 1,NLANDU
                  SLAA(L,SNR) = AREALR(L,RCURR)
                  STAA(L,SNR) = OTAA(L,RCURR)
 54             CONTINUE
                SNAME(SNR)  = ONAME(RCURR)
                XDOWN(SNR)  = RCURR
                NDN(NORD)  = RCURR
              ELSE
C               not named, just add to existing reach
                RCURR = RCUP(1,NORD)
                SLAREA(SNR) = SLAREA(SNR) + OLAREA(RCURR)
                DO 56 L = 1,NLANDU
                  SLAA(L,SNR) = SLAA(L,SNR) + AREALR(L,RCURR)
 56             CONTINUE
C                IF (DBGLEV.GE.5) THEN
C                  WRITE(99,*) 'UCICRE:SIMNET:nobnd',
C     #                  RCURR,XRANK(RCURR),SLAREA(SNR),OLAREA(RCURR)
C                END IF
              END IF
            ELSE IF (NUP.EQ.2) THEN
C             reach boundary here
C              IF (DBGLEV.GE.5) THEN
C                WRITE(99,*) 'UCICRE:SIMNET:branc',(RCUP(J,NORD),J=1,NUP)
C              END IF
C              IF (DBGLEV.GE.1) THEN
C                WRITE(99,*) 'UCICRE:SIMNET:new*2',
C     #                SNR,NORD,RCURR,NDN(NORD),STAREA(SNR),SLAREA(SNR)
C              END IF
              XUP(SNR) = RCURR
              SNR   = SNR + 1
              RCURR= RCUP(1,NORD)
              SLAREA(SNR) = OLAREA(RCURR)
              STAREA(SNR) = OTAREA(RCURR)
              DO 59 L = 1,NLANDU
                SLAA(L,SNR) = AREALR(L,RCURR)
                STAA(L,SNR) = OTAA(L,RCURR)
 59           CONTINUE
C             check to see if it is named
              READ (ONAME(RCURR),1000,ERR=57) ITEMP
              CALL INTCHR (SNR,12,1,JLEN,CTEMP)
              CALL CARVAR (12,CTEMP,12,SNAME(SNR))
              GO TO 58
 57           CONTINUE
                SNAME(SNR) = ONAME(RCURR)
 58           CONTINUE
              XDOWN(SNR)  = RCURR
              NORD = NORD+ 1
              NDN(NORD)  = RCURR
            END IF
          END IF
        IF (NORD.GT.0 .AND. RETCOD.EQ.0) GO TO 50
      END IF
C
      IF (RETCOD.EQ.0) THEN
        SNR = SNR - 1
C       find connections downstream of each new segment
        DO 60 J = 2,SNR
          DO 55 I = 1,ONR2R
            IF (OCON(I,1).EQ.XDOWN(J)) THEN
C             add this connection to list of new connections
              SNR2R = SNR2R + 1
              SCON(SNR2R,1) = XID(OCON(I,1))
              SCON(SNR2R,2) = XID(OCON(I,2))
            END IF
 55       CONTINUE
 60     CONTINUE
C
C        do 99 i = 1,snr
C          write(*,*) snr,sname(i)
C 99     continue
C
C       if percentage of total area exceeds threshold, create new reach
        IF (SNR.GT.1) THEN
          I = 1
 150      CONTINUE
            IF ((SLAREA(I)/OTAREA(OUTLET)).GT.CLARGE) THEN
C             calculate ideal size of new reach and target area
              NTARG = INT((SLAREA(I)/OTAREA(OUTLET))/CLARGE) + 1
              RTARG = SLAREA(I)/FLOAT(NTARG)
C             find reaches upstream of i
              AUP = 0.0
              DO 101 L = 1,NLANDU
                AUPA(L) = 0.0
 101          CONTINUE
              DO 100 J = 1,SNR2R
                IF (SCON(J,2).EQ.I) THEN
                  AUP = AUP + STAREA(SCON(J,1))
                  DO 102 L = 1,NLANDU
                    AUPA(L) = AUPA(L) + STAA(L,SCON(J,1))
 102              CONTINUE
                END IF
 100          CONTINUE
              ASUM  = OTAREA(XUP(I)) - AUP
              DO 103 L = 1,NLANDU
                ASUMA(L) = OTAA(L,XUP(I)) - AUPA(L)
 103          CONTINUE
              IF (ASUM.LT.RTARG) THEN
                TMPDN = XUP(I)
 110            CONTINUE
C                 look downstream and add some more area
                  TMPUP = TMPDN
                  J = 0
 105              CONTINUE
                    J = J + 1
                    IF (OCON(J,1).EQ.TMPUP) THEN
                      TMPDN = OCON(J,2)
                      ASUM  = OTAREA(TMPDN) - AUP
                      DO 104 L = 1,NLANDU
                        ASUMA(L) = OTAA(L,TMPDN) - AUPA(L)
 104                  CONTINUE
                      J = ONR2R
                    END IF
                  IF (J.LT.ONR2R) GO TO 105
                IF (ASUM.LT.RTARG) GO TO 110
C               found our target area
                IF (ASUM.LT.(0.9*SLAREA(I))) THEN
C                 okay to create a new reach
                  NEWNUM = I + 1
                  SNR    = SNR + 1
                  DO 115 J = SNR,NEWNUM+1,-1
                    XUP(J)   = XUP(J-1)
                    XDOWN(J) = XDOWN(J-1)
                    SLAREA(J)= SLAREA(J-1)
                    STAREA(J)= STAREA(J-1)
                    DO 112 L = 1,NLANDU
                      SLAA(L,J) = SLAA(L,J-1)
                      STAA(L,J) = STAA(L,J-1)
 112                CONTINUE
                    SNAME(J) = SNAME(J-1)
                    READ (SNAME(J),1000,ERR=115) ITEMP
C                   not named
                    CALL INTCHR (J,12,1,JLEN,CTEMP)
                    CALL CARVAR (12,CTEMP,12,SNAME(J))
 115              CONTINUE
                  XUP(NEWNUM)   = XUP(I)
                  XDOWN(NEWNUM) = TMPDN
                  STAREA(NEWNUM)= ASUM + AUP
                  SLAREA(NEWNUM)= ASUM
                  DO 116 L = 1,NLANDU
                    STAA(L,NEWNUM)= ASUMA(L) + AUPA(L)
                    SLAA(L,NEWNUM)= ASUMA(L)
 116              CONTINUE
                  CALL INTCHR (NEWNUM,12,1,JLEN,CTEMP)
                  CALL CARVAR (12,CTEMP,12,SNAME(NEWNUM))
                  DO 130 J = 1,ONR
                    IF (XID(J).GE.NEWNUM) THEN
                      XID(J) = XID(J) + 1
                    ELSE IF (J.EQ.XDOWN(NEWNUM)) THEN
                      XID(J) = NEWNUM
                    END IF
                    IF (XID(J).EQ.I .AND. J.NE.XDOWN(I)) THEN
C                     see if associated with new reach
                      TMPDN = J
 120                  CONTINUE
                        K = 0
 125                    CONTINUE
                          K = K + 1
                          IF (OCON(K,1).EQ.TMPDN) THEN
                            TMPDN = OCON(K,2)
                            K = ONR2R
                          END IF
                        IF (K.LT.ONR2R) GO TO 125
                        IF (TMPDN.EQ.XDOWN(NEWNUM)) THEN
                          XID(J) = NEWNUM
                          DONEFG   = 1
                        ELSE IF (TMPDN.EQ.XDOWN(I)) THEN
                          DONEFG   = 1
                        ELSE
                          DONEFG   = 0
                        END IF
                      IF (DONEFG.EQ.0) GO TO 120
                    END IF
 130              CONTINUE
C                 set new connections
                  DO 135 J = 1,SNR2R
                    IF (SCON(J,1).GE.NEWNUM) THEN
                      SCON(J,1) = SCON(J,1) + 1
                    END IF
                    IF (SCON(J,2).GE.NEWNUM) THEN
                      SCON(J,2) = SCON(J,2) + 1
                    END IF
                    IF (SCON(J,2).EQ.I) THEN
                      SCON(J,2) = NEWNUM
                    END IF
 135              CONTINUE
                  SNR2R = SNR2R + 1
                  SCON(SNR2R,1) = NEWNUM
                  SCON(SNR2R,2) = I
C                 reset values for original reach
                  SLAREA(I) = SLAREA(I) - SLAREA(NEWNUM)
                  DO 137 L = 1,NLANDU
                    SLAA(L,I) = SLAA(L,I) - SLAA(L,NEWNUM)
 137              CONTINUE
                  J = 0
 140              CONTINUE
                    J = J + 1
                    IF (OCON(J,1).EQ.XDOWN(NEWNUM)) THEN
                      XUP(I) = OCON(J,2)
                      J = ONR2R
                    END IF
                  IF (J.LT.ONR2R) GO TO 140
                ELSE
C                 this reach is unbreakable
                  I = I + 1
                END IF
              ELSE
                I = I + 1
              END IF
            ELSE
              I = I + 1
            END IF
          IF (I.LE.SNR) GO TO 150
        END IF
C
C        do 98 i = 1,snr
C          write(*,*) snr,sname(i)
C 98     continue
C
C       eliminate reaches smaller than critial area
        I = 2
 200    CONTINUE
          READ (SNAME(I),1000,ERR=96) ITEMP
C         not named
          NAMED = 0
          GO TO 97
 96       CONTINUE
            NAMED = 1
 97       CONTINUE
          IF ((SLAREA(I)/STAREA(1)).LT.CSMALL .AND. NAMED.EQ.0) THEN
C           this reach is too small, add to downstream
            DO 170 J = 1,SNR2R
              IF (SCON(J,1).EQ.I) THEN
C               this is the connection
                IDOWN = SCON(J,2)
                SLAREA(IDOWN) = SLAREA(IDOWN) + SLAREA(I)
                DO 161 L = 1,NLANDU
                  SLAA(L,IDOWN) = SLAA(L,IDOWN) + SLAA(L,I)
 161            CONTINUE
C               now eliminate this reach from all arrays
                DO 160 K = 1,ONR
                  IF (XID(K).EQ.I) THEN
                    XID(K) = IDOWN
                  ELSE IF (XID(K).GT.I) THEN
                    XID(K) = XID(K) - 1
                  END IF
 160            CONTINUE
                SNR  = SNR - 1
                IDEND= XDOWN(I)
                DO 165 K = I,SNR
                  SLAREA(K)= SLAREA(K+1)
                  STAREA(K)= STAREA(K+1)
                  DO 167 L = 1,NLANDU
                    SLAA(L,K) = SLAA(L,K+1)
                    STAA(L,K) = STAA(L,K+1)
 167              CONTINUE
                  XDOWN(K) = XDOWN(K+1)
                  XUP(K)   = XUP(K+1)
                  SNAME(K) = SNAME(K+1)
                  READ (SNAME(K),1000,ERR=165) ITEMP
C                 not named
                  CALL INTCHR (K,12,1,JLEN,CTEMP)
                  CALL CARVAR (12,CTEMP,12,SNAME(K))
 165            CONTINUE
              END IF
 170        CONTINUE
C           now fix connections involving this reach
            J = 1
 175        CONTINUE
              IF (SCON(J,1).EQ.I) THEN
C               eliminate this record
                SNR2R = SNR2R - 1
                DO 180 K = J,SNR2R
                  SCON(K,1) = SCON(K+1,1)
                  SCON(K,2) = SCON(K+1,2)
 180            CONTINUE
              ELSE IF (SCON(J,2).EQ.I) THEN
C               change target to downstream reach
                SCON(J,2) = IDOWN
                XDOWN(SCON(J,1)-1) = IDEND
              ELSE
                J = J + 1
              END IF
            IF (J.LE.SNR2R) GO TO 175
C           connection numbers higher than i need to be decremented
            DO 185 K = 1,SNR2R
              IF (SCON(K,1).GE.I) THEN
                SCON(K,1) = SCON(K,1) - 1
              END IF
              IF (SCON(K,2).GE.I) THEN
                SCON(K,2) = SCON(K,2) - 1
              END IF
 185        CONTINUE
          ELSE
            I = I + 1
          END IF
        IF (I.LT.SNR) GO TO 200
C
C       new network has been numbered starting at the outlet,
C       reverse order to follow hspf convention
        DO 210 I = 1,ONR
          IF (XID(I).NE.0) THEN
C           reverse numbering of these reaches
            XID(I) = SNR + 1 - XID(I)
          END IF
 210    CONTINUE
        DO 220 I = 1,SNR2R
C         reverse numbering of these reaches
          SCON(I,1) = SNR + 1 - SCON(I,1)
          SCON(I,2) = SNR + 1 - SCON(I,2)
 220    CONTINUE
        DO 230 I = 1,SNR/2
C         reverse order of these array values
          RTEMP          = SLAREA(I)
          SLAREA(I)      = SLAREA(SNR+1-I)
          SLAREA(SNR+1-I)= RTEMP
          DO 225 L = 1,NLANDU
            RTEMP          = SLAA(L,I)
            SLAA(L,I)      = SLAA(L,SNR+1-I)
            SLAA(L,SNR+1-I)= RTEMP
 225      CONTINUE
          RTEMP          = STAREA(I)
          STAREA(I)      = STAREA(SNR+1-I)
          STAREA(SNR+1-I)= RTEMP
          ITEMP          = XDOWN(I)
          XDOWN(I)       = XDOWN(SNR+1-I)
          XDOWN(SNR+1-I) = ITEMP
          ITEMP          = XUP(I)
          XUP(I)         = XUP(SNR+1-I)
          XUP(SNR+1-I)   = ITEMP
          CTMP12         = SNAME(I)
          SNAME(I)       = SNAME(SNR+1-I)
          SNAME(SNR+1-I) = CTMP12
 230    CONTINUE
        DO 240 I = 1,SNR
C         rename reaches according to new order
          READ (SNAME(I),1000,ERR=240) ITEMP
C         not named
          CALL INTCHR (I,12,1,JLEN,CTEMP)
          CALL CARVAR (12,CTEMP,12,SNAME(I))
 240    CONTINUE
      END IF
C
      DO 999 L = 1,20
        WRITE(99,*) 'SIMNET SLAREA ',SLAREA(L)
 999  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   EXSMAK
C
C     + + + PURPOSE + + +
C     make the exs file for this basin
C
C     + + + PARAMETERS + + +
      INTEGER    MXSTRM,   MXSITE
      PARAMETER (MXSTRM=10,MXSITE=20)
      INTEGER    MXCAL
      PARAMETER (MXCAL=20)
      INTEGER    MXCOV,  MXSEN,   MXLINE
      PARAMETER (MXCOV=6,MXSEN=20,MXLINE=10)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER         NSITE,CURSIT,DATCNT,NWDM,DCNVFG,I1,I12,K,I64,
     1                STSTRM(6,MXSTRM),ENSTRM(6,MXSTRM),STATDN(MXSITE),
     2                IPCTFG(12),NSTATS,ISUBJC(23),RETCOD,I0,ARHLOG(2),
     3                IVAR(2),VARCNT(2),UNCALD,OUTFL,PLCLR(MXSEN),
     4                PLNTYP(MXSEN),PPATRN(MXSEN),PSYMBL(MXSEN),QFLAG,
     5                MAPCOV(MXCOV),I,SAIND,SALEN,START(6),ENDDT(6),
     6                INDX(1000),BUFMAX,TS,TU,NVALS,ODSN,OUTLET,DTRAN,
     7                NANAL,ADSN(MXLINE)
      REAL            BAREA(MXSITE),RERTRM(10),
     1                STAT1(MXCAL*MXSITE,33),RHSDT1(33*MXSITE),
     2                RCHNGE(12),RERROR(12,19),RLAT(2),RLNG(2),
     3                RPARM1(100),RPARM2(100),RDERR(100,19),FSTRM,
     4                RDUM1(MXSTRM),RDUM2(MXSTRM),RDUM3(MXSTRM),YX(1000)
      CHARACTER*1     SITNAM(20,MXSITE),BLNK,FILE1(64)
      CHARACTER*4     LWDID(1)
      CHARACTER*8     PLTPRE
      CHARACTER*8     SITENM(MXCAL*MXSITE),CALINM(MXCAL*MXSITE)
      CHARACTER*64    UCIPTH,FILNAM
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   HSXINI,EXSNEW,HSXPUT,ZIPI,WININI,ZIPC,WDBSGC,LENSTR
      EXTERNAL   CMSTRM,TIMDIF,TSBTIM,TSBWDS,TSBGET,CVARAR,CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I12= 12
      I64= 64
      BLNK= ' '
C
C     open .exs file
      OUTFL = 50
      OPEN(UNIT=OUTFL,FILE='basin.exs')
C     set default uci path
      UCIPTH = ' '
      INQUIRE (UNIT=OUTFL,NAME=FILNAM)
      CALL CVARAR(I64,FILNAM,I64,FILE1)
      I= LENSTR(I64,FILE1)
      CALL CARVAR(I-9,FILE1,I-9,UCIPTH)
C
C     initialize exs file
      CALL HSXINI (MXSITE,
     O             ISUBJC,RERTRM,
     O             STATDN,RHSDT1)
C     initialize history and sensitivity analyses
      CALL EXSNEW
     O            (NSTATS,RCHNGE,RERROR,IVAR,DCNVFG)
C     init windows
      CALL WININI
C     initialize other parameters
      NANAL    = 0
      MAPCOV(1)= 3
      MAPCOV(2)= 10
      MAPCOV(3)= 0
      MAPCOV(4)= 4
      MAPCOV(5)= 5
      MAPCOV(6)= 0
      ARHLOG(1)= 1
      ARHLOG(2)= 1
      RLAT(1)  = 24
      RLAT(2)  = 50
      RLNG(1)  = 66
      RLNG(2)  = 125
      CALL ZIPI (MXSEN,I0,PLNTYP)
      CALL ZIPI (MXSEN,I0,PPATRN)
      CALL ZIPI (MXSEN,I0,PSYMBL)
      CALL ZIPI (MXSEN,I0,PLCLR)
      NWDM = 1
      LWDID(1)= 'WDM1'
      PLTPRE  = 'basin   '
      UNCALD  = 1
      CALL ZIPI (I12,I0,IPCTFG)
C
C     call routine to set storms
      START(1) = SDATIM(1)
      START(2) = SDATIM(2)
      START(3) = SDATIM(3)
      START(4) = SDATIM(4)
      START(5) = SDATIM(5)
      START(6) = 0
      ENDDT(1) = EDATIM(1)
      ENDDT(2) = EDATIM(2)
      ENDDT(3) = EDATIM(3)
      ENDDT(4) = EDATIM(4)
      ENDDT(5) = EDATIM(5)
      ENDDT(6) = 0
      FSTRM    = 99.0
      BUFMAX   = 1000
C     figure out which dsn to use
      DO 50 I = 1,NREACH
        IF (RCHDSN(I,1).NE.0) THEN
C         have data here
          OUTLET = 1
          DO 60 K = 1,NRCONN
C           check each reach to reach connection
            IF (I.EQ.R2RCON(K,1)) THEN
C             this reach is a source to another reach, not outlet
              OUTLET = 0
            END IF
 60       CONTINUE
          IF (OUTLET.EQ.1) THEN
C           found outlet
            ODSN = RCHDSN(I,1)
          END IF
        END IF
 50   CONTINUE
      TU       = 4
      TS       = 1
      DTRAN    = 0
      QFLAG    = 31
      DATCNT   = 0
      CALL TIMDIF (START,ENDDT,TU,TS,NVALS)
      CALL TSBTIM (TU,TS,DTRAN,QFLAG)
      CALL TSBWDS (WDMSFL,ODSN)
      CALL TSBGET (START,NVALS,
     O             YX,RETCOD)
      IF (NVALS.GT.BUFMAX) THEN
        NVALS = BUFMAX
      END IF
      IF (NVALS.GE.100 .AND. RETCOD.GE.0) THEN
C       need at least 100 values to search for storms
        CALL CMSTRM (START,ENDDT,MXSTRM,MXSTRM,FSTRM,
     I               BUFMAX,YX,TS,TU,NVALS,
     M               INDX,
     O               DATCNT,STSTRM,ENSTRM,RDUM1,RDUM2,RDUM3)
      END IF
      IF (DATCNT.EQ.0) THEN
C       no storm dates found
        DATCNT  = 1
        STSTRM(1,1) = SDATIM(1)
        STSTRM(2,1) = SDATIM(2)
        STSTRM(3,1) = SDATIM(3)
        STSTRM(4,1) = SDATIM(4)
        STSTRM(5,1) = SDATIM(5)
        STSTRM(6,1) = 0
        ENSTRM(1,1) = EDATIM(1)
        ENSTRM(2,1) = EDATIM(2)
        ENSTRM(3,1) = EDATIM(3)
        ENSTRM(4,1) = EDATIM(4)
        ENSTRM(5,1) = EDATIM(5)
        ENSTRM(6,1) = 0
      END IF
C
C     set output locations with upstream areas
      NSITE = 0
      DO 10 I = 1,NREACH
        IF (RCHDSN(I,1).NE.0) THEN
C         count this output location
          NSITE = NSITE + 1
          SAIND= 290
          SALEN= 8
          CALL WDBSGC (WDMSFL,RCHDSN(I,1),SAIND,SALEN,
     O                 SITNAM(1,NSITE),RETCOD)
          CALL ZIPC (I12,BLNK,SITNAM(9,NSITE))
C         store area upstream of this reach
          BAREA(NSITE) = UPAREA(I)
        END IF
 10   CONTINUE
      IF (NSITE.GT.0) THEN
        CURSIT  = 1
      ELSE
        CURSIT  = 0
      END IF
C     write out exs file
      CALL HSXPUT (OUTFL,MXSEN,MXCOV,MXLINE,MXCAL,MXSITE,MXSTRM,
     I             I1,PLCLR,PLNTYP,PSYMBL,PPATRN,ARHLOG,MAPCOV,
     I             RLAT,RLNG,UCIPTH,IVAR,RPARM1,RPARM2,RDERR,
     I             VARCNT,UNCALD,RCHNGE,RERROR,IPCTFG,ISUBJC,
     I             RERTRM,NSTATS,STAT1,SITENM,CALINM,BAREA,
     I             NSITE,CURSIT,DATCNT,NWDM,DCNVFG,
     I             STSTRM,ENSTRM,STATDN,SITNAM,PLTPRE,LWDID,
     I             WDMNAM,CSCEN,NANAL,ADSN,
     O             RETCOD)
C
C     close file
      CLOSE(UNIT=OUTFL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GISWRT
     I                    (MXR,MXLU,NEWID,NR,NLU,XLAREA,XNAME,
     I                     XDOWN,XUP,
     I                     XNCON,XRCONN,FUNRCH,FUNARE,FUNCON)
C
C     + + + PURPOSE + + +
C     write new version of gis files
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MXR,MXLU,NEWID(MXR),NR,NLU,XDOWN(MXR),XNCON,
     1              XRCONN(MXR,2),XUP(MXR),FUNRCH,FUNARE,FUNCON
      REAL          XLAREA(MXLU,MXR)
      CHARACTER*12  XNAME(MXR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NEWID  - array of new reach ids
C     NR     - number of new reaches
C     NLU    - number of land uses
C     XLAREA - array of local areas for each new reach
C     XNAME  - array of new reach names
C     XDOWN  - downstream most reach in each new reach
C     XUP    - upstream most reach in each new reach
C     XNCON  - new number of reach to reach connections
C     XRCONN - array of reach to reach connections
C     FUNRCH - file unit number for new reach file
C     FUNARE - file unit number for new area file
C     FUNCON - file unit number for new connection file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'pmxts.inc'
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,FILE
      REAL          TLEN,TDELTH
      CHARACTER*80  CTXT80
C
C     + + + INPUT FORMATS + + +
 2010 FORMAT (A8,2X,A8)
 2017 FORMAT (A12,3X,I5,2F10.3,2F10.2,4F10.5)
 2030 FORMAT (A8,2X,10F10.2)
 2060 FORMAT (A80)
C
C     + + + END SPECIFICATIONS + + +
C
C     write out new reach file
      FILE = FUNRCH
C     write header
      CTXT80 = ' '
      WRITE(FILE,2060) CTXT80
C     write units line
      WRITE(FILE,2060) CTXT80
C     write record for each reach
      DO 10 I = 1,NR
        TDELTH = 0.0
        TLEN   = 0.0
        DO 5 J = 1,NREACH
          IF (NEWID(J).EQ.I) THEN
            TDELTH = TDELTH + DELTH(J)
            TLEN   = TLEN   + LEN(J)
          END IF
 5      CONTINUE
        WRITE(FILE,2017) XNAME(I),RCHMET(XDOWN(I)),TLEN,TDELTH,
     2            UPELEV(XUP(I)),DNELEV(XDOWN(I)),UPLAT(XUP(I)),
     3            UPLON(XUP(I)),DNLAT(XDOWN(I)),DNLON(XDOWN(I))
 10   CONTINUE
C
C     write out new area file
      FILE = FUNARE
C     write header
      CTXT80 = ' '
      WRITE(FILE,2060) CTXT80
C     write units line
      WRITE(FILE,2060) CTXT80
C     write record for each reach
      DO 20 I = 1,NR
        WRITE (FILE,2030) XNAME(I),(XLAREA(J,I),J=1,NLU)
 20   CONTINUE
C
C     write out new reach connection file
      FILE = FUNCON
C     write header line
      WRITE(FILE,2060) CTXT80
      DO 30 I = 1,XNCON
        WRITE (FILE,2010) XNAME(XRCONN(I,1)),XNAME(XRCONN(I,2))
 30   CONTINUE
C
      RETURN
      END
