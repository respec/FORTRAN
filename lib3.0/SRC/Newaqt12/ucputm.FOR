C
C
C
      SUBROUTINE   GISREA
     I                    (FILE,RDBFG,EXPFG,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     read input gis files into common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       RETCOD,FILE(5),RDBFG(5),EXPFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RETCOD - return code from gis read routine
C     FILE   - array of input file unit numbers
C     EXPFG  - expert system flag, used to model land surface response
C     RDBFG  - files are rdb format flag
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'pmxts.inc'
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,
     1              TMPDSN,FOUND,ITYPE
      REAL          RTEMP(MXLAND)
C     CHARACTER*1   TXT1(80)
      CHARACTER*1   CHR1(256)
      CHARACTER*8   CNAME(2),CCON,CSEN
      CHARACTER*80  CTXT80
      CHARACTER*120 CHR120
C
C     + + + INTRINSICS + + +
      INTRINSIC   INT,CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    TSESPC,TSDSM,RDBEXP
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A120)
 1010 FORMAT (A8,2X,A8)
 1017 FORMAT (A12,3X,I5,2F10.3,2F10.2,4F10.5)
 1020 FORMAT (A12,3X,I5)
 1030 FORMAT (A8,2X,10F10.2)
 1060 FORMAT (A80)
 1070 FORMAT (I5,5X,A8)
 1080 FORMAT (20X,14A10)
 1090 FORMAT (80X)
 1100 FORMAT (256A1)
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C
C      write(*,*) 'in gisrea',file
C     array of reach names, met segments assoc with each reach
C     also length of reach and change in water elevation
      NREACH = 0
      IF (RDBFG(2).EQ.0) THEN
C       regular text file
C       read header
        READ(FILE(2),1060,END=20,ERR=91) CTXT80
C       read units line
        READ(FILE(2),1060,END=20,ERR=91) CTXT80
      ELSE IF (RDBFG(2).EQ.1) THEN
C       rdb file, read headers
 17     CONTINUE
          READ(FILE(2),1100,END=20,ERR=91) CHR1
        IF (CHR1(1).EQ.'/' .AND. CHR1(2).EQ.'*') GOTO 17
        ITYPE = 2
        INIT  = 1
      END IF
 15   CONTINUE
        IF (RDBFG(2).EQ.0) THEN
C         do straight read
          READ(FILE(2),1000,END=20,ERR=91) CHR120
        ELSE IF (RDBFG(2).EQ.1) THEN
C         need to read and then expand out
          IF (INIT.EQ.0) THEN
            READ(FILE(2),1100,END=20,ERR=91) CHR1
          END IF
          INIT = 0
          CALL RDBEXP (ITYPE,CHR1,
     O                 CHR120)
        END IF
C       write(99,*) CHR120
        READ(CHR120,1017,ERR=91) CRCH(NREACH+1),
     1      RCHMET(NREACH+1),LEN(NREACH+1),DELTH(NREACH+1),
     2      UPELEV(NREACH+1),DNELEV(NREACH+1),UPLAT(NREACH+1),
     3      UPLON(NREACH+1),DNLAT(NREACH+1),DNLON(NREACH+1)
C       write(99,*) CRCH(NREACH+1),
C    1      RCHMET(NREACH+1),LEN(NREACH+1),DELTH(NREACH+1),
C    2      UPELEV(NREACH+1),DNELEV(NREACH+1),UPLAT(NREACH+1),
C    3      UPLON(NREACH+1),DNLAT(NREACH+1),DNLON(NREACH+1)
        NREACH = NREACH + 1
        GO TO 15
 20   CONTINUE
C
C     array of reach to reach connections
      NRCONN = 0
      NUMEXT = 0
      IF (RDBFG(1).EQ.0) THEN
C       regular text file
C       read header line
        READ(FILE(1),1060,END=10,ERR=91) CTXT80
      ELSE IF (RDBFG(1).EQ.1) THEN
C       rdb file, read headers
 8      CONTINUE
          READ(FILE(1),1100,END=10,ERR=91) CHR1
        IF (CHR1(1).EQ.'/' .AND. CHR1(2).EQ.'*') GOTO 8
        ITYPE = 1
        INIT  = 1
      END IF
 5    CONTINUE
        IF (RDBFG(1).EQ.0) THEN
C         do straight read
          READ(FILE(1),1000,END=10,ERR=91) CHR120
        ELSE IF (RDBFG(1).EQ.1) THEN
C         need to read and then expand out
          IF (INIT.EQ.0) THEN
            READ(FILE(1),1100,END=10,ERR=91) CHR1
          END IF
          INIT = 0
          CALL RDBEXP (ITYPE,CHR1,
     O                 CHR120)
        END IF
        READ(CHR120,1010,ERR=91) CNAME
C       write(99,*) CNAME(1),CNAME(2)
        NRCONN = NRCONN + 1
        R2RCON(NRCONN,1) = 0
        R2RCON(NRCONN,2) = 0
        DO 7 I = 1,NREACH
C         store the sequential reach number
          IF (CRCH(I).EQ.CNAME(1)) THEN
C           this is the upstream reach
            R2RCON(NRCONN,1) = I
          END IF
          IF (CRCH(I).EQ.CNAME(2)) THEN
C           this is the downstream reach
            R2RCON(NRCONN,2) = I
          END IF
 7      CONTINUE
        IF (R2RCON(NRCONN,1) .EQ. 0) THEN
C         did not find this reach in list of reach names
C         is it an observed flow data set?
          CCON = 'FLOW    '
          CSEN = 'OBSERVED'
C         see if we have one of these in wdm file
          CALL TSESPC (CSEN,CNAME(1),CCON)
          TMPDSN = 0
          CALL TSDSM (TMPDSN)
          IF (TMPDSN.GT.0) THEN
C           must be an external source
            NUMEXT = NUMEXT + 1
            EXTSRC(NUMEXT,1) = TMPDSN
            EXTSRC(NUMEXT,2) = R2RCON(NRCONN,2)
            CEXTSR(NUMEXT) = CNAME(1)
          END IF
          NRCONN = NRCONN - 1
        END IF
        GO TO 5
 10   CONTINUE
C
C     array of land use names, perv/imperv for each land use,
C     other parameters specified in headings
      IF (FILE(3).GT.0 .AND. EXPFG.EQ.1) THEN
        NLANDU = 0
        IF (RDBFG(3).EQ.0) THEN
C         regular text file
C         read header
          READ(FILE(3),1060,END=30,ERR=91) CTXT80
C         read units line
          READ(FILE(3),1060,END=30,ERR=91) CTXT80
        ELSE IF (RDBFG(3).EQ.1) THEN
C         rdb file, read headers
 23       CONTINUE
            READ(FILE(3),1100,END=30,ERR=91) CHR1
          IF (CHR1(1).EQ.'/' .AND. CHR1(2).EQ.'*') GOTO 23
          ITYPE = 3
          INIT  = 1
        END IF
 25     CONTINUE
          IF (RDBFG(3).EQ.0) THEN
C           do straight read
            READ(FILE(3),1000,END=30,ERR=91) CHR120
          ELSE IF (RDBFG(3).EQ.1) THEN
C           need to read and then expand out
            IF (INIT.EQ.0) THEN
              READ(FILE(3),1100,END=30,ERR=91) CHR1
            END IF
            INIT = 0
            CALL RDBEXP (ITYPE,CHR1,
     O                   CHR120)
          END IF
          READ(CHR120,1020,ERR=91) CLAND(NLANDU+1),
     1                                    LANDPI(NLANDU+1)
          NLANDU = NLANDU + 1
          GO TO 25
 30     CONTINUE
      ELSE
C       take defaults
        NLANDU   = 1
        CLAND(1) = 'GENERAL'
        LANDPI(1)= 1
      END IF
C
C     array of areas of each land use contrib to each reach
      IF (RDBFG(4).EQ.0) THEN
C       regular text file
C       read header line
        READ(FILE(4),1060,END=40,ERR=91) CTXT80
C       read units line
        READ(FILE(4),1060,END=40,ERR=91) CTXT80
      ELSE IF (RDBFG(4).EQ.1) THEN
C       rdb file, read headers
 41     CONTINUE
          READ(FILE(4),1100,END=40,ERR=91) CHR1
        IF (CHR1(1).EQ.'/' .AND. CHR1(2).EQ.'*') GOTO 41
        ITYPE = 4
        INIT  = 1
      END IF
 42   CONTINUE
        IF (RDBFG(4).EQ.0) THEN
C         do straight read
          READ(FILE(4),1000,END=40,ERR=91) CHR120
        ELSE IF (RDBFG(4).EQ.1) THEN
C         need to read and then expand out
          IF (INIT.EQ.0) THEN
            READ(FILE(4),1100,END=40,ERR=91) CHR1
          END IF
          INIT = 0
          CALL RDBEXP (ITYPE,CHR1,
     O                 CHR120)
        END IF
        READ(CHR120,1030,ERR=91) CNAME(1),(RTEMP(J),J=1,NLANDU)
C       write(99,*) CHR120
C       write(99,*) CNAME(1),RTEMP(1)
        FOUND = 0
        DO 43 I = 1,NREACH
C         store area info for sequential reach number
          IF (CRCH(I).EQ.CNAME(1)) THEN
C           found this reach in list
            FOUND = 1
            DO 44 J = 1,NLANDU
              AREALR(J,I) = RTEMP(J)
 44         CONTINUE
          END IF
 43     CONTINUE
        IF (FOUND.EQ.0 .AND. NUMEXT.GT.0) THEN
C         not found, see if this reach is an external source in rconn table
          DO 46 I = 1,NUMEXT
            IF (CNAME(1).EQ.CEXTSR(I)) THEN
C             store this area with the external source record
              RAEXT(I) = 0.0
              DO 47 J = 1,NLANDU
                RAEXT(I) = RAEXT(I) + RTEMP(J)
 47           CONTINUE
            END IF
 46       CONTINUE
        END IF
        GO TO 42
 40   CONTINUE
C
C     array of met station names, prec or evap flags
      IF (FILE(5).GT.0 .AND. EXPFG.EQ.1) THEN
        NMETSG = 0
        NEVAP  = 0
        IF (RDBFG(5).EQ.0) THEN
C         regular text file
C         read header line
          READ(FILE(5),1060,ERR=91) CTXT80
        ELSE IF (RDBFG(5).EQ.1) THEN
C         rdb file, read headers
 51       CONTINUE
            READ(FILE(5),1100,END=50,ERR=91) CHR1
          IF (CHR1(1).EQ.'/' .AND. CHR1(2).EQ.'*') GOTO 51
          ITYPE = 5
          INIT  = 1
        END IF
 45     CONTINUE
          IF (RDBFG(5).EQ.0) THEN
C           do straight read
            READ(FILE(5),1000,END=50,ERR=91) CHR120
          ELSE IF (RDBFG(5).EQ.1) THEN
C           need to read and then expand out
            IF (INIT.EQ.0) THEN
              READ(FILE(5),1100,END=50,ERR=91) CHR1
            END IF
            INIT = 0
            CALL RDBEXP (ITYPE,CHR1,
     O                   CHR120)
          END IF
          READ(CHR120,1070,ERR=91) PRCEVP(NMETSG+NEVAP+1),
     1                             CMETS(NMETSG+NEVAP+1)
          IF (PRCEVP(NMETSG+NEVAP+1).EQ.1) THEN
C           precip data set
            NMETSG = NMETSG + 1
          ELSE
C           evap data set
            NEVAP  = NEVAP + 1
          END IF
          GO TO 45
 50     CONTINUE
      ELSE
C       take defaults
        NMETSG = 1
        NEVAP  = 0
        PRCEVP(1)= 1
        CMETS(1) = 'STA1'
      END IF
C
      GO TO 99
 91   CONTINUE
C       file read error
        RETCOD= -1
        GO TO 99
 99   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   RDBEXP
     I                    (ITYPE,CHR1,
     O                     CHR120)
C
C     + + + PURPOSE + + +
C     expand an rdb record into a character*120 record
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       ITYPE
      CHARACTER*1   CHR1(120)
      CHARACTER*120 CHR120
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITYPE  - type of rdb record
C              1 - connection
C              2 - reach
C              3 - land use
C              4 - area
C              5 - met station
C     CHR1   - character*1 array of characters from rdb line
C     CHR120 - character*120 line reformatted without tab characters
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,TABCNT,ISTR,ILEN,I120
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC   ICHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR,CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      I120   = 120
      CHR120 = ' '
      ILEN = LENSTR (I120,CHR1)
      I = 0
      ISTR = 1
      TABCNT = 0
C
      IF (ITYPE.EQ.1) THEN
C       connection
 10     CONTINUE
          I = I + 1
          IF (ICHAR(CHR1(I)).EQ.9) THEN
C           tab character
            TABCNT = TABCNT + 1
            IF (TABCNT.EQ.1) THEN
              CALL CARVAR (I-1,CHR1(ISTR),I-1,CHR120(1:I-1))
            END IF
            ISTR = I + 1
          END IF
        IF (I.LT.ILEN) GO TO 10
        CALL CARVAR (I-ISTR+1,CHR1(ISTR),I-ISTR+1,CHR120(11:I-ISTR+11))
      ELSE IF (ITYPE.EQ.2) THEN
C       reach
 20     CONTINUE
          I = I + 1
          IF (ICHAR(CHR1(I)).EQ.9) THEN
C           tab character
            TABCNT = TABCNT + 1
            IF (TABCNT.EQ.1) THEN
              CALL CARVAR (I-1,CHR1(ISTR),I-1,CHR120(1:I-1))
              CHR120(20:20) = '0'
            ELSE IF (TABCNT.EQ.2) THEN
              CALL CARVAR (I-ISTR,CHR1(ISTR),I-ISTR,
     O                     CHR120(21:I-ISTR+20))
            ELSE IF (TABCNT.EQ.3) THEN
              CALL CARVAR (I-ISTR,CHR1(ISTR),I-ISTR,
     O                     CHR120(31:I-ISTR+30))
              CHR120(I-ISTR+31:I-ISTR+31) = '.'
            ELSE IF (TABCNT.EQ.4) THEN
              CALL CARVAR (I-ISTR,CHR1(ISTR),I-ISTR,
     O                     CHR120(41:I-ISTR+40))
              CHR120(I-ISTR+41:I-ISTR+41) = '.'
            ELSE IF (TABCNT.EQ.5) THEN
              CALL CARVAR (I-ISTR,CHR1(ISTR),I-ISTR,
     O                     CHR120(51:I-ISTR+50))
              CHR120(I-ISTR+51:I-ISTR+51) = '.'
            ELSE IF (TABCNT.EQ.6) THEN
              CALL CARVAR (I-ISTR,CHR1(ISTR),I-ISTR,
     O                     CHR120(61:I-ISTR+60))
            ELSE IF (TABCNT.EQ.7) THEN
              CALL CARVAR (I-ISTR,CHR1(ISTR),I-ISTR,
     O                     CHR120(71:I-ISTR+70))
            ELSE IF (TABCNT.EQ.8) THEN
              CALL CARVAR (I-ISTR,CHR1(ISTR),I-ISTR,
     O                     CHR120(81:I-ISTR+80))
            END IF
            ISTR = I + 1
          END IF
        IF (I.LT.ILEN) GO TO 20
        CALL CARVAR (I-ISTR+1,CHR1(ISTR),I-ISTR+1,CHR120(91:I-ISTR+91))
      ELSE IF (ITYPE.EQ.3) THEN
C       land use
 30     CONTINUE
          I = I + 1
          IF (ICHAR(CHR1(I)).EQ.9) THEN
C           tab character
            TABCNT = TABCNT + 1
            IF (TABCNT.EQ.1) THEN
              CALL CARVAR (I-1,CHR1(ISTR),I-1,CHR120(1:I-1))
            END IF
            ISTR = I + 1
          END IF
        IF (I.LT.ILEN) GO TO 30
        CALL CARVAR (I-ISTR+1,CHR1(ISTR),I-ISTR+1,CHR120(16:I-ISTR+16))
      ELSE IF (ITYPE.EQ.4) THEN
C       area
 40     CONTINUE
          I = I + 1
          IF (ICHAR(CHR1(I)).EQ.9) THEN
C           tab character
            TABCNT = TABCNT + 1
            IF (TABCNT.EQ.1) THEN
              CALL CARVAR (I-1,CHR1(ISTR),I-1,CHR120(1:I-1))
            END IF
            ISTR = I + 1
          END IF
        IF (I.LT.ILEN) GO TO 40
        CALL CARVAR (I-ISTR+1,CHR1(ISTR),I-ISTR+1,CHR120(11:I-ISTR+11))
      ELSE IF (ITYPE.EQ.5) THEN
C       met stations
 50     CONTINUE
          I = I + 1
          IF (ICHAR(CHR1(I)).EQ.9) THEN
C           tab character
            TABCNT = TABCNT + 1
            IF (TABCNT.EQ.1) THEN
              CALL CARVAR (I-1,CHR1(ISTR),I-1,CHR120(1:I-1))
            END IF
            ISTR = I + 1
          END IF
        IF (I.LT.ILEN) GO TO 50
        CALL CARVAR (I-ISTR+1,CHR1(ISTR),I-ISTR+1,CHR120(11:I-ISTR+11))
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SETUCI
     I                    (IOUTLE,IRESMF,IRUNFG,ISPOUT,IUNIT,
     I                     ISDATI,IEDATI,IRNINF,IWDMFL,ISCEN,IWNAME)
C
C     + + + PURPOSE + + +
C     set info in common for making a uci file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IOUTLE,IRESMF,IRUNFG,ISPOUT,
     1              ISDATI(5),IEDATI(5),IUNIT,IWDMFL
      CHARACTER*8   ISCEN
      CHARACTER*64  IWNAME
      CHARACTER*80  IRNINF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOUTLE - hspf output level
C     IRESMF - resume flag
C     IRUNFG - run flag
C     ISPOUT - special actions output level
C     IUNIT  - unit system
C     ISDATI - start date
C     IEDATI - end date
C     IRNINF - run info
C     IWDMFL - wdm file unit number
C     ISCEN  - new scenario name
C     IWNAME - wdm file name
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      OUTLEV = IOUTLE
      RESMFG = IRESMF
      RUNFG  = IRUNFG
      SPOUT  = ISPOUT
      DO 10 I = 1,5
        SDATIM(I) = ISDATI(I)
        EDATIM(I) = IEDATI(I)
 10   CONTINUE
      UNIT   = IUNIT
      RNINFO = IRNINF
      WDMSFL = IWDMFL
      CSCEN  = ISCEN
      WDMNAM = IWNAME
C
      RETURN
      END
C
C
C
      SUBROUTINE SETRAT
     I                  (FIN1)
C
C     + + + PURPOSE + + +
C     This routine reads rating tables for use in creating
C     FTABLEs for HSPF input.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER FIN1
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FIN1   - Fortran unit number of rating curve input file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + +
      INTEGER I, J, K
      CHARACTER*80 LINE
C
C     + + + END SPECIFICATIONS + + +
C
C     zero out buffers
      DO 5 I = 1,MAXRAT
        NUMROW(I) = 0
        AREA(I)   = 0.0
        DO 4 J = 1,MAXROW
          DO 3 K = 1,4
            RAT(I,J,K) = 0.0
 3        CONTINUE
 4      CONTINUE
 5    CONTINUE
C
      I = 0
 10   CONTINUE
C
        READ(FIN1,FMT='(A)',ERR=91,END=99) LINE
C       write(*,*) line
C
        IF (LINE(1:1) .EQ. 'C' .OR. LINE(1:1) .EQ. 'c'
     $      .OR. LINE(1:1). EQ. '#') THEN
C         comment line, ignore
        ELSE IF (LINE(1:1) .EQ. 'R' .OR. LINE(1:1) .EQ. 'r') THEN
C         new rating, get drainage area
          I = I + 1
          READ(LINE,FMT='(1X,F9.0)') AREA(I)
          J = 1
        ELSE
C         read rating records
          IF (J .LE. MAXROW) THEN
C           use new record if array of MAXROW not reached
            READ(LINE,*,ERR=91) (RAT(I,J,K),K=1,4)
            IF (J .EQ. 1 .AND. RAT(I,J,4) .GT. 1E-9) THEN
              WRITE(99,*) 'WARNING: Discharge for first row should',
     $                   ' be zero for HSPF FTABLEs.'
            END IF
            NUMROW(I) = J
            J = J + 1
          END IF
        END IF
      IF (I .LT. MAXRAT) GO TO 10
C
      GO TO 99
 91   CONTINUE
C       WRITE(*,*) 'ERROR reading rating number',I
        IF (NUMROW(I) .LE. 1) I = I - 1
 99   CONTINUE
      NUMRAT = I
C     close file with ratings
      CLOSE (FIN1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCRINI
C
C     + + + PURPOSE + + +
C     init uci create common
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J
C
C     + + + EXTERNALS + + +
      EXTERNAL      ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize default land use table
      NLANDU   = 1
      CLAND(1) = 'GENERAL'
      LANDPI(1)= 1
C     initialize default met segment table
      NMETSG = 1
      NEVAP  = 0
      PRCEVP(1)= 1
      CMETS(1) = 'STA1'
C
C     initialize array of output sites to -999
      DO 10 I = 1,MXRCH
        OUTSIT(I) = -999
 10   CONTINUE
c
C     initialize output dsn array
      I = 0
      CALL ZIPI (MXRCH*10,I,RCHDSN)
C
      DO 55 I = 1,MXLAND
        DO 75 J = 1,MXMET
C         fraction of segment covered by forest
          FOREST(I,J) = 0.0
C         lower zone nominal storage
          LZSN(I,J)   = 0.01
C         index to infiltration capacity of soil
          INFILT(I,J) = 0.001
C         length of assumed overland flow plane
          LSUR(I,J)   = 1.0
C         slope of assumed overland flow plane
          SLSUR(I,J)  = 0.001
C         groundwater recession parameter
          KVARY(I,J)  = 0.0
C         groundwater recession rate
          AGWRC(I,J)  = 0.001
C         interception storage capacity
          CEPSC(I,J)  = 0.0
C         upper zone nominal storage
          UZSN(I,J)   = 0.01
C         mannings n for assumed overland flow plane
          NSUR(I,J)   = 0.1
C         interflow infow parameter
          INTFW(I,J)  = 0.001
C         interflow recession parameter
          IRC(I,J)    = 0.001
C         lower zone et parameter
          LZETP(I,J)  = 0.0
C         retention storage capacity of surface
          RETSC(I,J)  = 0.0
 75     CONTINUE
 55   CONTINUE
C
      DO 65 I = 1,MXRCH
C       ftable number for this reach
        FTABNO(I) = I
C       correction to the rchres depth to calculate stage
        STCOR(I)  = 0.0
C       weighting factor for hydrologic routing
        KS(I)     = 0.0
C       median diameter of bed sediment
        DB50(I)   = 0.01
C       hydr printout level flag
        R2HYDR(I) = 5
C       pyr printout level flag
        R2PYR(I)  = 12
C       number of exits
        R3NEXI(I) = 1
C       lake flag
        R3LKFG(I) = 0
C       rchres hydr flags
        R4HYFG(1,I)= 0
        R4HYFG(2,I)= 0
        R4HYFG(3,I)= 0
        R4HYFG(4,I)= 0
        R4HYFG(5,I)= 4
        R4HYFG(6,I)= 0
        R4HYFG(7,I)= 0
        R4HYFG(8,I)= 0
        R4HYFG(9,I)= 0
        R4HYFG(10,I)= 0
        R4HYFG(11,I)= 0
        R4HYFG(12,I)= 0
        R4HYFG(13,I)= 0
        R4HYFG(14,I)= 0
        R4HYFG(15,I)= 1
        R4HYFG(16,I)= 1
        R4HYFG(17,I)= 1
        R4HYFG(18,I)= 1
        R4HYFG(19,I)= 1
 65   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCPEXT
     I                    (INUMEX,IEXTSR,IRAEXT,ICEXTS)
C
C     + + + PURPOSE + + +
C     put external source flow info to common
C     * default values filled in GISREA *
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INUMEX,IEXTSR(INUMEX,2)
      REAL          IRAEXT(INUMEX)
      CHARACTER*8   ICEXTS(INUMEX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUMEX - number of external source flows
C     IEXTSR - array of data set number and reach it connects to
C     IRAEXT - array of areas of each external source
C     ICEXTS - array of names of exernal source locations
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      NUMEXT = INUMEX
      DO 10 I = 1,NUMEXT
        EXTSRC(I,1) = IEXTSR(I,1)
        EXTSRC(I,2) = IEXTSR(I,2)
        CEXTSR(I)   = ICEXTS(I)
        RAEXT(I)    = IRAEXT(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCPLIN
     I                    (INDSNI,IDSNIN,IAREAI)
C
C     + + + PURPOSE + + +
C     put local inflow candidate info to common
C     * default values can be filled using UCIDSN *
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INDSNI,IDSNIN(INDSNI)
      REAL          IAREAI(INDSNI)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDSNI - number of local inflow data set candidates
C     IDSNIN - array of data set number for each local inflow candidates
C     IAREAI - array of areas of each local inflow candidate
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      NDSNIN = INDSNI
      DO 10 I = 1,NDSNIN
        DSNIN(I) = IDSNIN(I)
        AREAIN(I)= IAREAI(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCPOUT
     I                    (INR,IOUT)
C
C     + + + PURPOSE + + +
C     put output site info to common
C     * default values can be filled using UCIDSN *
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INR,IOUT(INR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INR    - number of reaches
C     IOUT   - flag indicating if output is desired here (-999 no, otherwise
C              base data set for numbering output data sets)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,INR
        OUTSIT(I) = IOUT(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCPRCH
     I                    (INR,ICRCH,IRCHME,ILEN,IDELTH,IUPELE,IDNELE,
     I                     IUPLAT,IUPLON,IDNLAT,IDNLON)
C
C     + + + PURPOSE + + +
C     put reach table info to common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INR,IRCHME(INR)
      REAL          ILEN(INR),IDELTH(INR),IUPELE(INR),IDNELE(INR),
     1              IUPLAT(INR),IUPLON(INR),IDNLAT(INR),IDNLON(INR)
      CHARACTER*12  ICRCH(INR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INR    - number of reaches
C     ICRCH  - reach name
C     IRCHME - array of met segments assoc with each reach
C     ILEN   - (hspf parm) length of each reach
C     IDELTH - (hspf parm) diff in elevation for each reach
C     IUPELE - elevation at upstream end of each reach
C     IDNELE - elevation at downstream end of each reach
C     IUPLAT - latitude of upstream end of each reach
C     IUPLON - longitude of upstream end of each reach
C     IDNLAT - latitude of downstream end of each reach
C     IDNLON - longitude of downstream end of each reach
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      NREACH = INR
      DO 10 I = 1,NREACH
        CRCH(I)   = ICRCH(I)
        RCHMET(I) = IRCHME(I)
        LEN(I)    = ILEN(I)
        DELTH(I)  = IDELTH(I)
        UPELEV(I) = IUPELE(I)
        DNELEV(I) = IDNELE(I)
        UPLAT(I)  = IUPLAT(I)
        UPLON(I)  = IUPLON(I)
        DNLAT(I)  = IDNLAT(I)
        DNLON(I)  = IDNLON(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCPCON
     I                    (INRC,IR2R)
C
C     + + + PURPOSE + + +
C     put reach connection table info to common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INRC,IR2R(INRC,2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INRC   - number of reach to reach connections
C     IR2R   - array of reach to reach source/target pairs
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      NRCONN = INRC
      DO 10 I = 1,NRCONN
        R2RCON(I,1) = IR2R(I,1)
        R2RCON(I,2) = IR2R(I,2)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCPLAN
     I                    (INLU,ICLAND,ILANDP)
C
C     + + + PURPOSE + + +
C     put land use table info to common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INLU,ILANDP(INLU)
      CHARACTER*12  ICLAND(INLU)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INLU   - number of land uses
C     ICLAND - array of land use names
C     ILANDP - array of type of each land use 1-perv, 2-imperv
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      NLANDU = INLU
      DO 10 I = 1,NLANDU
        CLAND(I) = ICLAND(I)
        LANDPI(I)= ILANDP(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCPARE
     I                    (INLU,INR,IAREA)
C
C     + + + PURPOSE + + +
C     put area table info to common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INLU,INR
      REAL          IAREA(INLU,INR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INLU   - number of land uses
C     INR    - number of reaches
C     IAREA  - array of areas of each land use contributing to each reach
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J
C
C     + + + END SPECIFICATIONS + + +
C
      DO 20 J = 1,INR
        DO 10 I = 1,INLU
          AREALR(I,J)= IAREA(I,J)
 10     CONTINUE
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCPMET
     I                    (INMET,INEVAP,IPE,ICMETS)
C
C     + + + PURPOSE + + +
C     put met table info to common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INMET,INEVAP,IPE(INMET+INEVAP)
      CHARACTER*8   ICMETS(INMET+INEVAP)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INMET  - number of met segments
C     INEVAP - number of evap data sets
C     IPE    - array of flags indicating if this is a precip or evap segment
C     ICMETS - array of met station names
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      NMETSG = INMET
      NEVAP  = INEVAP
      DO 10 I = 1,INMET+INEVAP
        PRCEVP(I) = IPE(I)
        CMETS(I)  = ICMETS(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE UCPRAT
     I                  (IMXROW,INRAT,INUMR,IAREA,IRAT)
C
C     + + + PURPOSE + + +
C     put rating curve info to common block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  IMXROW,INRAT,INUMR(INRAT)
      REAL     IAREA(INRAT),IRAT(INRAT,IMXROW,4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IMXROW - max number of rows in a rating curve
C     INRAT  - number of rating curves
C     INUMR  - number of rows in each rating curve
C     IAREA  - area associated with each rating curve
C     IRAT   - rating curve data
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cgisin.inc'
C
C     + + + LOCAL VARIABLES + +
      INTEGER I, J, K
C
C     + + + END SPECIFICATIONS + + +
C
      NUMRAT = INRAT
      DO 30 I = 1,NUMRAT
        NUMROW(I) = INUMR(I)
        AREA(I)   = IAREA(I)
        DO 20 J = 1,NUMROW(I)
          DO 10 K = 1,4
            RAT(I,J,K) = IRAT(I,J,K)
 10       CONTINUE
 20     CONTINUE
 30   CONTINUE
C
      RETURN
      END
