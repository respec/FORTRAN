C
C
C
      SUBROUTINE   INPUT1
     I                   (MESSFL,WDMSFL,IBCPUN,ECHFUN,MAXPKS,
     I                    STAID,PKSABG,IWYSN,XQUAL,
     O                    NHIST,NSYS,HISTPD,BEGYR,ENDYR,QHIOUT,QLWOUT,
     O                    GAGEB,GENSKU,RMSEGS,ISKUOP,NSKIP1,EMAOPT,IRC)
C
C     + + + PURPOSE + + +
C     This routine gets data from the WDM file for analysis by J407,
C     flood frequency analysis by WRC guidelines.
C
C     + + + HISTORY + + +
C     updated for batch version of PEAKFQ, 9/03
C     Paul Hummel of AQUA TERRA Consultants
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMSFL, IBCPUN, ECHFUN, MAXPKS, IWYSN(MAXPKS),
     &          NHIST, NSYS, BEGYR, ENDYR, ISKUOP, NSKIP1, EMAOPT, IRC
      REAL      PKSABG(MAXPKS), HISTPD, QHIOUT, QLWOUT, GAGEB, GENSKU,
     &          RMSEGS
      CHARACTER*5  XQUAL(MAXPKS)
      CHARACTER*90  STAID
C
C     + + + ARGUMENT DEFINITION + + +
C     MESSFL - Fortran unit number of message file
C     WDMSFL - Fortran unit number of WDM file
C     IBCPUN - Additional output indicator,
C              0 - none
C              1 - WDM attributes
C              2 - Watstore BCD file
C              3 - Both WDM and BCD
C              4 - Tab-separated file
C              5 - Both WDM and tab-separated
C     ECHFUN - FORTRAN unit number for input echo file 
C     MAXPKS - max number of peaks that can be stored in data arrays
C     STAID  - character string station id number and name
C               1-15 - 15-digit station id number or
C                       8-digit downstream order number
C              16-20 - agency code
C              21-78 - station name, left justified
C              79-90 - used by j407
C     PKSABG - Flood peak discharges:
C              historical values in first NHIST (if any)
C              followed by NSYS systematic peaks
C              (negative values will exclude from the analysis)
C     IWYSN  - water years or sequence numbers of PKSABG peaks:
C              < 0 - historic peak
C              >=0 - systematic peak
C     XQUAL  - qualification codes for PKSABG
C     NHIST  - number of historic peaks returned
C     NSYS   - number of systematic peaks returned
C     HISTPD - length of historic period
C     BEGYR  - beginning year of analysis
C     ENDYR  - ending year of analysis
C     QHIOUT - user-set high-outlier discharge threshold
C     QLWOUT - user-set low-outlier discharge threshhold
C     GAGEB  - gage base discharge
C     GENSKU - generalized skew
C     RMSEGS - RMS error of generalized skew
C     ISKUOP - generalized skew option:
C               1 - generalized skew (GEN)
C               0 - weighted skew (WTD)
C              -1 - station skew (STA)
C     NSKIP1 - number of stations skipped because of input errors
C     EMAOPT - Analysis option,
C              0 - Bull. 17B
C              1 - EMA
C     IRC    - return code
C              0 - no error
C              1 - errors
C              2 - end of file
C              3 - errors and end of file
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxpk.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cpkdsn.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,L1,L4,L5,L12,L15,L16,L70,L90,NREC,WYR(MXPK),
     1             DSN,SALEN,URBREG,SAIND,     JUST,
     1                    RETCOD,TABID,TBCNT,LREC,TGRPT,INITFG,
     2             SCLU,GROUP,NROW,OLEN,LEN,SKIP,POS,IWRT,
     3             II,ITEMP
Cprh     $             IBUF(3),L3,MAXL,SCNFG,
      REAL         PK(MXPK),XGAGEB,RTEMP,FLAT,FLONG,SYSHI,HSTLOW
      CHARACTER*1  DASH,CBUFF(90),MFID(2),QFLG(12,MXPK),STANO(16),
     1             C3,C4,C7,C8,CC,C6,BLK
Cprh     $             STAR,
      CHARACTER*5  BLNK5,CTEMP
      CHARACTER*16 TNAME,CURSTA
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CTEMP,CTEMP1)
      CHARACTER*1  CTEMP1(5)
C
C      + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + FUNCTIONS + + +
      INTEGER      STRLNX, STRFND, SIMIN, SIMAX
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDBSGC, WDBSGI, INTCHR, ZIPC, WDTBFX, GETTS, GETTB
      EXTERNAL     STRLNX, CARVAR, STRFND, WDBSGR, SIMIN, SIMAX
      EXTERNAL     CHRCHR, WDBSAI, COMSKU, DSINF1, LFTSTR
      EXTERNAL     PARSESTASPECS, ECHOINPUT
C
C     + + + DATA INITIALIZATION + + +
      DATA  L1/1/, L5/5/, L4/4/, L70/70/, L90/90/, 
     $      L12/12/, L16/16/, L15/15/
      DATA  TNAME/'AN.PEAKS        '/,  BLK/' '/,  BLNK5/'     '/
      DATA  C3/'3'/, C8/'8'/, C4/'4'/, CC/'C'/, C7/'7'/, C6/'6'/
      DATA  DASH/'-'/
Cprh      DATA  STAR/'*'/,  DASH/'-'/,  L3/3/,  MAXL/20/,  SCNFG/1/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(I5)
C
C     + + + END SPECIFICATIONS + + +
C
C     set skipping bad data indicator
      NSKIP1= -1
      SCLU  = 121
      JUST  = 0
C
      INITFG= 1
 10   CONTINUE
C       loop if bad data on input
        NSKIP1 = NSKIP1 + 1
        DSNIND = DSNIND + 1
        CALL ZIPC (L90, BLK, CBUFF)
C
        IRC = 0
        IF (DSNIND .GT. DSNCNT) THEN
C         finished processing
          IRC = 2
          NHIST = 0
          NSYS = 0
        ELSE
          IF (NSKIP1 .GT. 0) THEN
C           Dataset & could not be processed.
Cprh            GROUP = 21
Cprh            CALL PRNTXI (MESSFL, SCLU, GROUP, DSNBUF(DSNIND-1))
            !LOG IT
          ELSE
C           Processing dataset number &.
Cprh            GROUP= 22
Cprh            IWRT = 1
Cprh            CALL PMXTXI (MESSFL,SCLU,GROUP,MAXL,SCNFG,IWRT,L1,
Cprh     1                   DSNBUF(DSNIND))
            !LOG IT
          END IF
C         Get by station attributes and check for quality flags
          DSN=DSNBUF(DSNIND)
          CALL ZIPC (L16,BLK,STANO)
          CALL DSINF1 (WDMSFL,DSN,L16,STANO)
          IF (STANO(1) .NE. BLK .AND. STANO(16) .EQ. BLK) THEN
            CALL CHRCHR (L15,STANO(1),CBUFF(1))
          ELSE
            CALL CHRCHR (L15,STANO(2),CBUFF(1))
          END IF
          CBUFF(16) = BLK
C
C         get begining and ending year
          SAIND = 278
          SALEN = 1
          CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,BEGYR,RETCOD)
          IF (RETCOD .NE. 0) BEGYR = -1
          SAIND = 279
          CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,ENDYR,RETCOD)
          IF (RETCOD .NE. 0) ENDYR = -1
C
C         get low outlier
          SAIND = 269
          CALL WDBSGR (WDMSFL,DSN,SAIND,SALEN,QLWOUT,RETCOD)
          IF (RETCOD .NE. 0) QLWOUT = 0.0
C
C         get high outlier
          SAIND = 270
          CALL WDBSGR (WDMSFL,DSN,SAIND,SALEN,QHIOUT,RETCOD)
          IF (RETCOD .NE. 0) QHIOUT = -1.0
C
C         get generalized skew option
          SAIND = 271
          CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,ISKUOP,RETCOD)
          IF (RETCOD .NE. 0) ISKUOP = 0
C
          IF (ISKUOP .NE. -1) THEN
C           get generalized skew
            SAIND = 272
            CALL WDBSGR (WDMSFL,DSN,SAIND,SALEN,GENSKU,RETCOD)
            IF (RETCOD .NE. 0) THEN
C             problem or attribute not there, so compute
              CALL COMSKU (WDMSFL, DSN, GENSKU, RETCOD)
              IF (RETCOD .NE. 0) THEN
C               Could not compute skew from lat-long for dataset &.
Cprh                GROUP = 23
Cprh                CALL PRNTXI (MESSFL, SCLU, GROUP, DSN)
                !LOG IT
                GENSKU = 0.0
              END IF
            END IF
          ELSE
C           use station skew only
            GENSKU = 0.0
          END IF
C
          IF (RETCOD .NE. 0) GENSKU = 0.0
C
C         get base gage discharge
          SAIND = 273
          CALL WDBSGR (WDMSFL,DSN,SAIND,SALEN,GAGEB,RETCOD)
          IF (RETCOD .NE. 0) GAGEB = 0.0
C
C         get historic return period
          SAIND = 81
          CALL WDBSGR (WDMSFL,DSN,SAIND,SALEN,HISTPD,RETCOD)
          IF (RETCOD .NE. 0) HISTPD = -1.0
C
C         get RMS error of gen skew
          SAIND = 275
          CALL WDBSGR (WDMSFL,DSN,SAIND,SALEN,RMSEGS,RETCOD)
          IF (RETCOD .NE. 0) RMSEGS = 0.55
C
C         get urban-reg include option
          SAIND = 276
          CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,URBREG,RETCOD)
          IF (RETCOD .NE. 0) URBREG = 0
C
C         check for data set type
          TABID=1
          CALL WDTBFX(WDMSFL,DSN,TABID,TNAME,TBCNT,LREC,TGRPT,MFID,
     &              SCLU,GROUP,NROW,RETCOD)
        write (*,*) 'INPUT1: Checked WDMSFL, DSN, TABID, TNAME ',
     $               WDMSFL,DSN,TABID,TNAME
        write (*,*) 'INPUT1: WDTBFX RETCOD ',RETCOD
          IF(RETCOD.EQ.0) THEN
        write (*,*) 'INPUT1, GETTB: MESSFL,TNAME,WDMSFL,DSN ',
     $                              MESSFL,TNAME,WDMSFL,DSN
        write (*,*) 'INPUT1, GETTB: MAXPKS,L1,INITFG,NROW ',
     $                              MAXPKS,L1,INITFG,NROW
            CALL GETTB (MESSFL,TNAME,WDMSFL,DSN,
     I                  MAXPKS,L1,INITFG,
     M                  NROW,
     O                  PK(1),WYR(1),QFLG(1,1),RETCOD)
        write (*,*) 'INPUT1: Got TABLE data, RETCOD ',RETCOD
            INITFG= 0
            NREC  = NROW
          ELSE IF(RETCOD.EQ.-82)THEN
            CALL GETTS (WDMSFL,DSN,MAXPKS,PK(1),WYR(1),QFLG(1,1)
     &                  ,NREC)
            IF (NREC .GT. 0) RETCOD = 0
          ELSE
C           Couldn't get data from dataset &.
Cprh            GROUP = 8
Cprh            CALL PRNTXI (MESSFL, SCLU, GROUP, DSNBUF(DSNIND))
            !LOG IT
          ENDIF
          IF (NREC .GT. 0) THEN
            IF (BEGYR .LT. 0) BEGYR = SIMIN(NREC,WYR)
            IF (ENDYR .LT. 0) ENDYR = SIMAX(NREC,WYR)
          ELSE
C           Couldn't get data from dataset &.
Cprh            GROUP = 8
Cprh            CALL PRNTXI (MESSFL, SCLU, GROUP, DSNBUF(DSNIND))
            !LOG IT
          END IF
C
          IF (NREC .GT. 0) THEN
C           retrieved peaks, get largest sytematic, smallest historic
            HSTLOW = 1.0E20
            SYSHI  = 0.0
            DO 20 I = 1, NREC
              IF (STRFND(L12,QFLG(1,I),L1,C7) .GT. 0) THEN
C               historic peak
                IF (PK(I) .LT. HSTLOW) THEN
C                 new low historic peak
                  HSTLOW = PK(I)
                END IF
              ELSE
C               systematic peak
                IF (PK(I) .GT. SYSHI) THEN
C                 new high systematic peak
                  SYSHI = PK(I)
                END IF
              END IF
 20         CONTINUE
            IF (HSTLOW .GT. 1.0E19) THEN
C             no historic peaks found
              HSTLOW = -1.0
            END IF
C
C           get any specs from spec file
            WRITE(CTEMP,2000) DSN
            CALL LFTSTR(L5,
     M                  CTEMP1)
            CURSTA = TRIM(CTEMP)
            CALL PARSESTASPECS (CURSTA,SYSHI,HSTLOW,
     M                          GENSKU,HISTPD,QHIOUT,QLWOUT,
     M                          GAGEB,RMSEGS,BEGYR,ENDYR,
     M                          ISKUOP,URBREG,FLAT,FLONG,EMAOPT)
C
C           write inputs to echo file
            CALL ECHOINPUT (ECHFUN,CURSTA,EMAOPT,BEGYR,ENDYR,
     I                      HISTPD,ISKUOP,GENSKU,RMSEGS,QLWOUT,
     I                      QHIOUT,GAGEB,URBREG,FLAT,FLONG)
C
C           Get station description/station name
            SALEN=48
            SAIND=45
            CALL WDBSGC(WDMSFL,DSN,SAIND,SALEN,CBUFF(21),RETCOD)
            LEN = STRLNX (L70,CBUFF(21))
            IF (LEN .LE. 50) THEN
C             room to add date
              IF (BEGYR .GT. 0 .AND. ENDYR .GT. 0) THEN
                CALL INTCHR (BEGYR, L4, JUST, OLEN, CBUFF(70))
                CALL CHRCHR (L1,DASH,CBUFF(74))
                CALL INTCHR (ENDYR, L4, JUST, OLEN, CBUFF(75))
              END IF
            END IF
            CALL CARVAR (L90,CBUFF,L90,STAID)
C
            J = 0
            NHIST = 0
            NSYS = 0
            DO 40 I = 1,NREC
              XQUAL(I) = BLNK5
 40         CONTINUE
C           look for historic peaks
            DO 50 I = 1,NREC
              SKIP = 0
              POS = 0
              IF (STRFND(L12,QFLG(1,I),L1,C7) .GT. 0) THEN
                IF (WYR(I) .GE. BEGYR .AND. WYR(I) .LE. ENDYR) THEN
C                 historic peak
                  J = J + 1
C                 check condition for skipping historic peaks
                  IF (HISTPD .LE. 0.0) SKIP = 1
                  IF (PK(I)+0.1 .LT. QHIOUT) SKIP = 1
C                 set XQUAL
                  IF (STRFND(L12,QFLG(1,I),L1,C3) .GT. 0) THEN
                    POS = POS + 1
                    XQUAL(J)(POS:POS) = 'D'
                  END IF
                  IF (STRFND(L12,QFLG(1,I),L1,C8) .GT. 0) THEN
                    IF (POS .GT. 0) THEN
C                     must be both 3 & 8
                      XQUAL(J)(POS:POS) = 'X'
                    ELSE
C                     is just 8
                      POS = POS + 1
                      XQUAL(J)(POS:POS) = 'G'
                    END IF
                    SKIP = 1
                  END IF
                  IF (STRFND(L12,QFLG(1,I),L1,C4) .GT. 0) THEN
                    POS = POS + 1
                    XQUAL(J)(POS:POS) = 'L'
                  END IF
                  IF (STRFND(L12,QFLG(1,I),L1,C6) .GT. 0 .OR.
     &              STRFND(L12,QFLG(1,I),L1,CC) .GT. 0) THEN
                    POS = POS + 1
                    XQUAL(J)(POS:POS) = 'K'
                    IF (URBREG .EQ. 1) SKIP = 1
                  END IF
                  POS = POS + 1
                  XQUAL(J)(POS:POS) = 'H'
                  IF (HISTPD .GT. 0.0 .AND. PK(I) .GE. QHIOUT
     $                .AND. SKIP .NE. 1) THEN
C                   historic flood to be used, so tag year with -
                    IWYSN(J) = -ABS(WYR(I))
C                   count historic peaks to use
                    NHIST = NHIST + 1
                  ELSE
C                   historic peaks not to be used
                    IWYSN(J) = WYR(I)
                    NSYS = NSYS + 1
                  END IF
C                 fill peak flow array
                  PKSABG(J) = PK(I)
C                 make negative so won't be used
                  IF (SKIP .EQ. 1) PKSABG(J) = -ABS(PKSABG(J))
                END IF
              END IF
 50         CONTINUE
C           sort large to small so positive historic peaks come
C           first and minus tagged peaks come last
C           sort small to large on year so negative values
C           come first
            DO 56 II = 1,J-1      
              DO 54 I = 2,J
C               IF (PKSABG(I) .GT. PKSABG(I-1)) THEN
                IF (IWYSN(I) .LT. IWYSN(I-1)) THEN
C                 switch peaks
                  RTEMP = PKSABG(I)
                  PKSABG(I) = PKSABG(I-1)
                  PKSABG(I-1) = RTEMP
C                 switch years
                  ITEMP = IWYSN(I)
                  IWYSN(I) = IWYSN(I-1)
                  IWYSN(I-1) = ITEMP
C                 switch codes
                  CTEMP = XQUAL(I)
                  XQUAL(I) = XQUAL(I-1)
                  XQUAL(I-1) = CTEMP
                END IF
 54           CONTINUE
 56         CONTINUE
C
C           put NHIST on dataset attribute for possible use in GLS
            IF (NHIST .GT. 0) THEN
              SAIND = 274
              SALEN = 1
              CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,L1,NHIST,RETCOD)
              IF (RETCOD .NE. 0) THEN
C               Could not put attribute & on data set &. Error code &.
Cprh                GROUP  = 10
Cprh                IBUF(1)= SAIND
Cprh                IBUF(2)= DSN
Cprh                IBUF(3)= RETCOD
Cprh                IWRT   = 0
Cprh                CALL PMXTXI (MESSFL,SCLU,GROUP,MAXL,SCNFG,IWRT,
Cprh     &                       L3,IBUF)
                !LOG IT
              END IF
            END IF

            IF (IBCPUN.EQ.1 .OR. IBCPUN.EQ.3 .OR. IBCPUN.EQ.5) THEN
C             set input specification attributes
              SAIND = 278  ! J407BY
              CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,L1,BEGYR,RETCOD)
              SAIND = 279  ! J407EY
              CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,L1,ENDYR,RETCOD)
              SAIND = 271  ! J407SO
              CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,L1,ISKUOP,RETCOD)
              SAIND = 276  ! J407UR
              CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,L1,URBREG,RETCOD)
              SAIND = 272  ! J407GS
              CALL WDBSAR (WDMSFL,DSN,MESSFL,SAIND,L1,GENSKU,RETCOD)
              SAIND = 275  ! J407SE
              CALL WDBSAR (WDMSFL,DSN,MESSFL,SAIND,L1,RMSEGS,RETCOD)
              SAIND = 270  ! J407HO
              CALL WDBSAR (WDMSFL,DSN,MESSFL,SAIND,L1,QHIOUT,RETCOD)
              SAIND = 81   ! YRSHPK
              CALL WDBSAR (WDMSFL,DSN,MESSFL,SAIND,L1,HISTPD,RETCOD)
              SAIND = 269  ! J407LO
              CALL WDBSAR (WDMSFL,DSN,MESSFL,SAIND,L1,QLWOUT,RETCOD)
              SAIND = 273  ! J407BQ
              CALL WDBSAR (WDMSFL,DSN,MESSFL,SAIND,L1,GAGEB,RETCOD)
            END IF

C           look for systematic peaks
            XGAGEB = -1.0E20
            DO 60 I = 1,NREC
              SKIP = 0
              POS = 0
              IF (STRFND(L12,QFLG(1,I),L1,C7) .LE. 0) THEN
                IF (WYR(I) .GE. BEGYR .AND. WYR(I) .LE. ENDYR) THEN
                  NSYS = NSYS + 1
                  J = J + 1
C                 set XQUAL
                  IF (STRFND(L12,QFLG(1,I),L1,C3) .GT. 0) THEN
                    POS = POS + 1
                    XQUAL(J)(POS:POS) = 'D'
                    SKIP = 1
                  END IF
                  IF (STRFND(L12,QFLG(1,I),L1,C8) .GT. 0) THEN
                    IF (POS .GT. 0) THEN
C                     must be both 3 & 8
                      XQUAL(J)(POS:POS) = 'X'
                    ELSE
C                     is just 8
                      POS = POS + 1
                      XQUAL(J)(POS:POS) = 'G'
                    END IF
                    SKIP = 1
                  END IF
                  IF (STRFND(L12,QFLG(1,I),L1,C4) .GT. 0) THEN
                    POS = POS + 1
                    XQUAL(J)(POS:POS) = 'L'
                    IF (PK(I) .GT. XGAGEB) XGAGEB = PK(I)
                  END IF
                  IF (STRFND(L12,QFLG(1,I),L1,C6) .GT. 0 .OR.
     &                STRFND(L12,QFLG(1,I),L1,CC) .GT. 0) THEN
                    POS = POS + 1
                    XQUAL(J)(POS:POS) = 'K'
                    IF (URBREG .EQ. 1) SKIP = 1
                  END IF
                  IWYSN(J) = WYR(I)
                  PKSABG(J) = PK(I)
                  IF (SKIP .EQ. 1) PKSABG(J) = -ABS(PKSABG(J))
                END IF
              END IF
 60         CONTINUE
C
            IF (GAGEB.LE.0.0 .AND. XGAGEB.GT.0.0) THEN
C             set GAGEB since its not supplied
              GAGEB = XGAGEB
            END IF
          END IF
        END IF
      IF (NREC.LE.0 .AND. DSNIND.LT.DSNCNT) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   OUTPT1
     I                    (STAID, SYSUAV, SYSUSD, SYSSKW, WRCUAV,
     I                     WRCUSD, WRCSKW, WRCFC, NHSTPN, NSYS,
     I                     MESSFL, WDMSFL, PAUSE)
C
C     + + + PURPOSE + + +
C     This routine puts computed statistics on user's WDM file as
C     attributes.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NHSTPN, NSYS, MESSFL, WDMSFL, PAUSE
      REAL      WRCFC(*), WRCSKW, WRCUSD, WRCUAV, SYSSKW,
     &          SYSUSD, SYSUAV
      CHARACTER*90   STAID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STAID  - station identification number
C     SYSUAV - mean of systematic peaks
C     SYSUSD - standard deviation of systematic peaks
C     SYSSKW - skew of systematic peaks
C     WRCUAV - WRC mean of peaks
C     WRCUSD - WRC standard deviation of peaks
C     WRCSKW - WRC skew of peaks
C     WRCFC  - array of logs of computed peaks
C     NHSTPN - number of historic peaks
C     NSYS   - number of systematic peaks
C     MESSFL - Fortran unit number of J407 message file
C     WDMSFL - Fortran unit number of user's WDM file
C     PAUSE  - Indicator flag for pause between stations
C              1 - yes, pause and wait for user response
C              2 - no, display summary of results and continue
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cpkdsn.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I, J, RETCNT, RETC(17), MAXL, ORD(9), 
     $             INDX(15), DSN, XTRIND(2)
Cprh     $             RTCMND, ONUM, OTYP(4), 
Cprh     $             DSNX(3), INIT, IWRT, GROUP, SCLU, LEN, LEN1, TXTL(13),
      REAL         PEAKST(17), XTRORD(2)
Cprh     $             XTRYR(2), RDUM
Cprh      DOUBLE PRECISION  DDUM

      CHARACTER*1  BLNK, FLAG, TXT(32)
      CHARACTER*50 TXTW
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNAL + + +
cprh      EXTERNAL    QEXTRA
      EXTERNAL    ZIPI, ZIPC, CVARAR, LFTSTR
      EXTERNAL    WDBSAR
Cprh      EXTERNAL    PMXTXM, Q1INIT, QSTCTF, QSETCT, QSETI, QSETR, Q1EDIT
C
C     + + + DATA INITIALIZATIONS + + +
C                <--------- return period --------->  wrc/systematic
C                1.25  2   5  10  25  50 100 200 500    mn  sd  sk
Cprh  indices adjusted to account for inclusion of 1.5 and 2.33 intervals, 5/05
      DATA ORD  / 12, 17, 21, 22, 24, 26, 27, 28, 29 /
      DATA INDX / 65, 66, 67, 68, 69, 70, 71, 72, 73,   78, 79, 77,
     $                                                  74, 75, 76 /
Cprh      DATA        XTRYR,       XTRIND
Cprh     $          / 1.5, 2.33,   448, 449 /
      DATA        XTRORD,     XTRIND
     $          / 14, 18,     448, 449 /
      DATA TXTW / 'WARNING:  problem with flagged (*) attributes     '/
      DATA SCLU, LEN1, MAXL, BLNK, FLAG   !, ONUM, OTYP,   TXTL
     $    / 121,    1,   20,  ' ',  '*' / !,    4, 4,1,1,1, 15, 12*1 /
C
C     + + + OUTPUT FORMATS + + +
 2001 FORMAT (//, ' WARNING:  problem adding attribute to wdm file',
     $         /, '           station id  = ', A15,
     $         /, '           data set #  = ', I5,
     $         /, '           attribute # = ', I5, ' value = ', F10.2,
     $         /, '           return code = ', I5 )
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize values
      I = 17
      RETCNT = 0
      CALL ZIPI ( I, RETCNT, RETC )
      I = 32
      CALL ZIPC ( I, BLNK, TXT )
      DSN = DSNBUF(DSNIND)
C
C     put standard t-year peaks on wdm as attributes
      DO 20 I = 1, 9
        J = ORD(I)
        IF (ABS(WRCFC(J)) .LT. 20.0) THEN
          PEAKST(I) = 10.0**WRCFC(J)
          CALL WDBSAR ( WDMSFL, DSN, MESSFL, INDX(I), LEN1, PEAKST(I),
     O                  RETC(I) )
        ELSE
C         magnitude of the exponent is too large
          PEAKST(I) = -999.
          RETC(I)   = -9999
        ENDIF
        IF (RETC(I) .NE. 0) THEN
C         problem writing to wdm file
          RETCNT = RETCNT + 1
          TXT(I+15) = FLAG
          WRITE (91,2001) STAID(1:15), DSN, INDX(I), PEAKST(I), RETC(I)
        END IF
   20 CONTINUE
C
C     put non-standard t-year peaks on wdm as attributes
      DO 25 I = 1, 2
Cprh    1.5 and 2.33 yr intervals now part of standard calculated peaks
Cprh        CALL QEXTRA ( XTRYR(I), PEAKST(I+15) )
Cprh        IF (PEAKST(I) .GT. 0) THEN
        J = XTRORD(I)
        IF (ABS(WRCFC(J)) .LT. 20.0) THEN
          PEAKST(I+15) = 10.0**WRCFC(J)
          CALL WDBSAR ( WDMSFL, DSN, MESSFL, XTRIND(I), LEN1,
     $                  PEAKST(I+15), RETC(I+15) )
        ELSE
C         magnitude of the exponent is too large
          RETC(I+15)   = -9999
        ENDIF
        IF (RETC(I+15) .NE. 0) THEN
C         problem writing to wdm file
          RETCNT = RETCNT + 1
          TXT(I+30) = FLAG
          WRITE (91,2001) STAID(1:15), DSN, XTRIND(I), PEAKST(I+15),
     $                    RETC(I+15)
        END IF
   25 CONTINUE
C
C     put bulletin 17b & systematic mean, sd, and skew of log of Q on wdm
      PEAKST(10) = WRCUAV
      PEAKST(11) = WRCUSD
      PEAKST(12) = WRCSKW
      PEAKST(13) = SYSUAV
      PEAKST(14) = SYSUSD
      PEAKST(15) = SYSSKW
      DO 30 I = 10, 15
        CALL WDBSAR ( WDMSFL, DSN, MESSFL, INDX(I), LEN1, PEAKST(I),
     O                  RETC(I) )
        IF (RETC(I) .NE. 0) THEN
C         problem writing to wdm file
          RETCNT = RETCNT + 1
          TXT(I+15) = FLAG
          WRITE (91,2001) STAID(1:15), DSN, INDX(I), PEAKST(I), RETC(I)
        END IF
   30 CONTINUE
C
C     results to screen left-justify station id in txt buffer
      I = 15
      CALL CVARAR ( I, STAID(1:I), I, TXT )
      CALL LFTSTR ( I, TXT )
C
CprhC     put current dsn, index of dsn, and total dsn in buffer
Cprh      DSNX(1) = DSN
Cprh      DSNX(2) = DSNIND
Cprh      DSNX(3) = DSNCNT
C
Cprh      IF (PAUSE .EQ. 2) THEN
CprhC       no pause, summery text and go,
Cprh        IF (DSNIND .EQ. 1) THEN
CprhC         first data set
Cprh          INIT = 1
Cprh          IWRT = -1
Cprh        ELSE 
CprhC         continuation
Cprh          INIT = -1
Cprh          IWRT = -1
Cprh        END IF
        IF (RETCNT .EQ. 0) THEN
C         analyzed station &, dsn & (& of &)
          !LOG IT
Cprh          GROUP = 52
        ELSE
C         problems with station &, dsn & (& of &)
          !LOG IT
Cprh          GROUP = 53
        END IF
Cprh        LEN = 15
Cprh        CALL PMXTXM ( MESSFL, SCLU, GROUP,
Cprh     I                MAXL, INIT, ONUM, OTYP, IWRT,
Cprh     I                DSNX, RDUM, DDUM, LEN, TXT )
Cprh      ELSE
CprhC       pause, bulletin 17B estimates to screen
Cprh        GROUP = 54
Cprh        CALL Q1INIT ( MESSFL, SCLU, GROUP )
Cprh        LEN = 12
Cprh        CALL QSETR ( LEN, PEAKST )
Cprh        LEN = 3
Cprh        CALL QSETI ( LEN, DSNX )
Cprh        I   = 13
Cprh        LEN = 27
Cprh        CALL QSETCT ( I, TXTL, LEN, TXT )
Cprh        IF (RETCNT .GT. 0) THEN
CprhC         include warning text
Cprh          I = 14
Cprh          LEN = 50
Cprh          CALL QSTCTF ( I, LEN, TXTW )
Cprh        END IF
Cprh        CALL Q1EDIT ( RTCMND )
Cprh      END IF
C
      RETURN
      END
