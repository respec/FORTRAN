C
C
C
      SUBROUTINE   GETTB
     I                  (MESSFL,TNAME,WDMSFL,DSN,
     I                   MREC,J407FG,INITFG,
     M                   NROW,
     O                   PK,WYR,QFLG,RETCOD)
C
C     + + + PURPOSE + + +
C     GETTB retrieves annual peak data from ANNUAL PEAKS tables
C     on the WDM file for use in J407.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,WDMSFL,DSN,MREC,J407FG,INITFG,NROW
      INTEGER      WYR(MREC), RETCOD 
      REAL         PK(MREC)
      CHARACTER*16 TNAME
      CHARACTER*1  QFLG(12,MREC)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     TNAME  - table name of data on WDM
C     WDMSFL - WDM data file number
C     DSN    - data set number
C     MREC   - maximum number of peaks allowed
C     J407FG - 0 = use full period of record
C              1 = limit output based on start and end attributes
C                  J407BY and J407EY
C     INITFG - initialize screen flag
C              0 - dont init, add to existing text on screen
C              1 - first write to screen, init screen
C     NROW   - number of rows in table
C     PK     - annual peak discharge
C     WYR    - water year
C     QFLG   - peak discharge quality code
C     RETCOD - return code from wdm retrieval, standard codes
C              except -321 means table templete names didn't match
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K,I1,I80,GRP,QNU,XFLDS,XSPA,XLEN(12),
     &             XCOL(12),XNUM(4),AGRP,AQNU,AFLDS,ALEN(12),ACOL(12),
     &             ASPA,ANUM(12),TABID,DATFLG,MXTBTL,IMO(200),
     &             FROW,FSPA,CHGFG,MINYR,MAXYR,STEMP,ETEMP,
     &             SALEN,SAIND,ERRCOD,ECOL,LMESFL,LCLU,LGRP
      REAL         TBRBUF(4000)
      CHARACTER*1  TBCBUF(80,200),XTYP(12),ATYP(12)
      CHARACTER*5  FMTI
      CHARACTER*7  FMTR
      CHARACTER*16 MNAME
      CHARACTER*80 BUF
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDTBSP, WTBGET, WTBDCD, WDBSGI, CHRCHR
      EXTERNAL     WDTBCG
C
C     + + + END SPECIFICATIONS + + +
C
      I1    = 1
      I80   = 80
      MXTBTL= 20*NROW
      TABID = 1
      FROW  = 1
      FSPA  = 1
C
C     get actual location of table data set template
      GRP= 20 
      QNU= 1
      CALL WDTBCG (MESSFL,WDMSFL,GRP,QNU,
     O             LMESFL,LCLU,LGRP,RETCOD)
      write (*,*) 'GETTB, WDTBCG: RETCOD ',RETCOD
      IF (RETCOD.EQ.0) THEN
C       get info on table format
        CALL WDTBSP (LMESFL,LCLU,LGRP,MNAME,XFLDS,XTYP,XLEN,XCOL,
     &               XSPA,XNUM,AGRP,AQNU,AFLDS,ATYP,ALEN,ACOL,ASPA,
     &               ANUM,RETCOD)
       write (*,*) 'GETTB, WDTBSP: MNAME,TNAME,RETCOD ',
     $                             MNAME,TNAME,RETCOD
        IF(MNAME.EQ.TNAME) THEN
          DATFLG=1
C         get the table data
        write (*,*) 'GETTB, WTBGET: WDMSFL,DSN,TNAME,TABID ',
     $                              WDMSFL,DSN,TNAME,TABID
        write (*,*) 'GETTB, WTBGET: DATFLG,FROW,NROW,FSPA,XSPA ',
     $                              DATFLG,FROW,NROW,FSPA,XSPA
          CALL WTBGET (WDMSFL,DSN,TNAME,TABID,DATFLG,FROW,NROW,FSPA,
     &                 XSPA,TBRBUF,RETCOD)
        write (*,*) 'GETTB, WDBGET: RETCOD ',RETCOD
          IF(RETCOD.EQ.0) THEN
            CALL WTBDCD (XFLDS,NROW,XSPA,XLEN,XTYP,XCOL,TBRBUF,
     &                   MXTBTL,TBCBUF,RETCOD)
        write (*,*) 'GETTB, WTBDCD: RETCOD ',RETCOD
            IF(RETCOD.EQ.0) THEN
C             decode and load data arrays
              FMTI= '(I  )'
              DO 5 I= 1,NROW
                WRITE (FMTI(3:4),'(I2)') XLEN(1)
                ECOL= XCOL(1)+ XLEN(1)- 1
                CALL CHRCHR (I80,TBCBUF(1,I),BUF)
                READ (BUF(XCOL(1):ECOL),FMTI,ERR=7) WYR(I)
                WRITE (FMTI(3:4),'(I2)') XLEN(2)
                ECOL= XCOL(2)+ XLEN(2)- 1
                READ (BUF(XCOL(2):ECOL),FMTI,ERR=7) IMO(I)
 5            CONTINUE
 7            CONTINUE
C             adjust year to water year
              MINYR = 9999
              MAXYR = -9999
              DO 10 I = 1,NROW
                IF (IMO(I).GT.9) WYR(I) = WYR(I) + 1
                IF (WYR(I).LT.MINYR) MINYR = WYR(I)
                IF (WYR(I).GT.MAXYR) MAXYR = WYR(I)
 10           CONTINUE
              FMTR= '(F  .0)'
              WRITE (FMTR(3:4),'(I2)') XLEN(4)
              ECOL= XCOL(4)+ XLEN(4)- 1
              DO 15 I= 1,NROW
                CALL CHRCHR (I80,TBCBUF(1,I),BUF)
                READ (BUF(XCOL(4):ECOL),FMTR,ERR=17) PK(I)
                CALL CHRCHR(XLEN(5),TBCBUF(XCOL(5),I),QFLG(1,I))
 15           CONTINUE
 17           CONTINUE
C
              IF (J407FG .EQ. 1) THEN
C               get begining and ending year as attributes and check
                CHGFG = 0
                SAIND = 278
                SALEN = 1
                CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,STEMP,ERRCOD)
                IF (ERRCOD.EQ.0 .AND. STEMP.GT.MINYR .AND.
     1                                STEMP.LT.MAXYR) THEN
                  MINYR = STEMP
                  CHGFG = 1
                END IF
                SAIND = 279
                CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,ETEMP,ERRCOD)
                IF (ERRCOD.EQ.0 .AND. ETEMP.GT.MINYR .AND.
     1                                ETEMP.LT.MAXYR) THEN
                  MAXYR = ETEMP
                  CHGFG = 1
                END IF
                IF (CHGFG .EQ. 1) THEN
                  J = 0
                  DO 20 I = 1,NROW
                    IF (WYR(I).GE.MINYR .AND. WYR(I).LE.MAXYR) THEN
                      J = J + 1
                      WYR(J)= WYR(I)
                      PK(J) = PK(I)
                      DO 18 K = 1,12
                        QFLG(K,J) = QFLG(K,I)
 18                   CONTINUE
                    END IF
 20               CONTINUE
C                 reset number of rows(years)
                  NROW = J
                END IF
              END IF
            ENDIF
          ENDIF
        ELSE
C         set retcod if not 0
          IF (RETCOD .EQ. 0) RETCOD = -321
        ENDIF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETTS
     I                (WDMSFL,DSN,MREC,
     O                 PK,WYR,QFLG,NREC)
C
C     + + + PURPOSE + + +
C     Retrieves time series data from a WDM file for use in J407.
C     Does NOT retrieve any quality flags!!!  Returns blank
C     strings for quality flag.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL, DSN, MREC, NREC
      INTEGER   WYR(MREC)
      REAL      PK(MREC)
      CHARACTER*1 QFLG(12,MREC)
C
C     + + + ARGUMENT DEFINTION + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data set number
C     MREC   - maximum number of peaks to retrieve
C     PK     - annual maximum peak discharge
C     WYR    - water year of peak
C     QFLG   - data quality flags for peak
C     NREC   - number of annual peaks
C     MREC   - maximum number of peaks
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GPFLG, TDSFRC, SYR(6), EYR(6), RETCOD, TUNITS, STEMP,
     &          DTRAN, DELT, NVAL, FLG, JYR, J, L12, ERRCOD, ETEMP,
     &          SALEN, SAIND
      REAL      VAL(200)
      CHARACTER*1 BLK
C
C     + + + EXTERNALS + + +
      EXTERNAL WTFNDT, WDTGET, ZIPC, WDBSGI
C
C     + + + DATA INTIALIZATIONS + + +
      DATA  BLK, L12
     #    / ' ',  12 /
C
C     + + + END SPECIFICATIONS + + +
C
C     get start and end dates then peaks and quality flags
      GPFLG=1
      TDSFRC=1
      CALL WTFNDT(WDMSFL,DSN,GPFLG,TDSFRC,SYR,EYR,RETCOD)
C     get begining and ending year as attributes and check
      SAIND = 278
      SALEN = 1
      CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,STEMP,ERRCOD)
      IF (ERRCOD.EQ.0 .AND. STEMP.GT.SYR(1) .AND. STEMP.LT.EYR(1))
     &   SYR(1) = STEMP
      SAIND = 279
      CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,ETEMP,ERRCOD)
      IF (ERRCOD.EQ.0 .AND. ETEMP.GT.SYR(1) .AND. ETEMP.LT.EYR(1))
     &   EYR(1) = ETEMP
C
      IF(RETCOD.EQ.0) THEN
        TUNITS=6
        DTRAN=0
        DELT=1
        NVAL=EYR(1)-SYR(1)+1
        FLG=31
        CALL WDTGET(WDMSFL,DSN,DELT,SYR,NVAL,DTRAN,FLG,TUNITS,
     &              VAL,RETCOD)
C
        IF (RETCOD .EQ. 0) THEN
C         fill water year, peak, and quality arrays
          JYR=0
          L12=12
          DO 30 J=1,NVAL
            IF(VAL(J).GE.0.0) THEN
              JYR=JYR+1
              PK(JYR)=VAL(J)
              WYR(JYR)=SYR(1)+J-1
              CALL ZIPC(L12,BLK,QFLG(1,JYR))
            ENDIF
 30       CONTINUE
          NREC=JYR
        ELSE
C         couldn't get data
          NREC = -1
        END IF
      ELSE
        NREC = -1
C
      ENDIF
C
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDPKGT 
     I                   (MESSFL,WDMSFL,DSN,MAXPKS,
     O                    NUMPKS,PK,WYR)
C
C     + + + PURPOSE + + +
C     This routine retrieves annual time series from a WDM data set
C     for either a table or time series type data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMSFL, DSN, MAXPKS, NUMPKS, WYR(MAXPKS)
      REAL      PK(MAXPKS)
C     
C     + + + ARGUMENT DEFINITIONS + + + 
C     MESSFL - AIDE messgae file with table templet
C     WDMSFL - users WDM file containing the data set
C     DSN    - data set number for the annual time series
C     MAXPKS - size of PK and WY arrays
C     NUMPKS - number of peaks(years) retrieved
C     PK     - annual series 
C     WYR    - water year associated with each annual series value
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TABID, LREC, TBCNT, TGRPT, TGRP, TQNU,
     &          RETCOD, J407FG, INIT        
      CHARACTER*16 TNAME
      CHARACTER*1  QCOD(12,200), MFID(2)
C
C     + + + EXTERNALS + + + 
      EXTERNAL  GETTB,  GETTS, WDTBFX 
C
C     + + + END SPECIFICATIONS + + + 
C     
C     check for data set type
      TNAME = 'AN.PEAKS        '
      TABID = 1
      CALL WDTBFX(WDMSFL, DSN, TABID, TNAME,
     O            TBCNT, LREC, TGRPT, MFID, TGRP, TQNU, NUMPKS,
     O            RETCOD)
      IF(RETCOD.EQ.0) THEN
C       must be table dataset
        J407FG = 0
        INIT = 1
        CALL GETTB (MESSFL, TNAME, WDMSFL, DSN, MAXPKS,
     I              J407FG, INIT,
     M              NUMPKS,
     O              PK, WYR, QCOD,RETCOD)
      ELSE
C       must be time series data set
        CALL GETTS (WDMSFL,DSN,MAXPKS,
     &              PK(1),WYR(1),QCOD(1,1),NUMPKS)
      END IF
C
      RETURN
      END
