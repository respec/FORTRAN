C
C
C
      MODULE HMDISPLY
C
C     + + + PURPOSE + + +
C     module containing utility module DISPLY
C
C     + + + HISTORY + + +
C     2009/05/01 PBD - initial implementation of module version of DISPLY
C
C     + + + MODULE VARIABLES + + +
      IMPLICIT NONE
C
      TYPE HTDISPLY
        SEQUENCE
        INTEGER   TITLE(7),TRAN(1),PIVL,DIGIT1,FILE1,PYRFG,DIGIT2,
     $            FILE2,PYREND,DUMA(13),UUNITS,PSHFG,PDELT,NIVL,
     $            DISPNO,NDAY(12),TRNCOD,MSGFL,MESSU,FMT1(16),
     $            FMT2(30),FMT3(30),FMT4(30),FMT5(30),MONTHS(2,12),
     $            IVLDAY,HRSPAN(2),DASPAN(2),MNSPAN(2)
        REAL      BLANKR,STVAL,DELT,A,B,THRSH1,THRSH2,DATA(60,31),
     $            DYVAL(31,12)
        INTEGER   YR,MON,DAY,HR,MIN,IVL,IVL1,NDAYS,PIVLNO,NXTMON,
     $            HRFG,DAYFG,EDAYFG,EMONFG,EPYRFG,IFP
      END TYPE HTDISPLY
C
      CONTAINS   
C
C
C
      SUBROUTINE   PDISPL
     I                    (NDELT,SDATIM,NDAMON,EMFG,
     M                     OSVKEY)
C
C     + + + PURPOSE + + +
C     Process the input for the disply module
C
C     + + + HISTORY + + +
C     2009/04/22 JLK - use OSV module version
C     2009/05/01 PBD - initial implementation of module version of DISPLY
C
C     + + + MODULES + + +
      USE HIOOSV 
      USE HMMSG, ONLY : OMSTI,OMSTC,OMSGMSG
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDELT,SDATIM(5),NDAMON(12),EMFG,OSVKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     OSVKEY - last osv file record written
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION DISPLY1 + + +
      INCLUDE   'crin2.inc'
C
C     + + + MODULE VARIABLES + + +
      TYPE(HTDISPLY)  HDISPLY
      POINTER   (P,HDISPLY)
      POINTER   (PDATIM,DATIM),(POSV,OSV),
     $          (PINFO1,INFO1),(PINFO2,INFO2)
      INTEGER      INFO1(15),DATIM(5),OSV(2500)
      REAL         INFO2(4)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K,I1,I2,I4,I6,I15,I1440,MONTHT(2,12),
     $             COL,Y,MO,ND,ND28,
     $             FORS(3),FORF(2),FORA(2),FORE,FORT(3),
     $             NB4,ND4,OSVKND,
     $             OSVKST,OSVREC,SUBB,FMT11(16),SCLU,SGRP,
     $             INITFG,CONT,CLEN,EMSGCT
      INTEGER      BLNK1,ZEROC
      INTEGER      BASEADDR,ADDR
      CHARACTER*4  KWDLIB(6)
      CHARACTER*80 CHSTR,ERRMSG(4)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1),(KWDLIB,KWDLB1)
      CHARACTER*1  CHSTR1(80),KWDLB1(24)
C
C     + + + FUNCTION + + +
      INTEGER      DAYMNH,CHKSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC    MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL     ITABLE,HSCKFL,RTABLE,CHKSTR,SETVEC,WMSGTT,DAYMNH
C
C     + + + INPUT FORMATS + + +
 1020 FORMAT (6(A4,1X),2A1)
 1030 FORMAT (16A4)
 1040 FORMAT (12A4)
 1050 FORMAT (9A4)
 1060 FORMAT (3A4)
 1070 FORMAT (3(A4,A2))
 1080 FORMAT (A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),
     $        /,' PROCESSING DISPLY OPERATION NO.',I5,
     $            '  TIME INTERVAL',I5,' MINS')
 2030 FORMAT (/,' FINISHED PROCESSING DISPLAY OPERATION NO.',I5,
     $        /,' ',132('+'))
 2070 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 371
C
C     flag pointer addresses
      BASEADDR = LOC(HDISPLY%TITLE)
      ADDR     = LOC(HDISPLY%IFP)
      WRITE(99,*) 'address of IFP',1+ ((ADDR - BASEADDR)/4),BASEADDR/4
C
C     record the osv keys in opntab
      OSVKST= OSVKEY+ 1
      OSVREC= 5
      OSVKND= OSVKEY+ OSVREC
C
C     point the osv to its memory location 
      P = LOC(OSVM(1,OSVKST))
      PINFO1 = LOC(HDISPLY%TITLE)
      PINFO2 = LOC(HDISPLY%A)  
      PDATIM = LOC(HDISPLY%YR)  
      POSV   = LOC(HDISPLY)
C
C     record the keys in opntab
      OPNTAB(7,OPNO)= OSVKST
      OPNTAB(8,OPNO)= OSVKND
      OSVKEY        = OSVKND
C
      IF (RESMFG.EQ.1) THEN
C       read the osv from osvfl - not implemented in this release
C        of hspf
      ELSE
C       initialize the entire osv area
        DO 10 I= 1, OSVREC*500
          OSV(I)= -999
 10     CONTINUE
      END IF
C
      HDISPLY%MESSU = FILE(1)
      HDISPLY%MSGFL = FILE(15)
      I1   = 1
      I2   = 2
      I4   = 4
      I6   = 6
      I15  = 15
      I1440= 1440
C
      IF (OUTLEV.GT.0) THEN
        WRITE (HDISPLY%MESSU,2000) OPTNO,NDELT
      END IF
C
C     get keyword and other strings
      SGRP  = 1
      INITFG= 1
      CLEN  = 80
      CALL WMSGTT (HDISPLY%MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
C
      READ (CHSTR,1020) KWDLIB,BLNK1,ZEROC
C
C     incorporate information obtained from global and opn seq blocks
      HDISPLY%DISPNO= OPTNO
      HDISPLY%DELT  = NDELT
      DO 20 I= 1,5
        DATIM(I)= SDATIM(I)
 20   CONTINUE
      DO 30 I= 1,12
        HDISPLY%NDAY(I)= NDAMON(I)
 30   CONTINUE
C
      HDISPLY%PIVLNO= 0
      HDISPLY%UUNITS= EMFG
C
C     process table-type disply-info1
      CALL ITABLE
     I             (I1,I1,I15,I1,
     M              INFO1)
C
      IF (HDISPLY%DIGIT1.EQ.BLNK1) THEN
        HDISPLY%DIGIT1= ZEROC
      END IF
C
      IF (HDISPLY%DIGIT2.EQ.BLNK1) THEN
        HDISPLY%DIGIT2= ZEROC
      END IF
C
C     check output files - if not open, then open with standard name
      IF (HDISPLY%FILE1 .GT. 0) THEN
        CALL HSCKFL
     I              (HDISPLY%FILE1)
      END IF
C
      IF (HDISPLY%FILE2 .GT. 0) THEN
        CALL HSCKFL
     I              (HDISPLY%FILE2)
      END IF
C
C     check for valid keywords
      WRITE (CHSTR(1:4),2070) HDISPLY%TRAN(1)
      HDISPLY%TRNCOD= CHKSTR (I4,I6,CHSTR1,KWDLB1)
C
      IF (HDISPLY%TRNCOD .EQ. 0) THEN
C       invalid keyword
        WRITE (CHSTR(1:4),2070) HDISPLY%TRAN(1)
        CALL OMSTC (I4,CHSTR1)
        ERRMSG(1)= " An invalid transformation (aggregation) " //
     1             "functional was specified in the "
        ERRMSG(2)= " user's control input.  The valid values " //
     1             "are SUM, AVER, MAX, MIN and LAST. "
        ERRMSG(3)= " The value supplied is '&CCC'.  Was it " //
     1             "mis-spelled or placed in the "
        ERRMSG(4)= " wrong columns?"
        EMSGCT= 4
        SGRP= 2
        CALL OMSGMSG(HDISPLY%MESSU,SCLU,SGRP,
     I               EMSGCT,ERRMSG,
     M               ECOUNT)
      ELSE
C       check for default
        IF (HDISPLY%TRNCOD .EQ. 6) THEN
          HDISPLY%TRNCOD= 1
          READ (KWDLIB(1),1080) HDISPLY%TRAN(1)
        END IF
      END IF
C
C     determine the initial value to go into arrays/accumulators
C     casentry trncod
      GO TO (80,80,90,100,80),HDISPLY%TRNCOD
C
 80   CONTINUE
C       case 1, 2, and 5       sum, aver, and last
        HDISPLY%STVAL= 0.0
        GO TO 110
 90   CONTINUE
C       case 3                 max
        HDISPLY%STVAL= -1.0E30
        GO TO 110
100   CONTINUE
C       case 4                 min
        HDISPLY%STVAL= +1.0E30
        GO TO 110
C       endcase
110   CONTINUE
C
C     read formatting data from msgfl
      SGRP  = 3
      INITFG= 1
      CLEN  = 80
      CALL WMSGTT (HDISPLY%MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1030) HDISPLY%FMT1
C
      INITFG= 0
      CLEN  = 80
      CALL WMSGTT (HDISPLY%MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1030) FMT11
C
      CLEN= 80
      CALL WMSGTT (HDISPLY%MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1040) ((MONTHT(I,J),I=1,2),J=1,6)
C
      CLEN= 80
      CALL WMSGTT (HDISPLY%MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1040) ((MONTHT(I,J),I=1,2),J=7,12)
C
      CLEN= 80
      CALL WMSGTT (HDISPLY%MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1050) FORS,FORF,FORA,FORE,HDISPLY%BLANKR
C
      CLEN= 80
      CALL WMSGTT (HDISPLY%MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1060) FORT
C
      CLEN= 80
      CALL WMSGTT (HDISPLY%MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1070) HDISPLY%HRSPAN,HDISPLY%DASPAN,HDISPLY%MNSPAN
C
      IF (HDISPLY%PIVL.GT.0) THEN
C       short-span summary is to be printed
        HDISPLY%PDELT= HDISPLY%PIVL* NDELT
        IF (MOD(I1440,HDISPLY%PDELT).NE.0) THEN
C         error - data interval, as implied by pivl, must be an
C         integer fraction of a day
          CALL OMSTI (OPTNO)
          CALL OMSTI (HDISPLY%PDELT)        
          ERRMSG(1)= " The interval of the data in a short-span " //
     1               "display (pdelt= delt*pivl mins)"
          ERRMSG(2)= " must divide exactly into a day (1440 mins).  " //
     1               "For disply operation &I   ,"
          ERRMSG(3)= " this is not the case since the value of "//
     1               "pdelt is &I   ."
          EMSGCT= 3
          SGRP= 4
          CALL OMSGMSG(HDISPLY%MESSU,SCLU,SGRP,
     I                 EMSGCT,ERRMSG,
     M                 ECOUNT)
        END IF
C
C       determine the particulars of this display
        IF (HDISPLY%PDELT.LT.60) THEN
C         use alternate format, so hours go from 0: through 23:
          DO 125 I=1,16
            HDISPLY%FMT1(I)=FMT11(I)
 125      CONTINUE
          HDISPLY%PSHFG= 1
          HDISPLY%NIVL = 60/HDISPLY%PDELT
        ELSE
          HDISPLY%PSHFG= 2
          HDISPLY%NIVL = 1440/HDISPLY%PDELT
        END IF
C
C       put the user's specification of the number of decimal digits
C       into these format building blocks
C
        HDISPLY%FMT1(5) = HDISPLY%DIGIT1
        HDISPLY%FMT1(9) = HDISPLY%DIGIT1
        HDISPLY%FMT1(15)= HDISPLY%DIGIT1
C
C       initialize the data array
        DO 160 I=1,31
          DO 150 J=1,60
            HDISPLY%DATA(J,I)= HDISPLY%STVAL
 150      CONTINUE
 160    CONTINUE
      ELSE
C       no short span display
        HDISPLY%PSHFG= 0
      END IF
C
      IF (HDISPLY%PYRFG.EQ.1) THEN
C       annual summary is required - initialize data array and
C       build printout formats
        FORF(2)= HDISPLY%DIGIT2
        DO 190 I=1,3
          HDISPLY%FMT2(I)= FORS(I)
          HDISPLY%FMT3(I)= FORS(I)
          HDISPLY%FMT4(I)= FORS(I)
          HDISPLY%FMT5(I)= FORT(I)
 190    CONTINUE
C
        DO 310 COL= 1,12
          K= COL+ HDISPLY%PYREND
          IF (K.LE.12) THEN
            MO= K
          ELSE
            MO= K- 12
          END IF
C         subscript base for format array
          SUBB= (2*COL)+ 2
C         build heading
          DO 220 I=1,2
            HDISPLY%MONTHS(I,COL)= MONTHT(I,MO)
 220      CONTINUE
C         build formats
          DO 230 I=1,2
            HDISPLY%FMT2(SUBB+I-1)= FORF(I)
            HDISPLY%FMT5(SUBB+I-1)= FORF(I)
 230      CONTINUE
C         find no. of days in a leap year
          Y = 1976
          ND= DAYMNH(Y,MO,NDAMON)
C
C         casentry (nd-28)
          ND28= ND-28
          IF (ND28 .EQ. 1) THEN
C           feb
            DO 250 I=1,2
              HDISPLY%FMT3(SUBB+I-1)= FORA(I)
              HDISPLY%FMT4(SUBB+I-1)= FORA(I)
 250        CONTINUE
          ELSE IF (ND28 .EQ. 2) THEN
C           apr,jun,sep,nov
            DO 270 I=1,2
              HDISPLY%FMT3(SUBB+I-1)= FORF(I)
              HDISPLY%FMT4(SUBB+I-1)= FORA(I)
 270        CONTINUE
          ELSE IF (ND28 .EQ. 3) THEN
C           jan,mar,may,jul,aug,oct,dec
            DO 290 I=1,2
              HDISPLY%FMT3(SUBB+I-1)= FORF(I)
              HDISPLY%FMT4(SUBB+I-1)= FORF(I)
 290        CONTINUE
          END IF
C
C         initialize data array
          ND4= ND
          CALL SETVEC
     I                 (ND4,HDISPLY%STVAL,
     O                  HDISPLY%DYVAL(1,COL))
C         blanks
          NB4= 31- ND4
          IF (NB4.GT.0) THEN
            CALL SETVEC
     I                   (NB4,HDISPLY%BLANKR,
     O                    HDISPLY%DYVAL((ND+1),COL))
          END IF
 310    CONTINUE
C
C       finish the formats
        HDISPLY%FMT2(28)= FORE
        HDISPLY%FMT3(28)= FORE
        HDISPLY%FMT4(28)= FORE
        HDISPLY%FMT5(28)= FORE
C       find no. of intervals in a day
        HDISPLY%IVLDAY  = 1440/NDELT
      END IF
C
C     process table-type disply-info2
      CALL RTABLE
     I             (I2,I1,I4,I1,
     M              INFO2)
C
      IF (OUTLEV.GT.0) THEN
        WRITE (HDISPLY%MESSU,2030) OPTNO
      END IF
C
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   HDISPL
     I                   (STIVL,WIDTH,LSTCAL,OSVKST)
C
C     + + + PURPOSE + + +
C     display a time series in tabular form
C
C     + + + HISTORY + + +
C     2009/05/05 PBD - initial implementation of module version  
C
C     + + + MODULES + + +
      USE       HIOOSV 
      USE HMMSG, ONLY : OMSTI,OMSG
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STIVL,WIDTH,LSTCAL,OSVKST
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIVL  - in inpad row
C     WIDTH  - of inpad row
C     LSTCAL - flag indicating last interval of run
C     OSVKST - starting block in osv
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION DISPLY2 + + +
      INCLUDE   'cmpad.inc'
      INCLUDE   'cmosv.inc'
C
C     + + + MODULE VARIABLES + + +
      TYPE(HTDISPLY)  HDISPLY
      POINTER   (P,HDISPLY)
      POINTER   (PDATIM,DATIM),
     $          (PINFO1,INFO1),(PINFO2,INFO2)
      INTEGER       INFO1(15),DATIM(5)
      REAL          INFO2(4)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IDELT,NDATE,NROWS,I,J,K,COL,DPIVL,IINDEX
      REAL      VAL
C
C     + + + EXTERNALS + + +
      EXTERNAL  ADDTIM,TRANS,SHDISP,YRDISP
C
C     + + + END SPECIFICATIONS + + +
C
C     point the osv to its memory location
      P = LOC(OSVM(1,OSVKST))
      PINFO1 = LOC(HDISPLY%TITLE)
      PINFO2 = LOC(HDISPLY%A)  
      PDATIM = LOC(HDISPLY%YR)  
C
      HDISPLY%IVL  = STIVL- 1
      HDISPLY%IVL1 = STIVL
      IDELT= HDISPLY%DELT
C
C     dummy pivl, so pivlno can't overflow in subroutine addtim
      DPIVL= 999
C
C     time loop
      DO 130 IINDEX = STIVL,WIDTH+ STIVL- 1
        HDISPLY%IVL = IINDEX
        HDISPLY%IVL1= HDISPLY%IVL1+ 1
C       increment time and set time-related flags
        CALL ADDTIM (IDELT,HDISPLY%NDAY,DPIVL,HDISPLY%PYREND,
     M               DATIM,HDISPLY%PIVLNO,
     O               HDISPLY%NDAYS,HDISPLY%NXTMON,HDISPLY%HRFG,
     O               HDISPLY%DAYFG,HDISPLY%EDAYFG,
     O               HDISPLY%EMONFG,HDISPLY%EPYRFG)
C
        IF (HDISPLY%IFP.GE.1) THEN
C         get and linearly transform the data
          VAL= HDISPLY%A* PAD(HDISPLY%IFP+HDISPLY%IVL1)+ HDISPLY%B
        ELSE
          VAL= 0.0
        END IF
C
        IF (HDISPLY%PSHFG.GT.0) THEN
C         determine element of data(*,*) with which we are now working
          IF (HDISPLY%PSHFG.EQ.1) THEN
            I= ((HDISPLY%MIN-1)/HDISPLY%PDELT)+ 1
            J= HDISPLY%HR
          ELSE
            I= (((HDISPLY%HR-1)*60+ HDISPLY%MIN- 1)/HDISPLY%PDELT)+ 1
            J= HDISPLY%DAY
          END IF
          CALL TRANS (HDISPLY%TRNCOD,HDISPLY%PIVL,VAL,
     M                HDISPLY%DATA(I,J))
        END IF
C
        IF (HDISPLY%PYRFG.GT.0) THEN
C         transform data to annual summary (daily) interval
          K= HDISPLY%MON- HDISPLY%PYREND
          IF (K.LE.0) THEN
            COL= K+ 12
          ELSE
            COL= K
          END IF
          CALL TRANS (HDISPLY%TRNCOD,HDISPLY%IVLDAY,VAL,
     M                HDISPLY%DYVAL(HDISPLY%DAY,COL))
        END IF
C
        IF (HDISPLY%EDAYFG.EQ.1) THEN
C         last interval in the day
          IF (HDISPLY%PSHFG.EQ.1) THEN
            NDATE= 3
            NROWS= 24
            CALL SHDISP (HDISPLY%HRSPAN,HDISPLY%DASPAN,NDATE,NROWS,
     $                   HDISPLY%FILE1,HDISPLY%TITLE,DATIM,
     $                   HDISPLY%PDELT,HDISPLY%TRAN,HDISPLY%NIVL,
     $                   HDISPLY%TRNCOD,HDISPLY%FMT1,
     $                   HDISPLY%STVAL,HDISPLY%THRSH1,
     M                   HDISPLY%DATA)
          END IF
C
          IF (HDISPLY%EMONFG.EQ.1) THEN
C           last interval in the month
            IF (HDISPLY%PSHFG.EQ.2) THEN
              NDATE= 2
              NROWS= HDISPLY%NDAYS
              CALL SHDISP (HDISPLY%DASPAN,HDISPLY%MNSPAN,NDATE,NROWS,
     $                     HDISPLY%FILE1,HDISPLY%TITLE,DATIM,
     $                     HDISPLY%PDELT,HDISPLY%TRAN,HDISPLY%NIVL,
     $                     HDISPLY%TRNCOD,HDISPLY%FMT1,
     $                     HDISPLY%STVAL,HDISPLY%THRSH1,
     M                     HDISPLY%DATA)
            END IF
C
            IF (HDISPLY%EPYRFG.EQ.1) THEN
C             last interval in printout year
              IF (HDISPLY%PYRFG.EQ.1) THEN
                NDATE= 2
                CALL YRDISP (NDATE,HDISPLY%FILE2,HDISPLY%TITLE,
     $                       DATIM,HDISPLY%TRAN,HDISPLY%TRNCOD,
     $                       HDISPLY%PYREND,HDISPLY%FMT2,HDISPLY%FMT3,
     $                       HDISPLY%FMT4,HDISPLY%FMT5,HDISPLY%STVAL,
     $                       HDISPLY%MONTHS,HDISPLY%NDAY,HDISPLY%BLANKR,
     M                       HDISPLY%DYVAL)
              END IF
            END IF
          END IF
        END IF
 130  CONTINUE
C
      IF (LSTCAL.EQ.1) THEN
C       this is the end of the run, dump any partly completed tables
        IF (HDISPLY%PSHFG.GT.0) THEN
          IF (HDISPLY%PSHFG.EQ.1) THEN
            IF (HDISPLY%EDAYFG.NE.1) THEN
              NDATE= 3
              NROWS= 24
              CALL SHDISP (HDISPLY%HRSPAN,HDISPLY%DASPAN,NDATE,NROWS,
     $                     HDISPLY%FILE1,HDISPLY%TITLE,
     $                     DATIM,HDISPLY%PDELT,HDISPLY%TRAN,
     $                     HDISPLY%NIVL,HDISPLY%TRNCOD,
     $                     HDISPLY%FMT1,HDISPLY%STVAL,
     $                     HDISPLY%THRSH1,
     M                     HDISPLY%DATA)
            END IF
          ELSE
            IF (HDISPLY%EMONFG.NE.1) THEN
              NDATE= 2
              NROWS= HDISPLY%NDAYS
              CALL SHDISP (HDISPLY%DASPAN,HDISPLY%MNSPAN,NDATE,NROWS,
     $                     HDISPLY%FILE1,HDISPLY%TITLE,
     $                     DATIM,HDISPLY%PDELT,HDISPLY%TRAN,
     $                     HDISPLY%NIVL,HDISPLY%TRNCOD,
     $                     HDISPLY%FMT1,HDISPLY%STVAL,
     $                     HDISPLY%THRSH1,
     M                     HDISPLY%DATA)
            END IF
          END IF
        END IF
C
        IF (HDISPLY%PYRFG.EQ.1) THEN
          IF (HDISPLY%EPYRFG.NE.1) THEN
            NDATE= 2
            CALL YRDISP (NDATE,HDISPLY%FILE2,HDISPLY%TITLE,DATIM,
     $                   HDISPLY%TRAN,HDISPLY%TRNCOD,HDISPLY%PYREND,
     $                   HDISPLY%FMT2,HDISPLY%FMT3,HDISPLY%FMT4,
     $                   HDISPLY%FMT5,HDISPLY%STVAL,HDISPLY%MONTHS,
     $                   HDISPLY%NDAY,
     $                   HDISPLY%BLANKR,
     M                   HDISPLY%DYVAL)
          END IF
        END IF
      END IF
C
      RETURN
      END SUBROUTINE
C    
      END MODULE HMDISPLY
