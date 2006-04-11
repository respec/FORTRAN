C
C
C
      SUBROUTINE OUTCNC (FLCN,MODID,K)
C
C     + + + PURPOSE + + +
C
C     Prints daily, monthly and annual pesticide concentration profiles
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +               
C
      INTEGER     FLCN,K
      CHARACTER*3 MODID
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     FLCN  - Fortran unit number for output file FLCN
C     MODID - character string for output file identification
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CMET.INC'
C
C     + + + LOCAL VARIABLES + + +
C
      REAL       TOTAL,ADS,DISS,CGAS
      INTEGER    I
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,SUBOUT
C
C     + + + OUTPUT FORMATS + + +
C
2002  FORMAT (1X,A3,T6,'PESTICIDE CONCENTRATION PROFILE',/,1X,A3,/,
     1        1X,A3,T6,'DATE (DAY-MONTH-YEAR)',T31,I2,' ',A4,', ',I2,/,
     2        1X,A3,1X,A20,/,1X,A3,/,1X,A3,T6,'HORIZON',T18,
     3        'COMPARTMENT',T35,'TOTAL',T46,'ADSORBED',T57,'DISSOLVED',
     4        T68,'GAS CONC.',/,1X,A3,T35,'(MG/KG)',T46,'(MG/KG)',T57,
     5        '(MG/L)',T68,'(MG/L)',/,1X,A3,T6,74(1H-),/,1X,A3,/,1X,A3)
2003  FORMAT (1X,A3,T6,'PESTICIDE CONCENTRATION AND TEMPERATURE ',
     1        'PROFILE',/,1X,A3,/,1X,A3,T6,'DATE (DAY-MONTH-YEAR)',T31,
     2        I2,' ',A4,', ',I2,/,1X,A3,1X,A20,/,1X,A3,/,1X,A3,T6,
     3        'HORIZON',T18,'COMPARTMENT',T35,'TOTAL',T46,'ADSORBED',
     4        T57,'DISSOLVED',T68,'GAS CONC.',T78,'TEMPERATURE',/,1X,
     5        A3,T35,'(MG/KG)',T46,'(MG/KG)',T57,'(MG/L)',T68,'(MG/L)',
     6        T81,'(oC)',/,1X,A3,T6,84(1H-),/,1X,A3,/,1X,A3)
2010  FORMAT (1X,A3,I4,T14,I8,T35,G10.4,T46,G10.4,T57,G10.4,T68,G10.4)
2011  FORMAT (1X,A3,I4,T14,I8,T35,G10.4,T46,G10.4,T57,G10.4,T68,G10.4,
     1        T80,G10.4)
C
C     + + + END SPECIFICATONS + + +
C
      MESAGE = 'OUTCNC'
      CALL SUBIN(MESAGE)
C
      IF (ITFLAG .EQ. 0) THEN
          WRITE(FLCN,2002) (MODID,I=1,3),DOM,CMONTH(MONTH),IY,
     1                     MODID,PSTNAM(K),(MODID,I=1,6)
      ELSE
          WRITE(FLCN,2003) (MODID,I=1,3),DOM,CMONTH(MONTH),IY,
     1                     MODID,PSTNAM(K),(MODID,I=1,6)
      ENDIF
C
      DO 10 I=1, NCOM2, LFREQ3
        DISS  = X(I)* 1.E6
        ADS   = DISS* KD(K,I)
        CGAS  = DISS* KH(K,I)
        TOTAL = (THETN(I)* DISS+ BD(I)* ADS+CGAS*(THETAS(I)
     1          -THETN(I)))/BD(I)
        IF (ITFLAG .EQ. 0) THEN
          WRITE(FLCN,2010) MODID,HORIZN(I),I,TOTAL,ADS,DISS,CGAS
        ELSE
          WRITE(FLCN,2011) MODID,HORIZN(I),I,TOTAL,ADS,DISS,CGAS,SPT(I)
        ENDIF
10    CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE OUTRPT (FLSS,MODIDO,MODIDS,K)
C
C     + + + PURPOSE + + + 
C
C     Prints daily, monthly, and annual
C     pesticide concentration profiles to the MODOUT.DAT file
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INTEGER     FLSS,K
      CHARACTER*3 MODIDO,MODIDS
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     FLSS   - Fortran unit number for output file FLSS
C     MODIDO - character string for OUT output file identification
C     MODIDS - character string for SNS output file identification
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CMISC.INC'
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CACCUM.INC'
C
C     + + + LOCAL VARIABLES + + +
C
      REAL         TOTAL,ADS,DISS,ER,RO,PV,CGAS
      CHARACTER*4  DAY,MNTH,YEAR
      INTEGER      I,MNTHP1
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +  
C      
      EXTERNAL SUBIN,SUBOUT
C
C     + + + DATA STATEMENTS + + +
C
      DATA  DAY,MNTH,YEAR / ' DAY','MNTH','YEAR' /
C
C     + + + OUTPUT FORMATS + + +
C
2000  FORMAT (1X,A3,6X,'19',I2,2I3,' 24  0',4(2X,G12.4))
2010  FORMAT (1X,A3,1X,3I2,1X,I4,1X,I5,3X,4(1X,G10.4))
2020  FORMAT (1X,A3,/,1X,A3,T10,'FOR ',A64,/,1X,A3)
2030  FORMAT (1X,A3,/,1X,A3,2X,'DATE',3X,'HORZ',2X,'LAYER',
     1        4X,'TOTAL',4X,'ADSORBED',3X,'DISSOLVED',2X,
     2        'GAS CONC.',/,1X,A3,23X,'(MG/KG)',3X,
     3        '(MG/KG)',5X,'(MG/L)',5X,'(MG/L)',/,1X,A3)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'OUTRPT'
      CALL SUBIN(MESAGE)
C
C     should output to SNAPSHOT.DAT file occur?
C
      IF (SSFLAG.EQ.1 .AND. FLSS.GT.0) THEN
        IF (NCHEM .GT. 1) WRITE(FLSS,2020)
     1                         (MODIDS,I=1,2),PSTNAM(K),MODIDS
C
C     The following write statement and its corresponding format
C     statement were added by ssh at aqua terra, 2-89, so that
C     SNAPSHOT is labelled in the output file.
C
        WRITE(FLSS,2030)(MODIDS,I=1,4)
        DO 30 I=1, NCOM2
          DISS  = X(I)* 1.E6
          ADS   = DISS* KD(K,I)
          CGAS  = DISS* KH(K,I)
          TOTAL = (THETN(I)* DISS+ BD(I)*ADS+ CGAS*(THETAS(I)
     1            -THETN(I)))/BD(I)
          WRITE(FLSS,2010) MODIDS,DOM,MONTH,IY,HORIZN(I),I,TOTAL,
     1                     ADS,DISS,CGAS
  30    CONTINUE
      ENDIF
C
C     Should output to MODOUT.DAT occur?
      IF (STEP4 .EQ. DAY) THEN
C
C       data to be displayed on a daily basis.
        ER = ERFLUX(K) * 1.0E5
        RO = ROFLUX(K) * 1.0E5
        PV = PVFLUX(K,1) * 1.0E5
        IF (FLSS.GT.0) THEN
          IF (NCHEM .GT. 1) WRITE(FLSS,2020)
     1                           (MODIDO,I=1,2),PSTNAM(K),MODIDO
          WRITE(FLSS,2000) MODIDO,IY,MONTH,DOM,RO,ER,DCOFLX(K),PV
        ENDIF
      ELSEIF (STEP4 .EQ. MNTH) THEN
C
C       Data to be displayed on a monthly basis.
        MNTHP1 = MONTH + 1
        IF (JULDAY .EQ. CNDMO(LEAP,MNTHP1)) THEN
C
C         Monthly pesticide volatilization flux
          MOUTPV=VOUTM(K,1)
          IF (FLSS.GT.0) THEN
            IF (NCHEM .GT. 1) WRITE(FLSS,2020)
     1                             (MODIDO,I=1,2),PSTNAM(K),MODIDO
            WRITE(FLSS,2000) MODIDO,IY,MONTH,DOM,MOUTP2(K),
     1                       MOUTP3(K),MCOFLX(K),MOUTPV
          ENDIF
        ENDIF
      ELSEIF (STEP4 .EQ. YEAR) THEN
C
C       Data to be displayed on annual basis.
        IF (JULDAY .EQ. CNDMO(LEAP,13)) THEN
C
C         Yearly pesticide volatilization flux
          YOUTPV=VOUTY(K,1)
          IF (FLSS.GT.0) THEN
            IF (NCHEM .GT. 1) WRITE(FLSS,2020)
     1                             (MODIDO,I=1,2),PSTNAM(K),MODIDO
            WRITE(FLSS,2000) MODIDO,IY,MONTH,DOM,YOUTP2(K),
     1                       YOUTP3(K),YCOFLX(K),YOUTPV
          ENDIF
        ENDIF
      ENDIF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE OUTPST(
     I  FLPS,MODID,K)
C
C     + + + PURPOSE + + +
C
C     Accumulates and outputs daily, monthly, and
C     annual summaries for pesticides
C     Modification date: 7/24/92 JAM
C     Included the provision of getting the pesticide flux
C     in lateral flow, AQUA TERRA Consultants, 9/93
C
C     + + + DUMMY ARGUMENTS + + +
C
      INTEGER     FLPS,K
      CHARACTER*3 MODID
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     FLPS  - Fortran unit number for output file FLPS
C     MODID - character string for output file identification
C     K     - chemical number being simulated (1-3)
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXVDT.INC'
      INCLUDE 'PMXYRS.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'TABLE.INC'
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CCROP.INC'
      INCLUDE 'CMET.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CACCUM.INC'
      INCLUDE 'CPTCHK.INC'
C
C     + + + LOCAL VARIABLES + + +
C
      REAL        VAR1,VAR2,XP(NCMPTS),VAR2RZ,SU,SD,SUMXP,VAR3,FP,F0,
     1            FL,WF,SA,RF,EF,UPF,DKF,PB,CB,PA,JLK,VOUT,SV,DVF,TV,
     2            FMRMVX,VAR4,VAR5,TCFD,TCFM,TCFY,DTRFM(3,NCMPTS),LA
      INTEGER     I,II,J,BSUMM,MNTHP1,NOPRT,NSUMM,IYEAR
      CHARACTER*4 YEAR,PEST,MNTH,DAY
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,SUBOUT
C
C     + + + DATA INITIALIZATIONS + + +
C
      DATA YEAR/'YEAR'/,MNTH/'MNTH'/,DAY/' DAY'/,PEST/'PEST'/
C
C     + + +OUTPUT FORMATS + + +
C
2000  FORMAT (1X,A3,1X,'****************************',/,1X,A3,1X,
     1        '* DAILY PESTICIDE OUTPUT   *')
2010  FORMAT (1X,A3,1X,'****************************',/,1X,A3,1X,
     1        '* MONTHLY PESTICIDE OUTPUT *')
2020  FORMAT (1X,A3,1X,'****************************',/,1X,A3,1X,
     1        '* ANNUAL PESTICIDE OUTPUT  *')
2021  FORMAT (1X,A3,1X,'****************************',/,1X,A3,1X,
     1        '* ANNUAL PESTICIDE OUTPUT  *')
2030  FORMAT (1X,A3,1X,'*                          *',/,1X,A3,1X,
     1        '* DATE:     ',I2,' ',A4,', ',I2,4X,'*',/,1X,A3,1X,
     2        '****************************',/,1X,A3,
     3        T35,'STORAGE UNITS IN KG/HA',/,1X,A3,
     4        T35,'FLUX UNITS IN KG/HA/OUTPUT TIMESTEP')
2035  FORMAT (1X,A3,T10,A20)
2040  FORMAT (1X,A3,/,1X,A3,T6,'CURRENT CONDITIONS',/,1X,A3,
     1           T6,'------------------',/,1X,A3,/,1X,A3,
     2           T6,'CROP NUMBER',T45,I5)
2050  FORMAT (1X,A3,/,1X,A3,/,1X,A3,/,1X,A3,T6,'FLUXES AND STORAGES ',
     1        'FOR THIS PERIOD',/,1X,A3,
     1        T6,'-----------------------------------')
2060  FORMAT (1X,A3,/,1X,A3,/,1X,A3,T25,'FOLIAR',T40,'PREVIOUS',T124,
     1        'CURRENT',/,1X,A3,T25,'APPLICATION',T40,'STORAGE',T68,
     2        'DECAY',T88,'VOLATILIZATION',T110,'WASHOFF',T124,
     3        'STORAGE',/,1X,A3,/,1X,A3,T25,G10.3,T39,G10.4,T67,G10.4,
     4        T90,G10.4,T109,G10.4,T123,G10.4,/,1X,A3,T9,'CANOPY',
     5        /,1X,A3,T25,'HEIGHT (CM)',T65,'COMPARTMENT FLUX',T110,
     6        'CONCENTRATION (G/CM**3)',/1X,A3,/1X,A3,T27,F7.2,T68,
     7        G10.4,T115,G10.4)
2070  FORMAT (1X,A3,/,1X,A3,/,1X,A3,/,1X,A3,T7,'HO-',T12,'COM-',T19,
     1        'SOIL',T27,'PREVIOUS',T38,'LEACHING',T51,'DECAY*',T64,
     2        'GAS**',T74,'TRANSFOR-',T86,'PLANT',T98,'LEACHING',T112,
     +        'LATERAL',T123,
     3        'CURRENT',/,1X,A3,T6,'RIZON PTMNT',T19,'APPL',
     4        T27,'STORAGE',T38,'INPUT',T62,'DIFFUSION',T74,'MATION***',
     5        T86,'UPTAKE',T98,'OUTPUT',T112,'OUTFLOW',T123,'STORAGE',/,
     6        1X,A3,1X,128('-'),/,1X,A3)
2080  FORMAT (1X,A3,I4,1X,I4,T18,G10.4,T25,G10.4,T37,G10.4,T50,G10.4,
     1        T62,G10.4,T74,G10.4,T86,G10.4,2X,G10.4,2X,G10.4,2X,G10.4)
2085  FORMAT (/,1X,A3,'  *******************************************',
     1        '*******',/,1X,A3,'  WARNING: High velocities in the ', 
     2        'soil column during',/,1X,A3,'  this time period may', 
     3        ' cause mass balance problems',/,1X,A3,'  when using ', 
     4        'the MOC option.  Check the output and',/,1X,A3,'  if', 
     5        ' mass balance problems exist, use the backward',/,1X,
     6        A3,'  difference option instead of MOC.  Refer to Sec-',
     7        /,1X,A3,'  tion 5.2.3 of the Users Guide for help in',
     8        ' setting',/,1X,A3,'  the value of DISP for backward',
     9        ' difference.',/,1X,A3,'  ***************************',
     1        '***********************') 
2090  FORMAT (1X,A3,/,1X,A3,/,1X,A3,/,1X,A3,T6,'MATERIAL BALANCE',/,
     1        1X,A3,T6,'----------------',/,1X,A3,/,
     2        1X,A3,T6,'PESTICIDE BALANCE ERROR',T44,G10.4,/,
     3        1X,A3,T6,'CUMULATIVE ERROR',T44,G10.4)
2100  FORMAT (1X,A3,/,1X,A3,/,1X,A3,T6,'* DECAY FOR ',
     1        'COMPARTMENT 1 INCLUDES EROSION AND WASHOFF LOSS.',/1X,
     2        A3,T6,'** (-) INDICATES UPWARD DIFFUSION FLUX, (+) IS ',
     3        'DOWNWARD.',/,1X,A3,' *** (+) INDICATES GAIN , (-) IS ',
     4        'LOSS',/,1X,A3,/,1X,A3,
     5        T6,'SUMMARY FLUXES AND STORAGES FOR SOIL',/,1X,A3,
     6        T6,'------------------------------------',/1X,A3,/,1X,A3,
     7        T6,'TOTAL PLANT UPTAKE OF PESTICIDE',T44,G10.4,/,1X,A3,
     8        T6,'TOTAL SOIL DECAY OF PESTICIDE',T44,G10.4,/,1X,A3,
     9        T6,'TOTAL EROSION OF PESTICIDE',T44,G10.4,/,1X,A3,
     $        T6,'TOTAL RUNOFF OF PESTICIDE',T44,G10.4,/,1X,A3,
     +        T6,'LATERAL OUTFLOW OF PESTICIDE',T44,G10.4,/,1X,A3,
     1        T6,'PESTICIDE LEACHED BELOW ROOT ZONE',T44,G10.4,/,1X,A3,
     2        T6,'PESTICIDE LEACHED BELOW CORE DEPTH',T44,G10.4,/,1X,A3,
     3        T6,'TOTAL SOIL PESTICIDE VOLATILIZATION',T44,G10.4)
2105  FORMAT (1X,A3,/,1X,A3,/,1X,A3,/,1X,A3,
     2        T6,'SUMMARY FLUXES AND STORAGES FOR SOIL',/,1X,A3,
     3        T6,'------------------------------------',/,1X,A3,/,1X,A3,
     4        T6,'TOTAL PLANT UPTAKE OF PESTICIDE',T44,G10.4,/,1X,A3,
     5        T6,'TOTAL SOIL DECAY OF PESTICIDE',T44,G10.4,/,1X,A3,
     6        T6,'TOTAL EROSION OF PESTICIDE',T44,G10.4,/,1X,A3,
     7        T6,'TOTAL RUNOFF OF PESTICIDE',T44,G10.4,/,1X,A3,
     8        T6,'LATERAL OUTFLOW OF PESTICIDE',T44,G10.4,/,1X,A3,
     +        T6,'PESTICIDE LEACHED BELOW ROOT ZONE',T44,G10.4,/,1X,A3,
     9        T6,'PESTICIDE LEACHED BELOW CORE DEPTH',T44,G10.4,/,1X,A3,
     1        T6,'TOTAL SOIL PESTICIDE VOLATILIZATION',T44,G10.4)
2103  FORMAT (1X,A3,T6,'TOTAL PESTICIDE DEGRADED TO DAUGHTERS',T44,
     1        G10.4,/,1X,A3,T6,'TOTAL PESTICIDE FORMED FROM PARENTS',
     2        T44,G10.4)
2104  FORMAT (1X,A3,/,1X,A3,T6,'TOTAL PESTICIDE REMOVAL BY CROPPING',
     1        T44,G10.4)
2110  FORMAT (1X,A3,T6,'TOTAL PESTICIDE IN CORE',T44,G10.4)
2120  FORMAT (1X,A3)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'OUTPST'
      CALL SUBIN(MESAGE)
C
C     Zero summary variables and set beginning values if beginning of
C     month or year is encountered
C
      IF (DOM .EQ. 1) THEN
C       beginning of month
        MINPP1(K) = 0.0
        MINPP8(K) = 0.0
        DO 15 J = 1, NCOM2
          MINPP2(K,J) = 0.0
          MTRFM(K,J)  = 0.0
15      CONTINUE
        MOUTP1(K) = 0.0
        MOUTP2(K) = 0.0
        MOUTP3(K) = 0.0
        MOUTP4(K) = 0.0
        MOUTP5(K) = 0.0
        MOUTP6(K) = 0.0
        MOUTP7(K) = 0.0
        MOUTP8(K) = 0.0
        MOUTP9(K) = 0.0
        MSTRP1(K) = FOLP0(K)* 1.E5
        MCOFLX(K) = 0.0
        IF (JULDAY .EQ. 1) THEN
C
C         Beginning of year
C
          YINPP1(K) = 0.0
          YINPP8(K) = 0.0
          DO 25 J = 1, NCOM2
            YINPP2(K,J) = 0.0
            YTRFM(K,J)  = 0.0
25        CONTINUE
          YOUTP1(K) = 0.0
          YOUTP2(K) = 0.0
          YOUTP3(K) = 0.0
          YOUTP4(K) = 0.0
          YOUTP5(K) = 0.0
          YOUTP6(K) = 0.0
          YOUTP7(K) = 0.0
          YOUTP8(K) = 0.0
          YOUTP9(K) = 0.0
          YSTRP1(K) = FOLP0(K)*1.E5
          YCOFLX(K) = 0.0
        ENDIF
        DO 50 I=1,NCOM2
          MINPP(K,I)= 0.0
          MOUTP(K,I)= 0.0
          MDOUT(K,I)= 0.0
          MLOUT(K,I)= 0.0
          VOUTM(K,I)= 0.0
          MSTRP(K,I)= PESTR(K,I)*DELX(I)*THETN(I)*1.E5
          IF (JULDAY .EQ. 1) THEN
            YINPP(K,I)= 0.0
            YOUTP(K,I)= 0.0
            YDOUT(K,I)= 0.0
            YLOUT(K,I)= 0.0
            VOUTY(K,I)= 0.0
            YSTRP(K,I)= MSTRP(K,I)
          ENDIF
50      CONTINUE
      ENDIF
C
C     Calculate core flux values.
C     Multiply internal units of GR/CM**2 by 10**5 so output
C     is expressed in units of KG/HA (as in the input).
C
C      DCOFLX(K) = DISP(K,NCOM2)/DELX(NCOM2)*THETN(NCOM2)*X(NCOM2)
C     1          - DISP(K,NCOM2)/DELX(NCOM2)*THETN(NCOM2)*X(NCOM2)
C     2          + (VEL(NCOM2)*X(NCOM2)*THETN(NCOM2))
C      DCOFLX(K) = DCOFLX(K) * 1.0E5
C      MCOFLX(K) = MCOFLX(K) + DCOFLX(K)
C      YCOFLX(K) = YCOFLX(K) + DCOFLX(K)
C
      TV        = FPVLOS(K) - PVFLUX(K,1)
      IF (HEIGHT .GE. 5.0 .AND. HENRYK(K) .GT. 0.0) THEN
        TCNC(K) = TV * CRCNC(2)
      ELSE
        TCNC(K) = 0.0
      ENDIF
C
C     Multiply internal units of GR/CM**2 by 10**5 so output
C     is expressed in units of KG/HA (as in the input)
C
C     Accumulate values for summaries
C
      MINPP1(K)= MINPP1(K)+ PLNTAP(K)* 1.E5
      MINPP8(K)= MINPP8(K)+ TTRFLX(K)* 1.E5
      MOUTP1(K)= MOUTP1(K)+ WOFLUX(K)* 1.E5
      MOUTP2(K)= MOUTP2(K)+ ROFLUX(K)* 1.E5
      MOUTP3(K)= MOUTP3(K)+ ERFLUX(K)* 1.E5
      MOUTP4(K)= MOUTP4(K)+ FPDLOS(K)* 1.E5
      MOUTP5(K)= MOUTP5(K)+ SUPFLX(K)* 1.E5
      MOUTP6(K)= MOUTP6(K)+ SDKFLX(K)* 1.E5
      MOUTP7(K)= MOUTP7(K)+ FPVLOS(K)* 1.E5
      MOUTP8(K)= MOUTP8(K)+ TSRCFX(K)* 1.E5
C**** ADDED FOR LATERAL OUTFLOW OF PESTICIDE ****
      MOUTP9(K)= MOUTP9(K)+ LATFLX(K)* 1.E5
C
      YINPP1(K)= YINPP1(K)+ PLNTAP(K)* 1.E5
      YINPP8(K)= YINPP8(K)+ TTRFLX(K)* 1.E5
      YOUTP1(K)= YOUTP1(K)+ WOFLUX(K)* 1.E5
      YOUTP2(K)= YOUTP2(K)+ ROFLUX(K)* 1.E5
      YOUTP3(K)= YOUTP3(K)+ ERFLUX(K)* 1.E5
      YOUTP4(K)= YOUTP4(K)+ FPDLOS(K)* 1.E5
      YOUTP5(K)= YOUTP5(K)+ SUPFLX(K)* 1.E5
      YOUTP6(K)= YOUTP6(K)+ SDKFLX(K)* 1.E5
      YOUTP7(K)= YOUTP7(K)+ FPVLOS(K)* 1.E5
      YOUTP8(K)= YOUTP8(K)+ TSRCFX(K)* 1.E5
C**** ADDED FOR LATERAL OUTFLOW OF PESTICIDE ****
      YOUTP9(K)= YOUTP9(K)+ LATFLX(K)* 1.E5
C
      DO 80 I=1,NCOM2
        DTRFM(K,I) = (SRCFLX(K,I)-TRFLUX(K,I))*1.E5
        MTRFM(K,I) = MTRFM(K,I)+ DTRFM(K,I)
        YTRFM(K,I) = YTRFM(K,I)+ DTRFM(K,I)
        MINPP(K,I) = MINPP(K,I)+ (ADFLUX(K,I)+DFFLUX(K,I))* 1.E5
        MINPP2(K,I)= MINPP2(K,I)+SOILAP(K,I)* 1.E5
        MOUTP(K,I) = MOUTP(K,I)+ UPFLUX(K,I)* 1.E5
        MDOUT(K,I) = MDOUT(K,I)+ DKFLUX(K,I)* 1.E5
        VOUTM(K,I) = VOUTM(K,I)+ PVFLUX(K,I)* 1.E5
        YINPP(K,I) = YINPP(K,I)+ (ADFLUX(K,I)+DFFLUX(K,I))* 1.E5
        YINPP2(K,I)= YINPP2(K,I)+SOILAP(K,I)* 1.E5
        YOUTP(K,I) = YOUTP(K,I)+ UPFLUX(K,I)* 1.E5
        YDOUT(K,I) = YDOUT(K,I)+ DKFLUX(K,I)* 1.E5
        VOUTY(K,I) = VOUTY(K,I)+ PVFLUX(K,I)* 1.E5
C*** ADDED FOR LATERAL OUTFLOW OF PESTICIDE ****
        MLOUT(K,I) = MLOUT(K,I)+ LTFLUX(K,I)* 1.E5
        YLOUT(K,I) = YLOUT(K,I)+ LTFLUX(K,I)* 1.E5
80    CONTINUE
C
C     Make decision to output summary
C
      NOPRT= 0
      IF (ITEM2 .EQ. PEST .AND. STEP2 .EQ. DAY) THEN
        BSUMM=1
        NSUMM=1
        MNTHP1=MONTH+1
        IF (JULDAY .EQ. CNDMO(LEAP,MNTHP1)) NSUMM=2
        IF (JULDAY .EQ. CNDMO(LEAP,13)) NSUMM=3
      ELSEIF (ITEM2 .EQ. PEST .AND. STEP2 .EQ. MNTH) THEN
        BSUMM=2
        NSUMM=1
        MNTHP1=MONTH+1
        IF (JULDAY .EQ. CNDMO(LEAP,MNTHP1)) NSUMM=2
        IF (JULDAY .EQ. CNDMO(LEAP,13)) NSUMM=3
        IF (NSUMM .LT. BSUMM) NOPRT= 1
      ELSE
        BSUMM=3
        NSUMM=1
        IF (ITEM2 .NE. PEST .OR. STEP2 .NE. YEAR) NOPRT= 1
        IF (JULDAY .EQ. CNDMO(LEAP,13)) NSUMM=3
        IF (NSUMM .LT. BSUMM) NOPRT= 1
      ENDIF
C
C     Determine if today was an output summary day for PRZM
C
      PRNTIT = (NOPRT .NE. 1) .AND. (FLPS .GT. 0)
      IF (PRNTIT) THEN
C
C       Print summaries
C
        DO 250 I=BSUMM,NSUMM
          IF (MCOFLG .EQ. 0) THEN
            IF (I .EQ. 1) WRITE(FLPS,2000) (MODID,J=1,2)
            IF (I .EQ. 2) WRITE(FLPS,2010) (MODID,J=1,2)
            IF (I .EQ. 3) WRITE(FLPS,2020) (MODID,J=1,2)
          ELSE
            IF (I .EQ. 3) WRITE(FLPS,2021) (MODID,J=1,2)
          ENDIF
          WRITE(FLPS,2030) (MODID,J=1,2),DOM,CMONTH(MONTH),IY,
     1                     (MODID,J=1,3)
          IF (MCOFLG .EQ. 0) THEN
C            IF (NCHEM .GT. 1) WRITE(FLPS,2035) MODID,PSTNAM(K)
            WRITE(FLPS,2035) MODID,PSTNAM(K)
            WRITE(FLPS,2040) (MODID,J=1,5),NCROP
            WRITE(FLPS,2050) (MODID,J=1,5)
          ENDIF
          PA = PLNTAP(K)* 1.E5
          FP = FOLPST(K)* 1.E5
          F0 = FOLP0(K) * 1.E5
          FL = FPDLOS(K)* 1.E5
          WF = WOFLUX(K)* 1.E5
          RF = ROFLUX(K)* 1.E5
          EF = ERFLUX(K)* 1.E5
          SD = SDKFLX(K)* 1.E5
          SU = SUPFLX(K)* 1.E5
          IF (PVFLUX(K,1) .GT. 0.0) PVFLUX(K,1)=0.0
          SV = PVFLUX(K,1)* 1.E5
          DVF= FPVLOS(K) * 1.E5
          VAR4 = TTRFLX(K)*1.E5
          VAR5 = TSRCFX(K)*1.E5
C*** ADDED FOR LATERAL OUTFLOW OF PESTICIDE ***
          LA = LATFLX(K)* 1.E5
          IF (I .EQ. 1 .AND. MCOFLG .EQ. 0) THEN
            TCFD = DVF - SV
            WRITE(FLPS,2060) (MODID,J=1,6),PA,F0,FL,DVF,WF,FP,
     1                       (MODID,J=1,4),HEIGHT,TCFD,TCNC(K)
            WRITE(FLPS,2070) (MODID,J=1,7)
            DO 140 J=1,NCOM2,LFREQ2
              SA = SOILAP(K,J)* 1.E5
              IF (J.NE.1) THEN
                VAR1= (ADFLUX(K,J-1)+DFFLUX(K,J-1))* 1.E5
              ELSE
                VAR1 = WOFLUX(K)* 1.E5
              ENDIF
              VAR2 = (ADFLUX(K,J)+DFFLUX(K,J))* 1.E5
              XP(J)= X(J)*DELX(J)*(THETN(J)+KD(K,J)*BD(J)
     1               +(THETAS(J)-THETN(J))*KH(K,J))* 1.E5
              VAR3 = PESTR(K,J)*DELX(J)*THETN(J)* 1.E5
C
              IF (J .GE. 2) THEN
                IF (HORIZN(J) .GT. 1 .AND. HORIZN(J) .NE.
     1            HORIZN(J-1)) WRITE(FLPS,2120) MODID
              ENDIF
              UPF = UPFLUX(K,J)* 1.E5
              DKF = DKFLUX(K,J)* 1.E5
              JLK= DKF
              IF (J .EQ. 1) JLK= JLK+ EF+ RF
              VOUT=PVFLUX(K,J)*1.E5
              WRITE(FLPS,2080) MODID,HORIZN(J),J,SA,VAR3,VAR1,JLK,
     1                         VOUT,DTRFM(K,J),UPF,VAR2,LA,XP(J)
140         CONTINUE
            VAR2RZ= (ADFLUX(K,NCOMRZ)+DFFLUX(K,NCOMRZ))* 1.E5
            WRITE(FLPS,2100) (MODID,J=1,10),SU,MODID,SD,MODID,EF,MODID,
     1                        RF,MODID,LA,MODID,VAR2RZ,MODID,DCOFLX(K),
     2                        MODID,-SV
            IF (NCHEM .GT. 1) WRITE(FLPS,2103) MODID,VAR4,MODID,VAR5
          ELSEIF (I .EQ. 2 .AND. MCOFLG .EQ. 0) THEN
            TCFM = MOUTP7(K) - VOUTM(K,1)
            WRITE(FLPS,2060) (MODID,J=1,6),MINPP1(K),MSTRP1(K),
     1                       MOUTP4(K),MOUTP7(K),MOUTP1(K),FP,
     2                       (MODID,J=1,4),HEIGHT,TCFM,TCNC(K)
            WRITE(FLPS,2070) (MODID,J=1,7)
            DO 180 J=1,NCOM2,LFREQ2
              IF (J.NE.1) THEN
                VAR1= MINPP(K,J-1)
              ELSE
                VAR1= MOUTP1(K)
              ENDIF
              XP(J)= X(J)*DELX(J)*(THETN(J)+KD(K,J)*BD(J)
     1              +(THETAS(J)-THETN(J))*KH(K,J))* 1.E5
C
              IF (J .GE. 2) THEN
                IF (HORIZN(J) .GT. 1 .AND. HORIZN(J) .NE.
     1            HORIZN(J-1))  WRITE(FLPS,2120) MODID
              ENDIF
              JLK= MDOUT(K,J)
              IF (J .EQ. 1) JLK= JLK+ MOUTP3(K)+ MOUTP2(K)
              VOUT = VOUTM(K,J)
              WRITE(FLPS,2080) MODID,HORIZN(J),J,MINPP2(K,J),MSTRP(K,J),
     1              VAR1,JLK,VOUT,MTRFM(K,J),MOUTP(K,J),MINPP(K,J),
     2              MLOUT(K,J),XP(J)
180         CONTINUE
            WRITE(FLPS,2100) (MODID,J=1,10),MOUTP5(K),MODID,MOUTP6(K),
     1                       MODID,MOUTP3(K),MODID,MOUTP2(K),MODID,
     +                       MOUTP9(K),MODID,
     2                       MINPP(K,NCOMRZ),MODID,MCOFLX(K),MODID,
     3                       -VOUTM(K,1)
            IF (NCHEM .GT. 1) WRITE(FLPS,2103) MODID,MINPP8(K),
     1                                         MODID,MOUTP8(K)
C
C   ADDED TO ACCUMULATE TOTAL PESTICIDE OUTFLOW FOR TABLE, ATC 8/93
C
               IYEAR=(IY-STARTYR)+1
               ROPST(MONTH,IYEAR,K) = MOUTP2(K)
               LPST(MONTH,IYEAR,K)  = MOUTP9(K)
               BPST(MONTH,IYEAR,K)  = MCOFLX(K)
               ERPST(MONTH,IYEAR,K) = MOUTP3(K)
C
          ELSEIF (I .EQ. 3) THEN
            TCFY = YOUTP7(K) - VOUTY(K,1)
            IF (MCOFLG .EQ. 0) THEN
              WRITE(FLPS,2060) (MODID,J=1,6),YINPP1(K),YSTRP1(K),
     1                         YOUTP4(K),YOUTP7(K),YOUTP1(K),FP,
     2                         (MODID,J=1,4),HEIGHT,TCFY,TCNC(K)
              WRITE(FLPS,2070) (MODID,J=1,7)
              DO 220 J=1,NCOM2,LFREQ2
                IF (J .NE. 1) THEN
                  VAR1= YINPP(K,J-1)
                ELSE
                  VAR1= YOUTP1(K)
                ENDIF
                XP(J)= X(J)*DELX(J)*(THETN(J)+KD(K,J)*BD(J)
     1                +(THETAS(J)-THETN(J))*KH(K,J))* 1.E5
C
                IF (J .GE. 2) THEN
                  IF (HORIZN(J) .GT. 1 .AND. HORIZN(J)
     1               .NE. HORIZN(J-1))   WRITE(FLPS,2120) MODID
                ENDIF
                JLK= YDOUT(K,J)
                IF (J .EQ. 1) JLK= JLK+ YOUTP3(K)+ YOUTP2(K)
                VOUT= VOUTY(K,J)
                WRITE(FLPS,2080) MODID,HORIZN(J),J,YINPP2(K,J),
     1                           YSTRP(K,J),VAR1,JLK,VOUT,YTRFM(K,J),
     2                           YOUTP(K,J),YINPP(K,J),YLOUT(K,J),XP(J)
220           CONTINUE
            ENDIF
            IF (MCOFLG .EQ. 0) THEN
              WRITE(FLPS,2100) (MODID,J=1,10),YOUTP5(K),MODID,YOUTP6(K),
     1                         MODID,YOUTP3(K),MODID,YOUTP2(K),MODID,
     +                         YOUTP9(K),MODID,
     2                         YINPP(K,NCOMRZ),MODID,YCOFLX(K),MODID,
     3                         -VOUTY(K,1)
              IF (NCHEM .GT. 1) WRITE(FLPS,2103) MODID,YINPP8(K),
     1                                           MODID,YOUTP8(K)
            ELSE
              WRITE(FLPS,2105) (MODID,J=1,7),YOUTP5(K),MODID,YOUTP6(K),
     1                         MODID,YOUTP3(K),MODID,YOUTP2(K),MODID,
     2                         YOUTP9(K),MODID,YCOFLX(K),MODID,
     3                         -VOUTY(K,1)
            ENDIF
          ENDIF
          SUMXP= 0.0
C
          DO 240 II=1,NCOM2
            XP(II)= X(II)*DELX(II)*(THETN(II)+KD(K,II)*BD(II)
     1              +(THETAS(II)-THETN(II))*KH(K,II))* 1.E5
            SUMXP = SUMXP+ XP(II)
240       CONTINUE
          IF (FMRMVL(K) .GT. 0.0) THEN
            FMRMVX = FMRMVL(K) * 1.0E5
            WRITE(FLPS,2104) (MODID,J=1,2),FMRMVX
          ENDIF
          WRITE(FLPS,2110) MODID,SUMXP
C
C       The following lines and format statement 2085 were added by
C       ssh at aqua terra, 4-89, to warn when velocities may be too high
C       to use MOC.
C
          IF(MOCFLG.EQ.1) WRITE(FLPS,2085)(MODID,J=1,9)
          PB = PBAL(K) * 1.E5
          CB = CPBAL(K)* 1.E5
          WRITE(FLPS,2090) (MODID,J=1,7),PB,MODID,CB
250     CONTINUE
C
C     Added by ssh, 4-89.
C
      MOCFLG=0
      ENDIF
C
      CALL SUBOUT
      RETURN
      END
C
C
C
      SUBROUTINE OUTHYD(
     I  LPRZOT,LTMSRS,MODIDW,MODIDT,SEPTON)
C
C     + + + PURPOSE + + +
C
C     Accumulates and outputs daily, monthly, and
C     annual summaries for water
C     Modification date: 2/18/92 JAM
C     Modified in 8/93 at AQUA TERRA Consultants to
C     accumulate values for the output table
C
C     + + + DUMMY ARGUMENTS + + +
C
      INTEGER     LPRZOT,LTMSRS
      CHARACTER*3 MODIDW,MODIDT
      LOGICAL     SEPTON
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     LPRZOT - Fortran unit number for output file LPRZOT
C     FLSS   - Fortran unit number for output file LTMSRS
C     MODIDW - character string for LPRZOT output file
C     MODIDT - character string for LTMSRS output file
C     SEPTON - septic effluent on flag
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXYRS.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'TABLE.INC'
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CCROP.INC'
      INCLUDE 'CMET.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CACCUM.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'PMXVDT.INC'
      INCLUDE 'CPTCHK.INC'
      INCLUDE 'CSPTIC.INC'
C
C     + + + LOCAL VARIABLES + + +
C
      REAL         VARW1,VAR2D,VAR2M,VAR2Y,RZD
      INTEGER      I,J,BSUMM,NSUMM,MNTHP1,NOPRT,ILIN,IYEAR
      CHARACTER*4  YEAR,WATR,MNTH,DAY
      CHARACTER*80 MESAGE
C
C     + + + INTRINSICS + + + 
C      
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,SUBOUT
C
C     + + + DATA INITIALIZATIONS + + +
C
      DATA YEAR/'YEAR'/,MNTH/'MNTH'/,DAY/' DAY'/,WATR/'WATR'/
C
C     + + + OUTPUT FORMATS + + +
C
2000  FORMAT (1X,A3,1X,'****************************',/,1X,A3,1X,
     1        '* DAILY WATER OUTPUT       *')
2010  FORMAT (1X,A3,1X,'****************************',/,1X,A3,1X,
     1        '* MONTHLY WATER OUTPUT     *')
2020  FORMAT (1X,A3,1X,'****************************',/,1X,A3,1X,
     1        '* ANNUAL WATER OUTPUT      *')
2021  FORMAT (1X,A3,1X,'****************************',/,1X,A3,1X,
     1        '* ANNUAL WATER OUTPUT      *')
2030  FORMAT (1X,A3,1X,'*                          *',/,1X,A3,1X,
     1        '* DATE:     ',I2,' ',A4,', ',I2,4X,'*',/,1X,A3,1X,
     2        '****************************',/,1X,A3,
     3        T35,'ALL HYDROLOGY UNITS ARE CM OF WATER',/,1X,A3,
     4        T35,'SEDIMENT UNITS ARE METRIC TONNES',/,1X,A3,
     5        T35,'NUMBERS IN PARENTHESES ARE SOIL WATER CONTENTS')
2040  FORMAT (1X,A3,/,1X,A3,T6,'CURRENT CONDITIONS',/,1X,A3,
     1        T6,'------------------',/,1X,A3,/,1X,A3,
     2        T6,'CROP NUMBER',T44,I6,/,1X,A3,
     3        T6,'FRACTION OF GROUND COVER',T44,G10.4,/,1X,A3,
     4        T6,'INTERCEPTION POTENTIAL (CM)',T44,G10.4,/,1X,A3,
     5        T6,'DEPTH TO WHICH ET EXTRACTED(CM)',T44,G10.4)
2050  FORMAT (1X,A3,/,1X,A3,/,1X,A3,/,1X,A3,T6,'FLUXES AND STORAGES ',
     1        'FOR THIS PERIOD',/,1X,A3,T6,
     2        '-----------------------------------',/,1X,A3)
2060  FORMAT (1X,A3,/,1X,A3,/,1X,A3,T35,'PREVIOUS',T115,'CURRENT',/,
     1        1X,A3,T35,'STORAGE',T55,'PRECIPITATION',T75,'EVAPORATION',
     2        T95,'THROUGHFALL',T115,'STORAGE',/,1X,A3,/,1X,A3,
     3        ' CANOPY',T35,5(G10.4,10X))
2065  FORMAT (1X,A3,/,1X,A3,/,1X,A3,T55,'PRECIPITATION',T75,
     1        'EVAPORATION',T95,'THROUGHFALL',/,1X,A3,/,1X,A3,
     2        ' CANOPY',T55,3(G10.4,10X))
2070  FORMAT (1X,A3,/,1X,A3,/,1X,A3,T35,'PREVIOUS',T115,'CURRENT',/,
     1        1X,A3,T35,'SNOWPACK',T55,'SNOW',T95,'SNOWMELT',T115,
     2        'SNOWPACK',/,1X,A3,/,1X,A3,' SNOWPACK',T35,2(G10.4,10X),
     3        T95,2(G10.4,10X))
2075  FORMAT (1X,A3,/,1X,A3,/,1X,A3,T55,'SNOW',T95,'SNOWMELT',
     1        /,1X,A3,/,1X,A3,' SNOWPACK',T55,G10.4,T95,G10.4)
2080  FORMAT (1X,A3,/,1X,A3,/,1X,A3,T55,'THROUGHFALL',T75,'RUNOFF',
     1        T95,'INFILTRATION',/,1X,A3,/,1X,A3,
     2        ' SURFACE',T55,3(G10.4,10X))
2085  FORMAT (1X,A3,/,1X,A3,/,1X,A3,T35,'PREVIOUS',T55,'LEACHING',
     1          T100,'LEACHING',T115,'CURRENT',/,1X,A3,
     2          T11,'SOIL LAYERS',
     3          T35,'STORAGE',T55,'INPUT',T70,'TRANSPIRATION',
     4          T85,'LATERAL FLOW',T100,'OUTPUT',T115,'STORAGE',/,
     4          1X,A3,
     5          T6,'HORIZON   COMPARTMENT',/,1X,A3,
     6          T6,120('-'),/,1X,A3)
2090  FORMAT (1X,A3,T6,I4,T16,I6,T35,G10.4,T45,'(',F4.3,')',
     1        T55,G10.4,T70,G10.4,T85,G10.4,T100,G10.4,T115,
     2        G10.4,T125,'(',F4.3,')')
2100  FORMAT (1X,A3,/,1X,A3,/,1X,A3,/,1X,A3,1X,'MATERIAL BALANCE',/,
     1        1X,A3,1X,'----------------',/,1X,A3,/,
     2        1X,A3,1X,'WATER BALANCE ERROR',T44,G10.4,/,
     3        1X,A3,1X,'CUMULATIVE ERROR',T44,G10.4)
2110  FORMAT (1X,A3,T6,'RECHARGE BELOW ROOT ZONE',T44,G10.4,/,1X,A3,
     1        T6,'TOTAL LATERAL OUTFLOW',T44,G10.4)
2115  FORMAT (1X,A3,T6,'TOTAL LATERAL INFLOW',T44,G10.4)
2120  FORMAT (1X,A3)
2130  FORMAT (1X,A3,/,1X,A3,/,1X,A3,/,1X,A3,T6,'SUMMARY FLUXES',/,
     1        1X,A3,T6,'--------------',/,1X,A3,/,
     2        1X,A3,T6,'TOTAL SEDIMENT ERODED FROM SURFACE',T44,G10.4,/,
     3        1X,A3,T6,'TOTAL ET FROM PROFILE',T44,G10.4)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'OUTHYD'
      CALL SUBIN(MESAGE)
C
      IF (DOM .EQ. 1) THEN
C
C       Beginning of month, zero summary variables, set beginning values
C
        MOUTFL = 0.0
        MINFLO = 0.0
        MINPW1 = 0.0
        MINPW2 = 0.0
        MOUTW1 = 0.0
        MOUTW2 = 0.0
        MOUTW3 = 0.0
        MOUTW4 = 0.0
        MOUTW5 = 0.0
        MOUTW6 = 0.0
        MSTR1  = CINTB
        MSTR2  = OSNOW
C
        IF (JULDAY .EQ. 1) THEN
C
C         Beginning of year, zero summary variables, set beginning values
C
          YOUTFL = 0.0
          YINFLO = 0.0
          YINPW1 = 0.0
          YINPW2 = 0.0
          YOUTW1 = 0.0
          YOUTW2 = 0.0
          YOUTW3 = 0.0
          YOUTW4 = 0.0
          YOUTW5 = 0.0
          YOUTW6 = 0.0
          YSTR1  = CINTB
          YSTR2  = OSNOW
        ENDIF
C
        DO 50 I=1,NCOM2
          MINPW(I) = 0.0
          MOUTW(I) = 0.0
          MOOUTW(I)= 0.0
          MEOUTW(I)= 0.0
          MSTR(I)  = THETO(I)*DELX(I)
          IF (JULDAY .EQ. 1) THEN
            YINPW(I) = 0.0
            YOUTW(I) = 0.0
            YOOUTW(I)= 0.0
            YEOUTW(I)= 0.0
            YSTR(I)  = MSTR(I)
          ENDIF
50      CONTINUE
      ENDIF
C
C     Accumulate values for summaries
C
      MINPW1 = MINPW1+ PRECIP
      MINPW2 = MINPW2+ SNOWFL
      MOUTW1 = MOUTW1+ CEVAP
      MOUTW2 = MOUTW2+ THRUFL
      MOUTW3 = MOUTW3+ RUNOF
      MOUTW4 = MOUTW4+ SMELT
      MOUTW5 = MOUTW5+ TDET
      MOUTW6 = MOUTW6+ SEDL
      YINPW1 = YINPW1+ PRECIP
      YINPW2 = YINPW2+ SNOWFL
      YOUTW1 = YOUTW1+ CEVAP
      YOUTW2 = YOUTW2+ THRUFL
      YOUTW3 = YOUTW3+ RUNOF
      YOUTW4 = YOUTW4+ SMELT
      YOUTW5 = YOUTW5+ TDET
      YOUTW6 = YOUTW6+ SEDL
C
      DO 80 I=1,NCOM2
        MINPW(I)= MINPW(I)+ AINF(I)
        YINPW(I)= YINPW(I)+ AINF(I)
80    CONTINUE
C
C     Change for lateral drainage.
C     Variables  OUTFLO, MOOUTW, YOOUTW, DOUTFL, MOUTFL, OR YOUTFL
C     were added to allow for lateral drainage.
C     Variables  DINFLO, MINFLO, and YINFLO were added for lateral
C     inflow due to septic effluent.
C
      DOUTFL = 0.00
      DINFLO = 0.00
      DO 90 I=1,NCOM2
        MOUTW(I) = MOUTW(I)+ AINF(I+1)
        MEOUTW(I)= MEOUTW(I)+ ET(I)
        YOUTW(I) = YOUTW(I)+ AINF(I+1)
        YEOUTW(I)= YEOUTW(I)+ ET(I)
        MOOUTW(I)= MOOUTW(I)+ OUTFLO(I)
        YOOUTW(I)= YOOUTW(I)+ OUTFLO(I)
        DOUTFL   = DOUTFL   + OUTFLO(I)
        DINFLO   = DINFLO   + LINF(I)
90    CONTINUE
      MOUTFL = MOUTFL + DOUTFL
      YOUTFL = YOUTFL + DOUTFL
      MINFLO = MINFLO + DINFLO
      YINFLO = YINFLO + DINFLO
C
C     Make decision to output a summary
C
      NOPRT= 0
      IF (ITEM1 .EQ. WATR .AND. STEP1 .EQ. DAY) THEN
        BSUMM=1
        NSUMM=1
        MNTHP1=MONTH+1
        IF (JULDAY .EQ. CNDMO(LEAP,MNTHP1)) NSUMM=2
        IF (JULDAY .EQ. CNDMO(LEAP,13)) NSUMM=3
      ELSEIF (ITEM1 .EQ. WATR .AND. STEP1 .EQ. MNTH) THEN
        BSUMM=2
        NSUMM=1
        MNTHP1=MONTH+1
        IF (JULDAY .EQ. CNDMO(LEAP,MNTHP1)) NSUMM=2
        IF (JULDAY .EQ. CNDMO(LEAP,13)) NSUMM=3
        IF (NSUMM .LT. BSUMM) NOPRT= 1
      ELSE
        BSUMM=3
        NSUMM=1
        IF (ITEM1 .NE. WATR .OR. STEP1 .NE. YEAR) NOPRT= 1
        IF (JULDAY .EQ. CNDMO(LEAP,13)) NSUMM=3
        IF (NSUMM .LT. BSUMM) NOPRT= 1
      ENDIF
C
C     Record if today was an output summary day for VADOFT
C
      PRNTIT = (NOPRT .NE. 1)
      IF (PRNTIT) THEN
C
C       Print summaries
C
        DO 190 I=BSUMM,NSUMM
          IF (MCOFLG.EQ.0) THEN
            IF (I.EQ.1) WRITE(LPRZOT,2000) (MODIDW,J=1,2)
            IF (I.EQ.2) WRITE(LPRZOT,2010) (MODIDW,J=1,2)
            IF (I.EQ.3) WRITE(LPRZOT,2020) (MODIDW,J=1,2)
            WRITE(LPRZOT,2030) (MODIDW,J=1,2),DOM,CMONTH(MONTH),IY,
     1                       (MODIDW,J=1,4)
          ELSE IF (LTMSRS.GT.0) THEN
            IF (I.EQ.3) WRITE(LTMSRS,2021) (MODIDT,J=1,2)
            WRITE(LTMSRS,2030) (MODIDT,J=1,2),DOM,CMONTH(MONTH),IY,
     1                       (MODIDT,J=1,4)
          ENDIF
C
          RZD = 0.0
          DO 6099 ILIN = 1, NCOM1
            RZD = RZD + DELX(ILIN)
6099      CONTINUE
C
          IF (MCOFLG .EQ. 0) THEN
            WRITE(LPRZOT,2040) (MODIDW,J=1,5),NCROP,MODIDW,COVER,MODIDW,
     1                        DIN,MODIDW,RZD
            WRITE(LPRZOT,2050) (MODIDW,J=1,6)
          ENDIF
          IF (I .EQ. 1 .AND. MCOFLG .EQ. 0) THEN
            WRITE(LPRZOT,2060) (MODIDW,J=1,6),CINTB,PRECIP,
     1                        CEVAP,THRUFL,CINT
            IF (ABS(OSNOW-0.0).GT.1.0E-5.OR.ABS(SNOWFL-0.0).GT.1.0E-5)
     1        WRITE(LPRZOT,2070) (MODIDW,J=1,6),OSNOW,SNOWFL,SMELT,SNOW
            WRITE(LPRZOT,2080) (MODIDW,J=1,5),THRUFL,RUNOF,AINF(1)
            WRITE(LPRZOT,2085) (MODIDW,J=1,7)
            DO 130 J=1,NCOM2,LFREQ1
              VARW1= THETN(J)* DELX(J)
              VAR2D= THETO(J)* DELX(J)
C
              IF (J .GE. 2) THEN
                IF (HORIZN(J).GT.1 .AND. HORIZN(J).NE.HORIZN(J-1))
     1            WRITE(LPRZOT,2120) MODIDW
              ENDIF
              WRITE(LPRZOT,2090) MODIDW,HORIZN(J),J,VAR2D,THETO(J),
     1                         AINF(J),ET(J),OUTFLO(J),AINF(J+1),
     2                         VARW1,THETN(J)
130         CONTINUE
            WRITE(LPRZOT,2130) (MODIDW,J=1,7),SEDL,MODIDW,TDET
            WRITE(LPRZOT,2110) MODIDW,AINF(NCOMRZ+1),MODIDW,DOUTFL
            IF (SEPTON) THEN
C             total inflow from septic
              WRITE(LPRZOT,2115) MODIDW,DINFLO
            END IF
            WRITE(LPRZOT,2100) (MODIDW,J=1,7),WBAL,MODIDW,CWBAL
          ELSEIF (I .EQ. 2 .AND. MCOFLG .EQ. 0) THEN
            WRITE(LPRZOT,2060) (MODIDW,J=1,6),MSTR1,MINPW1,
     1                        MOUTW1,MOUTW2,CINT
            IF (ABS(MSTR2-0.0).GT.1.0E-5.OR.ABS(MINPW2-0.0).GT.1.0E-5)
     1        WRITE(LPRZOT,2070) (MODIDW,J=1,6),MSTR2,MINPW2,MOUTW4,SNOW
            WRITE(LPRZOT,2080) (MODIDW,J=1,5),MOUTW2,MOUTW3,MINPW(1)
            WRITE(LPRZOT,2085) (MODIDW,J=1,7)
            DO 150 J=1,NCOM2,LFREQ1
              VARW1= THETN(J)* DELX(J)
              VAR2M= MSTR(J)/DELX(J)
C
              IF (J .GE. 2) THEN
                IF (HORIZN(J) .GT. 1 .AND. HORIZN(J) .NE.
     1            HORIZN(J-1)) WRITE(LPRZOT,2120) MODIDW
              ENDIF
              WRITE(LPRZOT,2090) MODIDW,HORIZN(J),J,MSTR(J),VAR2M,
     1                         MINPW(J),MEOUTW(J),MOOUTW(J),
     2                         MOUTW(J),VARW1,THETN(J)
150         CONTINUE
            WRITE(LPRZOT,2130) (MODIDW,J=1,7),MOUTW6,MODIDW,MOUTW5
            WRITE(LPRZOT,2110) MODIDW,MOUTW(NCOMRZ),MODIDW,MOUTFL
            IF (SEPTON) THEN
C             total inflow from septic
              WRITE(LPRZOT,2115) MODIDW,MINFLO
            END IF
            WRITE(LPRZOT,2100) (MODIDW,J=1,7),WBAL,MODIDW,CWBAL
C
C    ADDED TO ACCUMULATE VALUES FOR OUTPUT TABLE, ATC 8/93
C
              IYEAR = (IY-STARTYR)+1
              RAIN(MONTH,IYEAR) = MINPW1
              SURF(MONTH,IYEAR) = MOUTW3
              IFLO(MONTH,IYEAR) = MOUTFL
              BFLO(MONTH,IYEAR) = MOUTW(NCOM2)
              EVPO(MONTH,IYEAR) = MOUTW5+MOUTW1
              SEDI(MONTH,IYEAR) = MOUTW6
              TFLO(MONTH,IYEAR) = MOUTW3+MOUTFL+MOUTW(NCOM2)
C
          ELSEIF (I .EQ. 3) THEN
            IF (MCOFLG .EQ. 0) THEN
              WRITE(LPRZOT,2060) (MODIDW,J=1,6),YSTR1,YINPW1,
     1                          YOUTW1,YOUTW2,CINT
              IF (ABS(YSTR2-0.0).GT.1.0E-5.OR.ABS(YINPW2-0.0).GT.1.0E-5)
     1          WRITE(LPRZOT,2070) (MODIDW,J=1,6),YSTR2,YINPW2,
     2          YOUTW4,SNOW
              WRITE(LPRZOT,2080) (MODIDW,J=1,5),YOUTW2,YOUTW3,YINPW(1)
              WRITE(LPRZOT,2085) (MODIDW,J=1,7)
              DO 170 J=1,NCOM2,LFREQ1
                VARW1= THETN(J)* DELX(J)
                VAR2Y= YSTR(J)/DELX(J)
C
                IF (J .GE. 2) THEN
                  IF (HORIZN(J) .GT. 1 .AND. HORIZN(J) .NE.
     1              HORIZN(J-1)) WRITE(LPRZOT,2120) MODIDW
                ENDIF
                WRITE(LPRZOT,2090) MODIDW,HORIZN(J),J,YSTR(J),VAR2Y,
     1                           YINPW(J),YEOUTW(J),YOOUTW(J),
     1                           YOUTW(J),VARW1,THETN(J)
170           CONTINUE
            ENDIF
            IF (MCOFLG .EQ. 0) THEN
              WRITE(LPRZOT,2130) (MODIDW,J=1,7),YOUTW6,MODIDW,YOUTW5
              WRITE(LPRZOT,2110) MODIDW,YOUTW(NCOMRZ),MODIDW,YOUTFL
              IF (SEPTON) THEN
C               total inflow from septic
                WRITE(LPRZOT,2115) MODIDW,YINFLO
              END IF
              WRITE(LPRZOT,2100) (MODIDW,J=1,7),WBAL,MODIDW,CWBAL
            ELSE IF (LTMSRS.GT.0) THEN
              WRITE(LTMSRS,2065) (MODIDT,J=1,5),YINPW1,YOUTW1,YOUTW2
              IF (ABS(YSTR2-0.0).GT.1.0E-5.OR.ABS(YINPW2-0.0).GT.1.0E-5)
     1          WRITE(LTMSRS,2075) (MODIDT,J=1,5),YINPW2,YOUTW4
              WRITE(LTMSRS,2080) (MODIDT,J=1,5),YOUTW2,YOUTW3,YINPW(1)
              WRITE(LTMSRS,2130) (MODIDT,J=1,7),YOUTW6,MODIDT,YOUTW5
              WRITE(LTMSRS,2110) MODIDT,YOUTW(NCOMRZ),MODIDT,YOUTFL
              IF (SEPTON) THEN
C               total inflow from septic
                WRITE(LPRZOT,2115) MODIDW,YINFLO
              END IF
              WRITE(LTMSRS,2100) (MODIDT,J=1,7),WBAL,MODIDT,CWBAL
            ENDIF
          ENDIF
190     CONTINUE
      ENDIF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE OUTTSR(
     I  SRNFG,EYRFG,LPRZOT,LTMSRS,LWDMS,MODIDW,MODIDT,HEIGHT)
C
C     + + + PURPOSE + + +
C
C     Outputs user specified time series to time series
C     plotting files
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INTEGER     SRNFG,EYRFG,LPRZOT,LTMSRS,LWDMS
      REAL        HEIGHT
      CHARACTER*3 MODIDW,MODIDT
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     SRNFG  - Start of run flag
C     EYRFG  - End of year flag
C     LWDMS  - Fortran unit number for WDMS file
C     LPRZOT - Fortran unit number for output file LPRZOT
C     LTMSRS - Fortran unit number for output file LTMSRS
C     MODIDW - character string for LPRZOT output file identification
C     MODIDT - character string for LTMSRS output file identification
C     HEIGHT - canopy height
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CMET.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CACCUM.INC'
      INCLUDE 'CNITR.INC'
C
C     + + + SAVES + + +
C
      SAVE  DLT,DTOVWR,QUALFG,TUNITS,NVAL,DATES,RVAL
C
C     + + + LOCAL VARIABLES + + +
C
      INTEGER     I,IARG1,CPLTFL,CPLTWD,K,ID1,
     1            DLT,DATES(6),NVAL,DTOVWR,QUALFG,TUNITS
C
C  ADDED OUTFLOW IN VARIABLE LISTING FOR LATERAL DRAINAGE
C
      REAL        PNBRN(12),RMULT,XSOIL(3),PRTBUF(12),RVAL(31,12),
     1            OUTFLOW
      CHARACTER*4 INTS,SWTR,SNOP,THET,PRCP,SNOF,THRF,INFL,RUNF,CEVP,
     1            TETD,ESLS,FPST,TPST,SPST,TPAP,FPDL,WFLX,DFLX,AFLX,
     2            UFLX,RFLX,EFLX,RZFX,TUPX,TDKF,TSER,TCUM,OUTF,COFX,
     3            VFLX,PRTNAM(12),SLET,DKFX,STMP,CHGT,PCNC,FPVL,LTFX,
     4            PLON,AMAD,AMSU,NO3,PLTN,SLON,PRON,SRON,ELON,EAMA,
     5            ERON,RAMA,RNO3,RLON,RRON,PSAM,OSAM,PSNI,OSNI,DENI,
     6            AMNI,AMIM,ONMN,DDAM,DDNI,DDON,WDAM,WDNI,WDON,NFIX,
     7            PSLN,OSLN,PSRN,OSRN,NIIM,AMVO,LARF,ANIU,AAMU,BNIU,
     8            BAMU,REAG,ARLN,ARRN,BRLN,BRRN,AGPN,LITN
      CHARACTER*80 MESAGE
C
C     + + + INTRINSICS + + +
C
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,WDTPUT,SUBOUT
C
C     + + + DATA INITIALIZATIONS + + +
C
      DATA INTS/'INTS'/,SWTR/'SWTR'/,THET/'THET'/,PRCP/'PRCP'/,
     1     SNOF/'SNOF'/,THRF/'THRF'/,INFL/'INFL'/,RUNF/'RUNF'/,
     2     CEVP/'CEVP'/,SLET/'SLET'/,TETD/'TETD'/,ESLS/'ESLS'/,
     3     FPST/'FPST'/,TPST/'TPST'/,SPST/'SPST'/,TPAP/'TPAP'/,
     4     FPDL/'FPDL'/,WFLX/'WFLX'/,DFLX/'DFLX'/,AFLX/'AFLX'/,
     5     DKFX/'DKFX'/,UFLX/'UFLX'/,RFLX/'RFLX'/,EFLX/'EFLX'/,
     6     RZFX/'RZFX'/,TUPX/'TUPX'/,TDKF/'TDKF'/,TSER/'TSER'/,
     7     TCUM/'TCUM'/,SNOP/'SNOP'/,STMP/'STMP'/,CHGT/'CHGT'/,
     8     VFLX/'VFLX'/,PCNC/'PCNC'/,FPVL/'FPVL'/,LTFX/'LTFX'/,
     9     OUTF/'OUTF'/,COFX/'COFX'/
      DATA PLON/'PLON'/,AMAD/'AMAD'/,AMSU/'AMSU'/,NO3/'NO3 '/,
     $     PLTN/'PLTN'/,SLON/'SLON'/,PRON/'PRON'/,SRON/'SRON'/,
     $     ELON/'ELON'/,EAMA/'EAMA'/,ERON/'ERON'/,RAMA/'RAMA'/,
     $     RNO3/'RNO3'/,RLON/'RLON'/,RRON/'RRON'/,PSAM/'PSAM'/,
     $     OSAM/'OSAM'/,PSNI/'PSNI'/,OSNI/'OSNI'/,DENI/'DENI'/,
     $     AMNI/'AMNI'/,AMIM/'AMIM'/,ONMN/'ONMN'/,DDAM/'DDAM'/,
     $     DDNI/'DDNI'/,DDON/'DDON'/,WDAM/'WDAM'/,WDNI/'WDNI'/,
     $     WDON/'WDON'/,NFIX/'NFIX'/,PSLN/'PSLN'/,OSLN/'OSLN'/,
     $     PSRN/'PSRN'/,OSRN/'OSRN'/,NIIM/'NIIM'/,AMVO/'AMVO'/,
     $     LARF/'LARF'/,ANIU/'ANIU'/,AAMU/'AAMU'/,BNIU/'BNIU'/,
     $     BAMU/'BAMU'/,REAG/'REAG'/,ARLN/'ARLN'/,ARRN/'ARRN'/,
     $     BRLN/'BRLN'/,BRRN/'BRRN'/,AGPN/'AGPN'/,LITN/'LITN'/
      DATA NVAL/0/
C
C     + + + OUTPUT FORMATS + + +
C
2000  FORMAT (1X,A3,'TIME SERIES OUTPUT FILES',/,1X,A3,2X,A78,/,
     1        1X,A3,21X,7(5X,A4,A1,4X))
2010  FORMAT (1X,A3,1X,'PRZ  19',I2,2I3,6X,10(2X,G12.4))
2020  FORMAT (1X,A3,1X,110(1H*),/,1X,A3,1X,110(1H*),/,1X,A3,/,1X,A3,50X,
     1        'E R R O R',/,1X,A3,/,1X,A3,5X,'ERROR WRITING DSN ',I5,
     2        ' ON WDMSFL, RETCOD:',I4,/,1X,A3,/,1X,A3,1X,110(1H*))
C
C     + + + END SPECIFCATIONS + + +
C
      MESAGE = 'OUTTSR'
      CALL SUBIN(MESAGE)
C
      CPLTWD = 0
      CPLTFL = 0
C
C     Section added to sum up soilap, see below
C
      DO 6 K=1,NCHEM
        XSOIL(K) = 0.00
        DO 4 I = 1, NCOM2
          XSOIL(K) = XSOIL(K) + SOILAP(K,I)
4       CONTINUE
6     CONTINUE
C
C *** Section added to sum up lateral outflow of water, ATC 8/93 ***
C
      OUTFLOW = 0.0
      DO 3 I = 1,NCOM2
         OUTFLOW = OUTFLOW + OUTFLO(I)
 3    CONTINUE
C
C ******************************************************************
C
      DO 10 I=1,NPLOTS
        IARG1= IARG(I)
        ID1 = 1
        IF (INDX(I) .EQ. '2') ID1 = 2
        IF (INDX(I) .EQ. '3') ID1 = 3
        OUTPUT(I)= 0.
C
C       Water storages  (units of CM)
C
        IF (PLNAME(I) .EQ. INTS) PNBRN(I)= CINT
        IF (PLNAME(I) .EQ. SWTR) PNBRN(I)= SW(IARG1)
        IF (PLNAME(I) .EQ. SNOP) PNBRN(I)= SNOW
C
C       Soil water storages (dimensionless)
C
        IF (PLNAME(I) .EQ. THET) PNBRN(I)= THETN(IARG1)
C
C       Water fluxes (units of CM/DAY)
C
        IF (PLNAME(I) .EQ. PRCP) PNBRN(I)= PRECIP
        IF (PLNAME(I) .EQ. SNOF) PNBRN(I)= SNOWFL
        IF (PLNAME(I) .EQ. THRF) PNBRN(I)= THRUFL
        IF (PLNAME(I) .EQ. INFL) PNBRN(I)= AINF(IARG1)
        IF (PLNAME(I) .EQ. RUNF) PNBRN(I)= RUNOF
        IF (PLNAME(I) .EQ. CEVP) PNBRN(I)= CEVAP
        IF (PLNAME(I) .EQ. SLET) PNBRN(I)= ET(IARG1)
        IF (PLNAME(I) .EQ. TETD) PNBRN(I)= TDET
        IF (PLNAME(I) .EQ. OUTF) PNBRN(I)= OUTFLOW
C
C       Sediment flux (units of KG/DAY)
        IF (PLNAME(I) .EQ. ESLS) PNBRN(I)= SEDL
C
C       Soil temperature (oC)
        IF (PLNAME(I) .EQ. STMP) PNBRN(I)=SPT(IARG1)
C
C       Canopy height (CM)
        IF (PLNAME(I) .EQ. CHGT) PNBRN(I)=HEIGHT
C
C       Pesticide storages (units of GRAMS/(CM**2)
        IF (PLNAME(I) .EQ. FPST) PNBRN(I)=FOLPST(ID1)
        IF (PLNAME(I) .EQ. TPST) PNBRN(I)=PESTR(ID1,IARG1)*DELX(IARG1)
     1                                    *THETN(IARG1)
        IF (PLNAME(I) .EQ. SPST) PNBRN(I)=SPESTR(ID1,IARG1)*THETN(IARG1)
     1                                    *DELX(IARG1)
        IF (PLNAME(I) .EQ. PCNC) PNBRN(I)=TCNC(ID1)
C
C       Pesticide fluxes (units of GRAMS/(CM**2-DAY)
        IF (PLNAME(I) .EQ. TPAP) PNBRN(I)= XSOIL(ID1) + PLNTAP(ID1)
        IF (PLNAME(I) .EQ. FPDL) PNBRN(I)= FPDLOS(ID1)
        IF (PLNAME(I) .EQ. WFLX) PNBRN(I)= WOFLUX(ID1)
        IF (PLNAME(I) .EQ. DFLX) PNBRN(I)= DFFLUX(ID1,IARG1)
        IF (PLNAME(I) .EQ. AFLX) PNBRN(I)= ADFLUX(ID1,IARG1)
        IF (PLNAME(I) .EQ. DKFX) PNBRN(I)= DKFLUX(ID1,IARG1)
        IF (PLNAME(I) .EQ. UFLX) PNBRN(I)= UPFLUX(ID1,IARG1)
        IF (PLNAME(I) .EQ. RFLX) PNBRN(I)= ROFLUX(ID1)
        IF (PLNAME(I) .EQ. EFLX) PNBRN(I)= ERFLUX(ID1)
        IF (PLNAME(I) .EQ. RZFX) PNBRN(I)= RZFLUX(ID1)
        IF (PLNAME(I) .EQ. TUPX) PNBRN(I)= SUPFLX(ID1)
        IF (PLNAME(I) .EQ. TDKF) PNBRN(I)= SDKFLX(ID1)
        IF (PLNAME(I) .EQ. LTFX) PNBRN(I)= LATFLX(ID1)
        IF (PLNAME(I) .EQ. COFX) PNBRN(I)= DCOFLX(ID1)*1.E-5
C
C       Add VFLX option to plot PVFLUX
C
        IF (PLNAME(I) .EQ. VFLX) PNBRN(I)= -PVFLUX(ID1,1)
        IF (PLNAME(I) .EQ. FPVL) PNBRN(I)= FPVLOS(ID1)
C
C       nitrogen storages
        IF (PLNAME(I) .EQ. PLON) PNBRN(I)= NIT(1,IARG1)
        IF (PLNAME(I) .EQ. AMAD) PNBRN(I)= NIT(2,IARG1)
        IF (PLNAME(I) .EQ. AMSU) PNBRN(I)= NIT(3,IARG1)
        IF (PLNAME(I) .EQ. NO3) PNBRN(I) = NIT(4,IARG1)
        IF (PLNAME(I) .EQ. PLTN) PNBRN(I)= NIT(5,IARG1)
        IF (PLNAME(I) .EQ. SLON) PNBRN(I)= NIT(6,IARG1)
        IF (PLNAME(I) .EQ. PRON) PNBRN(I)= NIT(7,IARG1)
        IF (PLNAME(I) .EQ. SRON) PNBRN(I)= NIT(8,IARG1)
C       nitrogen fluxes
        IF (PLNAME(I) .EQ. ELON) PNBRN(I)= NCFX1(1,1)
        IF (PLNAME(I) .EQ. EAMA) PNBRN(I)= NCFX1(2,1)
        IF (PLNAME(I) .EQ. ERON) PNBRN(I)= NCFX1(3,1)
        IF (PLNAME(I) .EQ. RAMA) PNBRN(I)= NCFX1(4,1)
        IF (PLNAME(I) .EQ. RNO3) PNBRN(I)= NCFX1(5,1)
        IF (PLNAME(I) .EQ. RLON) PNBRN(I)= NCFX1(6,1)
        IF (PLNAME(I) .EQ. RRON) PNBRN(I)= NCFX1(7,1)
        IF (PLNAME(I) .EQ. PSAM) PNBRN(I)= NCFX2(IARG1,1)
        IF (PLNAME(I) .EQ. OSAM) PNBRN(I)= NCFX3(IARG1,1)
        IF (PLNAME(I) .EQ. PSNI) PNBRN(I)= NCFX4(IARG1,1)
        IF (PLNAME(I) .EQ. OSNI) PNBRN(I)= NCFX5(IARG1,1)
        IF (PLNAME(I) .EQ. DENI) PNBRN(I)= NCFX6(IARG1,1)
        IF (PLNAME(I) .EQ. AMNI) PNBRN(I)= NCFX7(IARG1,1)
        IF (PLNAME(I) .EQ. AMIM) PNBRN(I)= NCFX8(IARG1,1)
        IF (PLNAME(I) .EQ. ONMN) PNBRN(I)= NCFX9(IARG1,1)
        IF (PLNAME(I) .EQ. DDAM) PNBRN(I)= NCFX10(1,1)
        IF (PLNAME(I) .EQ. DDNI) PNBRN(I)= NCFX10(2,1)
        IF (PLNAME(I) .EQ. DDON) PNBRN(I)= NCFX10(3,1)
        IF (PLNAME(I) .EQ. WDAM) PNBRN(I)= NCFX11(1,1)
        IF (PLNAME(I) .EQ. WDNI) PNBRN(I)= NCFX11(2,1)
        IF (PLNAME(I) .EQ. WDON) PNBRN(I)= NCFX11(3,1)
        IF (PLNAME(I) .EQ. NFIX) PNBRN(I)= NCFX12(IARG1,1)
        IF (PLNAME(I) .EQ. PSLN) PNBRN(I)= NCFX13(IARG1,1)
        IF (PLNAME(I) .EQ. OSLN) PNBRN(I)= NCFX14(IARG1,1)
        IF (PLNAME(I) .EQ. PSRN) PNBRN(I)= NCFX15(IARG1,1)
        IF (PLNAME(I) .EQ. OSRN) PNBRN(I)= NCFX16(IARG1,1)
        IF (PLNAME(I) .EQ. NIIM) PNBRN(I)= NCFX17(IARG1,1)
        IF (PLNAME(I) .EQ. AMVO) PNBRN(I)= NCFX18(IARG1,1)
        IF (PLNAME(I) .EQ. LARF) PNBRN(I)= NCFX19(IARG1,1)
        IF (PLNAME(I) .EQ. ANIU) PNBRN(I)= NCFX20(IARG1,1)
        IF (PLNAME(I) .EQ. AAMU) PNBRN(I)= NCFX21(IARG1,1)
        IF (PLNAME(I) .EQ. BNIU) PNBRN(I)= NCFX22(IARG1,1)
        IF (PLNAME(I) .EQ. BAMU) PNBRN(I)= NCFX23(IARG1,1)
        IF (PLNAME(I) .EQ. REAG) PNBRN(I)= NCFX24(1,1)
        IF (PLNAME(I) .EQ. ARLN) PNBRN(I)= NCFX25(IARG1,1)
        IF (PLNAME(I) .EQ. ARRN) PNBRN(I)= NCFX26(IARG1,1)
        IF (PLNAME(I) .EQ. BRLN) PNBRN(I)= NCFX27(IARG1,1)
        IF (PLNAME(I) .EQ. BRRN) PNBRN(I)= NCFX28(IARG1,1)
        IF (PLNAME(I) .EQ. AGPN) PNBRN(I)= AGPLTN
        IF (PLNAME(I) .EQ. LITN) PNBRN(I)= LITTRN

C
C       Change units of output using user supplied constant
C
        IF(ABS(CONST(I)).LT.1.0E-5) THEN
C         default 1.0
          RMULT= 1.0
        ELSE
C
C         Use user specified value
C
          RMULT= CONST(I)
        END IF
        PNBRN(I)= PNBRN(I)* RMULT
C
        IF (MODE(I).EQ.TSER) THEN
          OUTPUT(I)= PNBRN(I)
        ELSE IF (MODE(I).EQ.TCUM) THEN
C
C         Accumulate output variable if a cumulative plot is requested
C
          IF (SRNFG.EQ.1) THEN
            OUTPJJ(I)= PNBRN(I)
          ELSE
            OUTPUJ(I)= OUTPUT(I)+ PNBRN(I)
            OUTPJJ(I)= OUTPJJ(I)+ OUTPUJ(I)
          END IF
        END IF
        IF (PLTYP(I).NE.'W') THEN
C
C         Printer plot to file
C
          CPLTFL= CPLTFL+ 1
          IF (MODE(I).EQ.TSER) PRTBUF(CPLTFL)= OUTPUT(I)
          IF (MODE(I).EQ.TCUM) PRTBUF(CPLTFL)= OUTPJJ(I)
          IF (SRNFG.EQ.1) PRTNAM(CPLTFL)= PLNAME(I)
        END IF
        IF (PLTYP(I).NE.'P') THEN
C
C         Write data on WDMS file
C
          CPLTWD= CPLTWD+ 1
          IF (CPLTWD.EQ.1) NVAL= NVAL+ 1
          RVAL(NVAL,CPLTWD)= OUTPUT(I)
        END IF
 10   CONTINUE
C
      IF (CPLTFL.GT.0 .AND. LTMSRS.GT.0) THEN
C
C        Something to write to printer
C
C        IF (SRNFG.EQ.1) THEN
         IF (HEADER .EQ. 1) THEN
          WRITE(LTMSRS,2000) (MODIDT,I=1,2),TITLE,
     1                      MODIDT,(PRTNAM(I),INDX(I),I=1,CPLTFL)
        END IF
        WRITE(LTMSRS,2010) MODIDT,IY,MONTH,DOM,(PRTBUF(I),I=1,CPLTFL)
      END IF
      IF (CPLTWD.GT.0) THEN
C
C       Something going to wdms file
C
        IF (SRNFG.EQ.1) THEN
C
C         Initialize write parameters
C
          DLT     = 1
          DTOVWR  = 1
          QUALFG  = 0
          TUNITS  = 4
          DATES(1)= ISTYR+ 1900
          DATES(2)= ISMON
          DATES(3)= ISDAY
          DATES(4)= 0
          DATES(5)= 0
          DATES(6)= 0
        END IF
        IF (EYRFG.EQ.1) THEN
C
C         Time to write to WDMS file
C
          DO 30 I= 1,CPLTWD
            CALL WDTPUT(LWDMS,PLTDSN(I),DLT,DATES,NVAL,DTOVWR,QUALFG,
     1                    TUNITS,RVAL(1,I),RETCOD)
C
            IF (RETCOD.NE.0) THEN
C
C             Problem writing wdmsfl
C
              WRITE (LPRZOT,2020) (MODIDW,K=1,6),PLTDSN(I),RETCOD,
     1                          (MODIDW,K=1,2)
            END IF
 30       CONTINUE
C
C         Set parameters for next write
C
          NVAL    = 0
          DATES(2)= DATES(2)+ 1
          IF (DATES(2) .EQ. 13) THEN
            DATES(1) = DATES(1) + 1
            DATES(2)= 1
          ENDIF
          DATES(3)= 1
        END IF
      END IF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   OUTCNI
     I                   (FLCN,MODID)
C
C     + + + PURPOSE + + +
C
C     Prints daily, monthly and annual nitrogen species concentration profiles.
C     Modification date: 8/30/95 PRH
C
C     + + + DUMMY ARGUMENTS + + +               
C
      INTEGER     FLCN
      CHARACTER*3 MODID
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     FLCN  - Fortran unit number for output file FLCN
C     MODID - character string for output file identification
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CMET.INC'
      INCLUDE 'CNITR.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +
      EXTERNAL SUBIN,SUBOUT
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (1X,A3,/,1X,A3,T6,'NITROGEN SPECIES STORAGE PROFILE, IN ',
     1        'KG/HA',/,1X,A3,/,1X,A3,T6,'DATE (DAY-MONTH-YEAR)',T31,I2,
     2        ' ',A4,', ',I2,2(/,1X,A3),' ABOVE GROUND PLANT',T25,G10.4,
     3        /,1X,A3,' LITTER',T25,G10.4,2(/,1X,A3),T6,'HO-',T11,
     4        'COM-',T18,'NH4-N',T29,'NH4-N',T40,'NO3/',
     5        T51,'SOL LABILE',T62,'ADS LABILE',T73,'SOL REFR',T84,
     6        'ADS REFR',/,1X,A3,1X,'RIZON',T11,'PTMNT',T18,
     6        'SOLUTION',T29,'ADSORBED',T40,'NO2-N',T51,'ORGANIC N',
     7        T62,'ORGANIC N',T73,'ORGANIC N',T84,'ORGANIC N',T95,
     8        'PLANT N',/,1X,A3,T6,105(1H-),/,1X,A3)
 2010 FORMAT (1X,A3,I4,1X,I4,T17,8(1X,G10.4))
 2020 FORMAT (1X,A3,/,1X,A3,T8,'TOTALS',T17,8(1X,G10.4),/,1X,A3)
C
C     + + + END SPECIFICATONS + + +
C
      MESAGE = 'OUTCNI'
      CALL SUBIN(MESAGE)
C
      WRITE(FLCN,2000) (MODID,I=1,4),DOM,CMONTH(MONTH),IY,
     1                  MODID,MODID,AGPLTN,MODID,LITTRN,(MODID,I=1,5)
C
      DO 10 I=1, NCOM2, LFREQ3
        WRITE(FLCN,2010) MODID,HORIZN(I),I,NIT(3,I),NIT(2,I),NIT(4,I),
     $                   NIT(6,I),NIT(1,I),NIT(8,I),NIT(7,I),NIT(5,I)
10    CONTINUE
C
C     totals
      WRITE(FLCN,2020) MODID,MODID,TNIT(3),TNIT(2),TNIT(4),TNIT(6),
     $                 TNIT(1),TNIT(8),TNIT(7),TNIT(5),MODID
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   OUTNIT
     I                   (FLPS,MODID,SEPTON)
C
C     + + + PURPOSE + + +
C     Accumulates and outputs daily, monthly, and
C     annual summaries for nitrogen species.
C     Creattion date: 8/31/95 PRH
C     Included the provision of getting the nitrogen flux in
C     lateral flow, added to OUTPST by AQUA TERRA Consultants, 9/93
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     FLPS
      CHARACTER*3 MODID
      LOGICAL     SEPTON
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FLPS   - Fortran unit number for output file FLPS
C     MODID  - character string for output file identification
C     SEPTON - septic effluent on flag
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXVDT.INC'
      INCLUDE 'PMXYRS.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'TABLE.INC'
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CCROP.INC'
      INCLUDE 'CMET.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CACCUM.INC'
      INCLUDE 'CPTCHK.INC'
      INCLUDE 'CNITR.INC'
      INCLUDE 'CSPTIC.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I3,I,J,BSUMM,MNTHP1,NOPRT,NSUMM,NCP1
      REAL         R0
      CHARACTER*4  YEAR,MNTH,DAY,CNITR
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,SUBOUT,ZIPR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA YEAR/'YEAR'/,MNTH/'MNTH'/,DAY/' DAY'/,CNITR/'NITR'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (1X,A3,1X,'****************************',/,1X,A3,1X,
     1        '* DAILY NITROGEN OUTPUT    *')
 2010 FORMAT (1X,A3,1X,'****************************',/,1X,A3,1X,
     1        '* MONTHLY NITROGEN OUTPUT  *')
 2020 FORMAT (1X,A3,1X,'****************************',/,1X,A3,1X,
     1        '* ANNUAL NITROGEN OUTPUT   *')
 2030 FORMAT (1X,A3,1X,'*                          *',/,1X,A3,1X,
     1        '* DATE:     ',I2,' ',A4,', ',I2,4X,'*',/,1X,A3,1X,
     2        '****************************',/,1X,A3,
     3        T35,'STORAGE UNITS IN KG/HA',/,1X,A3,
     4        T35,'FLUX UNITS IN KG/HA/OUTPUT TIMESTEP')
 2040 FORMAT (1X,A3,/,1X,A3,T6,'CURRENT CONDITIONS',/,1X,A3,
     1           T6,'------------------',/,1X,A3,/,1X,A3,
     2           T6,'CROP NUMBER',T45,I5)
 2050 FORMAT (1X,A3,/,1X,A3,/,1X,A3,T6,'FLUXES ',
     1        'FOR THIS PERIOD',/,1X,A3,
     1        T6,'-----------------------------------')
 2060 FORMAT (1X,A3,/,1X,A3,/,1X,A3,' ATMOSPHERIC DEPOSITION',/,1X,A3,/,
     $        1X,A3,T26,'DRY',T38,'WET',T48,'TOTAL',/,
     $        1X,A3,'   NH4-N',T17,3(2X,G10.4),/,
     $        1X,A3,'   NO3-N',T17,3(2X,G10.4),/,
     $        1X,A3,'   ORGN',T17,3(2X,G10.4),/,1X,A3,/,
     $        1X,A3,' <---SEDIMENT EROSION OUTFLOW--->',T45,
     $        '<---------SOLUTION RUNOFF OUTFLOW--------->',/,
     $        1X,A3,'      NH4-N     LABILE     REFRAC',T45,
     $        '  NH4-N IN  NO3(+NO2)     LABILE     REFRAC',/,
     $        1X,A3,'   ADSORBED  ORGANIC N  ORGANIC N',T45,
     $        '  SOLUTION   SOLUTION  ORGANIC N  ORGANIC N',/,
     $        1X,A3,3(1X,G10.4),T45,4(G10.4,1X))
 2065 FORMAT (1X,A3,/,1X,A3,/,1X,A3,' SEPTIC EFFLUENT NITROGEN INFLOWS',
     $        /,1X,A3,'   AMMONIA    NITRATE  ORGANIC N',/,1X,A3,
     $        3(G10.4,1X))
 2070 FORMAT (1X,A3,/,1X,A3,/,1X,A3,T7,'HO-  COM-',T19,
     3        'NH4 LEACH-',T31,'NH4 LATERAL',T43,'ABV-GROUND',T55,
     4        'BLW-GROUND',T67,'NH3 NITRI-',T79,'NH3 IMMOB-',T91,
     5        'NH3 VOLAT-',T103,'ORG-N MIN-',/,1X,A3,T6,'RIZON PTMNT',
     6        T19,'ING OUTPUT',T31,'OUTFLOW',T43,'NH3 UPTAKE',T55,
     7        'NH3 UPTAKE',T67,'FICATION',T79,'ILIZATION',T91,
     8        'ILIZATION',T103,'ERALIZATION',/,1X,A3,1X,108('-'),/,
     9        1X,A3)
 2071 FORMAT (1X,A3,/,1X,A3,/,1X,A3,T7,'HO-  COM-',T19,
     3        'NO3 LEACH-',T31,'NO3 LATERAL',T43,'ABV-GROUND',T55,
     4        'BLW-GROUND',T67,'NO3 IMMOB-',T79,'NITROGEN',T91,
     5        'DENITRIF-',/,1X,A3,T6,'RIZON PTMNT',T19,'ING OUTPUT',
     6        T31,'OUTFLOW',T43,'NO3 UPTAKE',T55,'NO3 UPTAKE',T67,
     7        'ILIZATION',T79,'FIXATION',T91,'ICATION',/,1X,A3,1X,
     9        95('-'),/,1X,A3)
 2072 FORMAT (1X,A3,/,1X,A3,/,1X,A3,T19,'<',11('-'),
     1        ' LABILE ORGANIC N ',12('-'),'>',T64,'<',9('-'),
     2        ' REFRACTORY ORGANIC N ',10('-'),'>',/,1X,A3,T7,
     3        'HO-  COM-',T19,'LEACHING',T30,'LATERAL',T41,'LITTER',
     4        T52,'BLW-GROUND',T64,'LEACHING',T75,'LATERAL',T86,
     5        'LITTER',T97,'BLW-GROUND',T109,'LABILE-->REFRACTORY',/,
     6        1X,A3,T6,'RIZON PTMNT',T19,'OUTPUT',T30,'OUTFLOW',T41,
     7        'RETURN',T52,'RETURN',T64,'OUTPUT',T75,'OUTFLOW',T86,
     8        'RETURN',T97,'RETURN',T109,'CONVERSION',/,1X,A3,1X,
     9        118('-'),/,1X,A3)
 2080 FORMAT (1X,A3,I4,1X,I4,T19,8(G10.4,2X))
 2081 FORMAT (1X,A3,I4,1X,I4,T19,7(G10.4,2X))
 2082 FORMAT (1X,A3,I4,1X,I4,T19,4(G10.4,1X),T64,4(G10.4,1X),T109,G10.4)
 2090 FORMAT (1X,A3,/,1X,A3,/,1X,A3,/,1X,A3,T6,'MATERIAL BALANCE',/,
     1        1X,A3,T6,'----------------',/,1X,A3,/,
     2        1X,A3,T6,'TOTAL NITROGEN BALANCE ERROR',T44,G10.4,/,
     3        1X,A3,T6,'CUMULATIVE ERROR',T44,G10.4)
 2100 FORMAT (1X,A3,/,1X,A3,' TOTALS',T19,8(G10.4,2X))
 2101 FORMAT (1X,A3,/,1X,A3,' TOTALS',T19,7(G10.4,2X))
 2102 FORMAT (1X,A3,/,1X,A3,' TOTALS',T19,4(G10.4,1X),T64,
     $        4(G10.4,1X),T109,G10.4)
C 2100 FORMAT (1X,A3,/,1X,A3,/,1X,A3,
C     $        T6,'SUMMARY FLUXES FOR NH3/NH4',/,1X,A3,
C     $        T6,'--------------------------',/1X,A3,/,1X,A3,
C     $        T6,'TOTAL LATERAL OUTFLOW OF NH4',T46,G10.4,/,1X,A3,
C     $        T6,'TOTAL ABOVE-GROUND PLANT UPTAKE OF NH3',T46,G10.4,/,
C     $        1X,A3,T6,'TOTAL BELOW-GROUND PLANT UPTAKE OF NH3',T46,
C     $        G10.4,/,1X,A3,T6,'TOTAL NH3 NITRIFICATION',T46,G10.4,/,
C     $        1X,A3,T6,'TOTAL NH3 IMMOBILIZATION',T46,G10.4,/,1X,A3,
C     $        T6,'TOTAL NH3 VOLATILIZATION',T46,G10.4,/,1X,A3,
C     $        T6,'TOTAL ORGANIC N MINERALIZATION',T46,G10.4)
C 2101 FORMAT (1X,A3,/,1X,A3,/,1X,A3,
C     $        T6,'SUMMARY FLUXES FOR NO3',/,1X,A3,
C     $        T6,'----------------------',/,1X,A3,/,1X,A3,
C     $        T6,'TOTAL LATERAL OUTFLOW OF NO3',T46,G10.4,/,1X,A3,
C     $        T6,'TOTAL ABOVE-GROUND PLANT UPTAKE OF NO3',T46,G10.4,/,
C     $        1X,A3,T6,'TOTAL BELOW-GROUND PLANT UPTAKE OF NO3',T46,
C     $        G10.4,/,1X,A3,T6,'TOTAL NO3 IMMOBILIZATION',T46,G10.4,/,
C     $        1X,A3,T6,'TOTAL NITROGEN FIXATION',T46,G10.4,/,1X,A3,
C     $        T6,'TOTAL NO3 DENITRIFICATION',T46,G10.4)
C 2102 FORMAT (1X,A3,/,1X,A3,/,1X,A3,
C     $        T6,'SUMMARY FLUXES FOR ORGANIC N',/,1X,A3,
C     $        T6,'----------------------------',/,1X,A3,/,1X,A3,T6,
C     $        'TOTAL LATERAL OUTFLOW OF LABILE ORGN',T54,G10.4,/,1X,A3,
C     $        T6,'TOTAL LITTER RETURN TO LABILE ORGN',T54,G10.4,/,1X,A3,
C     $        T6,'TOTAL BELOW-GROUND RETURN TO LABILE ORGN',T54,G10.4,/,
C     $        1X,A3,T6,'TOTAL LATERAL OUTFLOW OF REFRACTORY ORGN',T54,
C     $        G10.4,/,1X,A3,T6,'TOTAL LITTER RETURN TO REFRACTORY ORGN',
C     $        T54,G10.4,/,1X,A3,T6,'TOTAL BELOW-GROUND RETURN TO ',
C     $        'REFRACTORY ORGN',T54,G10.4,/,1X,A3,T6,'TOTAL CONVERSION',
C     $        ' OF LABILE TO REFRACTORY ORGN',T54,G10.4)
 2120 FORMAT (1X,A3)
C
C     + + + END SPECIFICATIONS + + +
C
      I3 = 3
      R0 = 0.0
      NCP1 = NCOM2 + 1
C
      MESAGE = 'OUTNIT'
      CALL SUBIN(MESAGE)
C
C     Zero summary variables and set beginning values if beginning of
C     month or year is encountered
C
      IF (DOM .EQ. 1) THEN
C       beginning of month
        CALL ZIPR (I3,R0,MSNINF)
        I = 7
        CALL ZIPR (I,R0,NCFX1(1,2))
        CALL ZIPR (NCOM2,R0,NCFX2(1,2))
        CALL ZIPR (NCP1,R0,NCFX3(1,2))
        CALL ZIPR (NCOM2,R0,NCFX4(1,2))
        CALL ZIPR (NCP1,R0,NCFX5(1,2))
        CALL ZIPR (NCP1,R0,NCFX6(1,2))
        CALL ZIPR (NCP1,R0,NCFX7(1,2))
        CALL ZIPR (NCP1,R0,NCFX8(1,2))
        CALL ZIPR (NCP1,R0,NCFX9(1,2))
        CALL ZIPR (I3,R0,NCFX10(1,2))
        CALL ZIPR (I3,R0,NCFX11(1,2))
        CALL ZIPR (NCP1,R0,NCFX12(1,2))
        CALL ZIPR (NCOM2,R0,NCFX13(1,2))
        CALL ZIPR (NCP1,R0,NCFX14(1,2))
        CALL ZIPR (NCOM2,R0,NCFX15(1,2))
        CALL ZIPR (NCP1,R0,NCFX16(1,2))
        CALL ZIPR (NCP1,R0,NCFX17(1,2))
        CALL ZIPR (NCP1,R0,NCFX18(1,2))
        CALL ZIPR (NCP1,R0,NCFX19(1,2))
        CALL ZIPR (NCP1,R0,NCFX20(1,2))
        CALL ZIPR (NCP1,R0,NCFX21(1,2))
        CALL ZIPR (NCP1,R0,NCFX22(1,2))
        CALL ZIPR (NCP1,R0,NCFX23(1,2))
        NCFX24(1,2) = 0.0
        CALL ZIPR (NCP1,R0,NCFX25(1,2))
        CALL ZIPR (NCP1,R0,NCFX26(1,2))
        CALL ZIPR (NCP1,R0,NCFX27(1,2))
        CALL ZIPR (NCP1,R0,NCFX28(1,2))
        IF (JULDAY .EQ. 1) THEN
C
C         Beginning of year
C
          CALL ZIPR (I3,R0,YSNINF)
          I = 7
          CALL ZIPR (I,R0,NCFX1(1,3))
          CALL ZIPR (NCOM2,R0,NCFX2(1,3))
          CALL ZIPR (NCP1,R0,NCFX3(1,3))
          CALL ZIPR (NCOM2,R0,NCFX4(1,3))
          CALL ZIPR (NCP1,R0,NCFX5(1,3))
          CALL ZIPR (NCP1,R0,NCFX6(1,3))
          CALL ZIPR (NCP1,R0,NCFX7(1,3))
          CALL ZIPR (NCP1,R0,NCFX8(1,3))
          CALL ZIPR (NCP1,R0,NCFX9(1,3))
          CALL ZIPR (I3,R0,NCFX10(1,3))
          CALL ZIPR (I3,R0,NCFX11(1,3))
          CALL ZIPR (NCP1,R0,NCFX12(1,3))
          CALL ZIPR (NCOM2,R0,NCFX13(1,3))
          CALL ZIPR (NCP1,R0,NCFX14(1,3))
          CALL ZIPR (NCOM2,R0,NCFX15(1,3))
          CALL ZIPR (NCP1,R0,NCFX16(1,3))
          CALL ZIPR (NCP1,R0,NCFX17(1,3))
          CALL ZIPR (NCP1,R0,NCFX18(1,3))
          CALL ZIPR (NCP1,R0,NCFX19(1,3))
          CALL ZIPR (NCP1,R0,NCFX20(1,3))
          CALL ZIPR (NCP1,R0,NCFX21(1,3))
          CALL ZIPR (NCP1,R0,NCFX22(1,3))
          CALL ZIPR (NCP1,R0,NCFX23(1,3))
          NCFX24(1,3) = 0.0
          CALL ZIPR (NCP1,R0,NCFX25(1,3))
          CALL ZIPR (NCP1,R0,NCFX26(1,3))
          CALL ZIPR (NCP1,R0,NCFX27(1,3))
          CALL ZIPR (NCP1,R0,NCFX28(1,3))
        ENDIF
      ENDIF
C
C     Multiply internal units of GR/CM**2 by 10**5 so output
C     is expressed in units of KG/HA (as in the input)
C
C     Accumulate values for summaries
C
      CALL ZIPR (I3,R0,DSNINF)
      NCFX1(1,2) = NCFX1(1,2) + NCFX1(1,1)
      NCFX1(2,2) = NCFX1(2,2) + NCFX1(2,1)
      NCFX1(3,2) = NCFX1(3,2) + NCFX1(3,1)
      NCFX1(4,2) = NCFX1(4,2) + NCFX1(4,1)
      NCFX1(5,2) = NCFX1(5,2) + NCFX1(5,1)
      NCFX1(6,2) = NCFX1(6,2) + NCFX1(6,1)
      NCFX1(7,2) = NCFX1(7,2) + NCFX1(7,1)
      NCFX1(1,3) = NCFX1(1,3) + NCFX1(1,1)
      NCFX1(2,3) = NCFX1(2,3) + NCFX1(2,1)
      NCFX1(3,3) = NCFX1(3,3) + NCFX1(3,1)
      NCFX1(4,3) = NCFX1(4,3) + NCFX1(4,1)
      NCFX1(5,3) = NCFX1(5,3) + NCFX1(5,1)
      NCFX1(6,3) = NCFX1(6,3) + NCFX1(6,1)
      NCFX1(7,3) = NCFX1(7,3) + NCFX1(7,1)
      DO 100 I = 1,NCOM2
        DSNINF(1)  = DSNINF(1) + AMMINF(I)
        DSNINF(2)  = DSNINF(2) + NITINF(I)
        DSNINF(3)  = DSNINF(3) + ORGINF(I)
        NCFX2(I,2) = NCFX2(I,2) + NCFX2(I,1)
        NCFX3(I,2) = NCFX3(I,2) + NCFX3(I,1)
        NCFX4(I,2) = NCFX4(I,2) + NCFX4(I,1)
        NCFX5(I,2) = NCFX5(I,2) + NCFX5(I,1)
        NCFX6(I,2) = NCFX6(I,2) + NCFX6(I,1)
        NCFX7(I,2) = NCFX7(I,2) + NCFX7(I,1)
        NCFX8(I,2) = NCFX8(I,2) + NCFX8(I,1)
        NCFX9(I,2) = NCFX9(I,2) + NCFX9(I,1)
        NCFX12(I,2) = NCFX12(I,2) + NCFX12(I,1)
        NCFX13(I,2) = NCFX13(I,2) + NCFX13(I,1)
        NCFX14(I,2) = NCFX14(I,2) + NCFX14(I,1)
        NCFX15(I,2) = NCFX15(I,2) + NCFX15(I,1)
        NCFX16(I,2) = NCFX16(I,2) + NCFX16(I,1)
        NCFX17(I,2) = NCFX17(I,2) + NCFX17(I,1)
        NCFX18(I,2) = NCFX18(I,2) + NCFX18(I,1)
        NCFX19(I,2) = NCFX19(I,2) + NCFX19(I,1)
        NCFX20(I,2) = NCFX20(I,2) + NCFX20(I,1)
        NCFX21(I,2) = NCFX21(I,2) + NCFX21(I,1)
        NCFX22(I,2) = NCFX22(I,2) + NCFX22(I,1)
        NCFX23(I,2) = NCFX23(I,2) + NCFX23(I,1)
        NCFX25(I,2) = NCFX25(I,2) + NCFX25(I,1)
        NCFX26(I,2) = NCFX26(I,2) + NCFX26(I,1)
        NCFX27(I,2) = NCFX27(I,2) + NCFX27(I,1)
        NCFX28(I,2) = NCFX28(I,2) + NCFX28(I,1)
        NCFX2(I,3) = NCFX2(I,3) + NCFX2(I,1)
        NCFX3(I,3) = NCFX3(I,3) + NCFX3(I,1)
        NCFX4(I,3) = NCFX4(I,3) + NCFX4(I,1)
        NCFX5(I,3) = NCFX5(I,3) + NCFX5(I,1)
        NCFX6(I,3) = NCFX6(I,3) + NCFX6(I,1)
        NCFX7(I,3) = NCFX7(I,3) + NCFX7(I,1)
        NCFX8(I,3) = NCFX8(I,3) + NCFX8(I,1)
        NCFX9(I,3) = NCFX9(I,3) + NCFX9(I,1)
        NCFX12(I,3) = NCFX12(I,3) + NCFX12(I,1)
        NCFX13(I,3) = NCFX13(I,3) + NCFX13(I,1)
        NCFX14(I,3) = NCFX14(I,3) + NCFX14(I,1)
        NCFX15(I,3) = NCFX15(I,3) + NCFX15(I,1)
        NCFX16(I,3) = NCFX16(I,3) + NCFX16(I,1)
        NCFX17(I,3) = NCFX17(I,3) + NCFX17(I,1)
        NCFX18(I,3) = NCFX18(I,3) + NCFX18(I,1)
        NCFX19(I,3) = NCFX19(I,3) + NCFX19(I,1)
        NCFX20(I,3) = NCFX20(I,3) + NCFX20(I,1)
        NCFX21(I,3) = NCFX21(I,3) + NCFX21(I,1)
        NCFX22(I,3) = NCFX22(I,3) + NCFX22(I,1)
        NCFX23(I,3) = NCFX23(I,3) + NCFX23(I,1)
        NCFX25(I,3) = NCFX25(I,3) + NCFX25(I,1)
        NCFX26(I,3) = NCFX26(I,3) + NCFX26(I,1)
        NCFX27(I,3) = NCFX27(I,3) + NCFX27(I,1)
        NCFX28(I,3) = NCFX28(I,3) + NCFX28(I,1)
 100  CONTINUE
      MSNINF(1)   = MSNINF(1) + DSNINF(1)
      MSNINF(2)   = MSNINF(2) + DSNINF(2)
      MSNINF(3)   = MSNINF(3) + DSNINF(3)
      YSNINF(1)   = YSNINF(1) + DSNINF(1)
      YSNINF(2)   = YSNINF(2) + DSNINF(2)
      YSNINF(3)   = YSNINF(3) + DSNINF(3)
      NCFX10(1,2) = NCFX10(1,2) + NCFX10(1,1)
      NCFX10(2,2) = NCFX10(2,2) + NCFX10(2,1)
      NCFX10(3,2) = NCFX10(3,2) + NCFX10(3,1)
      NCFX11(1,2) = NCFX11(1,2) + NCFX11(1,1)
      NCFX11(2,2) = NCFX11(2,2) + NCFX11(2,1)
      NCFX11(3,2) = NCFX11(3,2) + NCFX11(3,1)
      NCFX10(1,3) = NCFX10(1,3) + NCFX10(1,1)
      NCFX10(2,3) = NCFX10(2,3) + NCFX10(2,1)
      NCFX10(3,3) = NCFX10(3,3) + NCFX10(3,1)
      NCFX11(1,3) = NCFX11(1,3) + NCFX11(1,1)
      NCFX11(2,3) = NCFX11(2,3) + NCFX11(2,1)
      NCFX11(3,3) = NCFX11(3,3) + NCFX11(3,1)
      NCFX24(1,2) = NCFX24(1,2) + NCFX24(1,1)
      NCFX24(1,3) = NCFX24(1,3) + NCFX24(1,1)
C     accumulate total flux in core for needed variables
      NCFX3(NCP1,2) = NCFX3(NCP1,2) + NCFX3(NCP1,1)
      NCFX5(NCP1,2) = NCFX5(NCP1,2) + NCFX5(NCP1,1)
      NCFX6(NCP1,2) = NCFX6(NCP1,2) + NCFX6(NCP1,1)
      NCFX7(NCP1,2) = NCFX7(NCP1,2) + NCFX7(NCP1,1)
      NCFX8(NCP1,2) = NCFX8(NCP1,2) + NCFX8(NCP1,1)
      NCFX9(NCP1,2) = NCFX9(NCP1,2) + NCFX9(NCP1,1)
      NCFX12(NCP1,2) = NCFX12(NCP1,2) + NCFX12(NCP1,1)
      NCFX14(NCP1,2) = NCFX14(NCP1,2) + NCFX14(NCP1,1)
      NCFX16(NCP1,2) = NCFX16(NCP1,2) + NCFX16(NCP1,1)
      NCFX17(NCP1,2) = NCFX17(NCP1,2) + NCFX17(NCP1,1)
      NCFX18(NCP1,2) = NCFX18(NCP1,2) + NCFX18(NCP1,1)
      NCFX19(NCP1,2) = NCFX19(NCP1,2) + NCFX19(NCP1,1)
      NCFX20(NCP1,2) = NCFX20(NCP1,2) + NCFX20(NCP1,1)
      NCFX21(NCP1,2) = NCFX21(NCP1,2) + NCFX21(NCP1,1)
      NCFX22(NCP1,2) = NCFX22(NCP1,2) + NCFX22(NCP1,1)
      NCFX23(NCP1,2) = NCFX23(NCP1,2) + NCFX23(NCP1,1)
      NCFX25(NCP1,2) = NCFX25(NCP1,2) + NCFX25(NCP1,1)
      NCFX26(NCP1,2) = NCFX26(NCP1,2) + NCFX26(NCP1,1)
      NCFX27(NCP1,2) = NCFX27(NCP1,2) + NCFX27(NCP1,1)
      NCFX28(NCP1,2) = NCFX28(NCP1,2) + NCFX28(NCP1,1)
      NCFX3(NCP1,3) = NCFX3(NCP1,3) + NCFX3(NCP1,1)
      NCFX5(NCP1,3) = NCFX5(NCP1,3) + NCFX5(NCP1,1)
      NCFX6(NCP1,3) = NCFX6(NCP1,3) + NCFX6(NCP1,1)
      NCFX7(NCP1,3) = NCFX7(NCP1,3) + NCFX7(NCP1,1)
      NCFX8(NCP1,3) = NCFX8(NCP1,3) + NCFX8(NCP1,1)
      NCFX9(NCP1,3) = NCFX9(NCP1,3) + NCFX9(NCP1,1)
      NCFX12(NCP1,3) = NCFX12(NCP1,3) + NCFX12(NCP1,1)
      NCFX14(NCP1,3) = NCFX14(NCP1,3) + NCFX14(NCP1,1)
      NCFX16(NCP1,3) = NCFX16(NCP1,3) + NCFX16(NCP1,1)
      NCFX17(NCP1,3) = NCFX17(NCP1,3) + NCFX17(NCP1,1)
      NCFX18(NCP1,3) = NCFX18(NCP1,3) + NCFX18(NCP1,1)
      NCFX19(NCP1,3) = NCFX19(NCP1,3) + NCFX19(NCP1,1)
      NCFX20(NCP1,3) = NCFX20(NCP1,3) + NCFX20(NCP1,1)
      NCFX21(NCP1,3) = NCFX21(NCP1,3) + NCFX21(NCP1,1)
      NCFX22(NCP1,3) = NCFX22(NCP1,3) + NCFX22(NCP1,1)
      NCFX23(NCP1,3) = NCFX23(NCP1,3) + NCFX23(NCP1,1)
      NCFX25(NCP1,3) = NCFX25(NCP1,3) + NCFX25(NCP1,1)
      NCFX26(NCP1,3) = NCFX26(NCP1,3) + NCFX26(NCP1,1)
      NCFX27(NCP1,3) = NCFX27(NCP1,3) + NCFX27(NCP1,1)
      NCFX28(NCP1,3) = NCFX28(NCP1,3) + NCFX28(NCP1,1)
C
C     Make decision to output summary
C
      NOPRT= 0
      IF (ITEM2 .EQ. CNITR .AND. STEP2 .EQ. DAY) THEN
        BSUMM=1
        NSUMM=1
        MNTHP1=MONTH+1
        IF (JULDAY .EQ. CNDMO(LEAP,MNTHP1)) NSUMM=2
        IF (JULDAY .EQ. CNDMO(LEAP,13)) NSUMM=3
      ELSEIF (ITEM2 .EQ. CNITR .AND. STEP2 .EQ. MNTH) THEN
        BSUMM=2
        NSUMM=1
        MNTHP1=MONTH+1
        IF (JULDAY .EQ. CNDMO(LEAP,MNTHP1)) NSUMM=2
        IF (JULDAY .EQ. CNDMO(LEAP,13)) NSUMM=3
        IF (NSUMM .LT. BSUMM) NOPRT= 1
      ELSE
        BSUMM=3
        NSUMM=1
        IF (ITEM2 .NE. CNITR .OR. STEP2 .NE. YEAR) NOPRT= 1
        IF (JULDAY .EQ. CNDMO(LEAP,13)) NSUMM=3
        IF (NSUMM .LT. BSUMM) NOPRT= 1
      ENDIF
C
C     Determine if today was an output summary day for PRZM
C
      PRNTIT = (NOPRT .NE. 1) .AND. (FLPS .GT. 0)
      IF (PRNTIT) THEN
C
C       Print summaries
C
        DO 300 I=BSUMM,NSUMM
          IF (MCOFLG .EQ. 0) THEN
            IF (I .EQ. 1) WRITE(FLPS,2000) (MODID,J=1,2)
            IF (I .EQ. 2) WRITE(FLPS,2010) (MODID,J=1,2)
            IF (I .EQ. 3) WRITE(FLPS,2020) (MODID,J=1,2)
          ELSE
            IF (I .EQ. 3) WRITE(FLPS,2020) (MODID,J=1,2)
          ENDIF
          WRITE(FLPS,2030) (MODID,J=1,2),DOM,CMONTH(MONTH),IY,
     1                     (MODID,J=1,3)
          IF (MCOFLG .EQ. 0) THEN
            WRITE(FLPS,2040) (MODID,J=1,5),NCROP
            WRITE(FLPS,2050) (MODID,J=1,4)
          ENDIF
          IF (I .EQ. 1 .AND. MCOFLG .EQ. 0) THEN
            WRITE(FLPS,2060) (MODID,J=1,6),NCFX10(1,1),NCFX11(1,1),
     $        NCFX10(1,1)+NCFX11(1,1),MODID,NCFX10(2,1),NCFX11(2,1),
     $        NCFX10(2,1)+NCFX11(2,1),MODID,NCFX10(3,1),NCFX11(3,1),
     $        NCFX10(3,1)+NCFX11(3,1),(MODID,J=1,5),NCFX1(2,1),
     $        NCFX1(1,1),NCFX1(3,1),NCFX1(4,1),NCFX1(5,1),NCFX1(6,1),
     $        NCFX1(7,1)
            IF (SEPTON) THEN
C             total nitrogen inflow from septic effluent
              WRITE(FLPS,2065) (MODID,J=1,5),(DSNINF(J),J=1,3)
            END IF
            WRITE(FLPS,2070) (MODID,J=1,6)
            DO 140 J=1,NCOM2,LFREQ2
              IF (J .GE. 2) THEN
                IF (HORIZN(J) .GT. 1 .AND. HORIZN(J) .NE.
     1            HORIZN(J-1)) WRITE(FLPS,2120) MODID
              ENDIF
              WRITE(FLPS,2080) MODID,HORIZN(J),J,NCFX2(J,1),NCFX3(J,1),
     $                         NCFX21(J,1),NCFX23(J,1),NCFX7(J,1),
     $                         NCFX8(J,1),NCFX18(J,1),NCFX9(J,1)
 140        CONTINUE
            WRITE(FLPS,2100) MODID,MODID,NCFX2(NCOM2,1),NCFX3(NCP1,1),
     $                       NCFX21(NCP1,1),NCFX23(NCP1,1),
     $                       NCFX7(NCP1,1),NCFX8(NCP1,1),
     $                       NCFX18(NCP1,1),NCFX9(NCP1,1)
            WRITE(FLPS,2071) (MODID,J=1,6)
            DO 150 J=1,NCOM2,LFREQ2
              IF (J .GE. 2) THEN
                IF (HORIZN(J) .GT. 1 .AND. HORIZN(J) .NE.
     1            HORIZN(J-1)) WRITE(FLPS,2120) MODID
              ENDIF
              WRITE(FLPS,2081) MODID,HORIZN(J),J,NCFX4(J,1),NCFX5(J,1),
     $                         NCFX20(J,1),NCFX22(J,1),NCFX17(J,1),
     $                         NCFX12(J,1),NCFX6(J,1)
 150        CONTINUE
            WRITE(FLPS,2101) MODID,MODID,NCFX4(NCOM2,1),
     $                        NCFX5(NCP1,1),NCFX20(NCP1,1),
     $                        NCFX22(NCP1,1),NCFX17(NCP1,1),
     $                        NCFX12(NCP1,1),NCFX6(NCP1,1)
            WRITE(FLPS,2072) (MODID,J=1,7)
            DO 160 J=1,NCOM2,LFREQ2
              IF (J .GE. 2) THEN
                IF (HORIZN(J) .GT. 1 .AND. HORIZN(J) .NE.
     1            HORIZN(J-1)) WRITE(FLPS,2120) MODID
              ENDIF
              WRITE(FLPS,2082) MODID,HORIZN(J),J,NCFX13(J,1),
     $                         NCFX14(J,1),NCFX25(J,1),NCFX27(J,1),
     $                         NCFX15(J,1),NCFX16(J,1),NCFX26(J,1),
     $                         NCFX28(J,1),NCFX19(J,1)
 160        CONTINUE
            WRITE(FLPS,2102) MODID,MODID,NCFX13(NCOM2,1),
     $                       NCFX14(NCP1,1),NCFX25(NCP1,1),
     $                       NCFX27(NCP1,1),NCFX15(NCOM2,1),
     $                       NCFX16(NCP1,1),NCFX26(NCP1,1),
     $                       NCFX28(NCP1,1),NCFX19(NCP1,1)
          ELSEIF (I .EQ. 2 .AND. MCOFLG .EQ. 0) THEN
            WRITE(FLPS,2060) (MODID,J=1,6),NCFX10(1,2),NCFX11(1,2),
     $        NCFX10(1,2)+NCFX11(1,2),MODID,NCFX10(2,2),NCFX11(2,2),
     $        NCFX10(2,2)+NCFX11(2,2),MODID,NCFX10(3,2),NCFX11(3,2),
     $        NCFX10(3,2)+NCFX11(3,2),(MODID,J=1,5),NCFX1(2,2),
     $        NCFX1(1,2),NCFX1(3,2),NCFX1(4,2),NCFX1(5,2),NCFX1(6,2),
     $        NCFX1(7,2)
            IF (SEPTON) THEN
C             total nitrogen inflow from septic effluent
              WRITE(FLPS,2065) (MODID,J=1,5),(MSNINF(J),J=1,3)
            END IF
            WRITE(FLPS,2070) (MODID,J=1,6)
            DO 180 J=1,NCOM2,LFREQ2
              IF (J .GE. 2) THEN
                IF (HORIZN(J) .GT. 1 .AND. HORIZN(J) .NE.
     1            HORIZN(J-1))  WRITE(FLPS,2120) MODID
              ENDIF
              WRITE(FLPS,2080) MODID,HORIZN(J),J,NCFX2(J,2),NCFX3(J,2),
     $                         NCFX21(J,2),NCFX23(J,2),NCFX7(J,2),
     $                         NCFX8(J,2),NCFX18(J,2),NCFX9(J,2)
 180        CONTINUE
            WRITE(FLPS,2100) MODID,MODID,NCFX2(NCOM2,2),NCFX3(NCP1,2),
     $                       NCFX21(NCP1,2),NCFX23(NCP1,2),
     $                       NCFX7(NCP1,2),NCFX8(NCP1,2),
     $                       NCFX18(NCP1,2),NCFX9(NCP1,2)
            WRITE(FLPS,2071) (MODID,J=1,6)
            DO 190 J=1,NCOM2,LFREQ2
              IF (J .GE. 2) THEN
                IF (HORIZN(J) .GT. 1 .AND. HORIZN(J) .NE.
     1            HORIZN(J-1))  WRITE(FLPS,2120) MODID
              ENDIF
              WRITE(FLPS,2081) MODID,HORIZN(J),J,NCFX4(J,2),NCFX5(J,2),
     $                         NCFX20(J,2),NCFX22(J,2),NCFX17(J,2),
     $                         NCFX12(J,2),NCFX6(J,2)
 190        CONTINUE
            WRITE(FLPS,2101) MODID,MODID,NCFX4(NCOM2,2),
     $                        NCFX5(NCP1,2),NCFX20(NCP1,2),
     $                        NCFX22(NCP1,2),NCFX17(NCP1,2),
     $                        NCFX12(NCP1,2),NCFX6(NCP1,2)
            WRITE(FLPS,2072) (MODID,J=1,7)
            DO 200 J=1,NCOM2,LFREQ2
              IF (J .GE. 2) THEN
                IF (HORIZN(J) .GT. 1 .AND. HORIZN(J) .NE.
     1            HORIZN(J-1))  WRITE(FLPS,2120) MODID
              ENDIF
              WRITE(FLPS,2082) MODID,HORIZN(J),J,NCFX13(J,2),
     $                         NCFX14(J,2),NCFX25(J,2),NCFX27(J,2),
     $                         NCFX15(J,2),NCFX16(J,2),NCFX26(J,2),
     $                         NCFX28(J,2),NCFX19(J,2)
 200        CONTINUE
            WRITE(FLPS,2102) MODID,MODID,NCFX13(NCOM2,2),
     $                       NCFX14(NCP1,2),NCFX25(NCP1,2),
     $                       NCFX27(NCP1,2),NCFX15(NCOM2,2),
     $                       NCFX16(NCP1,2),NCFX26(NCP1,2),
     $                       NCFX28(NCP1,2),NCFX19(NCP1,2)
C
C   ADDED TO ACCUMULATE TOTAL PESTICIDE OUTFLOW FOR TABLE, ATC 8/93
C
C               IYEAR=(IY-STARTYR)+1
C               ROPST(MONTH,IYEAR,K) = MOUTP2(K)
C               LPST(MONTH,IYEAR,K)  = MOUTP9(K)
C               BPST(MONTH,IYEAR,K)  = MCOFLX(K)
C               ERPST(MONTH,IYEAR,K) = MOUTP3(K)
C
          ELSEIF (I .EQ. 3) THEN
            IF (MCOFLG .EQ. 0) THEN
              WRITE(FLPS,2060) (MODID,J=1,6),NCFX10(1,3),NCFX11(1,3),
     $          NCFX10(1,3)+NCFX11(1,3),MODID,NCFX10(2,3),NCFX11(2,3),
     $          NCFX10(2,3)+NCFX11(2,3),MODID,NCFX10(3,3),NCFX11(3,3),
     $          NCFX10(3,3)+NCFX11(3,3),(MODID,J=1,5),NCFX1(2,3),
     $          NCFX1(1,3),NCFX1(3,3),NCFX1(4,3),NCFX1(5,3),NCFX1(6,3),
     $          NCFX1(7,3)
              IF (SEPTON) THEN
C               total nitrogen inflow from septic effluent
                WRITE(FLPS,2065) (MODID,J=1,5),(YSNINF(J),J=1,3)
              END IF
              WRITE(FLPS,2070) (MODID,J=1,6)
              DO 220 J=1,NCOM2,LFREQ2
                IF (J .GE. 2) THEN
                  IF (HORIZN(J) .GT. 1 .AND. HORIZN(J)
     1               .NE. HORIZN(J-1))   WRITE(FLPS,2120) MODID
                ENDIF
              WRITE(FLPS,2080) MODID,HORIZN(J),J,NCFX2(J,3),NCFX3(J,3),
     $                         NCFX21(J,3),NCFX23(J,3),NCFX7(J,3),
     $                         NCFX8(J,3),NCFX18(J,3),NCFX9(J,3)
 220          CONTINUE
              WRITE(FLPS,2100) MODID,MODID,NCFX2(NCOM2,3),
     $                         NCFX3(NCP1,3),NCFX21(NCP1,3),
     $                         NCFX23(NCP1,3),NCFX7(NCP1,3),
     $                         NCFX8(NCP1,3),NCFX18(NCP1,3),
     $                         NCFX9(NCP1,3)
              WRITE(FLPS,2071) (MODID,J=1,6)
              DO 230 J=1,NCOM2,LFREQ2
                IF (J .GE. 2) THEN
                  IF (HORIZN(J) .GT. 1 .AND. HORIZN(J)
     1               .NE. HORIZN(J-1))   WRITE(FLPS,2120) MODID
                ENDIF
              WRITE(FLPS,2081) MODID,HORIZN(J),J,NCFX4(J,3),NCFX5(J,3),
     $                         NCFX20(J,3),NCFX22(J,3),NCFX17(J,3),
     $                         NCFX12(J,3),NCFX6(J,3)
 230          CONTINUE
              WRITE(FLPS,2101) MODID,MODID,NCFX4(NCOM2,3),
     $                          NCFX5(NCP1,3),NCFX20(NCP1,3),
     $                          NCFX22(NCP1,3),NCFX17(NCP1,3),
     $                          NCFX12(NCP1,3),NCFX6(NCP1,3)
              WRITE(FLPS,2072) (MODID,J=1,7)
              DO 240 J=1,NCOM2,LFREQ2
                IF (J .GE. 2) THEN
                  IF (HORIZN(J) .GT. 1 .AND. HORIZN(J)
     1               .NE. HORIZN(J-1))   WRITE(FLPS,2120) MODID
                ENDIF
                WRITE(FLPS,2082) MODID,HORIZN(J),J,NCFX13(J,3),
     $                           NCFX14(J,3),NCFX25(J,3),NCFX27(J,3),
     $                           NCFX15(J,3),NCFX16(J,3),NCFX26(J,3),
     $                           NCFX28(J,3),NCFX19(J,3)
 240          CONTINUE
              WRITE(FLPS,2102) MODID,MODID,NCFX13(NCOM2,3),
     $                         NCFX14(NCP1,3),NCFX25(NCP1,3),
     $                         NCFX27(NCP1,3),NCFX15(NCOM2,3),
     $                         NCFX16(NCP1,3),NCFX26(NCP1,3),
     $                         NCFX28(NCP1,3),NCFX19(NCP1,3)
            ENDIF
C            IF (MCOFLG .EQ. 0) THEN
C              WRITE(FLPS,2100) (MODID,J=1,10),YOUTP5(K),MODID,YOUTP6(K),
C     1                         MODID,YOUTP3(K),MODID,YOUTP2(K),MODID,
C     +                         YOUTP9(K),MODID,
C     2                         YINPP(K,NCOMRZ),MODID,YCOFLX(K),MODID,
C     3                         -VOUTY(K,1)
C            ELSE
C              WRITE(FLPS,2105) (MODID,J=1,7),YOUTP5(K),MODID,YOUTP6(K),
C     1                         MODID,YOUTP3(K),MODID,YOUTP2(K),MODID,
C     2                         YOUTP9(K),MODID,YCOFLX(K),MODID,
C     3                         -VOUTY(K,1)
C            ENDIF
          ENDIF
C          SUMXP= 0.0
C
C          DO 250 II=1,NCOM2
C            XP(II)= X(II)*DELX(II)*(THETN(II)+KD(K,II)*BD(II)
C     1              +(THETAS(II)-THETN(II))*KH(K,II))* 1.E5
C            SUMXP = SUMXP+ XP(II)
C 250      CONTINUE
C          IF (FMRMVL(K) .GT. 0.0) THEN
C            FMRMVX = FMRMVL(K) * 1.0E5
C            WRITE(FLPS,2104) (MODID,J=1,2),FMRMVX
C          ENDIF
C          WRITE(FLPS,2110) MODID,SUMXP
C
          WRITE(FLPS,2090) (MODID,J=1,7),PBAL(1),MODID,CPBAL(1)
 300    CONTINUE
C
C     Added by ssh, 4-89.
C
      MOCFLG=0
      ENDIF
C
      CALL SUBOUT
      RETURN
      END

