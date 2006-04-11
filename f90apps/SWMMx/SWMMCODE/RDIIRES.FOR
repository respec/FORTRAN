      SUBROUTINE RDIIRES
C ======================================================================
C     COMPUTES RDII RESPONSE CURVES FROM RAINFALL
C     WRITES TO SCRATCH FILE(8) - NEW FOR RUNOFF
C
C     WRITTEN BY CHUCK MOORE, CDM, 8/93.
C     EDITED FOR STYLE BY WCH, 8/93
C     WCH, 1/4/94.  CORRECTIONS TO RAINFALL TOTAL AND PLACEMENT OF
C       UHs ON FILE FOR CASE WHEN RAIN ENDS BEFORE SIMULATION ENDS.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'RDII.INC'
      PARAMETER(MAXARRAY=300)  
      DIMENSION RMAX(NTK,3),QPEAK(NTK,3),TTKMAX(NTK,3),ISHIFT(NTK,3)
      DIMENSION ARRAY(NTK,3,MAXARRAY)
      DIMENSION REIN(10),JSTA(10),BAIN(10)
      DIMENSION EXCESS(NTK,3),SUMEX(NTK,3)
      DOUBLE PRECISION SUMEX
      LOGICAL  INZERO
      CHARACTER*10 PRT(4,2)
      DATA PRT /'cfs/ac-in ','in','in/day','ac',
     1          'cms/ha-mm ','mm','mm/day','ha'/
C=======================================================================
C  FIND MAXIMUM R VALUES
C=======================================================================
      INZERO = .TRUE.
      IULDAY  = JULDAY
      STIMDAY = TIMDAY
      TIME    = TZERO
      NNZERO  = 0
C#### WCH, 1/4/94.  NEW VARIABLE IFEND.
      IFEND   = 0
      DO 10 J = 1,NTK
      DO 10 I = 1,3
      SUMEX(J,I)   = 0.0
      STORAGE(J,I) = AMIN1(DSTORE(J,I),STORAGE(J,I))
      STORAGE(J,I) = AMAX1(0.0,STORAGE(J,I))
      DREC(J,I)    = DREC(J,I) / (24.0*60.0*60.0)
      TTKMAX(J,I)  = 0.0
      RMAX(J,I)    = 0.0
      DO 10 K = 1,2
   10 IZERO(J,I,K) = .FALSE.
      RRMAX        = 0.0
      RDIIAREA     = 0.0
      DO 21 J=1,NOW
      IC = ICURVE(J)
      IF(IC.GT.0) THEN 
           RDIIAREA = RDIIAREA + SEWAREA(J)
           DO 20 I=1,3
           RRMAX = AMAX1(RDIIR(J,I),RRMAX)
   20      RMAX(IC,I) = AMAX1(RDIIR(J,I),RMAX(IC,I))
           ENDIF
   21 CONTINUE
C=======================================================================
C  RRMAX IS INDICATOR FOR WHETHER I/I CALCULATIONS ARE PERFORMED.
C=======================================================================
      IF(RRMAX.EQ.0.0) RETURN
C=======================================================================
      WRITE(*,*)
      WRITE(*,*)  ' Computing RDII response curve from rainfall data.'
      WRITE(*,*)
C=======================================================================
C  CHECK FOR EXISTANCE OF SCRATCH FILE 8
C=======================================================================
      NRDII = NSCRAT(8)
      IF(NRDII.EQ.0) THEN 
           WRITE(N6,*) ' SCRATCH UNIT NUMBER 8 (NSCRAT(8))',
     1 ' MUST BE NON-ZERO TO PERFORM RDII CALCULATIONS'
           WRITE(*,*) ' SCRATCH UNIT NUMBER 8 (NSCRAT(8))',
     1 ' MUST BE NON-ZERO TO PERFORM RDII CALCULATIONS'
           STOP
           ENDIF 
C=======================================================================
      TKMAX = 0.0
      TMIN  = 999.0
      DO 25 J=1,NNTK
      DO 25 I=1,3
      IF(RDIIT(J,I).GT.0.0) TMIN = AMIN1(RDIIT(J,I),TMIN)
      TTKMAX(J,I) = AMAX1(RDIIT(J,I)*(1.0+RDIIK(J,I)),TTKMAX(J,I))
   25 TKMAX = AMAX1(RDIIT(J,I)*(1.0+RDIIK(J,I)),TKMAX)
C
      TSTEPS  = TSTEP*60.0*60.0
      TSTEP2S = TSTEP2*60.0*60.0
      WRITE(N6,6010) TSTEP,TSTEP2
C=======================================================================
C  CHECK MAXARRAY
C=======================================================================
      NARRAY = INT(TKMAX/TSTEP2)+IRATIOS+1
      DO 32 J=1,NNTK
      DO 32 I=1,3
      ISHIFT(J,I) = INT(TTKMAX(J,I)/TSTEP2)+IRATIOS+1
   32 CONTINUE
      IF(NARRAY.GT.MAXARRAY) THEN
           WRITE(N6,6000) NARRAY
           STOP
           ENDIF
      DO 35 K = 1,NNTK
      DO 35 I = 1,3
      DO 35 M = 1,MAXARRAY
   35 ARRAY(K,I,M) = 0.0
C=======================================================================
C  COMPUTE QPEAK/(AIR)  THIS IS UNIT QPEAK TO BE MULTIPLIED LATER BY 
C    AREA, RAINFALL, AND R FACTOR TO COMPUTE PEAK FLOW
C=======================================================================
      DO 40 J = 1,NNTK
      DO 40 I = 1,3
C=======================================================================
C  RAIN (INCHES)  AREA (ACRES)  T (HOURS)  Q (CFS)
C  AREA OF TRIANGULAR UH IS 1 AC-IN. = 1/12 AC-FT.
C=======================================================================
      IF(RDIIT(J,I).GT.0.0) THEN
           QPEAK(J,I) = (2.0*43560.0/12.0)/
     +       (RDIIT(J,I)*(1.0+RDIIK(J,I))*60.0*60.0)
           ELSE
           IF(RMAX(J,I).NE.0.0) THEN
                WRITE(N6,*) ' T CANNOT EQUAL ZERO WHEN R IS NON-ZERO'
                WRITE(N6,*) ' RESPONSE CURVE = ',J,' TRIANGLE = ',I
                STOP
                ENDIF 
           ENDIF
   40 CONTINUE
C=======================================================================
C  PRINT DATA FROM F4 LINES.
C=======================================================================
      WRITE(N6,6016) PRT(1,METRIC),PRT(2,METRIC),PRT(2,METRIC),
     1               PRT(3,METRIC)
      WRITE(N6,6017) PRT(1,METRIC),PRT(2,METRIC),PRT(2,METRIC),
     1               PRT(3,METRIC)
      DO 50 J = 1,NNTK
      DO 50 I = 1,3
      QPP = QPEAK(J,I)
C=======================================================================
C  CFS/AC-IN / 360 = CMS/HA-MM
C=======================================================================
      IF(METRIC.EQ.2) QPP = QPP/360.0
      IF(I.EQ.1) WRITE(N6,6019) J,NRDHYET(J),I,RDIIT(J,I),RDIIK(J,I),
     1 TTKMAX(J,I),QPP,DSTORE(J,I),STORAGE(J,I),DREC(J,I)*60.0*60.0*24.
      IF(I.GT.1) WRITE(N6,6020) I,RDIIT(J,I),RDIIK(J,I),TTKMAX(J,I),
     1 QPP,DSTORE(J,I),STORAGE(J,I),DREC(J,I)*60.0*60.0*24.0
C=======================================================================
C  CONVERT DEPTH PARAMETERS FROM METRIC (MM) TO U.S. (IN.)
C=======================================================================
      IF(METRIC.EQ.2) THEN
           DSTORE(J,I)  = DSTORE(J,I)/25.4
           STORAGE(J,I) = STORAGE(J,I)/25.4
           DREC(J,I)    = DREC(J,I)/25.4
           ENDIF
   50 CONTINUE
C=======================================================================
C  PRINT SUBCATCHMENT DATA FROM H5 LINES.
C=======================================================================
      WRITE(N6,7000) PRT(4,METRIC)
      DO 60 J = 1,NOW
      IC = ICURVE(J)
      IF(IC.GT.0) THEN 
           SEWW = SEWAREA(J)
           IF(METRIC.EQ.2) SEWW = SEWW/2.471
           IF(JCE.EQ.0) WRITE(N6,7010) J,NAMEW(J),IC,SEWW,
     1                                 (RDIIR(J,I),I=1,3)
           IF(JCE.EQ.1) WRITE(N6,7011) J,KAMEW(J),IC,SEWW,
     1                                 (RDIIR(J,I),I=1,3)
           ENDIF
   60 CONTINUE
      SEWW = RDIIAREA
      IF(METRIC.EQ.2) SEWW = SEWW/2.471
      WRITE(N6,7015) SEWW,PRT(4,METRIC)
C=======================================================================
C  IF IIRDII = 1, USE PREVIOUSLY CALCULATED RESPONSE CURVE.
C  SKIP UNIT HYDROGRAPH CALCULATIONS.
C=======================================================================
      IF(IIRDII.EQ.1) THEN
           WRITE(N6,7050) NRDII,FFNAME(58)
           GO TO 210
           ENDIF
C=======================================================================
C  READ RAINFALL TAPE
C=======================================================================
      NREIN = NSCRAT(1)
      REWIND NREIN
      READ(NREIN,END=777,IOSTAT=IOSS,ERR=777) NSTA,MRAIN,
     1              (JSTA(I),I=1,NSTA)
      WRITE(N6,2115)  NSTA
      WRITE(N6,2120) (I,JSTA(I),I=1,NSTA)
      READ(NREIN,END=210,IOSTAT=IOSS,ERR=777) JDAY,TMDAY,THISTO,
     1                                    (BAIN(J),J=1,NSTA)
      IF(THISTO.NE.TSTEPS) THEN
           WRITE(N6,*) ' ERROR - RAINFALL TIME STEP IN RAINFALL FILE ',
     1       'DOES NOT EQUAL RAINFALL TIME STEP SET ON F3 CARD'
           STOP
           ENDIF
      JOLDAY  = JDAY
      TOMDAY  = TMDAY
      JODAY   = JDAY
      TODAY   = TMDAY
      TOHISTO = THISTO
      DO  99 J = 1,NSTA
   99 REIN(J)  = BAIN(J)
C#### WCH, 1/3/94.  DON'T BACKSPACE OR GET EXTRA RAIN AFTER FIRST 
C                   TIME STEP.
C####      BACKSPACE(NREIN)
      JULDAY = JDAY
      TIMDAY = TMDAY
      CALL DATED
      WRITE(N6,272) MONTH,NDAY,NYEAR,JULDAY,TIMDAY,TIMDAY/3600.0
      REWIND NRDII
C=======================================================================
C  BEGIN LOOPING OVER RAINFALL USING TIME STEP EQUAL TO RAINFALL STEP
C=======================================================================
      CNRAIN = 0.0
  100 CONTINUE
      IF(JULDAY.GT.JDAY.OR.(JULDAY.EQ.JDAY.AND.TIMDAY.GE.TMDAY)) THEN
           JOLDAY  = JDAY
           TOMDAY  = TMDAY
           TOHISTO = THISTO
           DO 101 J = 1,NSTA
  101      REIN(J)  = BAIN(J)
C#### WCH, 1/4/94.  NEW END= LOCATION (151, NOT 210).
           READ(NREIN,END=151,IOSTAT=IOSS,ERR=777) JDAY,TMDAY,THISTO,
     +                                    (BAIN(J),J=1,NSTA)
           IF(ABS(THISTO-TSTEPS).GT.0.001) THEN
                WRITE(N6,*) ' ERROR - RAINFALL TIME STEP IN RAINFALL ',
     1          'FILE DOES NOT EQUAL RAINFALL TIME STEP SET ON F3 CARD'
                STOP
                ENDIF
           WRITE(*,104) JDAY
           ELSE 
           CALL NTIME(JOLDAY,TOMDAY,DELTA)
           IF(ABS(DELTA).GT.(1.0-1.0/FLOAT(IRATIOS))*TOHISTO) THEN
                DO 105 J = 1,NSTA
  105           REIN(J) = 0.0
                ENDIF
           ENDIF
C=======================================================================
C COMPUTE RAINFALL EXCESS AMOUNTS AND ADJUST INITIAL ABSTRACTION STORAGE
C=======================================================================
      DO 550 J = 1,NNTK
      IF(NRDHYET(J).GT.0) THEN
           IF(REIN(NRDHYET(J)).GT.0.0) THEN
                RAINFALL = REIN(NRDHYET(J))*TSTEP
                CNRAIN   = CNRAIN + RAINFALL
                DO 540 I = 1,3
                EXCESS(J,I) = RAINFALL-STORAGE(J,I)
                EXCESS(J,I) = AMAX1(EXCESS(J,I),0.0)
                STORAGE(J,I) = STORAGE(J,I)-(RAINFALL-EXCESS(J,I))
                SUMEX(J,I) = SUMEX(J,I) + EXCESS(J,I)
  540           CONTINUE
                ELSE
C=======================================================================
C  STORAGE RECOVERS IF NOT RAINING
C=======================================================================
                DO 545 I = 1,3
                EXCESS(J,I) = 0.0
                STORAGE(J,I) = STORAGE(J,I)+DREC(J,I)*TSTEPS
                STORAGE(J,I) = AMIN1(STORAGE(J,I),DSTORE(J,I))
  545           CONTINUE
                ENDIF
           ENDIF
  550 CONTINUE
C=======================================================================
C  LOOP OF TSTEP2
C  LOOP OVER UNIT HYDROGRAPHS TO COMPUTE ARRAY VALUES
C=======================================================================
      DO 150 J=1,NNTK
C=======================================================================
C  LOOP OVER TRIANGLES
C=======================================================================
      DO 140 I = 1,3
      IF(EXCESS(J,I).GT.0.0) THEN 
           IF(RMAX(J,I).GT.0.0) THEN
C======================================================================
C  LOOP OVER TIMESTEPS TO FILL ARRAY
C=======================================================================
                TRDII = 0.0
                DO 139 IT = 2,ISHIFT(J,I)
                TRDII = TRDII + TSTEP2
                IF(TRDII.LE.RDIIT(J,I)) THEN
                     FLOW = QPEAK(J,I)*TRDII/RDIIT(J,I)*EXCESS(J,I)
                     ELSE
                     RRATIO = 1.0 - (TRDII-RDIIT(J,I))/
     +                              (RDIIT(J,I)*RDIIK(J,I))
                     RRATIO = AMAX1(RRATIO,0.0)
                     FLOW = QPEAK(J,I)*RRATIO*EXCESS(J,I)
                     ENDIF 
                ARRAY(J,I,IT) = ARRAY(J,I,IT)+FLOW
                IF(FLOW.LE.0.0) GO TO 140
  139           CONTINUE
                ENDIF  
           ENDIF 
  140 CONTINUE
  150 CONTINUE
C#######################################################################
C#### WCH, 1/4/93.  
C  PROVIDE NEW CODE FOR SITUATION IN WHICH END OF RAINFALL IS REACHED
C  WELL BEFORE END OF SIMULATION.
C=======================================================================
      GO TO 153
  151 IFEND = 1
      IF(TIME.GE.(LONG+TZERO)) GO TO 210
  153 DO 200 JJJ = 1,IRATIOS
C=======================================================================
C  WRITE THIS FLOW TO FILE  (MOSTLY, WRITE JUST NON-ZERO VALUES).
C  USE ARRAY IZERO TO INDICATE PRESENCE OF NON-ZERO I/I EXCESS VALUE.
C  IZERO = .TRUE. ==> NON-ZERO EXCESS VALUE TO WRITE.
C  IZERO = .FALSE. => DON'T WRITE ZERO VALUE OF ARRAY().
C=======================================================================
      IF(NNZERO.GT.0) THEN
           IF(INZERO)     
     +        WRITE(NRDII) JODAY,TODAY,((IZERO(J,I,2),I=1,3),J=1,NTK)
           INZERO = .FALSE.
           WRITE(NRDII) JULDAY,TIMDAY,((IZERO(J,I,1),I=1,3),J=1,NTK)
           ELSE 
C=======================================================================
C  WRITE FIRST ZERO VALUE
C=======================================================================
           IF(.NOT.INZERO) THEN
             WRITE(NRDII) JULDAY,TIMDAY,((IZERO(J,I,1),I=1,3),J=1,NTK)
             INZERO = .TRUE.
             ENDIF
           ENDIF
C=======================================================================
C  SHIFT ARRAYS
C=======================================================================
C#### WCH, 1/4/93.  MOVE TIME CHANGES TO AFTER 160 LOOP FOR CONVENIENCE
C                   ONLY.
C
      NNZERO = 0
      DO 160 J = 1,NNTK
      DO 160 I = 1,3
C=======================================================================
C  ONLY STORE NON-ZERO I/I EXCESS VALUES (IZERO = .TRUE.).
C=======================================================================
      IF(IZERO(J,I,1)) WRITE(NRDII) ARRAY(J,I,1)
      DO 155 IT = 1,ISHIFT(J,I)-1
  155 ARRAY(J,I,IT) = ARRAY(J,I,IT+1)
      IZERO(J,I,2)  = IZERO(J,I,1)
      IZERO(J,I,1)  = (ARRAY(J,I,1).NE.0.0)
      IF(IZERO(J,I,1)) NNZERO = NNZERO+1
  160 ARRAY(J,I,ISHIFT(J,I)) = 0.0
C#### WCH, 1/4/94.  TIME CHANGES MOVED TO HERE.
      JODAY = JULDAY
      TODAY = TIMDAY
      CALL STIME(TSTEP2S)
      TIME   = TIME+TSTEP2S
  200 CONTINUE
C#######################################################################
C#### WCH, 1/4/94
C  CHECK TO BE SURE ARRAY() IS "FLUSHED" OF ALL NON-ZERO VALUES  AND
C  A LAST ZERO STORED ON FILE BEFORE QUITTING.
C=======================================================================
      IF(IFEND.EQ.1.AND.NNZERO.GT.0) GO TO 151
      IF(IFEND.EQ.1.AND.NNZERO.LE.0) THEN
           IFEND = 2
           GO TO 153
           ENDIF
      IF(IFEND.EQ.2) GO TO 210
C=======================================================================
C  READ NEXT RAINFALL
C=======================================================================
      IF(TIME.LT.(LONG+TZERO)) GO TO 100
  210 CONTINUE
C=======================================================================
C  APPROXIMATE AVERAGE RAIN BY DIVIDING TOTAL BY NUMBER OF UH SETS.
C=======================================================================
      CNRAIN = CNRAIN/DBLE(NNTK)
      JULDAY = IULDAY
      TIMDAY = STIMDAY
      CALL DATED
      REWIND NRDII
C=======================================================================
C  READ FIRST TWO VALUES SETTING UP INPUT FILE FOR RDIISHED
C=======================================================================
      CALL READNEXT
      CALL READNEXT
      DO 300 I = 1,3
 300  CNTRDII(I) = 0.0
      SUMRS = 0.0
      CNEXCESS = 0.0
      DO 400 JJ = 1,NOW
      J = ICURVE(JJ)
      IF(J.NE.0) THEN
           DO 350 I = 1,3
           SUMRS = SUMRS+SEWAREA(JJ)*RDIIR(JJ,I)
 350       CNEXCESS = CNEXCESS + SUMEX(J,I)*SEWAREA(JJ)*RDIIR(JJ,I)
           ENDIF
 400  CONTINUE
      CNEXCESS = CNEXCESS/SUMRS
      RETURN
C=======================================================================
 777  WRITE(N6,9777) IOSS
      STOP
C=======================================================================
  104 FORMAT(1H+,' Reading rainfall Julian day',I8)
  272 FORMAT(/,
     1' ************************************************',/,
     2' *      INITIAL RAINFALL RECORD                 *',/,
     3' ************************************************',//,
     4' Date (Mo/Day/Year)   = ',T50,I2,'/',I2,'/',I2,/,
     6' Julian Date  =',T45,5X,I8,/,
     7' Time of day  =',T45,F13.0,' seconds.',/,
     8' Time of day  =',T45,F13.2,'   hours.',/)
 2115 FORMAT(//,
     +' ***************************************************',/,
     +' *  Precipitation Interface File Summary           *',/,
     +' *  Number of precipitation station....',I9,'   *',/,
     +' ***************************************************',/)
 2120 FORMAT(' Location Station Number',/,
     +       ' -------- --------------',/,
     +       10(I9,'. ',I13,/))
 6000 FORMAT(' ERROR - ARRAY DIMENSIONS IN RDIIRES NOT SUFFICIENT TO',
     1' SIMULATE COMPLETE HYDROGRAPH.',/,' EITHER DECREASE TP(1+K) OR ',
     2'CHANGE PARAMETER STATEMENT IN RDIIRES.',/,
     3' DIMENSION REQUIRED FOR THIS SIMULATION = ',I5)
 6010 FORMAT('1 RAINFALL DEPENDENT INFILTRATION/INFLOW (RDII) CALCULATIO
     1NS',/,' DATA FROM F3 AND F4 LINES FOR COMPUTATION OF UNIT HYDROGRA
     2PH RESPONSE:',//
     3 '  TSTEP  = RAINFALL INCREMENT (MUST = HYETOGRAPH TIME INTERVAL,'
     4,/,12X,'THISTO)   =',F10.3,' HOURS',/,
     5 '  TSTEP2 = TIME STEP FOR UH RESPONSE (MUST BE INTEGER FRACTION'
     6,/,12X,'OF TSTEP) =',F10.3,' HOURS')
 6015 FORMAT(//,' LINE F4 DATA INPUTS FOR TRIANGULAR UNIT HYDROGRAPH NUM
     1BER ',I2)
 6016 FORMAT(/,
     1'  Qpeak   = Peak RDII flow produced by 1 unit of rainfall excess 
     2',/,13X,'over 1 area unit, ',A10,'.',/,
     3'  DSTORE  = Maximum depression storage to be satified before I/I 
     4begins, ',A2,'.',/,
     5'  STORAGE = Initial depression storage, ',A2,'.',/,
     6'  DREC    = DSTORE recovery rate when not raining, ',A6,'.',/,
     7'  TP      = Time to peak, hr.',/,
     8'  K       = Ratio, recession time to TP.',/,
     9'  TB      = Time base = TP*(1+K), hr.')
 6017 FORMAT(/,
     1'   UH   HYET  TRIANG. TP     K      TB    Qpeak  DSTORE  STORAGE 
     2  DREC',/,
C                                              cfs/ac-in  in       in    in/day
     3' SET NO. NO.    NO.   hr            hr  ',A10,1X,A2,7X,A2,4X,
     4 A6,/,
     5' ------ ----  ----- ------ ------ ------ -------- ------- -------
     6 -------')
C          1     1      2   3.00   2.00   9.00  33.86877  0.275   0.376   0.084
 6019 FORMAT(/,2I5,I7,3F7.2,F10.5,F7.3,2F8.3)
 6020 FORMAT(I17,3F7.2,F10.5,F7.3,2F8.3)
C
 7000 FORMAT(//,' SUBCATCHMENT PARAMETERS FROM H5 DATA LINES',//,
     1' SUBCATCHMENT  USE  SEWERED  RATIOS FOR TRIANGLE NOS.',/,
     2' SEQ.     ID    UH    AREA        1       2       3',/,
     3'  NO.     NO.   NO.    ',A2,/,
     4'----- ------- ----- --------  ------- ------- -------')
C         4     977     3   4568.93   0.3333  0.3333  0.3333
 7010 FORMAT(I4,I8,I6,F10.2,F9.4,2F8.4)
 7011 FORMAT(I4,A8,I6,F10.2,F9.4,2F8.4)
 7015 FORMAT(19X,'---------',/,' TOT. SEWERED AREA =',F8.2,1X,A2)
 7050 FORMAT(/,' PARAMETER IIRDII = 1.  SKIP UH RESPONSE CURVE CALCULATI
     1ONS.',/,' USE RESPONSE HYDROGRAPH STORED ON UNIT',I3,', DOS FILE '
     2,A10,/,' DATES ON FILE MUST OVERLAP DATES REQUIRED FOR THIS SIMULA
     3TION')
 9777 FORMAT(/,' ===> Error !!  Reading precipitation time history',
     1' on the NSCRAT(1) file.',/,' IOSTAT =',I5,' Run stopped from Subr
     2outine RDIIRES')
      END
