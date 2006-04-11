      SUBROUTINE GTRAIN(IDO,IGO,ILOST,M4)
C#######################################################################
C     This program reads 12 versions of precipitation files.
C     WCH, 3/2/93.  Precautionary IF statement if IHR = 25.
C     WCH, 8/93.  Add option for recent EarthInfo ASCII files in 
C       Subroutine G9RAIN and misc. corrections.
C     WCH, 8/93. Add code to ensure 15-min. data are properly placed
C       on interface file.
C     WCH, 10/11/93.  Revise input format for IFORM = 4 or 6 for 15-min.
C       rainfall.  Include calc. of JULDAY when necessary.
C     WCH, 4/25/94.  Correct computation of Julian date for 15-min. 
C       data for IFORM = 0 and 1.  
C     WCH, 4/26/94.  Major code modifications to permit averaging of
C       NCDC accumulated rainfall and to list special codes in data.
C     WCH, 5/24/94.  Fix subscript for FLAG1 for IFORM=1 and remove
C       ILOOP (no discernable purpose).  
C#######################################################################
C     IFORM = 0 NWS POST 1980 FORMAT
C     IFORM = 1 NWS POST 1980 VARIABLE FORMAT
C     IFORM = 2 NWS PRE-1980 FORMAT
C     IFORM = 3 USER-SPECIFIED INPUT FORMAT AND FILE (SUB. G3RAIN)
C     IFORM = 4 NWS MICROCOMPUTER FORMAT WITH QUOTATION MARKS (REL.B,COND.)
C     IFORM = 5 AES PRECIPITATION DATA
C     IFORM = 6 NWS MICROCOMPUTER FORMAT WITH APOSTROPHES
C     IFORM = 7 EARTH INFO NCDC  DISPLAY COMPUTER FORMAT.
C     IFORM = 8 EARTH INFO ASCII DISPLAY COMPUTER FORMAT.
C         THE FOLLOWING OPTIONS ARE IN SUBROUTINE G9RAIN:
C     IFORM = 9 EARTH INFO UNPROCESSED ASCII OUTPUT FILE - 1993, 15-MIN.
C     IFORM =10 EARTH INFO UNPROCESSED ASCII OUTPUT FILE - 1993, HOURLY.
C     IFORM =11 EARTH INFO PROCESSED ASCII FILE - 1993, 15-MIN.
C     IFORM =12 EARTH INFO PROCESSED ASCII FILE - 1993, HOURLY.
C=======================================================================
C     IDO   = 0 SEARCH FOR STATION NUMBER
C     IDO   = 1 READ STATION RAINFALL
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'PRECIP.INC'
      INTEGER DAY,YEAR,STA,XRAIN(24),IHOUR(100),VALUE(100)
      REAL    YRAIN(24)
      CHARACTER*3 XRA(12),ZIM,BIB,BLK
      CHARACTER STNAME*20,UNITS*2,CH(50)*1,FLAG1(100)*1,FLAG2(100)*1
      CHARACTER IA*1,IM*1,ICODE*1,ELMTYP*4,SQUOTE*1
C#### WCH, 10/11/93 AND 4/26/94
      CHARACTER*1 M3LINE(50),BLANK,IKODE
      DATA IM/'M'/,IA/'A'/,ZIM/'0- '/,BIB/'   '/,BLK/'-  '/
      DATA SQUOTE/''''/,BLANK/' '/
C=======================================================================
C     First find the correct Rainfall station (IDO = 0)
C=======================================================================
      IF(IFORM.EQ.4) THEN
                     M3 = NSCRAT(3)
                     IF(M3.EQ.0) CALL ERROR(104)
                     ENDIF
      IF(IDO.EQ.0)   THEN
C=======================================================================
      IF(IFORM.EQ.0) THEN
2              READ(IO,1000,ERR=2,END=40) IBUF,ELMTYP,
     +                                    NEWYR,NEWMON,NEWDAY
               IF(IBUF.NE.ISTA) GO TO 2
               IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1))  GO TO 2
               IF(NEWMON.LT.IYBEG(2).AND.
     +                            NEWYR.EQ.IYBEG(1))    GO TO 2
               IF(NEWMON.LE.IYBEG(2).AND.NEWDAY.LT.IYBEG(3)
     +                       .AND.NEWYR.EQ.IYBEG(1))    GO TO 2
               IF(ELMTYP.EQ.'QPCP') THISTO = 900.0
               IF(ELMTYP.EQ.'HPCP') THISTO = 3600.0
               ENDIF
C=======================================================================
      IF(IFORM.EQ.1) THEN
3              READ(IO,1060,ERR=3,END=40) IBUF,ELMTYP,
     +                                    NEWYR,NEWMON,NEWDAY
               IF(IBUF.NE.ISTA) GO TO 3
               IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1))  GO TO 3
               IF(NEWMON.LT.IYBEG(2).AND.
     +                            NEWYR.EQ.IYBEG(1))    GO TO 3
               IF(NEWMON.LE.IYBEG(2).AND.NEWDAY.LT.IYBEG(3)
     +                       .AND.NEWYR.EQ.IYBEG(1))    GO TO 3
               IF(ELMTYP.EQ.'QPCP') THISTO = 900.0
               IF(ELMTYP.EQ.'HPCP') THISTO = 3600.0
               ENDIF
C=======================================================================
      IF(IFORM.EQ.2) THEN
4              READ(IO,1040,ERR=4,END=40) IBUF,NEWYR,NEWMON,NEWDAY
               IF(IBUF.NE.ISTA) GO TO 4
               IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1))  GO TO 4
               IF(NEWMON.LT.IYBEG(2).AND.
     +                            NEWYR.EQ.IYBEG(1))    GO TO 4
               IF(NEWMON.LE.IYBEG(2).AND.NEWDAY.LT.IYBEG(3)
     +                       .AND.NEWYR.EQ.IYBEG(1))    GO TO 4
C#### WCH, 8/93.
               THISTO = 3600.0               
               ENDIF
C=======================================================================
      IF(IFORM.EQ.5) THEN
7              READ(IO,1045,ERR=7,END=40) IBUF,NEWYR,NEWMON,NEWDAY
               IF(IBUF.NE.ISTA) GO TO 7
               IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1))  GO TO 7
               IF(NEWMON.LT.IYBEG(2).AND.
     +                            NEWYR.EQ.IYBEG(1))    GO TO 7
               IF(NEWMON.LE.IYBEG(2).AND.NEWDAY.LT.IYBEG(3)
     +                       .AND.NEWYR.EQ.IYBEG(1))    GO TO 7
C#### WCH, 8/93.
               THISTO = 3600.0               
               ENDIF
C=======================================================================
      IF(IFORM.EQ.7) THEN
                     READ(IO,1050,END=40) YEAR
                     IF(YEAR.GT.1900) THEN
                                      YEAR = YEAR - 1900
                                      ELSE
                                      BACKSPACE IO
                                      READ(IO,1051,ERR=40,END=40) YEAR
                                      YEAR = YEAR - 1900
                                      ENDIF
                     BACKSPACE IO
                     IBUF   = ISTA
                     THISTO = 3600.0
                     NEWYR  = YEAR
                     ENDIF
C=======================================================================
      IF(IFORM.EQ.8) THEN
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1150,ERR=40,END=40) YEAR
                     BACKSPACE IO
                     IBUF   = ISTA
                     NEWYR  = YEAR
                     THISTO = 3600.0
                     ENDIF
C=======================================================================
C     Read Rainfall lines 1 to 5 for IFORM = 4 and IFORM = 6.
C=======================================================================
      IF(IFORM.EQ.4) THEN
C=======================================================================
C                    Open rainfall file
C======================================================================
C#### WCH, 8/93.  PERMIT PROCESSED NSCRAT(3) FILE TO BE SAVED.
            IF(JKP(53).NE.2.AND.M3.GT.0.AND.FFNAME(53).NE.'SCRT3.UF') 
     +      OPEN(M3,FILE=FFNAME(53),FORM='FORMATTED',STATUS='UNKNOWN')
            IF(JKP(53).NE.2.AND.M3.GT.0.AND.FFNAME(53).EQ.'SCRT3.UF')
     +      OPEN(M3,FORM='FORMATTED',STATUS='SCRATCH')
                     REWIND M3
                     WRITE(*,1090)
                     DO 2000 J = 1,1000000
                     READ(IO,1080,END=2001) CH
C=======================================================================
C                    Change double quotes " in record to single '
C                    so that character strings can be read by
C                    List-Directed I/O
C=======================================================================
                     DO 30 I = 1,50
                     IF(CH(I).EQ.'"') CH(I) = SQUOTE
   30                CONTINUE
                     WRITE(M3,1080) CH
 2000                CONTINUE
 2001                CONTINUE
                     REWIND M3
                     READ(M3,*,ERR=888) LN
                     READ(M3,*,ERR=888) LN,STNAME
                     READ(M3,*,ERR=888) LN,ISTA
                     READ(M3,*,ERR=888) LN,NEWYR,NEWMON,IEY,IEM
                     READ(M3,*,ERR=888) LN,ELMTYP
                     NEWYR = NEWYR - 1900
                     IF(ELMTYP.EQ.'QPCP') THISTO = 900.0
                     IF(ELMTYP.EQ.'HPCP') THISTO = 3600.0
                     ENDIF
      IF(IFORM.EQ.6) THEN
                     M3 = IO
                     REWIND M3
                     READ(M3,*,ERR=888) LN
                     READ(M3,*,ERR=888) LN,STNAME
                     READ(M3,*,ERR=888) LN,ISTA
                     READ(M3,*,ERR=888) LN,NEWYR,NEWMON,IEY,IEM
                     READ(M3,*,ERR=888) LN,ELMTYP
                     NEWYR = NEWYR - 1900
                     IF(ELMTYP.EQ.'QPCP') THISTO = 900.0
                     IF(ELMTYP.EQ.'HPCP') THISTO = 3600.0
                     ENDIF
C#######################################################################
C    WCH, 8/93.  CHECK FOR COMPATIBILITY OF HYETOGRAPH TIME INTERVALS.
C#######################################################################
      IF(HIST.GT.0.0) THEN
           IF(ABS(HIST-THISTO).GT.0.1) THEN
                WRITE(N6,9110) THISTO,HIST
                WRITE(*,9110)  THISTO,HIST
                STOP
                ENDIF
           ELSE
           HIST = THISTO
           ENDIF
C#### WCH, 4/26/94.
      NEND = 24
      IF(ABS(HIST-900.0).LE.0.1) NEND = 96
C=======================================================================
      ENDIF   
C=======================================================================
C====> End of IDO = 0 (Executed only once).
C=======================================================================
C     Read the rainfall values (IDO = 1)
C=======================================================================
C     CLEAR THIS YEAR'S MATRIX
C     BACKSPACE FOR A NEW YEAR OR NEW STATION
C     COMPUTE STARTING DAY NUMBER FOR THIS YEAR
C=======================================================================
      IF(IDO.EQ.1) THEN
C=======================================================================
C     IF IFORM.GE.9, DO ALL CALCULATIONS IN SUB. G9RAIN
C=======================================================================
      IF(IFORM.LT.4.OR.IFORM.EQ.5) BACKSPACE IO
      CALL SETIA(HOUR,366,27,0)
      CALL SETIA(RDAY,3000,1,0)
C#### WCH, 4/26/94.  INITIALIZE NEW VARIABLE ACODE.
      DO 8 I = 1,366
      DO 8 J = 1,4
    8 ACODE(I,J) = BLANK
      IOK    = 0
C#### WCH, 5/24/94.  DON'T SEE A REASON FOR ILOOP???
C####      ILOOP  = 0
      ILOST  = 1
      NSTORM = 0
      IDAST  = KDATE(0,1,NEWYR)
      IF(NEND.EQ.96) REWIND M4
C=======================================================================
C     Read this year's rainfall.
C=======================================================================
      IF(IFORM.EQ.7) THEN
                     READ(IO,1050,ERR=40,END=40) YEAR
                     IF(YEAR.GT.1900) THEN
                                      YEAR = YEAR - 1900
                                      ELSE
                                      BACKSPACE IO
                                      READ(IO,1051,ERR=40,END=40) YEAR
                                      YEAR = YEAR - 1900
                                      ENDIF
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     IBUF  = ISTA
                     NEWYR = YEAR
                     IDAST = KDATE(0,1,NEWYR)
                     ENDIF
      IF(IFORM.EQ.8) THEN
                     READ(IO,1150,ERR=40,END=40) YEAR
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     IBUF  = ISTA
                     NEWYR = YEAR
                     IDAST = KDATE(0,1,NEWYR)
                     ENDIF
C#### WCH, 4/26/94.  INITIALIZE VARIABLE FOR NEW KODEA OPTION.
      JDOLD = 0
C=======================================================================
10    IF(IFORM.EQ.0) READ(IO,1000,ERR=10,END=40) STA,ELMTYP,YEAR,
     *               MONTH,DAY,IHR,IRAIN,ICODE
C#### WCH, 8/93.  ZERO OUT ALL 100 CELLS OF ARRAY 'VALUE'.
      IF(IFORM.EQ.1) CALL SETIA(VALUE,100,1,0)
      IF(IFORM.EQ.1) READ(IO,1060,ERR=10,END=40) STA,ELMTYP,YEAR,
     *               MONTH,DAY,NUM,(IHOUR(I),
     *               VALUE(I),FLAG1(I),FLAG2(I),I=1,NUM)
      IF(IFORM.EQ.2) READ(IO,1040,ERR=10,END=40) STA,YEAR,MONTH,
     *               DAY,ICARD,XRA
      IF(IFORM.EQ.5) READ(IO,1045,ERR=10,END=40) STA,YEAR,MONTH,
     *               DAY,XRAIN
      IF(IFORM.EQ.7) THEN
                     READ(IO,1085,ERR=49,END=40) MONTH,DAY,
     +                                           (YRAIN(I),I=1,12)
                     READ(IO,1095,ERR=10,END=40) (YRAIN(I),I=13,24)
                     ILOST = 0
                     IF(IYBEG(1).NE.0.AND.YEAR.LT.IYBEG(1))  GO TO 10
                     IF(MONTH.LT.IYBEG(2).AND.
     +                                     YEAR.EQ.IYBEG(1)) GO TO 10
                     IF(MONTH.LE.IYBEG(2).AND.DAY.LT.IYBEG(3)
     +                                .AND.YEAR.EQ.IYBEG(1)) GO TO 10
                     STA   = ISTA
                     ILOST = 1
                     ENDIF
      IF(IFORM.EQ.8) THEN
                     READ(IO,1155,ERR=40,END=40) MONTH,DAY,XRAIN
                     STA   = ISTA
                     ILOST = 0
                     IF(MONTH.EQ.0) GO TO 10
                     IF(IYBEG(1).NE.0.AND.YEAR.LT.IYBEG(1))  GO TO 10
                     IF(MONTH.LT.IYBEG(2).AND.
     +                                     YEAR.EQ.IYBEG(1)) GO TO 10
                     IF(MONTH.LE.IYBEG(2).AND.DAY.LT.IYBEG(3)
     +                                .AND.YEAR.EQ.IYBEG(1)) GO TO 10
                     ILOST = 1
                     ENDIF
C=======================================================================
C     IFORM = 4 or 6.  Read Rainfall lines 8 and 9 (first column) 
C       for microcomputer format.
C=======================================================================
      IF(IFORM.EQ.4.OR.IFORM.EQ.6) THEN
                     READ(M3,*,END=940,ERR=950,IOSTAT=IOS) LN
                     IF(LN.EQ.10) GO TO 40
                     BACKSPACE M3
C=======================================================================
C     Here, should be reading data line LN = 8.
C=======================================================================
                     READ(M3,*,ERR=950,IOSTAT=IOS) LN,YEAR,MONTH,DAY,
     1                     UNITS,NR
                     YEAR    = YEAR - 900
                     IF(IYEND(1).NE.0.AND.YEAR.EQ.IYEND(1).AND.
     +               MONTH.GE.IYEND(2).AND.DAY.GT.IYEND(3))  GO TO 40
                     IF(IYEND(1).NE.0.AND.YEAR.EQ.IYEND(1).AND.
     +               MONTH.GT.IYEND(2))                      GO TO 40
                     IF(IYEND(1).NE.0.AND.YEAR.GT.IYEND(1))  GO TO 40
                     IF(YEAR.GT.NEWYR.AND.IOK.EQ.1) THEN
                                                    BACKSPACE M3
                                                    RETURN
                                                    ENDIF
                     IF(IYBEG(1).NE.0.AND.YEAR.LT.IYBEG(1))  GO TO 55
                     IF(MONTH.LT.IYBEG(2).AND.
     +                                     YEAR.EQ.IYBEG(1)) GO TO 55
                     IF(MONTH.LE.IYBEG(2).AND.DAY.LT.IYBEG(3)
     +                                .AND.YEAR.EQ.IYBEG(1)) GO TO 55
C#### WCH, 5/24/94.  DON'T SEE A REASON FOR ILOOP???
C####                     ILOOP   = ILOOP + 1
C####                     IF(ILOOP.EQ.1)                          GO TO 55
C=======================================================================
C     HERE, FINALLY, READ THE ACTUAL PRECIP. VALUES. (DATA LINE LN = 9)
C     NEED SEPARATE READS FOR HOURLY AND 15-MIN. DATA.
C=======================================================================
C#### WCH, 4/26/94.
C     ALLOW FOR AVERAGING OF NCDC VALUES WITH "A" (ACCUMULATED TOTALS), 
C     OR ELSE THE OPTION FOR USING THAT RAINFALL VALUE LUMPED AT THE 
C     TIME INDICATED FOR THE "A".  
C
C     KODEA = 0, OLD PROCEDURE.  DON'T INCLUDE ACCUMULATED TOTALS, BUT
C                DO KEEP A TOTAL AT END OF EVENT SUMMARY.
C     KODEA = 1, AVERAGE ACCUMULATED TOTAL OVER PRECEDING HOURS SINCE
C                LAST "A", "I", OR "M" VALUE OR FROM BEGINNING OF DAY.
C     KODEA = 1, USE ACCUMULATED TOTAL AS RAINFALL AT TIME OF "A".
C=======================================================================
C     SAVE OLD TIME FOR KODEA = 1 OPTION.
C=======================================================================
                     JD1    = JDATE(DAY,MONTH,YEAR)
                     JULDAY = JD1 + YEAR*1000
                     IHROLD = 1
                     MINOLD = 0
                     RHOURO = 0.0
                     MIN    = 0
                     DO 50 J = 1,NR
                     IF(NEND.LT.96) READ(M3,*,ERR=960,IOSTAT=IOS) 
     1                       LN,IHR,    IRAIN,ICODE
                     IF(NEND.EQ.96) READ(M3,*,ERR=960,IOSTAT=IOS) 
     1                       LN,IHR,MIN,IRAIN,ICODE
C#######################################################################
C  WCH, 3/2/93 AND 10/8/93.
C  LAST ENTRY IS TOTAL FOR DAY.  SKIP CALCULATIONS FOR THIS CASE.
C#######################################################################
                     IF(ICODE.EQ.'0') ICODE = BLANK
                     IF(J.EQ.NR) THEN
                          CALL RAINCD(ICODE,JD1)
                          GO TO 50
                          ENDIF
                     IF(IRAIN.GE.99999) THEN
                                        IRAIN = 0
                                        ICODE = IM
                                        IF(KODEA.EQ.1) THEN
C=======================================================================
C     HERE, SAVE ENDING TIME OF MISSING RAINFALL VALUE.
C=======================================================================
                                           IHROLD = IHR
                                           MINOLD = MIN
                                           RHOURO = FLOAT(IHR)*3600. +
     1                                        FLOAT(MIN)*60.
                                           ENDIF
                                        ENDIF
C#######################################################################
C#### WCH, 4/26/94.  MAKE NOTE OF SPECIAL CODES FOR EVENT PRINT-OUT.
C=======================================================================
                     CALL RAINCD(ICODE,JD1)
                     IOK   = 1
                     ILOST = 1
                     IDAY  = KDATE(DAY,MONTH,YEAR) - IDAST
C#### WCH, 8/93
                     IF(NEND.EQ.96.AND.IRAIN.GT.0) THEN
C=======================================================================
C  FOR 15-MIN. DATA, COMPUTE STARTING HOUR AND MINUTE (IHRS, 0-23 AND 
C    MIN, 0-45) AND ENDING HOUR (IHR, 1-24).  
C    RELEASE-B CONDENSED HAS HOURS GOING FROM 0-24 AND MINUTES 
C    FROM 0 - 45.  SMALLEST TIME SHOULD BE HR=0 AND 
C    MIN=15 AND LARGEST TIME SHOULD BE HR = 24 AND MIN = 0.  HOWEVER
C    PROTECT AGAINST POSSIBILITY OF HR = 0 AND MIN = 0 IN CODE AND 
C    AGAINST POSSIBILITY OF HR = 24 AND MIN > 0.
C=======================================================================
                        IHRS = IHR 
                        MIN = MIN - 15
                        IF(MIN.EQ.-15) THEN
                             MIN = 45
                             IHRS = IHRS - 1
                             ENDIF
                        IHR  = IHRS + 1
                        JPJDAY = JULDAY
C#### WCH, 10/8/93.  WORRY ABOUT RAIN ENDING EXACTLY AT MIDNITE AND IHR=0.
C#### JUST IN CASE HR=0 AND MIN=0 START SHOWING UP ON INPUT DATA:
                        IF(IHRS.EQ.-1.AND.MIN.EQ.45) THEN
                           IHR  = 1
                           IHRS = 23
                           IF(JULDAY-(JULDAY/1000)*1000.GT.1) THEN
                               JPJDAY = JPJDAY - 1
                               ELSE
                               JPJDAY = JDATE(31,12,YEAR-1) +
     1                                   (YEAR-1)*1000
                               ENDIF
                           ENDIF        
                        RHOUR = FLOAT(IHRS)*3600.0 + FLOAT(MIN)*60.0
C=======================================================================
C  IF KODEA = 1, PLACE AVERAGE ON FILE DURING INTERVENING HOURS.
C=======================================================================
                        IF(KODEA.EQ.1.AND.ICODE.EQ.IA) THEN
                           CALL RAINAVG(RHOUR,RHOURO,JPJDAY,
     1                       IDAY,IHROLD,MINOLD,NEND,IRAIN,M4)
                           ICODE = ' '
C=======================================================================
C  PLACE 15-MIN. PRECIP. VALUE ON SCRATCH FILE IN UNITS OF INCHES/HOUR.
C=======================================================================
                           ELSE
                           REIN  = FLOAT(IRAIN)/100.0/0.25
                           WRITE(M4) JPJDAY,RHOUR,THISTO,REIN
                           HOUR(IDAY,IHR) = HOUR(IDAY,IHR) + IRAIN
                           ENDIF
C
                        ELSE IF(NEND.EQ.96) THEN
                           IHR = IHR + 1                           
                           IF(KODEA.EQ.1.AND.ICODE.EQ.IA) THEN
                              RHOURO = FLOAT(IHR-1)*3600.+FLOAT(MIN)*60.
                              IHROLD = IHR
                              MINOLD = MIN
                              ENDIF
                        ENDIF
C=======================================================================
C#### WCH, 10/8/93.  RECOGNIZE POSSIBLE ERROR WHEN 15-MIN RAINFALL ENDS 
C       AT MIDNIGHT.  PREVIOUS 15-MIN INCREMENT COUNTED IN FIRST HOUR
C       OF NEXT DAY FOR PRINT-OUT OF HOURLY TOTALS, ONLY.  SHOULD BE
C       CORRECT ON PRECIPITATION INTERFACE FILE.
C=======================================================================
                     IF(IHR.GT.24) IHR = 24
                     IF(NEND.EQ.24) THEN 
                        IF(KODEA.EQ.1.AND.ICODE.EQ.IA
     1                              .AND.IRAIN.GT.0) THEN
                           RHOUR = FLOAT(IHR-1)*3600.
                           CALL RAINAVG(RHOUR,RHOURO,JPJDAY,
     1                       IDAY,IHROLD,MINOLD,NEND,IRAIN,M4)
                           ICODE = ' '
                           ELSE
                           HOUR(IDAY,IHR) = IRAIN + HOUR(IDAY,IHR)
                           ENDIF
                        IF(KODEA.EQ.1.AND.ICODE.EQ.IA.AND.
     1                                          IRAIN.EQ.0) THEN
                           RHOURO = FLOAT(IHR)*3600.
                           IHROLD = IHR
                           ENDIF
                        ENDIF
C=======================================================================
C                    Special codes for missing rainfall.
C=======================================================================
                     IF(ICODE.EQ.IA.AND.KODEA.EQ.0) THEN
                                     HOUR(IDAY,IHR) = -2
                                     SUM(1) = SUM(1) + FLOAT(IRAIN)*CONV
                                     ENDIF
                     IF(ICODE.EQ.IM) HOUR(IDAY,IHR) = -1
   50                CONTINUE
                     GO TO 52
C=======================================================================
C  Here if need to read data to advance file before arriving at 
C    desired start time.
C=======================================================================
   55                CONTINUE
                     ILOST = 0
                     IF(YEAR.GT.NEWYR) THEN
                                       NEWYR = YEAR
                                       BACKSPACE M3
                                       RETURN
                                       ENDIF
                     DO 51 J = 1,NR
C#### WCH, 10/8/93.  NEED SEPARATE READS FOR HOURLY AND 15-MIN.
                     IF(NEND.LT.96) READ(M3,*,ERR=950,IOSTAT=IOS) 
     1                    LN,IHR,IRAIN,ICODE
                     IF(NEND.EQ.96) READ(M3,*,ERR=950,IOSTAT=IOS) 
     1                    LN,IHR,MIN,IRAIN,ICODE
   51                CONTINUE
                     NEWYR = YEAR
                     IDAST = KDATE(0,1,NEWYR)
   52                CONTINUE
                     ENDIF
C=======================================================================
C     Check for same station.     Check for same year.

C     Add to this year's matrix.  Find the day number and indicate
C                                 unusual conditions.
C     Hour =  -2 means meter stuck.
C     Hour =  -1 means missing data.
C=======================================================================
      IF((IFORM.EQ.4.OR.IFORM.EQ.6).AND.IOK.EQ.0)  GO TO 10
      IF((IFORM.EQ.4.OR.IFORM.EQ.6).AND.IOK.EQ.1)  GO TO 777
      IF(STA.NE.ISTA) GO TO 40
      IF(IYEND(1).NE.0.AND.YEAR.EQ.IYEND(1).AND.MONTH.GE.IYEND(2)
     *                   .AND.DAY.GT.IYEND(3))   GO TO 40
      IF(IYEND(1).NE.0.AND.YEAR.EQ.IYEND(1).AND.MONTH.GT.IYEND(2))
     *                                           GO TO 40
      IF(IYEND(1).NE.0.AND.YEAR.GT.IYEND(1))     GO TO 40
      IF(YEAR.GT.NEWYR)                          RETURN
      IDAY = KDATE(DAY,MONTH,YEAR) - IDAST
C=======================================================================
C     Special codes for new format of rainfall.
C=======================================================================
      IF(IFORM.EQ.0) THEN
C=======================================================================
C     SAVE OLD TIME FOR KODEA = 1 OPTION.
C=======================================================================
                     JD1   = JDATE(DAY,MONTH,YEAR)
                     JDNEW = JD1 + YEAR*1000
                     IF(KODEA.EQ.1) THEN
                        IF(JDNEW.GT.JDOLD.OR.JD1.EQ.1) THEN
                           IHROLD = 1
                           MINOLD = 0
                           RHOURO = 0.0
                           JDOLD  = JDNEW
                           ENDIF
                        ENDIF
C
                     IF(IRAIN.GE.99999) THEN
                                        IRAIN = 0
                                        ICODE = IM
                                        IF(KODEA.EQ.1) THEN
C=======================================================================
C     HERE, SAVE ENDING TIME OF MISSING RAINFALL VALUE.
C=======================================================================
                                           IHROLD = IHR
                                           MINOLD = MIN
                                           RHOURO = FLOAT(IHR)*3600. +
     1                                        FLOAT(MIN)*60.
                                           ENDIF
                                       ENDIF
C#######################################################################
C#### WCH, 4/26/94.  MAKE NOTE OF SPECIAL CODES FOR EVENT PRINT-OUT.
C=======================================================================
                     CALL RAINCD(ICODE,JD1)
C#### WCH, 8/93
                     IF(NEND.EQ.96.AND.IRAIN.GT.0) THEN
C=======================================================================
C  FOR 15-MIN. DATA, COMPUTE STARTING HOUR AND MINUTE (IHRS, 0-23 AND 
C    MIN, 0-45) AND ENDING HOUR (IHR, 1-24)
C  FORM OF IHR IS HRMN WHERE HR = HOUR, FROM 0 - 24, AND MN = MIN, E.G.,
C    0045, 1530, 2300.  THIS IS END TIME OF RAINFALL VALUE.  
C=======================================================================
                        MIN  = IHR - (IHR/100)*100 - 15
                        IHRS = IHR/100
                        IF(MIN.LT.0) THEN
                           MIN  = 45
                           IHRS = IHRS - 1
                           ENDIF
                        IHR  = IHRS + 1
                        IF(IRAIN.GT.0) THEN
C#### WCH, 4/25/94.  DON'T SUBTRACT 1900 FROM YEAR.  ALREADY 2-DIGIT.  
                           JULDAY = JDATE(DAY,MONTH,YEAR) 
     1                                 + YEAR*1000
                           RHOUR = FLOAT(IHRS)*3600.0 + FLOAT(MIN)*60.0
C=======================================================================
C  IF KODEA = 1, PLACE AVERAGE ON FILE DURING INTERVENING HOURS.
C=======================================================================
                           IF(KODEA.EQ.1.AND.ICODE.EQ.IA) THEN
                              CALL RAINAVG(RHOUR,RHOURO,JULDAY,
     1                         IDAY,IHROLD,MINOLD,NEND,IRAIN,M4)
                              ICODE = ' '
                              JDOLD = JULDAY
C=======================================================================
C  PLACE 15-MIN. PRECIP. VALUE ON SCRATCH FILE IN UNITS OF INCHES/HOUR.
C=======================================================================
                              ELSE
                              REIN  = FLOAT(IRAIN)/100.0/0.25
                              WRITE(M4) JULDAY,RHOUR,THISTO,REIN
                              HOUR(IDAY,IHR) = HOUR(IDAY,IHR) + IRAIN
                              ENDIF
                           ENDIF
C
                        ELSE IF(NEND.EQ.96.AND.KODEA.EQ.1.AND.
     1                            ICODE.EQ.IA) THEN
                              IHROLD = IHR
                              MINOLD = MIN
                              RHOURO = FLOAT(IHRS)*3600.+FLOAT(MIN)*60.
                        ENDIF
C=======================================================================
C  ALLOW AVERAGING OF HOURLY RAINFALL ALSO.
C=======================================================================
                     IF(NEND.EQ.24) THEN
                        IF(KODEA.EQ.1.AND.ICODE.EQ.IA
     1                             .AND.IRAIN.GT.0) THEN
                           RHOUR = FLOAT(IHR-1)*3600.
                           CALL RAINAVG(RHOUR,RHOURO,JDNEW,
     2                          IDAY,IHROLD,MINOLD,NEND,IRAIN,M4)
                           ICODE = ' '
                           JDOLD = JDNEW
                           ELSE
                           HOUR(IDAY,IHR) = IRAIN + HOUR(IDAY,IHR)
                           ENDIF
                        IF(KODEA.EQ.1.AND.ICODE.EQ.IA) THEN
                           RHOURO = FLOAT(IHR)*3600.
                           IHROLD = IHR
                           JDOLD = JDNEW
                           ENDIF
                        ENDIF
C
                     IF(ICODE.EQ.IA.AND.KODEA.EQ.0) THEN
                                     HOUR(IDAY,IHR) = -2
                                     SUM(1) = SUM(1) + FLOAT(IRAIN)*CONV
                                     ENDIF
                     IF(ICODE.EQ.IM) HOUR(IDAY,IHR) = -1
                     ENDIF
C=======================================================================
      IF(IFORM.EQ.1) THEN
                     IF(KODEA.EQ.1) THEN
                        IHROLD = 1
                        MINOLD = 0
                        RHOURO = 0.0
                        ENDIF
                     ICODE = ' '
                     DO 8000 I = 1,NUM
                     IRAIN     = VALUE(I)
                     JD1 = JDATE(DAY,MONTH,YEAR)
                     JULDAY = JD1 + YEAR*1000
                     IF(IRAIN.GE.99999) THEN
                                        IRAIN = 0
                                        ICODE = IM
                                        IF(KODEA.EQ.1) THEN
C=======================================================================
C     HERE, SAVE ENDING TIME OF MISSING RAINFALL VALUE.
C=======================================================================
                                           IHROLD = IHR
                                           MINOLD = MIN
                                           RHOURO = FLOAT(IHR)*3600. +
     1                                        FLOAT(MIN)*60.
                                           ENDIF
                                        ENDIF
C#######################################################################
C#### WCH, 4/26/94.  MAKE NOTE OF SPECIAL CODES FOR EVENT PRINT-OUT.
C=======================================================================
                     IKODE = BLANK
                     IF(ICODE.NE.BLANK)    IKODE = ICODE
C#### WCH, 5/24/94.  CHANGE SUBCRIPT 1 TO I FOR FLAG1().
                     IF(FLAG1(I).NE.BLANK) IKODE = FLAG1(I)
                     CALL RAINCD(IKODE,JD1)
                     IF(I.EQ.NUM) GO TO 8000
C#######################################################################
C  WCH, 8/93,  Need to perform this time calculation for both 15-min.
C    and hourly data because IHOUR is of form HRMN, e.g.,
C    2330, 0015, etc.
C#######################################################################
                     IHR  = IHOUR(I)
                     MIN  = IHR - (IHR/100)*100 - 15
                     IHRS = IHR/100
                     IF(MIN.LT.0) THEN
                        MIN  = 45
                        IHRS = IHRS - 1
                        ENDIF
                     IHR  = IHRS + 1
                     IF(NEND.EQ.96.AND.IRAIN.GT.0) THEN
C#### WCH, 4/25/94.  DON'T SUBTRACT 1900 FROM YEAR.  ALREADY 2-DIGIT.  
                        RHOUR = FLOAT(IHRS)*3600.0 + FLOAT(MIN)*60.0
C=======================================================================
C  IF KODEA = 1, PLACE AVERAGE ON FILE DURING INTERVENING HOURS.
C=======================================================================
C#### WCH, 5/24/94.  CHANGE SUBCSRIPT 1 TO I FOR FLAG1().
                        IF(KODEA.EQ.1.AND.FLAG1(I).EQ.IA) THEN
                           CALL RAINAVG(RHOUR,RHOURO,JULDAY,
     1                       IDAY,IHROLD,MINOLD,NEND,IRAIN,M4)
C#### WCH, 5/24/94.  CHANGE SUBCSRIPT 1 TO I FOR FLAG1().
                           FLAG1(I) = ' '
                           JDOLD = JULDAY
C=======================================================================
C  PLACE 15-MIN. PRECIP. VALUE ON SCRATCH FILE IN UNITS OF INCHES/HOUR.
C=======================================================================
                           ELSE
                           REIN  = FLOAT(IRAIN)/100.0/0.25
                           WRITE(M4) JULDAY,RHOUR,THISTO,REIN
                           HOUR(IDAY,IHR) = HOUR(IDAY,IHR) + IRAIN
                           ENDIF
C
C#### WCH, 5/24/94.  CHANGE SUBCSRIPT 1 TO I FOR FLAG1().
                        ELSE IF(NEND.EQ.96.AND.FLAG1(I).EQ.IA.AND.
     1                       KODEA.EQ.1) THEN
                           IHROLD = IHR
                           MINOLD = MIN
                           RHOURO = FLOAT(IHRS)*3600.+FLOAT(MIN)*60.
                        ENDIF
C=======================================================================
C  ALLOW AVERAGING OF HOURLY RAINFALL ALSO.
C=======================================================================
                     IF(NEND.EQ.24) THEN
C#### WCH, 5/24/94.  CHANGE SUBCSRIPT 1 TO I FOR FLAG1().
                        IF(KODEA.EQ.1.AND.FLAG1(I).EQ.IA
     1                                 .AND.IRAIN.GT.0) THEN
                           RHOUR = FLOAT(IHR-1)*3600.
                           CALL RAINAVG(RHOUR,RHOURO,JDNEW,
     1                          IDAY,IHROLD,MINOLD,NEND,IRAIN,M4)
C#### WCH, 5/24/94.  CHANGE SUBCSRIPT 1 TO I FOR FLAG1().
                           FLAG1(I) = ' '
                           ELSE
                           HOUR(IDAY,IHR) = IRAIN + HOUR(IDAY,IHR)
                           ENDIF
C#### WCH, 5/24/94.  CHANGE SUBCSRIPT 1 TO I FOR FLAG1().
                        IF(KODEA.EQ.1.AND.FLAG1(I).EQ.IA) THEN
                           RHOURO = FLOAT(IHR)*3600.
                           IHROLD = IHR
                           ENDIF
                        ENDIF
C
                     IF(FLAG1(I).EQ.IA.AND.KODEA.EQ.0) THEN
                                     HOUR(IDAY,IHR) = -2
                                     SUM(1) = SUM(1) + FLOAT(IRAIN)*CONV
                                     ENDIF
                     IF(ICODE.EQ.IM) HOUR(IDAY,IHR) = -1
 8000                CONTINUE
                     ENDIF
C=======================================================================
C     Special codes for old format of rainfall.
C=======================================================================
      IF(IFORM.EQ.2) THEN
                     IF(ICARD.EQ.1) J1 =  0
                     IF(ICARD.EQ.2) J1 = 12
                     DO 35 J = 1,12
                     IF(XRA(J).EQ.ZIM) THEN
                                       HOUR(IDAY,J+J1) = -2
C#### WCH, 4/26/94.
                             CALL RAINCD(IA,IDAY)
                                       GO TO 35
                                       ENDIF
                     IF(XRA(J).EQ.BIB) THEN
                                       HOUR(IDAY,J+J1) = -1
C#### WCH, 4/26/94.
                             CALL RAINCD(IM,IDAY)
                                       GO TO 35
                                       ENDIF
                     IF(XRA(J).EQ.BLK) THEN
                                       HOUR(IDAY,J+J1) = 0
                                       GO TO 35
                                       ENDIF
                     HOUR(IDAY,J+J1) = INTCHR(XRA(J))
35                   CONTINUE
                     ENDIF
C=======================================================================
C     Special codes for AES Canadian data.
C=======================================================================
      IF(IFORM.EQ.5) THEN
                     DO 45 J = 1,24
                     IF(XRAIN(J).GE.9999) THEN
                             SUM(1)       = SUM(1)+FLOAT(XRAIN(J))*CONV
                             HOUR(IDAY,J) = -2
C#### WCH, 4/26/94.
                             CALL RAINCD(IA,IDAY)
                             GO TO 45
                             ENDIF
                     IF(XRAIN(J).LE.-999) THEN
                             HOUR(IDAY,J) = -1
C#### WCH, 4/26/94.
                             CALL RAINCD(IM,IDAY)
                             GO TO 45
                             ENDIF
                     HOUR(IDAY,J)         = XRAIN(J)
45                   CONTINUE
                     ENDIF
C=======================================================================
C     Earth Info precipitation data.
C     Old format, data retrieved from display.  For new ASCII files, 
C       use Subroutine G9RAIN (IFORM = 9-12).  
C=======================================================================
      IF(IFORM.EQ.7) THEN
                     DO 60 J       = 1,24
                     HOUR(IDAY,J)  = IFIX(YRAIN(J)*100.0+0.1)
60                   CONTINUE
                     ENDIF
C=======================================================================
C     Earth Info Precipitation data.
C=======================================================================
      IF(IFORM.EQ.8) THEN
                     DO 65 J       = 1,24
                     HOUR(IDAY,J)  = XRAIN(J)
65                   CONTINUE
                     ENDIF
C=======================================================================
 777  HOUR(IDAY,25) = MONTH
      HOUR(IDAY,26) =   DAY
      HOUR(IDAY,27) =  YEAR
      GO TO 10
      ENDIF
C=======================================================================
C     Skip if the new station is not the station selected.
C     Return if the new year is less than the selected ending year.
C=======================================================================
   49 RETURN
C=======================================================================
  940 WRITE(N6,9400) M3
   40 IGO = 1
      RETURN
  888 WRITE(N6,1070)
      STOP
C#######################################################################
C     WCH, 10/11/93.  Additional error messages while reading precip. 
C     file.
C=======================================================================
  950 WRITE(N6,9500) M3,LN,YEAR,MONTH,DAY,NR,IOS
      WRITE(*,9501)  M3,IOS
      GO TO 965
  960 WRITE(N6,9600) M3,LN,YEAR,MONTH,DAY,NR,J,IHR,MIN,IRAIN,IOS
      WRITE(N6,9601) M3,IOS
  965 BACKSPACE M3
      READ(M3,9650,END=966) M3LINE
  966 WRITE(N6,9651) M3,M3LINE
      STOP
C=======================================================================
 1000 FORMAT(3X,I6,2X,A4,4X,I2,I2,I4,3X,I2,3X,I5,A1)
 1040 FORMAT(I6,3I2,I1,12A3)
 1045 FORMAT(I8,3I2,3X,24I4)
 1050 FORMAT(72X,I4)
 1051 FORMAT(73X,I4)
 1060 FORMAT(3X,I6,2X,A4,4X,I2,I2,I4,I3,100(I4,I6,2A1))
 1070 FORMAT(/,' ===> Error !! in your rainfall file input.',/)
 1080 FORMAT(50A1)
 1085 FORMAT(25X,I2,1X,I2,4X,4(F6.0,F7.0,F7.0,1X))
 1095 FORMAT(25X,9X,4(F6.0,F7.0,F7.0,1X))
 1090 FORMAT(/,' Changing double quotation marks to single quotation',
     +         ' marks in input file.',/)
 1150 FORMAT(66X,I2)
 1155 FORMAT(I2,1X,I2,24I5)
 9000 FORMAT(/,' Error ==> Limit of ',I6,
     +         ' Precipitation values exceeded for this year.')
C#### WCH, 8/93.
 9110 FORMAT(/,' ===> ERROR. Time interval for new data,',F7.1,
     1 ' sec. does not',/,'     agree with time interval,',F7.1,
     2 ' sec. of data on unit JOUT with which',/,
     3 '     new data are to be combined.  Run stopped.')
C#### WCH, 10/11/93.
 9400 FORMAT(/,' WARNING! END-OF-FILE REACHED ON UNIT',I3,' WHILE READIN
     1G PRECIPITATION DATA',/,' FOR IFORM = 4 OR 6.  SHOULD HAVE A NUMBE
     2R 10 IN FIRST TWO COLUMNS',/,' TO CONCLUDE THIS FILE.  POSSIBLE ER
     3ROR.  PROCESSING CONTINUES.')
 9500 FORMAT(' ERROR WHILE READING PRECIP. DATA ON UNIT',I3,' FOR WHAT S
     1HOULD BE A',/,' LINE 8 INPUT STRING. CURRENT PARAMETER VALUES ARE:
     2',/,' LINE ID =',I3,/,' YEAR =',I5,/,' MONTH =',I3
     3,/,' DAY =',I3,/,' NO. LINES =',I3,/,' RMFORT ERROR NO. =',I5
     4,/,' RUN STOPPED FROM RAIN BLOCK.')
 9501 FORMAT(' ERROR WHILE READING PRECIP. DATA ON UNIT',I3,' FOR WHAT S
     1HOULD BE A',/,' LINE 8 INPUT STRING. RMFORT ERROR NO. =',I5,/,' SE 
     2E OUTPUT FILE FOR COMPLETE MESSAGE.',/,
     3' RUN STOPPED FROM RAIN BLOCK.')
 9600 FORMAT(' ERROR WHILE READING PRECIP. DATA ON UNIT',I3,' FOR WHAT S
     1HOULD BE A',/,' LINE 9 INPUT STRING. CURRENT PARAMETER VALUES ARE:
     2',/,' LINE ID =',I3,/,' YEAR =',I5,/,' MONTH =',I3
     3,/,' DAY =',I3,/,' NO. LINES =',I3,/,' LINE NO. =',I3
     4,/,' HOUR =',I4,/,' MIN =',I4,/,' RAIN =',I5
     5,/,' RMFORT ERROR NO. =',I5
     6,/,' RUN STOPPED FROM RAIN BLOCK.')
 9601 FORMAT(' ERROR WHILE READING PRECIP. DATA ON UNIT',I3,' FOR WHAT S
     1HOULD BE A',/,' LINE 9 INPUT STRING. RMFORT ERROR NO. =',I5,/,' SE 
     2E OUTPUT FILE FOR COMPLETE MESSAGE.',/,
     3' RUN STOPPED FROM RAIN BLOCK.')
 9650 FORMAT(50A1)
 9651 FORMAT(' OFFENDING LINE FROM UNIT',I3,' MAY BE:',/,1X,50A1)
C=======================================================================
      END
