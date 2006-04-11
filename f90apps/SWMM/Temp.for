      SUBROUTINE TEMP
C	TEMP BLOCK
C=======================================================================
C     CREATED APRIL, 1988 BY BOB DICKINSON
C     UPDATED 11/92 BY WCH TO CORRECT FEBRUARY LENGTH CALCULATION
C       AND TO CORRECT STATEMENT WITH EVAP. PAN COEF. MULTIPLICATION
C     UPDATED 4/93 BY WCH TO READ PAN COEFS FOR KTYPE = 3 AND 6.  
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TEW.INC'
      DIMENSION TMEAN(12),TMIN(12),TMAX(12)
      CHARACTER*10 MMNTH(12)
      DATA MMNTH/'JANUARY','FEBRUARY','MARCH','APRIL',
     +           'MAY','JUNE','JULY','AUGUST','SEPTEMBER',
     +           'OCTOBER','NOVEMBER','DECEMBER'/
C=======================================================================
      INCNT  = INCNT + 1
      IOUTCT = IOUTCT + 1
      IO     = JIN(INCNT)
      NEXT   = JOUT(IOUTCT)
      IGO    = 0
C=======================================================================
      IF(JIN(INCNT).GT.0.AND.(FFNAME(INCNT).EQ.'JOT.UF'.OR.
     +      FFNAME(INCNT).EQ.'JIN.UF'))
     +      OPEN(JIN(INCNT),FORM='FORMATTED',STATUS='SCRATCH')
      IF(JIN(INCNT).GT.0.AND.FFNAME(INCNT).NE.'JOT.UF'.AND.
     +      FFNAME(INCNT).NE.'JIN.UF')
     +      OPEN(JIN(INCNT),FILE=FFNAME(INCNT),FORM='FORMATTED',
     +      STATUS='UNKNOWN')
      IF(JOUT(IOUTCT).GT.0.AND.(FFNAME(25+IOUTCT).EQ.'JOT.UF'.OR.
     +      FFNAME(25+IOUTCT).EQ.'JIN.UF'))
     +      OPEN(JOUT(IOUTCT),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JOUT(IOUTCT).GT.0.AND.FFNAME(25+IOUTCT).NE.'JOT.UF'.AND.
     +      FFNAME(25+IOUTCT).NE.'JIN.UF')
     +      OPEN(JOUT(IOUTCT),FILE=FFNAME(25+IOUTCT),FORM='UNFORMATTED',
     +      STATUS='UNKNOWN')
C=======================================================================
C     OPEN STATEMENT FOR VAX COMPUTERS
C=======================================================================
C     OPEN(IO,FILE=FFNAME(INCNT),FORM='FORMATTED',
C    +                           STATUS='UNKNOWN',RECL=350)
      REWIND IO
      WRITE(N6,1000)
      WRITE(*,1000)
C=======================================================================
      DO 10 J = 1,12
  10  PAN(J)  = 0.70
C=======================================================================
C >>>>>>>>>>>>>> READ DATA GROUP A1 <<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,TITLE(1)
      READ(N5,*,ERR=888) CC,TITLE(2)
      WRITE(N6,68) TITLE(1),TITLE(2)
C=======================================================================
C >>>>>>>>>>>>>> READ DATA GROUP B1 <<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,IFORM,ISTA,KTYPE,JYBEG,JYEND,IYEAR
C#### WCH, 4/3/93.  ADD KTYPE = 3 AND 6.
      IF(KTYPE.EQ.1.OR.KTYPE.EQ.3.OR.KTYPE.EQ.6) THEN
                      BACKSPACE N5
                      READ(N5,*,ERR=888) CC,IFORM,ISTA,KTYPE,JYBEG,
     +                         JYEND,IYEAR,PAN
                      ENDIF
C=======================================================================
C FOR DATES, READ YR-MO-DAY AS 8 DIGIT NUMBER.
C (1) = YEAR, (2) = MONTH, (3) = DAY
C=======================================================================
      JYEAR = JYBEG/10000
      IF ((JYBEG.GT.0).AND.JYEAR.LT.100) THEN
      JYBEG = JYBEG - JYEAR*10000
      JYEAR = JYEAR + 1900
      JYBEG = JYBEG + JYEAR*10000
      ENDIF
      JYEAR = JYEND/10000
      IF ((JYEND.GT.0).AND.JYEAR.LT.100) THEN
      JYEND = JYEND - JYEAR*10000
      JYEAR = JYEAR + 1900
      JYEND = JYEND + JYEAR*10000
      ENDIF
      IYBEG(1) = JYBEG/10000
      IYBEG(2) = (JYBEG - IYBEG(1)*10000)/100
      IYEND(1) = JYEND/10000
      IYEND(2) = (JYEND - IYEND(1)*10000)/100
      IYEND(3) = JYEND - IYEND(1)*10000 - IYEND(2)*100
      IYBEG(3) = JYBEG - IYBEG(1)*10000 - IYBEG(2)*100
      WRITE(N6,1021) ISTA,IYBEG,IYEND,IFORM,KTYPE,IYEAR
      WRITE(NEXT) ISTA
      IF(KTYPE.EQ.1) WRITE(N6,1022) PAN
C#### WCH, 12/92.  IFORM = 1 NOT IMPLEMENTED.
      IF(IFORM.EQ.1) THEN
        WRITE(N6,2100) 
        WRITE(*,2100)
        STOP
      ENDIF
C#### WCH, 12/92.  PRINT WARNING IF START/STOP DATES = 0.
      IF(JYBEG.EQ.0.OR.JYEND.EQ.0) WRITE(N6,2150)
C=======================================================================
C >>>>>>>>>>>>>> READ DATA GROUP B2 <<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      IF(IFORM.EQ.2) THEN
                     IF(KTYPE.EQ.0) READ(N5,*,ERR=888) CC,FIRMAT,
     +                        METRIC,KUNIT,CONV,F1,F2,F3,F4,F5,F6
                     IF(KTYPE.GE.1) READ(N5,*,ERR=888) CC,FIRMAT,
     +                        METRIC,KUNIT,CONV,F1,F2,F3,F4,F5
                     WRITE(N6,1030) FIRMAT,METRIC,KUNIT,CONV,
     +                              F1,F2,F3,F4,F5,F6
                     ENDIF
C=======================================================================
      CALL GTCOLD(0,IGO)
    7 WRITE(*,911) NEWYR
      CALL GTCOLD(1,IGO)
C=======================================================================
C=======================================================================
      DO 375 K = 1,12
      TMIN(K)  = -99.0
      TMAX(K)  = -99.0
      TMEAN(K) = -99.0
  375 CONTINUE
      IF(KTYPE.EQ.0.OR.KTYPE.EQ.3.OR.KTYPE.EQ.4.OR.KTYPE.EQ.6) THEN
      KWRITE   = 0
      DO 400 K = 1,12
                                                KMONTH = 31
      IF(K.EQ.9.OR.K.EQ.4.OR.K.EQ.6.OR.K.EQ.11) KMONTH = 30
C ### WCH, 11/92.  REVERSE FEBRUARY CALCULATION
      IF(K.EQ.2.AND.MOD(NEWYR,4).EQ.0)         KMONTH = 29
      IF(K.EQ.2.AND.MOD(NEWYR,4).NE.0)         KMONTH = 28
      IF(BARAY(K,1).EQ.-99.0.AND.BARAY(K,KMONTH).EQ.-99.0.AND.
     +   SARAY(K,1).EQ.-99.0.AND.SARAY(K,KMONTH).EQ.-99.0) GO TO 400
      KWRITE  = KWRITE + 1
      IF(KWRITE.EQ.1) WRITE(N6,970) TITLE(1),TITLE(2)
      DO 450 J = 1,31
      IF(SARAY(K,J).EQ.-99) SARAY(K,J) = BARAY(K,J)
  450 IF(BARAY(K,J).EQ.-99) BARAY(K,J) = SARAY(K,J)
C#######################################################################
C WCH, 12/92. MODIFY PRINT AND STATISTICS FOR ARBITRARY START/STOP DATES
C#######################################################################
         IDB = 1
         IF(IYBEG(1).EQ.NEWYR.AND.K.EQ.IYBEG(2)) IDB = IYBEG(3)
         IDS = KMONTH
         IF(IYEND(1).EQ.NEWYR.AND.K.EQ.IYEND(2)) IDS = IYEND(3)
      IF(IYEAR.GT.0) WRITE(N6,990) (K,J,NEWYR,BARAY(K,J),
     +                              SARAY(K,J),J=IDB,IDS)
      TMIN(K)  =  200.0
      TMAX(K)  = -100.0
      TMEAN(K) =    0.0
      DO 475 J = IDB,IDS
      IF(SARAY(K,J).LT.TMIN(K)) TMIN(K) = SARAY(K,J)
      IF(BARAY(K,J).GT.TMAX(K)) TMAX(K) = BARAY(K,J)
      TMEAN(K) = TMEAN(K) + 0.5*(BARAY(K,J)+SARAY(K,J))
  475 CONTINUE
C### WCH, 12/92  USE CORRECT NUMBER OF DAYS.
      TMEAN(K) = TMEAN(K)/FLOAT(IDS-IDB+1)
  400 CONTINUE
      WRITE(N6,480) NEWYR
      DO 490 K = 1,12
      IF(TMEAN(K).EQ.-99.0) GO TO 490
      WRITE(N6,485) MMNTH(K),TMIN(K),TMAX(K),TMEAN(K)
  490 CONTINUE
      ENDIF
C=======================================================================
      IF(KTYPE.EQ.1.OR.KTYPE.EQ.3.OR.KTYPE.GE.5) THEN
      KWRITE   = 0
      DO 500 K = 1,12
                                                KMONTH = 31
      IF(K.EQ.9.OR.K.EQ.4.OR.K.EQ.6.OR.K.EQ.11) KMONTH = 30
C ### WCH, 11/92.  REVERSE FEBRUARY CALCULATION
      IF(K.EQ.2.AND.MOD(NEWYR,4).EQ.0)         KMONTH = 29
      IF(K.EQ.2.AND.MOD(NEWYR,4).NE.0)         KMONTH = 28
      IF(EARAY(K,1).EQ.-99.AND.EARAY(K,KMONTH).EQ.-99) GO TO 500
      KWRITE  = KWRITE + 1
      IF(KWRITE.EQ.1) WRITE(N6,975) TITLE(1),TITLE(2)
C#######################################################################
C WCH, 12/92. MODIFY PRINT AND STATISTICS FOR ARBITRARY START/STOP DATES
C#######################################################################
         IDB = 1
         IF(IYBEG(1).EQ.NEWYR.AND.K.EQ.IYBEG(2)) IDB = IYBEG(3)
         IDS = KMONTH
         IF(IYEND(1).EQ.NEWYR.AND.K.EQ.IYEND(2)) IDS = IYEND(3)
      IF(IYEAR.GT.0) WRITE(N6,995) (K,J,NEWYR,EARAY(K,J),J=IDB,IDS)
  500 CONTINUE
      ENDIF
C=======================================================================
      IF(KTYPE.EQ.2.OR.KTYPE.GE.4) THEN
      KWRITE   = 0
      DO 600 K = 1,12
                                                KMONTH = 31
      IF(K.EQ.9.OR.K.EQ.4.OR.K.EQ.6.OR.K.EQ.11) KMONTH = 30
C ### WCH, 11/92.  REVERSE FEBRUARY CALCULATION
      IF(K.EQ.2.AND.MOD(NEWYR,4).EQ.0)         KMONTH = 29
      IF(K.EQ.2.AND.MOD(NEWYR,4).NE.0)         KMONTH = 28
C### WCH, 12/92.  CHECK FOR CORRECT LAST DAY OF MONTH.
      IF(WARAY(K,1).EQ.-99.AND.WARAY(K,KMONTH).EQ.-99) GO TO 600
      KWRITE  = KWRITE + 1
      IF(KWRITE.EQ.1) WRITE(N6,980) TITLE(1),TITLE(2)
C#######################################################################
C WCH, 12/92. MODIFY PRINT AND STATISTICS FOR ARBITRARY START/STOP DATES
C#######################################################################
         IDB = 1
         IF(IYBEG(1).EQ.NEWYR.AND.K.EQ.IYBEG(2)) IDB = IYBEG(3)
         IDS = KMONTH
         IF(IYEND(1).EQ.NEWYR.AND.K.EQ.IYEND(2)) IDS = IYEND(3)
      IF(IYEAR.GT.0) WRITE(N6,995) (K,J,NEWYR,WARAY(K,J),J=IDB,IDS)
  600 CONTINUE
      ENDIF
C=======================================================================
C     WRITE THE INTERFACE FILE
C=======================================================================
      KDAY     = 0
      DO 700 K = 1,12
                                                KMONTH = 31
      IF(K.EQ.9.OR.K.EQ.4.OR.K.EQ.6.OR.K.EQ.11) KMONTH = 30
      IF(K.EQ.2.AND.MOD(NEWYR,4).EQ.0)         KMONTH = 29
      IF(K.EQ.2.AND.MOD(NEWYR,4).NE.0)         KMONTH = 28
      DO 700 J = 1,KMONTH
      KDAY     = KDAY + 1
      JULDAY   = NEWYR*1000 + KDAY
      IF(BARAY(K,1).EQ.-99.0.AND.BARAY(K,KMONTH).EQ.-99.0.AND.
     +   SARAY(K,1).EQ.-99.0.AND.SARAY(K,KMONTH).EQ.-99.0.AND.
     +   WARAY(K,1).EQ.-99.0.AND.WARAY(K,KMONTH).EQ.-99.0.AND.
     +   EARAY(K,1).EQ.-99.0.AND.EARAY(K,KMONTH).EQ.-99.0) GO TO 700
C#####
C WCH, 11/92.  CHANGE WARAY TO EARAY FOR EVAPORATION DATA
C#####
      IF(EARAY(K,J).NE.-99.0) EARAY(K,J) = EARAY(K,J)*PAN(J)
      WRITE(NEXT) JULDAY,BARAY(K,J),SARAY(K,J),EARAY(K,J),WARAY(K,J)
  700 CONTINUE
C=======================================================================
      NEWYR = NEWYR + 1
      IF(IGO.EQ.1) THEN
                   WRITE(*,2050)
                   WRITE(N6,2050)
                   CLOSE (JIN(INCNT))
                   RETURN
                   ENDIF
      GO TO 7
C=======================================================================
   68 FORMAT(/,1X,A80,/,2X,A80)
  480 FORMAT(/,1H1,/,
     +' **************************************************',/,
     +' * MONTHLY MINIMUM, MAXIMUM, AND MEAN TEMPERATURE *',/,
     +' **************************************************',/,
     +' *                 YEAR ==> ',I4,'                  *',/,
     +' **************************************************',//,
     +' MONTH          MINIMUM     MAXIMUM        MEAN',/,
     +' ----------     -------     -------      ------')
  485 FORMAT(1X,A10,3F12.3)
  911 FORMAT(' READ DATA FROM YEAR : ',I4)
  970 FORMAT(/,1H1,' DAILY TEMPERATURE FROM WEATHER SERVICE TAPE',
     *' IN DEGREES FAHRENHEIT',/,1X,A80,/,A80,//,
     *  1X,4('MO/DY/YEAR  MAX TEMP MIN TEMP '),/,
     *  1X,4('----------  -------- -------- '))
  975 FORMAT(/,1H1,' DAILY EVAPORATION FROM WEATHER SERVICE TAPE',
     *' IN HUNDRETHS OF AN INCH/DAY',/,1X,A80,/,A80,//,
     *  1X,4('MO/DY/YEAR EVAPORATION '),/,
     *  1X,4('---------- ----------- '))
  980 FORMAT(/,1H1,' DAILY WIND MOVEMENT FROM WEATHER SERVICE TAPE',
     *' IN MILES PER DAY',/,1X,A80,/,A80,//,
     *  1X,4('MO/DY/YEAR  WIND SPEED '),/,
     *  1X,4('----------  ---------- '))
  990 FORMAT(4(2(I2,'/'),I4,F9.1,1X,F9.1,1X))
  995 FORMAT(4(2(I2,'/'),I4,F13.2))
 1000 FORMAT(/,' ************************************************',/,
     1         ' * ENTRY MADE TO THE TEMP BLOCK                 *',/,
     2         ' * CREATED BY THE UNIVERSITY OF FLORIDA - 1988  *',/,
     3         ' * LAST UPDATED, DECEMBER 1992 AT O.S.U.        *',/,
     4         ' ************************************************',//)
 1021 FORMAT(1X,//,
     1' *******************************',/,
     1' *  TEMP BLOCK INPUT COMMANDS  *',/,
     1' *******************************',//,
     1 5X,'STATION, ISTA..........................',4X,I6,//,
     2 5X,'BEGINNING DATE, IYBEG (YR/MO/DY).......',
     22X,I4,'/',I2,'/',I2,//,
     3 5X,'ENDING DATE, IYEND (YR/MO/DY)..........',
     32X,I4,'/',I2,'/',I2,//,
     6 5X,'NWS FORMAT, IFORM (SEE TEXT)...........',I6,//,
     6 5X,'TYPE OF TIME SERIES DATA, KTYPE........',I6,//,
     6 5X,'PRINT ALL INPUT DATA (IYEAR)...........',I6,/,
     7 5X,'(O-NO PRINT; 1-PRINT ALL DATA; 2-PRINT SUMMARY)')
 1022 FORMAT(/,' MONTHLY PAN COEFFICIENTS ',/,2X,12F5.2)
 1030 FORMAT(1X,///,
     +' *****************************',/,
     +' * USER DEFINED INPUT FORMAT *',/,
     +' *****************************',//,
     + 5X,'USER FORMAT (FIRMAT)...................',A20,//,
     + 5X,'INPUT/OUTPUT UNITS 0 = U.S. CUSTOMARY..',/,
     + 5X,'                   1 = METRIC UNITS....',I6,//,
     + 5X,'UNITS OF TEMPERATURE (KUNIT)...........',I6,/,
     + 5X,' 0 --> DEGREES F; 1 --> DEGREES C......',//,
     + 5X,'WIND/EVAP CONVERSION FACTOR (CONV).....',F7.2,//,
     + 5X,'FIELD POSITION FOR STATION NUMBER, F1..',I6,//,
     + 5X,'FIELD POSITION FOR YEAR, F2............',I6,//,
     + 5X,'FIELD POSITION FOR MONTH, F3...........',I6,//,
     + 5X,'FIELD POSITION FOR DAY, F4.............',I6,//,
     + 5X,'FIELD POSITION FOR 1st VARIABLE, F5....',I6,//,
     + 5X,'FIELD POSITION FOR 2nd VARIABLE, F6....',I6,//)
C### WCH, 12/92 UNUSED FORMAT STATEMENT?
C2020 FORMAT(I5,1X,2(I2,'/'),I4,I5,F7.0,3(1X,F6.2),1X,F6.0,2(2X,I5))
 2050 FORMAT(/,' TEMP BLOCK EXITED NORMALLY. ')
 2100 FORMAT (/,' SORRY, IFORM=1 NOT IMPLEMENTED. RUN STOPPED IN TEMP BL
     +OCK.')
 2150 FORMAT (/,' *** WARNING ***.',/,' IF START/STOP DATES = 0, THEN PR
     +INT AND STATISTICS',/,' FOR PARTIAL START/STOP MONTHS MAY BE INCOR
     +RECT.')
C=======================================================================
 888  CALL IERROR
      END
