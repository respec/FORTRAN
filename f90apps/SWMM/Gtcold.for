      SUBROUTINE GTCOLD(IDO,IGO)
C	TEMP BLOCK
C     CALLED BY TEMP NEAR 110 and 112
C     
C=======================================================================
C     UPDATED 11/92 BY WCH TO CORRECT TYPO AT APPROX. LINE 91
C       AND TO ALLOW NO STATION NUMBER FOR USER-DEFINED INPUT
C     UPDATED 12/92 TO CORRECT DATE PROBLEMS.
C     UPDATED 9/30/93 TO ALLOW ZERO STATION NUMBER AND BETTER END-OF-
C       FILE MESSAGE.  WCH.
C     ENHANCE ERROR MESSAGE FOR READING OF DATA, WCH, 4/18/94.
C     ALTER IOSTAT NUMBER FOR LAHEY, WCH, 8/5/95.
C=======================================================================
C     THIS PROGRAM READS TWO FORMATS
C     IFORM = 0 MEANS POST 1980 FORMAT
C     IFORM = 2 MEANS USER DEFINED FORMAT
C     IDO   = 0 SEARCH FOR STATION NUMBER
C     IDO   = 1 READ STATION DATA
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TEW.INC'
      INTEGER  DAY,YEAR,STA,FF(6),IVALUE(31)
      CHARACTER ELMTYP*4,LERR*30
C=======================================================================
      IF(IDO.EQ.0)   THEN
      IF(IFORM.EQ.0) THEN
C#### WCH, 4/18/94.  ADD IOSTAT
2              READ(IO,1000,ERR=60,END=440,IOSTAT=IOS) IBUF,ELMTYP,
     +              NEWYR,NEWMON
               IF(NEWYR.LT.100) NEWYR = NEWYR + 1900
C#### WCH, 9/30/93.  ALLOW ZERO STATION NUMBER OPTION HERE TOO.
               IF(ISTA.GT.0.AND.IBUF.NE.ISTA) GO TO 2
               IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1))  GO TO 2
               IF(IYBEG(2).NE.0.AND.NEWMON.LT.IYBEG(2)) GO TO 2
               BACKSPACE IO
               ISTA = IBUF
               ENDIF
C
      IF(IFORM.EQ.2) THEN
C#######################################################################
C WCH, 11/92  CORRECT TO ALLOW NO STATION NO. ENTERED IF F1=0,
C               AS IN DOCUMENTATION
C#######################################################################
               NUMPAR = 6
               IF(KTYPE.EQ.1.OR.KTYPE.EQ.2) NUMPAR = NUMPAR - 1
               IF(F1.EQ.0)                  NUMPAR = NUMPAR - 1
C#### WCH, 4/18/94.  ADD IOSTAT
4              READ(IO,FIRMAT,ERR=60,END=440,IOSTAT=IOS) 
     +             (FF(I),I=1,NUMPAR)
               IF(F1.NE.0) IBUF = FF(F1)
               NEWYR  = FF(F2)
C#### WCH, 12/92.  NEED 2-DIGIT YEAR.
c              IF(NEWYR.GT.1900) NEWYR = NEWYR - 1900
               IF(NEWYR.LT.100)  NEWYR = NEWYR + 1900
               NEWMON = FF(F3)
               NEWDAY = FF(F4)
               IF(F1.NE.0.AND.IBUF.NE.ISTA) GO TO 4
               IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1))  GO TO 4
               IF(IYBEG(2).NE.0.AND.NEWMON.LT.IYBEG(2)) GO TO 4
               IF(IYBEG(3).NE.0.AND.NEWDAY.LT.IYBEG(3)) GO TO 4
               BACKSPACE IO
               ENDIF
      RETURN
C#### WCH, 9/30/93.  ADD ALTERNATE END OF FILE MESSAGE.
  440 WRITE(N6,9440) IO,ISTA,IBUF
      STOP
C=======================================================================
C  ENDIF FOR IDO = 0
C=======================================================================
      ENDIF
C
      IF(IDO.EQ.1) THEN
 7    BACKSPACE IO
      CALL SETZER(BARAY,12,31,-99.0)
      CALL SETZER(SARAY,12,31,-99.0)
      CALL SETZER(EARAY,12,31,-99.0)
      CALL SETZER(WARAY,12,31,-99.0)
C
10    CONTINUE
      IF(IFORM.EQ.0) THEN
C#### WCH, 4/18/94.  ADD IOSTAT
         READ(IO,1000,END=40,ERR=60,IOSTAT=IOS) STA,ELMTYP,
     +             YEAR,MONTH,(IDUM,IDUM,IVALUE(J),J=1,31)
         IF (YEAR.LT.100) YEAR = YEAR + 1900
C
         IF(KTYPE.EQ.0.AND.(ELMTYP.EQ.'TMIN'.OR.ELMTYP.EQ.'TMAX'))
     +              GO TO 15
      IF(KTYPE.EQ.1.AND.ELMTYP.EQ.'EVAP') GO TO 15
      IF(KTYPE.EQ.2.AND.ELMTYP.EQ.'WDMV') GO TO 15
      IF(KTYPE.EQ.3.AND.(ELMTYP.EQ.'TMIN'.OR.ELMTYP.EQ.'TMAX'.
     +                          OR.ELMTYP.EQ.'EVAP')) GO TO 15
      IF(KTYPE.EQ.4.AND.(ELMTYP.EQ.'TMIN'.OR.ELMTYP.EQ.'TMAX'.
     +                          OR.ELMTYP.EQ.'WDMV')) GO TO 15
      IF(KTYPE.EQ.5.AND.(ELMTYP.NE.'EVAP'.OR.ELMTYP.EQ.'WDMV'))GO TO 15
      IF(KTYPE.EQ.6.AND.(ELMTYP.EQ.'TMIN'.OR.ELMTYP.EQ.'TMAX'.
     +              OR.ELMTYP.EQ.'EVAP'.OR.ELMTYP.EQ.'WDMV')) GO TO 15
         GO TO 30
C
  15     CONTINUE
         DO 200 K = 1,31
         IF(IVALUE(K).EQ.-9999) GO TO 200
         IF(ELMTYP.EQ.'EVAP') EARAY(MONTH,K) = FLOAT(IVALUE(K))
         IF(ELMTYP.EQ.'WDMV') WARAY(MONTH,K) = FLOAT(IVALUE(K))
         IF(ELMTYP.EQ.'TMIN') SARAY(MONTH,K) = FLOAT(IVALUE(K))
         IF(ELMTYP.EQ.'TMAX') BARAY(MONTH,K) = FLOAT(IVALUE(K))
  200    CONTINUE
         ENDIF
C
      IF(IFORM.EQ.2) THEN
C#### WCH 11/92
C#### WCH, 4/18/94.  ADD IOSTAT
         READ(IO,FIRMAT,END=40,ERR=60,IOSTAT=IOS) (FF(I),I=1,NUMPAR)
         IF(F1.NE.0) STA = FF(F1)
         YEAR  = FF(F2)
C#### WCH, 12/92.  NEED 2-DIGIT YEAR.
c         IF(YEAR.GT.1900) YEAR = YEAR - 1900
          IF(YEAR.LT.100) YEAR = YEAR + 1900
         MONTH = FF(F3)
         DAY   = FF(F4)
         MAXU  = FF(F5)
         IF(MONTH.LE.0.OR.MONTH.GT.12) GO TO 30
         IF(DAY.LE.0.OR.DAY.GT.31)     GO TO 30
         IF(KTYPE.EQ.0) MINU  = FF(F6)
         IF(KTYPE.EQ.0) BARAY(MONTH,DAY) = FLOAT(MAXU)
         IF(KTYPE.EQ.0) SARAY(MONTH,DAY) = FLOAT(MINU)
         IF(KTYPE.EQ.1) EARAY(MONTH,DAY) = FLOAT(MAXU)*CONV
         IF(KTYPE.EQ.2) WARAY(MONTH,DAY) = FLOAT(MAXU)*CONV
         IF(KUNIT.EQ.1.AND.KTYPE.EQ.0) THEN
                   BARAY(MONTH,DAY) = 1.8*BARAY(MONTH,DAY) + 32.0
C#####
C WCH, 11/92 CHANGE BARAY TO SARAY
C#####
                   SARAY(MONTH,DAY) = 1.8*SARAY(MONTH,DAY) + 32.0
                   ENDIF
         ENDIF
C=======================================================================
C     SKIP IF THE NEW STATION IS NOT THE STATION SELECTED
C     RETURN IF THE NEW YEAR IS LESS THAN THE SELECTED ENDING YEAR
C=======================================================================
30    CONTINUE
C#######################################################################
C  WCH, 12/92  RETURN IF LAST DAY OF YEAR.
C  ALSO CHECK FOR NO ADDITIONAL RECORDS.
C  END = 40 IN READ MEANS THIS IS LAST YEAR IF REACH END OF FILE.
C#######################################################################
      IF(IFORM.EQ.0) DAY = 31
      IF(MONTH.EQ.12.AND.DAY.EQ.31)  THEN
C#### WCH, 4/18/94.  ADD IOSTAT
             IF(IFORM.EQ.0) READ(IO,1000,END=40,ERR=60,IOSTAT=IOS) 
     +             STA,ELMTYP,YEAR,MONTH,(IDUM,IDUM,IVALUE(J),J=1,31)
             IF (YEAR.LT.100) YEAR = YEAR + 1900
C#### WCH, 4/18/94.  ADD IOSTAT
             IF(IFORM.EQ.2) READ(IO,FIRMAT,END=40,ERR=60,IOSTAT=IOS) 
     +             (FF(I),I=1,NUMPAR)
             IF (FF(F2).LT.100) FF(F2) = FF(F2) + 1900
C  NOTE, IO BACKSPACED AT BEGINNING OF LOOP FOR IDO = 1 (STATEMENT 7).
             RETURN
             ENDIF           
C#### WCH, 11/92
      IF(IFORM.EQ.2.AND.F1.EQ.0) GO TO 33
      IF(STA.NE.ISTA)                            GO TO 40
C#######################################################################
C  WCH, 12/92 CORRECT WRONG SUBSCRIPTS FOR YEAR AND MONTH
C  (3) = DAY, (2) = MONTH, (1) = YEAR
C  ALSO, ALTER THE CHECK FOR ENDING DATE TO CHECK FOR EXACT 
C    DATE FOR IFORM = 2.
C#######################################################################
33    IF(IFORM.EQ.2) THEN
        IF(YEAR.EQ.IYEND(1).AND.MONTH.EQ.IYEND(2).AND.DAY.EQ.IYEND(3)) 
     *                                           GO TO 40
        ENDIF
      IF(IFORM.EQ.0) THEN
        IF(YEAR.EQ.IYEND(1).AND.MONTH.GE.IYEND(2)) GO TO 40
        ENDIF
      IF(IYEND(1).NE.0.AND.YEAR.GT.IYEND(1))     GO TO 40
      IF(YEAR.GT.NEWYR)                          RETURN
      GO TO 10
C=======================================================================
C     ENDIF FOR IDO = 1
C=======================================================================
      ENDIF
C=======================================================================
40    IGO = 1
      RETURN
C=======================================================================
C     ERROR MESSAGE FOR READING INPUT FILE.
C=======================================================================
C#### WCH, 4/18/94.  ADD IOSTAT
C#### WCH, 8/5/95.  ALTER IOS FOR LAHEY.
60    WRITE(N6,2000) IO,MOD(IOS,256)
      WRITE(*,2000)  IO,MOD(IOS,256)
      WRITE(*,2001)
      BACKSPACE IO
      READ(IO,3000)  LERR
      WRITE(N6,3001) LERR
      STOP
C=======================================================================
 1000 FORMAT(3X,I6,2X,A4,4X,I2,I2,7X,31(2I2,I6,2X))
C#### WCH, 4/18/94.  ADD IOSTAT
C#### WCH, 8/5/95.  ALTER IOSTAT NUMBER FOR LAHEY.
 2000 FORMAT(/,' ===> ERROR !!  READING THE INPUT FILE, UNIT',I3,/,
     1        '  LAHEY ERROR NUMBER =',I5,/,
     2        '  EXECUTION STOPS FROM TEMP BLOCK.')
C#### WCH, 4/18/94
 2001 FORMAT ('  SEE OUTPUT FILE FOR MORE INFORMATION.')
 3000 FORMAT(A30)
 3001 FORMAT(/,' FIRST 30 CHARACTERS OF THE LAST LINE READ ARE: ',/,
     +         3X,A30 )
C#### WCH, 9/30/93.
 9440 FORMAT (' ERROR FROM TEMP BLOCK.  END OF FILE REACHED ON UNIT',
     1I3,/,' EITHER ISTA > 0 AND CAN''T FIND MATCHING STATION NUMBER ON 
     2DATA FILE OR ELSE CAN''T FIND CORRECT DATES.',/,' ISTA =',I8,
     3 ' STA. NO. ON DATA FILE =',I8,/,' RUN STOPPED FROM TEMP BLOCK.')
C=======================================================================
      END
