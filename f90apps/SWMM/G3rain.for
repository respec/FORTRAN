      SUBROUTINE G3RAIN(IDO,IGO,ILOST)
C	RAIN BLOCK
C	CALLED BY RAIN NEAR LINES 440 AND 465
C	
C#######################################################################
C     This program reads a user defined time series.
C     Updated 2/93 by WCH to correct hour values for user-supplied data.
C     Updated 11/12/93 by WCH to allow for hours to range from 1 - 24.
C     Updated 10/25/94 by WCH to allow YEAR and NEWYR to be read as
C       either 19XX or XX.  Program uses two-digit form.
C     Updated 8/1/95 by WCH to change station ID to character after
C       input for IFORM = 3.
C     WCH, 8/1/95.  Make changes for character station ID.  No longer
C       compare requested ID with ID on data lines, to avoid character
C       comparison complications.
C     WCH, 10/2/96. Add new error message for READ(IO).
C     WCH, 12/3/96. Remove unused statement number 777.  Backspace IO
C       after reading first record of next year, to avoid missing that
C       record when returning to G3RAIN.
C#######################################################################
C     IFORM = 3 MEANS USER DEFINED FORMAT
C     IDO   = 0 SEARCH FOR STATION NUMBER
C     IDO   = 1 READ STATION RAINFALL
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'PRECIP.INC'
C#### WCH, 8/1/95.  CHANGE STATION ID TO CHARACTER.
C####      INTEGER DAY,YEAR,STA,FF(6),XRAIN(24)
      INTEGER DAY,YEAR,FF(6),XRAIN(24)
      CHARACTER*8 IBUF, STA
C=======================================================================
C     First find the correct rainfall station (IDO = 0)
C=======================================================================
      IF(IDO.EQ.0)   THEN
C#### WCH, 8/1/95.
      IPRINT = 0
C=======================================================================
      IF(F1.GT.0) THEN
C#### WCH, 10/2/96.  GO TO 970 ON ERROR, NOT BACK TO SAME STATEMENT.
C#### 5              READ(IO,FIRMAT,ERR=5,END=40) (FF(I),I=1,6)
5              READ(IO,FIRMAT,ERR=970,END=40,IOSTAT=IOS) (FF(I),I=1,6)
C#### WCH, 8/1/95.  CONVERT STATION ID TO CHARACTER.
C####               IBUF   = FF(F1)
               WRITE(IBUF,'(I8)') FF(F1)
C
               NEWYR  = FF(F2)
C#### WCH, 10/25/94.
c               IF(NEWYR.GT.1900) NEWYR = NEWYR - 1900
                IF(NEWYR.LT.100)  NEWYR = NEWYR + 1900
               NEWMON = FF(F3)
               NEWDAY = FF(F4)
C#### WCH, 8/1/95.  PRINT NAME INSTEAD OF TESTING EQUALITY.
C####               IF(IBUF.NE.ISTA) GO TO 5
               IF(IPRINT.EQ.0) THEN
                    WRITE(N6,990) IBUF,ISTA
                    WRITE(*,990) IBUF,ISTA
                    IPRINT = 1
                    ENDIF
C
               IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1))  GO TO 5
               IF(NEWMON.LT.IYBEG(2).AND.
     +                            NEWYR.EQ.IYBEG(1))    GO TO 5
               IF(NEWMON.LE.IYBEG(2).AND.NEWDAY.LT.IYBEG(3)
     +                       .AND.NEWYR.EQ.IYBEG(1))    GO TO 5
CC$$$$$5/3/92
                  BACKSPACE IO
               ENDIF
C=======================================================================
      IF(F1.EQ.0) THEN
C#### WCH, 10/2/96.  GO TO 970 ON ERROR, NOT BACK TO SAME STATEMENT.
6              READ(IO,FIRMAT,ERR=970,END=40,IOSTAT=IOS) (FF(I),I=1,5)
               NEWYR  = FF(F2)
C#### WCH, 10/25/94.
c               IF(NEWYR.GT.1900) NEWYR = NEWYR - 1900
                IF(NEWYR.LT.100)  NEWYR = NEWYR + 1900
               NEWMON = FF(F3)
               NEWDAY = FF(F4)
               IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1))  GO TO 6
               IF(NEWMON.LT.IYBEG(2).AND.
     +                            NEWYR.EQ.IYBEG(1))    GO TO 6
               IF(NEWMON.LE.IYBEG(2).AND.NEWDAY.LT.IYBEG(3)
     +                       .AND.NEWYR.EQ.IYBEG(1))    GO TO 6
CC$$$$$5/3/92
                   BACKSPACE IO
               ENDIF
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
      CALL SETIA(HOUR,366,27,0)
cim 11/07 update LIMRN values
c      CALL SETIA(RDAY,3000,1,0)
      CALL SETIA(RDAY,LIMRN,1,0)
      ILOST  = 1
      NSTORM = 0
      IDAST  = KDATE(0,1,NEWYR)
C=======================================================================
C     Read this year's rainfall.
C=======================================================================
C     Read Rainfall and station number > 0 for a
C             User defined time series.
C=======================================================================
   10 CONTINUE
      IF(NUVAL.EQ.1) THEN
           IF(F1.GT.0.AND.F7.EQ.7) THEN
C#### WCH, 10/2/96.  GO TO 970 ON ERROR, NOT BACK TO SAME STATEMENT.
                     READ(IO,FIRMAT,ERR=970,END=40,IOSTAT=IOS)
     1                   (FF(I),I=1,6),REIN
                     IF(REIN.NE.0.0) THEN
C#### WCH, 8/1/95.  CONVERT STATION ID TO CHARACTER.
C####                      STA   = FF(F1)
                          WRITE(STA,'(I8)') FF(F1)
C
                          YEAR  = FF(F2)
C#### WCH, 10/25/94.
c                         IF(YEAR.GT.1900) YEAR = YEAR - 1900
                          IF(YEAR.LT.100) YEAR = YEAR + 1900
                          MONTH = FF(F3)
                          DAY   = FF(F4)
                          REIN  = REIN*CONV
C###### WCH 2/9/93 REMOVE IF STMT FOR IHR
                          IHR   = FF(F5)
                          MINU  = FF(F6)
                          IF(METRIC.EQ.1) THEN
                               IRAIN = IFIX(REIN*1000.0+0.1)
                               ELSE
                               IRAIN = IFIX(REIN*100.0+0.1)
                               ENDIF
                          ELSE
                          GO TO 10
                          ENDIF
                     ENDIF
C=======================================================================
C     Read rainfall and station number > 0 for a
C       user-defined time series.
C=======================================================================
           IF(F1.GT.0.AND.F7.EQ.1) THEN
C#### WCH, 10/2/96.  GO TO 970 ON ERROR, NOT BACK TO SAME STATEMENT.
                READ(IO,FIRMAT,ERR=970,END=40,IOSTAT=IOS)
     1              REIN,(FF(I),I=1,6)
                IF(REIN.NE.0.0) THEN
C#### WCH, 8/1/95.  CONVERT STATION ID TO CHARACTER.
C####                      STA   = FF(F1)
                          WRITE(STA,'(I8)') FF(F1)
C
                          YEAR  = FF(F2)
C#### WCH, 10/25/94.
c                         IF(YEAR.GT.1900) YEAR = YEAR - 1900
                          IF(YEAR.LT.100)  YEAR = YEAR + 1900
                          MONTH = FF(F3)
                          DAY   = FF(F4)
                          REIN  = REIN*CONV
C###### WCH 2/9/93 REMOVE IF STMT FOR IHR
                          IHR   = FF(F5)
                          MINU  = FF(F6)
                          IF(METRIC.EQ.1) THEN
                               IRAIN = IFIX(REIN*1000.0+0.1)
                               ELSE
                               IRAIN = IFIX(REIN*100.0+0.1)
                               ENDIF
                          ELSE
                          GO TO 10
                          ENDIF
                     ENDIF
C=======================================================================
C     Read rainfall and station number = 0 for a
C             user-defined time series.
C=======================================================================
           IF(F1.EQ.0.AND.F7.GE.6) THEN
C#### WCH, 10/2/96.  GO TO 970 ON ERROR, NOT BACK TO SAME STATEMENT.
                READ(IO,FIRMAT,ERR=970,END=40,IOSTAT=IOS)
     1              (FF(I),I=1,5),REIN
                IF(REIN.NE.0.0) THEN
                     STA   = ISTA
                     YEAR  = FF(F2)
C#### WCH, 10/25/94.
c                    IF(YEAR.GT.1900) YEAR = YEAR - 1900
                     IF(YEAR.LT.100)  YEAR = YEAR + 1900
                     MONTH = FF(F3)
                     DAY   = FF(F4)
                     REIN  = REIN*CONV
C###### WCH 2/9/93 REMOVE IF STMT FOR IHR
                     IHR   = FF(F5)
                     MINU  = FF(F6)
                     IF(METRIC.EQ.1) THEN
                               IRAIN = IFIX(REIN*1000.0+0.1)
                               ELSE
                               IRAIN = IFIX(REIN*100.0+0.1)
                               ENDIF
                     ELSE
                     GO TO 10
                     ENDIF
                ENDIF
C=======================================================================
C     Read rainfall and station number = 0 for a
C             user-defined time series.
C=======================================================================
           IF(F1.EQ.0.AND.F7.EQ.1) THEN
C#### WCH, 10/2/96.  GO TO 970 ON ERROR, NOT BACK TO SAME STATEMENT.
                READ(IO,FIRMAT,ERR=970,END=40,IOSTAT=IOS)
     1              REIN,(FF(I),I=1,5)
                IF(REIN.NE.0.0) THEN
                     STA   = ISTA
                     YEAR  = FF(F2)
C#### WCH, 10/25/94.
c                    IF(YEAR.GT.1900) YEAR = YEAR - 1900
                     IF(YEAR.LT.100)  YEAR = YEAR + 1900
                     MONTH = FF(F3)
                     DAY   = FF(F4)
                     REIN  = REIN*CONV
C###### WCH 2/9/93 REMOVE IF STMT FOR IHR
                     IHR   = FF(F5)
                     MINU  = FF(F6)
                     IF(METRIC.EQ.1) THEN
                               IRAIN = IFIX(REIN*1000.0+0.1)
                               ELSE
                               IRAIN = IFIX(REIN*100.0+0.1)
                               ENDIF
                     ELSE
                     GO TO 10
                     ENDIF
                ENDIF
C=======================================================================
           ENDIF
C=======================================================================
C     Read rainfall and station number for a user-defined time series.
C=======================================================================
      IF(NUVAL.GT.1) THEN
C#### WCH, 10/2/96.  GO TO 970 ON ERROR, NOT BACK TO SAME STATEMENT.
                     READ(IO,FIRMAT,ERR=970,END=40,IOSTAT=IOS)
     1                   (FF(I),I=1,3),(XRAIN(K),K=1,NUVAL)
                     STA   = ISTA
                     YEAR  = FF(F2)
C#### WCH, 10/25/94.
c                    IF(YEAR.GT.1900) YEAR = YEAR - 1900
                     IF(YEAR.LT.100)  YEAR = YEAR + 1900
                     MONTH = FF(F3)
                     DAY   = FF(F4)
                     ENDIF
C=======================================================================
C     Check for same station.     Check for same year.
C     Add to this year's matrix.  Find the day number and indicate
C                                 unusual conditions.
C     Hour =  -2 means meter stuck.
C     Hour =  -1 means missing data.
C=======================================================================
C#### WCH, 8/1/95.  DON'T MAKE THIS CHECK ANYMORE.
C####      IF(STA.NE.ISTA) GO TO 40
      IF(IYEND(1).NE.0.AND.YEAR.EQ.IYEND(1).AND.MONTH.GE.IYEND(2)
     *                   .AND.DAY.GT.IYEND(3))   GO TO 40
      IF(IYEND(1).NE.0.AND.YEAR.EQ.IYEND(1).AND.MONTH.GT.IYEND(2))
     *                                           GO TO 40
      IF(IYEND(1).NE.0.AND.YEAR.GT.IYEND(1))     GO TO 40
C=======================================================================
C#### WCH, 12/3/96. NEED TO BACKSPACE IO, OTHERWISE MISS FIRST RECORD
C     OF NEXT YEAR.
C=======================================================================
C####      IF(YEAR.GT.NEWYR)                          RETURN
      IF(YEAR.GT.NEWYR) THEN
           BACKSPACE IO
           RETURN
           ENDIF
      IDAY = KDATE(DAY,MONTH,YEAR) - IDAST
C=======================================================================
C     User defined format for one value per line.
C=======================================================================
      IF(NUVAL.EQ.1) THEN
                     NSTORM         = NSTORM     + 1
C#######################################################################
C  WCH 2/9/93  CAUTION. IHR USED AS SUBSCRIPT.  USER INPUTS
C    FROM 0 TO 23, THUS ADD 1 FOR SUBSCRIPT FOR HOURS OF DAY.
C#######################################################################
C  WCH, 11/12/93.  ADD OPTION FOR IHR TO VARY FROM 1 - 24.
C#######################################################################
                     IF(IHH.EQ.0.AND.IHR.GE.24) THEN
                        WRITE(N6,9050) IHR,YEAR,MONTH,DAY
                        IHR = 23
                        ENDIF
                     IF(IHH.EQ.1) THEN
                        IHR = IHR - 1
                        IF(IHR.LT.0) THEN
                           WRITE(N6,9060) IHR+1,YEAR,MONTH,DAY
                           IHR = 1
                           ENDIF
                        ENDIF
                     HOUR(IDAY,IHR+1) = IRAIN      + HOUR(IDAY,IHR+1)
                     RTIME(NSTORM)  = FLOAT(IHR) + FLOAT(MINU)/60.0
                     RRAIN(NSTORM)  = IRAIN
                     RDAY(NSTORM)   = IDAY
c                    write(*,*) nstorm,rday(nstorm),rtime(nstorm)
cim change from EQ to GE 11/97
                     IF(NSTORM.GE.LIMRN) THEN
                                         WRITE(N6,9000) LIMRN
                                         GO TO 40
                                         ENDIF
                     ENDIF
C=======================================================================
C     User defined format for multiple values per line.
C=======================================================================
      IF(NUVAL.GT.1) THEN
                     DO 7000     J  = 1,24
                     IF(XRAIN(J).EQ.0.0) GO TO 7000
                     IF(METRIC.EQ.1) THEN
                               IRAIN = 10 * XRAIN(J)
                               ELSE
                               IRAIN =      XRAIN(J)
                               ENDIF
                     HOUR(IDAY,J)   = IRAIN
                     NSTORM         = NSTORM     + 1
                     RTIME(NSTORM)  = J
                     RRAIN(NSTORM)  = IRAIN
                     RDAY(NSTORM)   = IDAY
cim include check and write for nstorm here also
                     IF(NSTORM.GE.LIMRN) THEN
                                         WRITE(N6,9000) LIMRN
                                         GO TO 40
                                         ENDIF
 7000                CONTINUE
                     ENDIF
C=======================================================================
C#### WCH, 12/3/96.  REMOVE UNUSED STATEMENT NUMBER 777.
      HOUR(IDAY,25) = MONTH
      HOUR(IDAY,26) =   DAY
      HOUR(IDAY,27) =  YEAR
      GO TO 10
C=======================================================================
C     End of loop for IF(IDO.EQ.1)
C=======================================================================
      ENDIF
C=======================================================================
C     Skip if the new station is not the station selected.
C     Return if the new year is less than the selected ending year.
C=======================================================================
49    RETURN
C=======================================================================
C     Here if reach end of file or passed designated ending date.
C=================================================================
40    IGO = 1
c  do this to ensure that all rainfall on interface is written
      IF (NSTORM.LT.LIMRN) NSTORM = NSTORM + 1
      RDAY(NSTORM) = 999999
c
      RETURN
C=======================================================================
C#### WCH, 10/2/96.  NEW ERROR MESSAGE FOR READ(IO) ERRORS.
C=======================================================================
  970 WRITE(N6,9700) IO,MOD(IOS,256),FF
      WRITE(*,9700) IO,MOD(IOS,256),FF
      STOP 'Run stopped from Sub. G3RAIN.'
C=======================================================================
C#### WCH, 8/1/95.  NEW FORMAT STATEMENT 990.
  990 FORMAT(/,' STATION ID ON PRECIP. DATA INPUT FILE = ',A8,/,
     1' REQUESTED STATION ID = ',A8,' CHECK TO BE SURE THEY MATCH.',/)
 9000 FORMAT(/,' Error ==> Limit of ',I6,
     +         ' precipitation values exceeded for this year.')
C#### WCH, 11/12/93
 9050 FORMAT(' WARNING!! VALUE OF IHOUR =',I3,' AND IS .GE. 24.',
     1 ' HOURS SHOULD RUN FROM 0 (MIDNIGHT) TO 23.',/,
     2' VALUE RESET TO 23, YEAR = ',I4,', MONTH =',I3,', DAY =',I3)
 9060 FORMAT(' WARNING$$ VALUE OF IHOUR =',I3,' AND SHOULD NOT BE LESS T
     1HAN 1 FOR HOUR-OPTION YOU HAVE CHOSEN.',/,' (HOURS GO FROM 1-24)',
     2'  VALUE RESET TO 1.  YEAR = ',I4,', MONTH =',I3,', DAY =',I3)
C#### WCH, 10/2/96.
 9700 FORMAT(/,' ERROR DURING READ OF UNIT',I3,/,
     1 ' LAHEY ERROR NUMBER =',I5,/,
     2 ' CURRENT VALUES OF ARRAY FF(1-6): ',6I8)
C=======================================================================
      END
