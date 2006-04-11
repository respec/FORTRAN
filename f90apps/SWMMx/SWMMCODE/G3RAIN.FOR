      SUBROUTINE G3RAIN(IDO,IGO,ILOST)
C#######################################################################
C     This program reads a user defined time series.
C     Updated 2/93 by WCH to correct hour values for user-supplied data.
C     Updated 11/12/93 by WCH to allow for hours to range from 1 - 24.
C#######################################################################
C     IFORM = 3 MEANS USER DEFINED FORMAT
C     IDO   = 0 SEARCH FOR STATION NUMBER
C     IDO   = 1 READ STATION RAINFALL
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'PRECIP.INC'
      INTEGER DAY,YEAR,STA,FF(6),XRAIN(24)
C=======================================================================
C     First find the correct rainfall station (IDO = 0)
C=======================================================================
      IF(IDO.EQ.0)   THEN
C=======================================================================
      IF(F1.GT.0) THEN
5              READ(IO,FIRMAT,ERR=5,END=40) (FF(I),I=1,6)
               IBUF   = FF(F1)
               NEWYR  = FF(F2)
               NEWMON = FF(F3)
               NEWDAY = FF(F4)
               IF(IBUF.NE.ISTA) GO TO 5
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
6              READ(IO,FIRMAT,ERR=6,END=40) (FF(I),I=1,5)
               NEWYR  = FF(F2)
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
      CALL SETIA(RDAY,3000,1,0)
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
                     READ(IO,FIRMAT,ERR=10,END=40) (FF(I),I=1,6),REIN
                     IF(REIN.NE.0.0) THEN
                     STA   = FF(F1)
                     YEAR  = FF(F2)
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
                     READ(IO,FIRMAT,ERR=10,END=40) REIN,(FF(I),I=1,6)
                     IF(REIN.NE.0.0) THEN
                     STA   = FF(F1)
                     YEAR  = FF(F2)
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
                     READ(IO,FIRMAT,ERR=10,END=40) (FF(I),I=1,5),REIN
                     IF(REIN.NE.0.0) THEN
                     STA   = ISTA
                     YEAR  = FF(F2)
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
                     READ(IO,FIRMAT,ERR=10,END=40) REIN,(FF(I),I=1,5)
                     IF(REIN.NE.0.0) THEN
                     STA   = ISTA
                     YEAR  = FF(F2)
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
      ENDIF
C=======================================================================
C     Read rainfall and station number for a user-defined time series.
C=======================================================================
      IF(NUVAL.GT.1) THEN
                     READ(IO,FIRMAT,ERR=10,END=40) (FF(I),I=1,3),
     +                                       (XRAIN(K),K=1,NUVAL)
                     STA   = ISTA
                     YEAR  = FF(F2)
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
      IF(STA.NE.ISTA) GO TO 40
      IF(IYEND(1).NE.0.AND.YEAR.EQ.IYEND(1).AND.MONTH.GE.IYEND(2)
     *                   .AND.DAY.GT.IYEND(3))   GO TO 40
      IF(IYEND(1).NE.0.AND.YEAR.EQ.IYEND(1).AND.MONTH.GT.IYEND(2))
     *                                           GO TO 40
      IF(IYEND(1).NE.0.AND.YEAR.GT.IYEND(1))     GO TO 40
      IF(YEAR.GT.NEWYR)                          RETURN
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
                     IF(NSTORM.EQ.LIMRN) THEN
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
 7000                CONTINUE
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
49    RETURN
40    IGO = 1
      RETURN
C=======================================================================
 9000 FORMAT(/,' Error ==> Limit of ',I6,
     +         ' precipitation values exceeded for this year.')
C#### WCH, 11/12/93
 9050 FORMAT(' WARNING!! VALUE OF IHOUR =',I3,' AND IS .GE. 24.',
     1 ' HOURS SHOULD RUN FROM 0 (MIDNIGHT) TO 23.',/, 
     2' VALUE RESET TO 23, YEAR =',I4,', MONTH =',I3,', DAY =',I3)
 9060 FORMAT(' WARNING$$ VALUE OF IHOUR =',I3,' AND SHOULD NOT BE LESS T
     1HAN 1 FOR HOUR-OPTION YOU HAVE CHOSEN.',/,' (HOURS GO FROM 1-24)',
     2'  VALUE RESET TO 1.  YEAR =',I4,', MONTH =',I3,', DAY =',I3)
C=======================================================================
      END
