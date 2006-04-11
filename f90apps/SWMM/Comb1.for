      SUBROUTINE COMB1(ICOMB)
C	COMBINE BLOCK
C=======================================================================
C     COMB1 does the analysis for ICOMB options 3, 5, 6 and 7.
C     WCH, 11/30/93.  Fix units for quality totals and modify time step
C       multiplication during summations. 
C     WCH, 8/1/95.  Change precip. station ID to character.
C     WCH, 7/25/96.  Add option to create ASCII rainfall interface file.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'COMB.INC'
C#### WCH, 8/1/95.  MAKE JSTA A CHARACTER VARIABLE.
C####      DIMENSION JSTA(10),PCONV(10),SUMM(11)
CIMT change dimensions from 10 to MQUAL, and 11 to MQUAL+1
      DIMENSION PCONV(MQUAL),SUMM(MQUAL+1)
C#### WCH, 7/25/96.
      CHARACTER*60 NEWFIL
      CHARACTER JOHNNY*10,NOTED(3)*10,MOTED(3)*10, JSTA(10)*8
      DATA NOTED/'  Pounds  ',' Quantity ',' Unit*cf  '/
C#### WCH, 11/30/93.  CHANGE TO:             Unit*cum and Unit*cf.
      DATA MOTED/' Kilograms',' Quantity ',' Unit*cum '/
      DATA JOHNNY/' ---------'/
C=======================================================================
C#### WCH, 7/25/96.  Open formatted file on JOUT (= NEXT).
C=======================================================================
      IF(ICOMB.EQ.7) THEN
               INQUIRE(NEXT,NAME=NEWFIL)
               CLOSE(NEXT)
               OPEN(NEXT,FILE=NEWFIL,FORM='FORMATTED',STATUS='UNKNOWN')
               ENDIF
C=======================================================================
C===> Read interface file headers (ICOMB=3)
C=======================================================================
      IF(ICOMB.EQ.3) THEN
                     IF(LAST.GT.0) CALL INFACE(1,LAST)
                     RETURN
                     ENDIF
C=======================================================================
C===> Read file headers and calculate simple statistics (ICOMB=5).
C=======================================================================
      IF(ICOMB.EQ.5) THEN
                     METRIC = 1
                     XTIME  = 0.0
                     IF(LAST.GT.0) CALL INFACE(1,LAST)
                     IF(QCONV.NE.1.0) METRIC = 2
                     DO 7990 I = 1,LOCATS
C=======================================================================
C#### WCH, 11/30/93. Fix U.S. conversion factors.
C                        MULT BY SEC AND
C     NDIM  CONC  LOAD     CONVERT TO    PCONV 
C       0   MG/L CFS*MG/L     LB        62.425E-6 = 28.316*2.2046/1E6     
C       1    X/L CFS*X/L       X         28.316
C       2     X  CFS*X       CF*X         1.0
C=======================================================================
                     IF(NQUAL.GT.0) THEN
                         DO 7995 J  = 1,NQUAL
                            IF(METRIC.EQ.1) THEN
                               PCONV(J) = 62.425E-6
                               IF(NDIM(J).EQ.1) PCONV(J) = 28.316
                               ELSE
C=======================================================================
C#### WCH, 11/30/93.  Fix metric conversion factors.
C                        MULT BY SEC AND
C     NDIM  CONC  LOAD     CONVERT TO        PCONV 
C       0   MG/L CMS*MG/L     KG             0.001
C       1    X/L CMS*X/L       X             1000.
C       2     X  CMS*X       CUM*X            1.0
C=======================================================================
                               PCONV(J) = 0.001
                               IF(NDIM(J).EQ.1) PCONV(J) = 1000.0
                               ENDIF
                         IF(NDIM(J).GE.2) PCONV(J) = 1.0
 7995                    POLL2(J,I) = 0.0
                         ENDIF
 7990                QO2(I)    = 0.0
C=======================================================================
C                    Read the interface file.
C=======================================================================
                     ITEST     = 0
                     DELT1     = 0.0
                     DO 8888 K = 1,1000000
                     IF(NQUAL.LE.0) READ(LAST,END=8100) JDAY,TMDAY,
     +                              DELT2,(QO1(I),I=1,LOCATS)
                     IF(NQUAL.GT.0) READ(LAST,END=8100) JDAY,TMDAY,
     +                              DELT2,(QO1(I),(POLL1(J,I),
     +                              J=1,NQUAL),I=1,LOCATS)
                     JYEAR = JDAY/1000
                     IF (JYEAR.LT.100) THEN
                     JDAY = JDAY-JYEAR*1000
                     JYEAR = JYEAR + 1900
                     JDAY = JDAY + JYEAR*1000
                     ENDIF
C=======================================================================
C                    Calculate cumulative volumes and loads
C                    using trapezoidal integration.
C=======================================================================
                     XTIME         = XTIME + DELT2
C=======================================================================
C#### WCH, 11/30/93
C   Because time steps are variable from the Runoff Block there is the
C   possibility of a long, DRY time step, at end of which is a non-zero
C   runoff value although wouldn't expect this to happen.  (Would
C   expect zero at the end of DRY, followed by a WET time step and a
C   non-zero value.  However, this doesn't always happen.)  Thus:
C   Whenever a non-zero flow follows a zero flow, use the time step
C   associated with the non-zero flow for that time step only.  This
C   will usually mean that the first non-zero flow will be multiplied
C   by WET to get the volume, not DRY or WETDRY.  For continuous flows,
C   use average of old and new time step as before.
C=======================================================================
C   First, check to see if there is flow during this time step, then
C   use average or just new DELT2, depending on whether there was flow
C   last time step (ITEST = 1).
C=======================================================================
                     DO 8003 I = 1,LOCATS
                     IF(QO1(I).NE.0.0) THEN
                          POOP = 0.5*(DELT1 + DELT2)
                          IF(ITEST.EQ.0) POOP = DELT2
                          GO TO 8004
                          ENDIF
 8003                CONTINUE
C=======================================================================
C   Here no flow, so skip calculations.
C=======================================================================
                     ITEST = 0
                     GO TO 8888
C#### WCH, 11/30/93.  DON'T THINK SHOULD MULT DT BY 2 HERE.
C####                     IF(ITEST.EQ.0) POOP = 2.0*DELT2
 8004                ITEST         = 1
                     DO 8000 I     = 1,LOCATS
                     IF(NQUAL.GT.0) THEN
                        DO 8005 J  = 1,NQUAL
C#### WCH, 11/30/93.  MULT BY PCONV JUST ONCE, LATER.
 8005                   POLL2(J,I) = POLL2(J,I)+POLL1(J,I)*POOP
                        ENDIF
C#### WCH, 11/30/93          IF(QO1(I).NE.0.0) ITEST = 1
                     QO2(I)        = QO2(I) + QO1(I)*POOP
 8000                CONTINUE
                     DELT1         = DELT2
C#### WCH, 11/30/93          IF(ITEST.EQ.0) DELT1 = 0.0
 8888                CONTINUE
 8100                CONTINUE
C=======================================================================
C                    Write junction/inlet/manhole summaries.
C=======================================================================
                     NPOINT = K - 1
                                    IEND = 1
                     IF(NQUAL.GT.0) IEND = NQUAL + 1
                     IF(NQUAL.EQ.0) WRITE(N6,8290)
                     IF(NQUAL.GT.0) WRITE(N6,8290) (PNAME(J),J=1,NQUAL)
                     IF(METRIC.EQ.1.AND.NQUAL.EQ.0) WRITE(N6,8295)
                     IF(METRIC.EQ.1.AND.NQUAL.GT.0) WRITE(N6,8295)
     +                            (NOTED(NDIM(J)+1),J=1,NQUAL)
                     IF(METRIC.EQ.2.AND.NQUAL.EQ.0) WRITE(N6,8296)
                     IF(METRIC.EQ.2.AND.NQUAL.GT.0) WRITE(N6,8296)
     +                            (MOTED(NDIM(J)+1),J=1,NQUAL)
                     WRITE(N6,8297) (JOHNNY,J=1,IEND)
                     XT        = XTIME/3600.0
CIMT change upper limit of loop from 11 to MQUAL + 1
                     DO 8190 I = 1,MQUAL+1
 8190                SUMM(I)   = 0.0
                     DO 8200 I = 1,LOCATS
                     IF(NQUAL.GT.0) THEN
                                    DO 8205 J  = 1,NQUAL
C#### WCH, 11/30/93.  DO QUALITY CONVERSIONS HERE.
                                    POLL2(J,I) = POLL2(J,I) * PCONV(J)
 8205                               SUMM(J+1)  = SUMM(J+1) + POLL2(J,I)
                                    ENDIF
                     SUMM(1)   = SUMM(1) + QO2(I)
                     IF(XTIME.EQ.0.0) THEN
                                      XTIME = 1.0
                                      WRITE(N6,8301)
                                      ENDIF
                     IF(JCE.EQ.0.AND.NQUAL.EQ.0) WRITE(N6,8300) NLOC(I),
     +                                             QO2(I)/XTIME,QO2(I)
                     IF(JCE.EQ.0.AND.NQUAL.GT.0) WRITE(N6,8300) NLOC(I),
     +                        QO2(I)/XTIME,QO2(I),(POLL2(J,I),J=1,NQUAL)
                     IF(JCE.EQ.1.AND.NQUAL.EQ.0) WRITE(N6,8305) KAN(I),
     +                                             QO2(I)/XTIME,QO2(I)
                     IF(JCE.EQ.1.AND.NQUAL.GT.0) WRITE(N6,8305) KAN(I),
     +                       QO2(I)/XTIME,QO2(I),(POLL2(J,I),J=1,NQUAL)
 8200                CONTINUE
C=======================================================================
C                    Write the overall summary.
C=======================================================================
                     WRITE(N6,8310) (JOHNNY,J=1,IEND)
C#### WCH, 11/30/93.  CHECK FOR ZERO AREA. NOTE, TRIBA ALWAYS IN ACRES.
                     TERRA = 0.0
                     IF(TRIBA.GT.0.0) TERRA = SUMM(1)/(3630.0*TRIBA)
     1                                         *QCONV
                     IF(METRIC.EQ.2) TERRA = 25.4*TERRA
                     IF(NQUAL.EQ.0) WRITE(N6,8315) TERRA,SUMM(1)
                     IF(NQUAL.GT.0) WRITE(N6,8315) TERRA,SUMM(1),
     +                              (SUMM(J+1),J=1,NQUAL)
                     WRITE(N6,8311) (JOHNNY,J=1,IEND)
                     IF(METRIC.EQ.1.AND.NQUAL.EQ.0) WRITE(N6,8316)
                     IF(METRIC.EQ.1.AND.NQUAL.GT.0) WRITE(N6,8316)
     +                            (NOTED(NDIM(J)+1),J=1,NQUAL)
                     IF(METRIC.EQ.2.AND.NQUAL.EQ.0) WRITE(N6,8317)
                     IF(METRIC.EQ.2.AND.NQUAL.GT.0) WRITE(N6,8317)
     +                            (MOTED(NDIM(J)+1),J=1,NQUAL)
                     WRITE(N6,8320) XT,NPOINT
                     RETURN
                     ENDIF
C=======================================================================
C===> Read Rain Block interface file (ICOMB = 6 or 7)
C=======================================================================
      IF(ICOMB.EQ.6.OR.ICOMB.EQ.7) THEN
C=======================================================================
C                    Read the precipitation station number.
C=======================================================================
                     REWIND LAST
                     READ(LAST,ERR=335) NSTA,MRAIN,(JSTA(I),I=1,NSTA)
                     WRITE(N6,2115) NSTA
                     WRITE(N6,2120) (I,JSTA(I),I=1,NSTA)
C#######################################################################
C#### WCH, 7/25/96.  Write headers for ASCII file.
C=======================================================================
                     IF(ICOMB.EQ.7) THEN 
                        WRITE(NEXT,9600) NSTA,MRAIN,(I,JSTA(I),I=1,NSTA)
                        WRITE(NEXT,9605) (I,I=1,NSTA)
                        ENDIF
C=======================================================================
C                    Calculate the total precipitation.
C=======================================================================
                     DO 324 I = 1,NSTA
  324                QO1(I)   = 0.0
C#### WCH, 7/25/96
                     JFYL     = 0
                     WRITE(*,9615)
                     DO 325 I = 1,MRAIN
                     READ(LAST,END=330,ERR=330) JDAY,TMDAY,
     +                             THISTO,(QO2(J),J=1,NSTA)
                     JYEAR = JDAY/1000
                     IF (JYEAR.LT.100) THEN
                     JDAY = JDAY - JYEAR*1000
                     JYEAR = JYEAR + 1900
                     JDAY = JDAY + JYEAR*1000
                     ENDIF
                     IF(I.EQ.1) THEN
                                JFIRST = JDAY
                                TFIRST = TMDAY
                                ENDIF
C#### WCH, 7/25/96.
                     IF(ICOMB.EQ.7) WRITE(NEXT,9610) JDAY,
     +                      TMDAY/3600.,THISTO,(QO2(J),J=1,NSTA)
C#### WCH, 7/25/96.  ADD ON-SCREEN PRINT OF YEAR.
                     JFY      = JDAY/1000
                     IF(JFY.GT.JFYL) THEN
                          WRITE(*,9620) JFY
                          JFYL = JFY
                          ENDIF
                     DO 326 J = 1,NSTA
  326                QO1(J)   = QO1(J)   + QO2(J)*THISTO/3600.0
  325                CONTINUE
  330                CONTINUE
                     WRITE(N6,9550) I,JFIRST,TFIRST,JDAY,TMDAY
                     DO 327 J = 1,NSTA
  327                WRITE(N6,9555) JSTA(J),QO1(J)
                     RETURN
  335                WRITE(N6,9500)
                     RETURN
                     ENDIF
C=======================================================================
 2115 FORMAT(//,
     +' ########################################################',/,
     +' #  Precipitation output created using the Rain Block.  #',/,
     +' #  Number of precipitation stations...',I9,'        #',/,
     +' ########################################################',/)
C#### WCH, 8/1/95.  CHANGE I13 TO A13.
 2120 FORMAT(' Location Station number',/,
     +       ' -------- --------------',/,
     +       10(I9,'. ',A13,/))
 8290 FORMAT(/,
     +' ################################################',/
     +' #   Simple flow statistics from interface file #',/,
     +' ################################################',//,
     +'  Location #   Mean Flow  Total Flow ',99(2X,A8,2X))
 8295 FORMAT('                  cfs      cubic ft. ',
     +    10(2X,A10))
 8296 FORMAT('                  cms     cubic met. ',
     +    10(2X,A10))
 8297 FORMAT('  ----------   ---------',11(2X,A10))
 8301 FORMAT(/,' ===> Error !! Total time was 0.0 hours.')
 8300 FORMAT(1X,I10,1X,F12.5,11(1PE12.4))
 8305 FORMAT(2X,A10,F12.5,11(1PE12.4))
 8310 FORMAT(
     +'  ----------  ----------',11(2X,A10))
 8311 FORMAT(
     +'                --------',11(2X,A10))
 8315 FORMAT(' Total       ',F11.4,11(1PE12.4))
 8316 FORMAT('                  inches   cubic ft. ',
     +    10(2X,A10))
 8317 FORMAT('             millimeters  cubic met. ',
     +    10(2X,A10))
 8320 FORMAT(//,' Total simulation time ===> ',F20.4,' hours',//,
     +          ' Total number of steps ===> ',10X,I10)
 9500 FORMAT(/,' ===> Error !!  This file is probably not a Rain',
     +' or Temp Block interface file.')
 9550 FORMAT(//,' Total number of precipitation data points....',I12,/,
     +      ' First Julian day.............................',I12,/,
     +      ' First Julian day start time (seconds)........',F12.1,/,
     +      ' Last Julian day..............................',I12,/,
     +      ' Last Julian day start time (seconds).........',F12.1,//)
C#### WCH, 8/1/95.  CHANGE I12 TO A12.
 9555 FORMAT(
     +' ###############################################',/,
     +' # Station......................................',A12,/,
     +' # Total Precipitation (inches or millimeters)..',F12.3,/,
     +' ###############################################')
C#### WCH, 7/25/96.
 9600 FORMAT(' Rain Block Interface File',/,
     1 ' Number of stations =',I3,/,
     2 ' Max. possible number of time steps (MRAIN) =',I8,/,
     3 ' Precipitation stations:',/,
     4 ' Number     Station',/,
     5 ' ------   ----------',/,(I6,4X,A10))
 9605 FORMAT(/,' Precipitation intensity will have units of in/hr or mm/
     1hr,',/,' depending on U.S. or metric units in Rain Block.',//,
     2' Julian    Rainfall     Time',/,
     3'   Day    Start Time  Interval  Intensities for station no.:',/,
     4'            (hour)      (sec)',10I8)
 9610 FORMAT(I7,F13.3,F10.1,10F8.3)
 9615 FORMAT(/)
 9620 FORMAT('+Processing precipitation data for year ',I4)
C=======================================================================
      END
