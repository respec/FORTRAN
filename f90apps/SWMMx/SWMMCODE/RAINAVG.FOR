      SUBROUTINE RAINAVG(RHOUR,RHOURO,JPJDAY,IDAY,IHROLD,
     1  MINOLD,NEND,IRAIN,M4)
C=======================================================================
C     Subroutine to spread accumulated rainfall (NCDC code = A) over
C       intervening hours.
C     Written by Wayne Huber, April 1994.
C     WCH, 5/25/94.  Check for negative rain in final time increment. 
C=======================================================================
      INCLUDE 'PRECIP.INC'
C=======================================================================
C     Compute number of steps for averaging.
C=======================================================================
      INTV = IFIX((RHOUR-RHOURO)/THISTO+0.1) + 1
      AVG  = FLOAT(IRAIN)/FLOAT(INTV)/100.
      IVGS = 0
C#### WCH, 5/25/94.  NEED TO CHECK FOR NEGATIVE LAST INCREMENT.
C     SUBSTITUTE JRAIN FOR IFIX( ) IN DO 100 LOOP BELOW.
      JRAIN = IFIX(AVG*100.0+0.4)
      IF((INTV-1)*JRAIN.GT.IRAIN) JRAIN = JRAIN - 1
      IF(JRAIN.LT.0) JRAIN = 0
C
      DO 100 I = 1,INTV
      IF(NEND.EQ.96) WRITE(M4) JPJDAY,RHOURO,THISTO,AVG*3600./THISTO
C=======================================================================
C     Store hourly totals in array HOUR.
C=======================================================================
      IF(I.LT.INTV) THEN
         IVGS = IVGS + JRAIN
         HOUR(IDAY,IHROLD) = HOUR(IDAY,IHROLD) + JRAIN
         ELSE
C=======================================================================
C     For last increment, make sure total for all HOUR values adds up 
C       to IRAIN.
C=======================================================================
         HOUR(IDAY,IHROLD) = HOUR(IDAY,IHROLD) + IRAIN - IVGS
         ENDIF
      RHOURO = RHOURO + THISTO
      IF(NEND.EQ.24) THEN
         IHROLD = IHROLD + 1
         ELSE
         MINOLD = MINOLD + 15
         IF(MINOLD.EQ.60) THEN
            MINOLD = 0
            IHROLD = IHROLD + 1
            ENDIF
         ENDIF
      IF(IHROLD.GT.24) IHROLD = 1
  100 CONTINUE
      RETURN
      END
