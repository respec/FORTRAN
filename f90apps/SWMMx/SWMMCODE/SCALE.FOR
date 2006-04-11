      SUBROUTINE SCALE (ARRAY,AXLEN,NPTS,INC,NPLOT,KOKAY,ALPLOT)
C=======================================================================
C     Scale the data points.
C=======================================================================
      INCLUDE 'TAPES.INC'
      DIMENSION ARRAY(4),INT(5)
      CHARACTER ALPLOT*10
      DATA INT/2,4,5,8,10/
C=======================================================================
      INCT   = IABS(INC)
      KOKAY  = 0
C=======================================================================
C     SCAN FOR MAX AND MIN
C=======================================================================
      AMAX    = ARRAY(1)
      AMIN    = ARRAY(1)
      DO 10 N = 1,NPTS,INCT
      IF(AMAX.LT.ARRAY(N)) AMAX = ARRAY(N)
      IF(AMIN.GT.ARRAY(N)) AMIN = ARRAY(N)
   10 CONTINUE
C=======================================================================
C     RESET MAX AND MIN FOR ZERO RANGE
C=======================================================================
      IF(AMIN.EQ.0.0.AND.AMAX.EQ.0.0) THEN
                                      IF(JCE.EQ.0) WRITE(N6,130)  NPLOT
                                      IF(JCE.EQ.1) WRITE(N6,131) ALPLOT
                                      KOKAY = 1
                                      RETURN
                                      ENDIF
      IF(AMAX-AMIN.LT.0.001*AMIN) THEN
                                  IF(AMIN.GE.0.0) THEN
                                          AMIN = 0.99 * AMIN
                                          AMAX = 1.01 * AMAX
                                          ELSE
                                          AMAX = 0.0
                                          AMIN = 1.01 * AMIN
                                          ENDIF
                                  ENDIF
C=======================================================================
C     COMPUTE UNITS/INCH
C=======================================================================
      RATE = (AMAX-AMIN)/AXLEN
C=======================================================================
C     SCALE INTERVAL TO LESS THAN 10
C=======================================================================
      A = ALOG10(RATE)
      N = A
      IF(A.LT.0) N = A-0.9999
      RATE = RATE/(10.0**N)
      L    = RATE+1.00
C=======================================================================
C     FIND NEXT HIGHER INTERVAL
C=======================================================================
   60 DO 70 I = 1,5
      J       = I
      IF(L-INT(I)) 80,80,70
   70 CONTINUE
C=======================================================================
C     L IS NEXT HIGHER INTERVAL
C     RANGE IS SCALED BACK TO FULL SET
C=======================================================================
   80 L     = INT(J)
      RANGE = FLOAT(L)*10.**N
      IF(INC.LT.0) GO TO 110
C=======================================================================
C     SET UP POSITIVE STEPS
C=======================================================================
      K                = AMIN/RANGE
      IF(AMIN.LT.0.) K = K-1
C=======================================================================
C     CHECK FOR MAX VALUE IN RANGE
C=======================================================================
      IF(AMAX.LE.(K+AXLEN)*RANGE) THEN
                                  I        = NPTS*INCT+1
                                  ARRAY(I) = K*RANGE
                                  I        = I+INCT
                                  ARRAY(I) = RANGE
                                  RETURN
                                  ENDIF
C=======================================================================
C     IF OUTSIDE RANGE RESET L AND N
C=======================================================================
   90 L = L+1
      IF(L.LT.11) GO TO 60
      L = 2
      N = N+1
      GO TO 60
C=======================================================================
C     SET UP NEGATIVE STEPS
C=======================================================================
  110 K = AMAX/RANGE
      IF(AMAX.GT.0.0) K = K+1
      IF(AMIN.LT.(K+AXLEN)*RANGE) GO TO 90
      I        = INCT*NPTS+1
      ARRAY(I) = K*RANGE
      I        = I+INCT
      ARRAY(I) = -RANGE
C=======================================================================
  130 FORMAT(//,
     +' RANGE AND SCALE ARE ZERO ON PLOT ATTEMPT FOR LOCATION: ',I9)
  131 FORMAT(//,
     +' RANGE AND SCALE ARE ZERO ON PLOT ATTEMPT FOR LOCATION: ',A10)
C=======================================================================
      RETURN
      END
