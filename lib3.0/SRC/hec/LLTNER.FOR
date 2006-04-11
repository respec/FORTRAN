      LOGICAL FUNCTION LLTNER(X,Y,TOL)
C
C     ******************************************************************
C
C            LOGICAL FUNCTION LLTNER (LESS THAN NEAR)
C     THIS FUNCTION RETURNS TRUE OR FALSE (LOGICAL) ANSWERS
C     IF X IS LESS THAN Y (BEYOND TOLERANCE TOL) ANSWER IS TRUE
C
C     ******************************************************************
C
      LLTNER=.FALSE.
      R = Y - ABS(TOL)
      IF (X.LT.R) LLTNER = .TRUE.
      RETURN
      END
