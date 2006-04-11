      LOGICAL FUNCTION LGTNER(X,Y,TOL)
C
C     ******************************************************************
C
C        LOGICAL FUNCTION LGTNER (GREATER THAN NEAR)
C     THIS FUNCTION RETURNS TRUE OR FALSE (LOGICAL) ANSWERS
C     IF X IS GREATER THAN Y (BEYOND TOLERANCE TOL) ANSWER IS TRUE
C
C     ******************************************************************
C
      LGTNER=.FALSE.
      R = Y + ABS(TOL)
      IF (X.GT.R) LGTNER = .TRUE.
      RETURN
      END
