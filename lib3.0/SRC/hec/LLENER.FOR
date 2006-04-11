      LOGICAL FUNCTION LLENER(X,Y,TOL)
C
C     ******************************************************************
C
C        LOGICAL FUNCTION LLENER  (LESS OR EQUAL NEAR)
C     THIS FUNCTION RETURNS TRUE OR FALSE (LOGICAL) ANSWERS
C    IF X IS LESS THAN OR EQUAL Y (WITHIN TOLERANCE TOL) ANSWER IS TRUE
C
C     ******************************************************************
C
      LLENER=.FALSE.
      R = Y + ABS(TOL)
      IF (X.LE.R) LLENER = .TRUE.
      RETURN
      END
