      LOGICAL FUNCTION LGENER(X,Y,TOL)
C
C     ******************************************************************
C
C           LOGICAL FUNCTION LGENER  (GREATER OR EQUAL NEAR)
C     THIS FUNCTION RETURNS TRUE OR FALSE (LOGICAL) ANSWERS
C     IF X IS GREATER THAN OR EQUAL TO Y (WITHIN TOLERANCE TOL)
C     ANSWER IS TRUE
C
C     ******************************************************************
C
      LGENER=.FALSE.
      R = Y - ABS(TOL)
      IF (X.GE.R) LGENER = .TRUE.
      RETURN
      END
