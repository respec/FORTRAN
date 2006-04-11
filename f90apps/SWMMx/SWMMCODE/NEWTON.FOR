      SUBROUTINE NEWTON(ALPHA,PS,C11,C2,KFLAG)
C=======================================================================
C     NEWTON-RAPHSON ITERATION ASSUMING FUNCTIONAL FORM FOR Q-A CURVE.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      DIMENSION QI(NET),QO(NET)
      EQUIVALENCE (QO(1),Q(1,2,2)),(QI(1),Q(1,1,2))
C=======================================================================
C     FIRST GUESS FOR ALPHA ASSIGNED IN CALLING PROGRAM.
C=======================================================================
      KFLAG    = 1
      HELP     = 0.0
      ICHK     = 0
   10 DO 100 I = 1,20
      PS       = PSI(ALPHA)
      D        = (PS+C11*ALPHA+C2)/(DPSI(ALPHA)+C11)
      IF(ABS(D).LE.EPSIL) THEN
                          IF(ALPHA.GE.0.0.AND.ALPHA.LE.1.0) RETURN
                          GO TO 15
                          ENDIF
      ALPHA = ALPHA - D
      IF(ALPHA.LT.0.0.OR.ALPHA.GT.1.0.OR.ICHK.EQ.1) GO TO 15
  100 CONTINUE
      ALPHA = ALPHA + D/2.0
      ICHK  = 1
      GO TO 10
C=======================================================================
C     IF NEED BE, ASSIGN NEW VALUES TO ALPHA BY MARCHING ALONG ABSCISSA.
C=======================================================================
   15 ALPHA = HELP
      HELP  = HELP+0.05
      ICHK  = 0
      IF(HELP.LE.1.05) GO TO 10
C=======================================================================
C     ITERATION DOES NOT CONVERGE. RETURN TO MAIN FOR ERROR MESSAGE.
C=======================================================================
      KFLAG = 2
      RETURN
      END
