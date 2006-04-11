      SUBROUTINE FINDA(PS,AA)
C     TRANSPORT BLOCK
C     CALLED BY ROUTE NEAR LINE 240
C               INITAL NEAR LINE 211
C=======================================================================
C     CALCULATES THE FLOW AREA IN CONDUITS GIVEN THE FLOW RATE.
C
C     UPDATED (NEW COMMON) BY W.C.H., SEPTMEBER 1981.
C     UPDATED 12/31/93 BY RED.  FIX DO LOOP RANGE.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'FLODAT.INC'
C=======================================================================
      AA = 0.0
      IF(PS.EQ.0.0) RETURN
      NTPE = NTYPE(M)
C=======================================================================
C     CONDUITS WITH FUNCTIONAL Q-A RELATIONSHIP.
C=======================================================================
      IF(KLASS(NTPE).EQ.1) THEN
                         C2    = -PS
                         ALPHA = 0.2
                         CALL NEWTON(ALPHA,PS,0.0,C2,KFLAG)
                         IF(KFLAG.EQ.2) THEN
                                 IF(JCE.EQ.0) WRITE(N6,910)
     +                                 TIME,N,NOE(M),A(M,1,1)
                                 IF(JCE.EQ.1) WRITE(N6,911)
     +                                 TIME,N,KOE(M),A(M,1,1)
                                 AA = A(M,1,1)
                                 ELSE
                                 AA = ALPHA*AFULL(M)
                                 ENDIF
                         RETURN
                         ENDIF
C=======================================================================
C        SPECIAL ROUTINE FOR HIGH ACCURACY AT LOW FLOWS.
C=======================================================================
      IF(NTPE.EQ.1.AND.PS.LE.0.015) THEN
                                  CALL CIRCLE(ALPHA,PS,DN,3)
                                  AA = ALPHA*AFULL(M)
                                  RETURN
                                  ENDIF
C=======================================================================
C     CONDUITS WITH TABULAR Q-A RELATIONSHIP.
C=======================================================================
      IF(NTPE.EQ.16) THEN
C=======================================================================
C     PARABOLIC, POWER FUNCTION, OR NATURAL CROSS SECTIONS
C=======================================================================
      MMM     = 26
      KK      = NQC(M)
      DALPHA  = 0.04
C#### RED (WCH), 12/31/93.  CHANGE END OF LOOP TO MMM-1 FROM MMM TO
C####                       AVOID OUT OF BOUNDS ERROR IN LINE 207.
      DO 210 I = 1,MMM-1
      IF(PS-QCURVE(KK,3,I+1)) 207,208,210
  207 ALPHA = FLOAT(I-1)*0.04 + (PS-QCURVE(KK,3,I))/(QCURVE(KK,3,I+1)
     +                        - QCURVE(KK,3,I))*DALPHA
C=======================================================================
C        IMPROVE ESTIMATE WITH PARABOLIC INTERPOLATION.
C=======================================================================
      IF(PS.LT.0.015) ALPHA = ALPHA + (PS - QCURVE(KK,3,I)) *
     +        (PS - QCURVE(KK,3,I+1)) * (DALPHA/(QCURVE(KK,3,I+2) -
     +         QCURVE(KK,3,I+1))      - DALPHA/(QCURVE(KK,3,I+1)
     +       - QCURVE(KK,3,I)))/(QCURVE(KK,3,I+2) - QCURVE(KK,3,I))
      AA = ALPHA*AFULL(M)
      RETURN
  208 ALPHA = FLOAT(I-1)*0.04
      AA    = ALPHA*AFULL(M)
      RETURN
  210 CONTINUE
      AA = ALPHA*AFULL(M)
C=======================================================================
C     OTHER TABULAR CROSS SECTIONS
C=======================================================================
      ELSE
      MMM      = MM(NTPE)
      DALPHA   = ANORM(NTPE,2) - ANORM(NTPE,1)
      DO 110 I = 1,MMM-1
      IF(PS-QNORM(NTPE,I+1)) 107,108,110
  107 ALPHA = ANORM(NTPE,I) + (PS-QNORM(NTPE,I))/(QNORM(NTPE,I+1) -
     +                        QNORM(NTPE,I))*DALPHA
C=======================================================================
C        IMPROVE ESTIMATE WITH PARABOLIC INTERPOLATION.
C=======================================================================
      IF(PS.LT.0.015) ALPHA = ALPHA + 
     1                       (PS-QNORM(NTPE,I))*(PS-QNORM(NTPE,I+1))*
     2                       (DALPHA/(QNORM(NTPE,I+2)-QNORM(NTPE,I+1))-
     3                       DALPHA/(QNORM(NTPE,I+1) -
     4                       QNORM(NTPE,I)))/
     5                       (QNORM(NTPE,I+2)-QNORM(NTPE,I))
      AA = ALPHA*AFULL(M)
      RETURN
  108 ALPHA = ANORM(NTPE,I+1)
      AA    = ALPHA*AFULL(M)
      RETURN
  110 CONTINUE
      AA = ALPHA*AFULL(M)
      ENDIF
      RETURN
C=======================================================================
  910 FORMAT (/,' ===> WARNING !! NEWTON UNABLE TO FIND AREA GIVEN FLOW.
     1TIME = ',F7.1,', TIME STEP= ',I3,', EXT. ELE. NUM.=',I10,', USE OL
     2D UPSTREAM AREA = ',F6.2)
  911 FORMAT (/,' ===> WARNING !! NEWTON UNABLE TO FIND AREA GIVEN FLOW.
     1TIME = ',F7.1,', TIME STEP= ',I3,', EXT. ELE. NUM.=',A10,', USE OL
     2D UPSTREAM AREA = ',F6.2)
C=======================================================================
      END
