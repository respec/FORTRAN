      FUNCTION PSI(ALPHA)
C=======================================================================
C     FINDS Q/QFULL (PSI) GIVEN A/AFULL (ALPHA) FOR FUNCTIONAL Q-A CURVE
C
C     UPDATED (NEW COMMON) BY W.C.H., SEPTEMBER 1981.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'PSIDPS.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'FLODAT.INC'
C=======================================================================
      PSI = 0.0
      IF(ALPHA.LE.0.0) RETURN
      NT  = NTYPE(M)
C=======================================================================
C     CALL SPECIAL ROUTINE TO GET HIGH ACCURACY AT LOW FLOWS
C=======================================================================
      IF(NT.EQ.1) THEN
                  ALF = ALPHA
                  CALL CIRCLE(ALF,PS,DN,1)
                  PSI = PS
                  RETURN
                  ENDIF
C=======================================================================
C     SPECIAL FUNCTIONAL FORM FOR RECTANGULAR CONDUITS.
C=======================================================================
      IF(NT.EQ.2) THEN
                  R = P5(M)
                  IF(ALPHA.GT.ALFMAX(NT)) THEN
                           PSI = P4(M)+(ALPHA-ALFMAX(NT))*
     +                           (1.0-P4(M))/(1.0-ALFMAX(NT))
                           RETURN
                           ENDIF
                  AAA = 2.0*R*ALPHA+1.0
                  CATH = (ALPHA*P7(M)/AAA)**0.6666667
                  PSI  = ALPHA*CATH
                  RETURN
                  ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR MODIFIED BASKET-HANDLE.
C=======================================================================
      IF(NT.EQ.10) THEN
                   AA = ALPHA*AFULL(M)
                   IF (AA.GT.GEOM3(M)) THEN
                          RH = RADH(AA)
                          PSI = 1.49/ROUGH(M)*AA*RH**0.6666667/P1(M)
                          RETURN
                          ENDIF
                   ALF  = AA/GEOM3(M)
                   R    = GEOM1(M)/GEOM2(M)
                   AAA  = 2.0*ALF*R+1.0
                   CATH = (ALF*P7(M)/AAA)**0.6666667*P6(M)
                   PSI  = ALF*CATH
                   RETURN
                   ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR RECTANGULAR, TRIANGULAR BOTTOM.
C=======================================================================
      IF(NT.EQ.11) THEN
                   AB = GEOM3(M)*GEOM2(M)/2.0
                   AA = ALPHA*AFULL(M)
                   IF (AA.LE.AB) THEN
                                 PSI = P7(M)*ALPHA**1.333333
                                 RETURN
                                 ENDIF
                   IF (ALPHA.GT.ALFMAX(NT)) THEN
                           PSI = P4(M)+(ALPHA-ALFMAX(NT))*
     +                           (1.0-P4(M))/(1.0-ALFMAX(NT))
                           RETURN
                           ENDIF
                   AAA = GEOM3(M)/P5(M)-GEOM3(M)+(2.0*GEOM1(M) -
     +                   GEOM3(M))*ALPHA
                   CATH = (ALPHA*P6(M)/AAA)**0.6666667
                   PSI = ALPHA*CATH
                   RETURN
                   ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR RECTANGULAR, ROUND BOTTOM.
C=======================================================================
      IF(NT.EQ.12) THEN
                   AA = ALPHA*AFULL(M)
                   IF(AA.GT.P6(M)) THEN
                            IF (ALPHA.GT.ALFMAX(NT)) THEN
                                PSI = P4(M)+(ALPHA-ALFMAX(NT))*
     +                                (1.0-P4(M))/(1.0-ALFMAX(NT))
                                RETURN
                                ENDIF
                            D1 = GEOM3(M)*P5(M)+2.0*GEOM1(M)+GEOM2(M)
                            D2 = GEOM3(M)*P5(M)+2.0/GEOM2(M)*
     +                           (AFULL(M)*ALPHA-P6(M))
                            CATH = (ALPHA*D1/D2)**0.6666667
                            PSI  = ALPHA*CATH
                            RETURN
                            ENDIF
                   ALF = ALPHA*AFULL(M)/(3.1415965*GEOM3(M)*GEOM3(M))
                   IF(ALF.LT.0.04) THEN
                                   CALL CIRCLE (ALF,PS,DN,1)
                                   PSI = PS
                                   PSI = PSI*P7(M)
                                   RETURN
                                   ENDIF
                   I = IFIX(ALF/0.02) + 1
                   PSI = QNORM(1,I)+(QNORM(1,I+1)-QNORM(1,I))/0.02*
     +                              (ALF-ANORM(1,I))
                   PSI = PSI*P7(M)
                   RETURN
                   ENDIF
C=======================================================================
C    FUNCTIONAL FORM FOR TRAPEZOID
C=======================================================================
      IF(NT.EQ.13) THEN
                   AA   = ALPHA * AFULL(M)
                   AAA  = (-GEOM2(M) + SQRT(GEOM2(M)**2 +
     +                     4.0 * AA/GEOM3(M))) * 0.5 * GEOM3(M)
                   CATH =  AA/(GEOM2(M) + AAA * P5(M))/P6(M)
                   PSI  = ALPHA * CATH**0.6666667
                   RETURN
                   ENDIF
C=======================================================================
C     INCLUDE TABULAR PSI CALC. IN CASE PSI IS CALLED BY KLASS=2 CONDUIT
C=======================================================================
      IF(NT.EQ.16) THEN
                   KK = NQC(M)
                   DALPHA = QCURVE(KK,2,2) - QCURVE(KK,2,1)
                   I      = IFIX(ALPHA/DALPHA + 1.0)
                   IF(I.GE.26) I = 25
                   PSI    = QCURVE(KK,3,I) + (QCURVE(KK,3,I+1) -
     +                      QCURVE(KK,3,I)) / DALPHA *
     +                     (ALPHA - QCURVE(KK,2,I))
                   ELSE
                   DALPHA = ANORM(NT,2) - ANORM(NT,1)
                   I      = IFIX(ALPHA/DALPHA + 1.0)
                   IF(I.GE.26) I = 25
                   PSI    = QNORM(NT,I) + (QNORM(NT,I+1) -
     +                      QNORM(NT,I))/DALPHA*(ALPHA-ANORM(NT,I))
                   ENDIF
      RETURN
      END
