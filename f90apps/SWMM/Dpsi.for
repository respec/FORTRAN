      FUNCTION DPSI(ALPHA)
C	TRANSPORT BLOCK
C	CALLED FROM NEWTON NEAR LINE 21
C=======================================================================
C     FINDS DERIVATIVE OF FUNCTIONAL Q-A CURVE GIVEN A/AFULL (ALPHA).
C     NOTE, FOR NTPE = 2,10,11,12,13, SUB PSI MUST HAVE BEEN
C                                   CALLED PRIOR TO DPSI.
C     THIS WILL ALWAYS BE THE CASE IF DPSI IS CALLED ONLY FROM NEWTON.
C
C     UPDATED (NEW COMMON) BY W.C.H., SEPTEMBER 1981.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'PSIDPS.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'FLODAT.INC'
      INCLUDE 'NEW81.INC'
C=======================================================================
      NTPE = NTYPE(M)
C=======================================================================
C     SPECIAL ROUTINE FOR HIGH ACCURACY AT LOW FLOWS.
C=======================================================================
      IF(NTPE.EQ.1) THEN
                  IF(ALPHA.LE.1.0E-30) THEN
                                       DPSI = 1.0E-30
                                       RETURN
                                       ENDIF
                  ALF = ALPHA
                  GO TO 5
                  ENDIF
C=======================================================================
C     SPECIAL FUNCTIONAL FORM FOR RECTANGULAR CONDUITS.
C=======================================================================
      IF(NTPE.EQ.2) THEN
                  IF (ALPHA.LE.1.0E-30) THEN
                                        DPSI = 1.0E-30
                                        RETURN
                                        ENDIF
                  IF(ALPHA.GT.ALFMAX(NTPE)) THEN
                              DPSI = (1.0-P4(M))/(1.0-ALFMAX(NTPE))
                              RETURN
                              ENDIF
                  IF (ALPHA.LT.0.0001) THEN
                            DPSI = 1.666667*(ALPHA*P7(M))**0.6666667
                            ELSE
                            DPSI = CATH*(P6(M)*ALPHA/AAA+1.666667)
                            ENDIF
                  RETURN
                  ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR MODIFIED BASKET-HANDLE.
C=======================================================================
      IF (NTPE.EQ.10) THEN
                    IF (ALPHA.LE.1.0E-30) THEN
                                          DPSI = 1.0E-30
                                          RETURN
                                          ENDIF
                   IF(AA.GT.GEOM3(M)) THEN
                                      DPSI = (PSI(ALPHA+0.0005) -
     +                                        PSI(ALPHA-0.0005))/0.001
                                      RETURN
                                      ENDIF
                   IF (ALF.LT.0.0001) THEN
                              DPSI = P6(M)*1.666667*(ALF*
     +                               P7(M))**0.6666667*AFULL(M)/GEOM3(M)
                              RETURN
                              ELSE
                              DPSI = CATH*(-1.333333*R*ALF/AAA+
     +                                      1.666667)*AFULL(M)/GEOM3(M)
                              ENDIF
                   RETURN
                   ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR RECTANGULAR, TRIANGULAR BOTTOM.
C=======================================================================
      IF (NTPE.EQ.11) THEN
                    IF(ALPHA.LE.1.0E-30) THEN
                                         DPSI = 1.0E-30
                                         RETURN
                                         ENDIF
                    IF(AA.LE.AB) THEN
                                 DPSI = P7(M)*1.333333*ALPHA**0.3333333
                                 ELSE
                                 IF (ALPHA.GT.ALFMAX(NTPE)) THEN
                                   DPSI = (1.0-P4(M))/(1.0-ALFMAX(NTPE))
                                     RETURN
                                     ENDIF
                                 DPSI = CATH*(-0.6666667*ALPHA/AAA*
     +                            (2.0*GEOM1(M)-GEOM3(M))+1.666667)
                                 ENDIF
                    RETURN
                    ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR RECTANGULAR, ROUND BOTTOM.
C=======================================================================
      IF (NTPE.EQ.12) THEN
                    IF(ALPHA.LE.1.0E-30) THEN
                                         DPSI = 1.0E-30
                                         RETURN
                                         ENDIF
                    IF(AA.GT.P6(M)) THEN
                       IF (ALPHA.GT.ALFMAX(NTPE)) THEN
                                 DPSI = (1.0-P4(M))/(1.0-ALFMAX(NTPE))
                                 RETURN
                                 ENDIF
                       DPSI = CATH*(1.666667-1.333333*AA/GEOM2(M)/D2)
                       ELSE
                       IF(ALF.LT.0.04) GO TO 5
                       DPSI = (QNORM(1,I+1) - QNORM(1,I)) / 0.02
                       DPSI = DPSI * P7(M)
                       ENDIF
                    RETURN
                    ENDIF
C=======================================================================
C    FUNCTIONAL FORM FOR TRAPEZOID
C=======================================================================
      IF(NTPE.EQ.13) THEN
                   IF(ALPHA.LE.1.0E-30) THEN
                                        DPSI = 1.0E-30
                                        RETURN
                                        ENDIF
                    D1   = GEOM2(M) + P5(M) * AAA
                    D2   = SQRT(GEOM2(M) ** 0.5 + AA / GEOM3(M))
                    AB   = GEOM2(M) + P5(M) * GEOM1(M)
                    DRDA = AB/D1 * (1.0 - AA * P5(M) * 0.5/D2/D1)
                    DPSI = (0.6666667*ALPHA*DRDA+CATH)/CATH**0.3333333
                    RETURN
                    ENDIF
C=======================================================================
C     INCLUDE TABULAR DPSI CALC. IN CASE DPSI IS CALLED BY KLASS = 2
C     CONDUIT.
C=======================================================================
      IF(NTPE.EQ.16) THEN
                   MMM    = 26
                   KK     = NQC(M)
                   DALPHA = QCURVE(KK,2,2) - QCURVE(KK,2,1)
                   I      = IFIX(ALPHA/DALPHA + 1.0)
                   IF(I.EQ.MMM) I = I - 1
                   DPSI = (QCURVE(KK,3,I+1) - QCURVE(KK,3,I)) /
     +                    (QCURVE(KK,2,I+1) - QCURVE(KK,2,I))
                   RETURN
                   ELSE
                   MMM    = MM(NTPE)
                   DALPHA = ANORM(NTPE,2) - ANORM(NTPE,1)
                   I      = IFIX(ALPHA/DALPHA + 1.0)
                   IF(I.EQ.MMM) I = I - 1
                   DPSI = (QNORM(NTPE,I+1) - QNORM(NTPE,I)) /
     +                    (ANORM(NTPE,I+1) - ANORM(NTPE,I))
                   RETURN
                   ENDIF
C=======================================================================
C     SPECIAL ROUTINE FOR ACCUARACY AT LOW FLOWS
C=======================================================================
    5 DAL = 0.002
      A1  = ALF - 0.001
      A2  = ALF + 0.001
      IF(A1.LT.0.0) THEN
                    A1  = 0.0
                    DAL = ALF + 0.001
                    ENDIF
      CALL CIRCLE(A1,PS1,DN,1)
      CALL CIRCLE(A2,PS2,DN,1)
      DPSI = (A2 - A1) / DAL
      IF(DPSI.LE.1.0E-30) DPSI = 1.0E-30
      IF(NTPE.EQ.12) DPSI = DPSI * P7(M)
      RETURN
      END
