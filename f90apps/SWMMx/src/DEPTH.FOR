      FUNCTION DEPTH(ALPHA)
C=======================================================================
C     COMPUTES NORMALIZED DEPTH IN CONDUIT GIVEN
C     NORMALIZED AREA, ALPHA.   LAST UPDATED BY RED IN NOVEMBER, 1988.
C     RED, 12/31/93.  CHECK FOR RANGE OF SUBSCRIPT.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'FLODAT.INC'
      DIMENSION QI(NET),QO(NET)
      EQUIVALENCE (QO(1),Q(1,2,2)),(QI(1),Q(1,1,2))
C=======================================================================
C     KDEPTH(NT) = 1 FOR CONDUIT WITH A FUNCTIONAL D-A RELATIONSHIP.
C     KDEPTH(NT) = 2 FOR CONDUIT WITH A TABULAR D-A RELATIONSHIP.
C     KDEPTH(NT) = 3 FOR ELEMENT OTHER THAN CONDUIT.
C=======================================================================
      NT = NTYPE(M)
C=======================================================================
C     IN RECTANGULAR CONDUIT, NORMALIZED DEPTH EQUALS NORMALIZED AREA.
C=======================================================================
      IF(NT.EQ.2.OR.ALPHA.EQ.0.0) THEN
                                  DEPTH = ALPHA
                                  RETURN
                                  ENDIF
C=======================================================================
C     ROUTINE FOR TABULAR D-A CURVE.
C     LINEAR  INTERPOLATION BETWEEN TABULAR POINTS IS USED.
C=======================================================================
      IF(KDEPTH(NT).EQ.2) THEN
C=======================================================================
C     PARABOLIC, POWER FUNCTION OR NATURAL CHANNELS
C=======================================================================
      IF(NT.EQ.16) THEN
                   I     = 1 + IFIX(ALPHA/0.04)
                   IF(I.GT.25) I = 25
C#### RED (WCH), 12/31/93.  ADD ANOTHER CHECK FOR VALUE OF I.
                   IF(I.LE.0)  I = 1
                   DELTA = (ALPHA - 0.04*FLOAT(I-1))/0.04
                   M1    = NQC(M)
                   DEPTH = (QCURVE(M1,2,I) +
     +                    (QCURVE(M1,2,I+1)-QCURVE(M1,2,I))*DELTA)
                   RETURN
                   ENDIF
C=======================================================================
C     NON-CIRCULAR CONDUITS OR CIRCULAR CONDUITS WITH LARGE DEPTHS
C=======================================================================
      IF(NT.NE.1.OR.ALPHA.GE.0.04) THEN
                          DALPHA = 1.0 / (FLOAT(NN(NT)) - 1.0)
                          I      = IFIX(ALPHA/DALPHA) + 1
                          IF(I.GE.NN(NT)) THEN
                              DEPTH = DNORM(NT,I)
                              ELSE
                              DEPTH = DNORM(NT,I)+(ALPHA - ANORM(NT,I))/
     +                                DALPHA*(DNORM(NT,I+1)-DNORM(NT,I))
                              ENDIF
C=======================================================================
C                        IMPROVE ESTIMATE WITH PARABOLIC INTERPOLATION
*                                FOR NON-CIRCULAR CONDUITS.
C=======================================================================
                         IF(ALPHA.LT.0.04) DEPTH = DEPTH + (ALPHA -
     +                      ANORM(NT,I)) * (DNORM(NT,I) - 2.0 *
     +                      DNORM(NT,I+1)+DNORM(NT,I+2))/(2.0*DALPHA**2)
                         RETURN
                         ENDIF
C=======================================================================
C     SPECIAL ROUTINE FOR HIGH ACCURACY AT LOW FLOWS.
C=======================================================================
      ALF = ALPHA
      CALL CIRCLE(ALF,PS,DN,2)
                   DEPTH = DN
      IF(NT.EQ.12) DEPTH = DEPTH*2.0*GEOM3(M)/(P2(M)*DIST(M))
      RETURN
      ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR MODIFIED BASKET-HANDLE.
C=======================================================================
      IF (NT.EQ.10) THEN
                    AA = ALPHA*AFULL(M)
                    IF(AA.LE.GEOM3(M)) THEN
                          DEPTH = AA/GEOM2(M)/(GEOM1(M)+GEOM2(M)/2.0)
                          RETURN
                          ENDIF
                    ALF   = (AA-GEOM3(M)+P5(M)/2.0)/P5(M)
                    I     = IFIX(ALF/0.02) + 1
                    IF(I.EQ.51) THEN
                            DD = DNORM(1,I)
                            ELSE
                            DD    = DNORM(1,I)+(ALF-ANORM(1,I))/0.02 *
     +                              (DNORM(1,I+1)-DNORM(1,I))
                            DEPTH = ((DD-0.5)*GEOM2(M)+GEOM1(M))/
     +                              (GEOM1(M) + GEOM2(M)/2.0)
                            ENDIF
                    RETURN
                    ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR RECTANGULAR, TRIANGULAR BOTTOM.
C=======================================================================
      IF (NT.EQ.11) THEN
                    AA = ALPHA*AFULL(M)
                    AB = GEOM3(M)*GEOM2(M)/2.0
                    IF(AA-AB.LT.0.0) DEPTH = GEOM3(M)/GEOM1(M) *
     +                                       SQRT(AA/AB)
                    IF(AA-AB.EQ.0.0) DEPTH = GEOM3(M)/GEOM1(M)
                    IF(AA-AB.GT.0.0) DEPTH = (GEOM3(M) +
     +                                       (AA-AB)/GEOM2(M))/GEOM1(M)
                    RETURN
                    ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR RECTANGULAR, ROUND BOTTOM.
C=======================================================================
      IF (NT.EQ.12) THEN
                    AA = ALPHA*AFULL(M)
                    IF (AA.GT.P6(M)) THEN
                                     DD    = P2(M)*DIST(M)-GEOM1(M)
                                     DEPTH = (DD+(AA-P6(M))/GEOM2(M))/
     +                                       (P2(M)*DIST(M))
                                     RETURN
                                     ENDIF
                    ALF = ALPHA*AFULL(M)/(3.1415965*GEOM3(M)*GEOM3(M))
                    IF(ALF.LT.0.04) THEN
                           CALL CIRCLE(ALF,PS,DN,2)
                           DEPTH = DN
                           IF(NT.EQ.12) DEPTH = DEPTH*2.0*GEOM3(M)/
     +                                          (P2(M)*DIST(M))
                           RETURN
                           ENDIF
                    I     = IFIX(ALF/0.02) + 1
                    IF(I.EQ.51) THEN
                            DEPTH = DNORM(1,I)
                            ELSE
                            DEPTH = DNORM(1,I) + (DNORM(1,I+1) -
     +                              DNORM(1,I))/0.02*(ALF-ANORM(1,I))
                            ENDIF
                    DEPTH = DEPTH*2.0*GEOM3(M)/(P2(M)*DIST(M))
                    RETURN
                    ENDIF
C=======================================================================
C    FUNCTIONAL FORM FOR TRAPEZOID
C=======================================================================
      IF(NT.EQ.13) THEN
                   AA = ALPHA * AFULL(M)
                   DEPTH = (-GEOM2(M) + SQRT(GEOM2(M)**2 +
     +                     4.0 * AA/GEOM3(M))) * GEOM3(M)/2.0/GEOM1(M)
                   RETURN
                   ENDIF
      END
