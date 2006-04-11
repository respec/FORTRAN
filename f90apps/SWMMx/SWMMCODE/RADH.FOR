      FUNCTION RADH(AA)
C=======================================================================
C     FUNCTION TO COMPUTE THE HYDRAULIC RADIUS FOR A GIVEN FLOW AREA.
C
C     UPDATED (NEW COMMON) BY R.E.D., MAY, 1988.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'FLODAT.INC'
      REAL L1
C=======================================================================
C     PARAMETERS NEEDED ARE:
C
C     AA = AREA OF FLOW FOR WHICH RADH IS TO BE CALCULATED
C=======================================================================
      NT    = NTYPE(M)
      AF    = AFULL(M)
      IF(AA.LE.0.0) THEN
                    RADH = 0.0
                    RETURN
                    ENDIF
C=======================================================================
C     NO CALCULATIONS FOR NON CONDUITS
C=======================================================================
      IF(NT.GE.19) THEN
                   RADH = 0.0
                   RETURN
                   ENDIF
C=======================================================================
C     RECTANGULAR PIPE
C=======================================================================
      IF(NT.EQ.2) THEN
                  XL   = GEOM2(M)
                  DIAM = GEOM1(M)
                  RADH = AA/(XL+2.0*(AA/AF)*DIAM)
                  RETURN
                  ENDIF
C=======================================================================
C     HYDRAULIC RADIUS FOR MODIFIED BASKET HANDLE CONDUIT.
C=======================================================================
      IF(NT.EQ.10) THEN
                   DD = (GEOM1(M)+GEOM2(M)/2.0)*DEPTH(AA/AFULL(M))
                   IF(DD.LE.GEOM1(M)) THEN
                            RADH = AA/(GEOM2(M)+2.0*DD)
                            ELSE
                            CATHY = DD-GEOM1(M)
                            THETA = ASIN(2.0*CATHY/GEOM2(M))
                            PER   = 2.0*GEOM1(M)+GEOM2(M)*(1.0+THETA)
                            RADH  = AA/PER
                            ENDIF
                   RETURN
                   ENDIF
C=======================================================================
C     HYDRAULIC RADIUS FOR RECTANGULAR, TRIANGULAR BOTTOM.
C=======================================================================
      IF(NT.EQ.11) THEN
                   DD = GEOM1(M)*DEPTH(AA/AFULL(M))
                   IF(DD.LE.GEOM3(M)) THEN
                         PER   = DD/P5(M)
                         RADH  = AA/PER
                         ELSE
                         CATHY = DD-GEOM3(M)
                         PER    = 2.0*CATHY+GEOM3(M)/P5(M)
                         RADH   = AA/PER
                         ENDIF
                   RETURN
                   ENDIF
C=======================================================================
C     HYDRAULIC RADIUS FOR RECTANGULAR, ROUND BOTTOM.
C=======================================================================
      IF(NT.EQ.12) THEN
                   IF(AA.GT.P6(M)) THEN
                         CATHY = AA-P6(M)
                         PER   = 2.0*CATHY/GEOM2(M)+GEOM3(M)*P5(M)
                         RADH  = AA/PER
                         RETURN
                         ELSE
                         DIAM = 2.0*GEOM3(M)
                         AF   = 3.14159 *GEOM3(M)*GEOM3(M)
                         GO TO 105
                         ENDIF
                   ENDIF
C=======================================================================
C    HYDRAULIC RADIUS OF TRAPEZOIDAL CHANNEL
C=======================================================================
      IF(NT.EQ.13) THEN
                   YYY = (-GEOM2(M) + SQRT(GEOM2(M)**2 + 4.0 *
     +                          AA/GEOM3(M))) * GEOM3(M)/2.0
                   RADH = AA/(GEOM2(M) + YYY +P5(M))
                   RETURN
                   ENDIF
C=======================================================================
C    HYDRAULIC RADIUS OF PARABOLIC, POWER FUNCTION OR NATURAL CHANNEL
C=======================================================================
      IF(NT.EQ.16) THEN
                   RFULL           = QCURVE(M1,1,26)
                   QCURVE(M1,1,26) = 1.0
                   ASPOT           = AA/AF
                   I               = 1   + IFIX(ASPOT/0.04)
                   DELTA           = (ASPOT - 0.04*FLOAT(I-1))/0.04
                   M1              = NQC(M)
                   RADH            = (QCURVE(M1,1,I) + (QCURVE(M1,1,I+1)
     +                                          - QCURVE(M1,1,I))*DELTA)
                   RADH            = RFULL*RADH
                   QCURVE(M1,1,26) = RFULL
                   RETURN
                   ENDIF
C=======================================================================
C     CIRCULAR PIPE
C     ASSUME EQUIVALENT CIRCULAR PIPE FOR ODD SHAPES
C=======================================================================
      IF(NT.EQ.1) THEN
                  DIAM = GEOM1(M)
                  ELSE
                  DIAM = SQRT(4.0*AF/3.14159)
                  ENDIF
C=======================================================================
  105 ALPHA = AA/AF
      IF(ALPHA.GE.0.04) THEN
                        DALPHA = ANORM(1,2) - ANORM(1,1)
                        I      = IFIX(ALPHA/DALPHA) + 1
                        D1     = DNORM(1,I)+(ALPHA-ANORM(1,I))/DALPHA*
     +                           (DNORM(1,I+1)-DNORM(1,I))
                        ELSE
                        CALL CIRCLE(ALPHA,PS,D1,2)
                        ENDIF
      D1 = D1 * DIAM
      RR = DIAM/2.0
C=======================================================================
C     HERE IF FLOW IS BELOW HALF WAY MARK
C=======================================================================
      IF(D1-RR.LT.0.0) THEN
                       D2 = RR-D1
                       IF((RR-D2).LE.1.0E-5) THEN
                                             RADH = 0.0
                                             RETURN
                                             ENDIF
                       L1    = SQRT(RR**2-D2**2)
                       ARG   = L1/D2
                       THETA = 2.0*ATAN(ARG)
                       S     = RR*THETA
                       IF(S.LE.1.0E-10) THEN
                                        RADH = 0.0
                                        RETURN
                                        ENDIF
                       RADH = AA/S
                       RETURN
                       ENDIF
C=======================================================================
C     HERE IF FLOW IS AT HALF WAY MARK
C=======================================================================
      IF(D1-RR.EQ.0.0) THEN
                       RADH = AA/(3.14159*RR)
                       RETURN
                       ENDIF
C=======================================================================
C     HERE IF FLOW IS OVER HALF WAY MARK
C=======================================================================
      IF(D1-RR.GT.0.0) THEN
                       D2 = D1-RR
                       IF((RR-D2).LE.1.0E-15) THEN
                                              RADH = 0.0
                                              RETURN
                                              ENDIF
                       L1    = SQRT(RR**2-D2**2)
                       THETA = 2.0*ATAN2(L1,D2)
                       S     = RR*THETA
                       IF(S.LE.1.0E-15) THEN
                                        RADH = 0.0
                                        RETURN
                                        ENDIF
                       S     = 2.0*3.14159*RR-S
                       RADH = AA/S
                       RETURN
                       ENDIF
      END
