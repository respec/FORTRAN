      SUBROUTINE FIRST(MI,LYDIA)
C     TRANSPORT BLOCK
C     CALLED BY INTRAN NEAR LINE 797
C=======================================================================
C     ROUTINE PERFORMS INITIAL CALCULATIONS FOR EACH SEWER ELEMENT.
C     LYDIA=0, FIRST IS BEING CALLED AT BEGINNING OF PROGRAM.
C     LYDIA=1, FIRST IS BEING CALLED FOR A SINGLE ELEMENT, MI.
C     LYDIA=2, FIRST IS BEING CALLED AT END OF PROGRAM.
C
C     UPDATED (NEW COMMON) BY W.C.H., SEPTEMBER 1981.
C     WCH (CDM - CHUCK MOORE), 8/93.  CHECK FOR ZERO DIVIDE AND UPDATE
C       FOR TYPE 26 FLOW DIVIDER.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'NAMES.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'TST.INC'
      INCLUDE 'NEWTR.INC'
      INCLUDE 'FLODAT.INC'
      DIMENSION SURGE1(NET),SURGE2(NET),WELL2(NET)
      CHARACTER BLANK*4,BMJ*10
C
      EQUIVALENCE (WELL2(1),ROUGH(1))
      EQUIVALENCE (SURGE1(1),P1(1)),(SURGE2(1),P2(1))
      DATA BLANK/'    '/
C=======================================================================
C     CALCULATE AREAS AND MAX. FLOWS USING MANNING EQN. AND GEOMETRY.
C     ALSO CALCULATE CERTAIN PARAMETERS USED IN ROUTING COMPUTATIONS.
C
C     NTYPE=1 DENOTES CIRCULAR CONDUIT.
C     NTYPE=2 DENOTES RECTANGULAR CONDUIT.
C     NTYPE=3 DENOTES PHILLIPS STANDARD EGG-SHAPED CONDUIT.
C     NTYPE=4 DENOTES BOSTON HORSESHOE CONDUIT.
C     NTYPE=5 DENOTES GOTHIC CONDUIT.
C     NTYPE=6 DENOTES CATENARY CONDUIT.
C     NTYPE=7 DENOTES LOUISVILLE SEMI-ELLIPTIC CONDUIT.
C     NTYPE=8 DENOTES BASKET-HANDLE CONDUIT.
C     NTYPE=9 DENOTES SEMI-CIRCULAR CONDUIT.
C     NTYPE=10 DENOTES MODIFIED BASKET-HANDLE CONDUIT.
C     NTYPE=11 DENOTES RECTANGULAR CONDUIT, TRIANGULAR BOTTOM.
C     NTYPE=12 DENOTES RECTANGULAR CONDUIT, ROUND BOTTOM.
C     NTYPE=13 DENOTES TRAPEZOIDAL CHANNEL.
C     NTYPE=14 DENOTES PARABOLIC CHANNEL.
C     NTYPE=15 DENOTES POWER FUNCTION CHANNEL.
C     NTYPE=16 NATURAL CHANNELS.
C     NTYPE=17,18 ARE USER SUPPLIED CONDUIT SHAPES.
C     NTYPE=19 DENOTES MANHOLE.
C     NTYPE=20 DENOTES LIFT STATION.
C     NTYPE=21 DENOTES TYPE 21 FLOW DIVIDER.
C     NTYPE=22 DENOTES STORAGE UNIT.
C     NTYPE=23 DENOTES TYPE 23 FLOW DIVIDER.
C     NTYPE=24 DENOTES TYPE 24 FLOW DIVIDER.
C     NTYPE=25 DENOTES BACKWATER ELEMENT.
C     NTYPE=26 DENOTES TYPE 26 TABULAR FLOW DIVIDER
C     NTYPE=27 DENOTES TYPE 27 QUALITY FLOW SPLIT (HANDLED AS TYPE 19)
C=======================================================================
      K = 0
      IF(LYDIA.EQ.1) MSAVE = M
      DO 200 IM = 1,NE
      M         = IM
      IF(LYDIA.EQ.1) M = MI
      NTPE               = NTYPE(M)
C#### WCH (CDM), 8/93.  UPDATE FOR TYPE 26 FLOW DIVIDER.
CIMQP   UPDATE FOR TYPE 27 QUALITY DIVIDER
C NTPE=      1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16 17
      GO TO(10,20,10,10,10,10,10,10,10,100,110,120,130,140,140,140,10,
C      18  19  20  21  22  23  24  25  26  27
     1 10,190,160,190,190,170,190,190,190,190),NTPE
C=======================================================================
C     CALCULATIONS FOR SHAPES DESCRIBED BY TABULAR Q-A RELATIONSHIPS.
C     GEOM1(M) = MAXIMUM VERTICAL DIMENSION FOR THESE SHAPES.
C     UNITS OF FEET AND SECONDS ARE USED THROUGHOUT.
C=======================================================================
   10 AFULL(M) = AFACT(NTPE)*GEOM1(M)*GEOM1(M)
      P1(M)    = 1.486/ROUGH(M)*AFULL(M)*(RFACT(NTPE)*GEOM1(M))**
     1                                                         0.6666667
      P2(M)    = GEOM1(M)/DIST(M)
      P4(M)    = PSIMAX(NTPE)
      GO TO 150
C=======================================================================
C     CALCULATIONS FOR RECTANGULAR CONDUIT.
C=======================================================================
   20 AFULL(M) = GEOM1(M)*GEOM2(M)
      RH       = AFULL(M)/(2.0*(GEOM1(M)+GEOM2(M)))
      P1(M)    = 1.486/ROUGH(M)*AFULL(M)*RH**0.6666667
      P5(M)    = GEOM1(M)/GEOM2(M)
      P6(M)    = -1.333333*P5(M)
      P7(M)    = 2.0*P5(M) + 2.0
      P2(M)    = GEOM1(M)/DIST(M)
      P4(M)    = PSI(ALFMAX(NTPE))
      GO TO 150
C=======================================================================
C     CALCULATIONS FOR MODIFIED BASKET-HANDLE CONDUIT.
C=======================================================================
  100 GEOM3(M) = GEOM1(M)*GEOM2(M)
      P5(M)    = 0.7853982*GEOM2(M)*GEOM2(M)
      AFULL(M) = GEOM3(M)+P5(M)/2.0
      RH       = AFULL(M)/(GEOM2(M)*2.570796 +2.0*GEOM1(M))
      P1(M)    = 1.486/ROUGH(M)*AFULL(M)*RH**0.6666667
      RH       = GEOM3(M)/(2.0*(GEOM1(M)+GEOM2(M)))
      P6(M)    = 1.486/ROUGH(M)*GEOM3(M)*RH**0.6666667/P1(M)
      ALM      = (GEOM3(M)+P5(M)*(ALFMAX(1)-0.5))/AFULL(M)
      P7(M)    = 2.0*GEOM1(M)/GEOM2(M)+2.0
      P2(M)    = (GEOM1(M)+GEOM2(M)/2.0)/DIST(M)
      P4(M)    = PSI(ALM)
      GO TO 150
C=======================================================================
C     CALCULATIONS FOR RECTANGULAR CONDUIT, TRIANGULAR BOTTOM.
C=======================================================================
  110 AFULL(M) = GEOM2(M)*(GEOM1(M)-GEOM3(M)/2.0)
      P5(M)    = 0.5*SIN(ATAN(2.0*GEOM3(M)/GEOM2(M)))
      P6(M)    = GEOM3(M)/P5(M)+2.0*(GEOM1(M)-GEOM3(M))+GEOM2(M)
      RH       = P5(M)/SQRT(2.0*GEOM3(M)/GEOM2(M))*P6(M)
      P7(M)    = RH**0.6666667/AFULL(M)**0.3333333
      P1(M)    = 1.486/ROUGH(M)*AFULL(M)*(AFULL(M)/P6(M))**0.6666667
      P2(M)    = GEOM1(M)/DIST(M)
      P4(M)    = PSI(ALFMAX(NTPE))
      GO TO 150
C=======================================================================
C     CALCULATIONS FOR RECTANGULAR CONDUIT, ROUND BOTTOM.
C=======================================================================
  120 P5(M)    = 2.0*ASIN(GEOM2(M)/2.0/GEOM3(M))
      P6(M)    = GEOM3(M)*GEOM3(M)/2.0*(P5(M)-SIN(P5(M)))
      AFULL(M) = GEOM2(M)*GEOM1(M)+P6(M)
      RH       = AFULL(M)/(GEOM3(M)*P5(M)+2.0*GEOM1(M)+GEOM2(M))
      P1(M)    = 1.486/ROUGH(M)*AFULL(M)*RH**0.6666667
      AA       = 3.1415965*GEOM3(M)*GEOM3(M)
      P7(M)    = 1.486/ROUGH(M)*AA*(GEOM3(M)/2.0)**0.6666667/P1(M)
      P2(M)    = (GEOM1(M)+GEOM3(M)*(1.0-COS(P5(M)/2.0)))/DIST(M)
      P4(M)    = PSI(ALFMAX(NTPE))
      GO TO 150
C=======================================================================
C    CALCULATIONS FOR TRAPEZOIDAL CHANNEL
C=======================================================================
  130 AFULL(M) = GEOM1(M) * (GEOM2(M) + GEOM1(M)/GEOM3(M))
      P5(M)    = 2.0/SIN(ATAN(GEOM3(M)))
      P6(M)    = AFULL(M)/(GEOM2(M) + P5(M) * GEOM1(M))
      P7(M)    = (GEOM2(M) * GEOM3(M))**2/4.0
      P1(M)    = 1.486/ROUGH(M) * AFULL(M) * P6(M)**0.6666667
      P2(M)    = GEOM1(M)/DIST(M)
      P4(M)    = 1.0
      GO TO 150
C=======================================================================
C     CALCULATIONS FOR PARABOLIC, POWER FUNCTION
C                  OR NATURAL CROSS SECTION CHANNEL SHAPES
C=======================================================================
  140 KK       = NQC(M)
      RH       = QCURVE(KK,1,26)
      WP       = AFULL(M)/RH
      P1(M)    = 1.486/ROUGH(M)*AFULL(M)*RH**0.6666667
C####  WCH (CDM), 8/93.  ADD ZERO DIVIDE CHECKS.
      IF(GEOM2(M).LE.0.0) THEN
         IF(JCE.EQ.0) WRITE(N6,9000) M,NOE(M)
         IF(JCE.EQ.1) WRITE(N6,9001) M,KOE(M)
         STOP
         ENDIF
      P5(M) = GEOM1(M)/GEOM2(M)
      P6(M)    = -1.333333*P5(M)
      P7(M) = WP/GEOM2(M)
      P2(M)    = GEOM1(M)/DIST(M)
      P4(M)    = 1.0
C=======================================================================
C     CALCULATIONS COMMON TO ALL CONDUITS.
C     CONVERT SLOPE FROM FT/100 FT TO FT/FT IF ISLOPE = 0 (DEFAULT).
C=======================================================================
  150 IF(LYDIA.EQ.0) THEN
                     IF(ISLOPE.EQ.0) SLOPE(M) = SLOPE(M)*0.01
                     DXDT(M)  = DIST(M)/DT
                     ENDIF
      QFULL(M) = P1(M)*SQRT(SLOPE(M))
      QMAX(M)  = P4(M)*QFULL(M)
C#### WCH (CDM), 8/93.  CHECK FOR ZERO DIVIDE.
      IF(QFULL(M).LE.0.0) THEN
           IF(JCE.EQ.0) WRITE (N6,9010) M,NOE(M)
           IF(JCE.EQ.1) WRITE (N6,9011) M,KOE(M)
           C1(M) = 0.0
           ELSE
           C1(M) = DXDT(M)*AFULL(M)/QFULL(M)
           ENDIF
C=======================================================================
C     DETERMINE IF FLOW IN CONDUIT IS SUPER-CRITICAL MOST OF TIME.
C     CRITERION IS DERIVED BY COMPARING NORMAL AND CRITICAL VELOCITIES.
C     BOTH VELOCITIES DEPEND UPON DEPTH OF FLOW.
C     FACTOR OF 0.3 CORRESPONDS TO CIRCULAR PIPE 95% FULL.
C=======================================================================
      IF(SCF(M).EQ.GNO) THEN
                AA  = 1.486/ROUGH(M)*SQRT(SLOPE(M)/32.2)*(P2(M)*
     +                                   DIST(M))**.1666667*0.3
                              SCF(M) = GNO
                IF(AA.GE.1.0) SCF(M) = YES
                ENDIF
      IF(LYDIA.NE.1) GO TO 200
      M = MSAVE
      RETURN
C=======================================================================
C     CALCULATIONS FOR LIFT STATION.
C     INITIAL VOLUME IN WET WELL IS HALF THE CAPACITY.
C=======================================================================
  160 WELL2(M) = 0.5*GEOM1(M)
      GO TO 190
C=======================================================================
C     DETERMINE NUMBER AND ELEMENT NUMBERS OF FLOW DIVIDER TYPE 23'S.
C=======================================================================
  170 K       = K+1
      IOLD(K) = M
C=======================================================================
C     INITIALIZE SURCHARGE VOLUME IN NON-CONDUITS.
C=======================================================================
  190 SURGE1(M) = 0.0
      SURGE2(M) = 0.0
      SCF(M)    = BLANK
  200 CONTINUE
      IF(K.EQ.0)   RETURN
      IF(NTPE.NE.24) RETURN
C=======================================================================
C     INITIALIZE FLOW SETTING FOR TYPE 24 FLOW DIVIDERS.
C=======================================================================
      DO 220 I = 1,K
      M        = IOLD(I)
      L        = GEOM3(M)
      BMJ      = KGEOM(M)
      L        = NIN(L,BMJ)
  220 GEOM1(M) = (QFULL(L)/2.0)*BARREL(L)
      RETURN
C=======================================================================
 9000 FORMAT (/,' ERROR.  GEOM2 CANNOT BE ZERO FOR THIS ELEMENT.',/,
     1 ' ORDER OF INPUT =',I3,', ELEMENT NO. =',I10)
 9001 FORMAT (/,' ERROR.  GEOM2 CANNOT BE ZERO FOR THIS ELEMENT.',/,
     1 ' ORDER OF INPUT =',I3,', ELEMENT NO. =',A10)
 9010 FORMAT (/,' WARNING!!! THIS CONDUIT SHOULD NOT HAVE QFULL = 0.0'
     1,/,' ORDER OF INPUT =',I3,', ELEMENT NO. =',I10,/,
     2 ' RUN CONTINUES BUT ERROR LIKELY TO FOLLOW.')
 9011 FORMAT (/,' WARNING!!! THIS CONDUIT SHOULD NOT HAVE QFULL = 0.0'
     1,/,' ORDER OF INPUT =',I3,', ELEMENT NO. =',A10,/,
     2 ' RUN CONTINUES BUT ERROR LIKELY TO FOLLOW.')
      END
